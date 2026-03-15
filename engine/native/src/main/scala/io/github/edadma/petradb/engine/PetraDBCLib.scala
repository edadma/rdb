package io.github.edadma.petradb.engine

import scala.scalanative.unsafe.*
import scala.collection.mutable

import io.github.edadma.petradb.{Session as PSession, *}

// ── Handle management ──────────────────────────────────────────────

private val _dbs = mutable.HashMap[Int, DB]()
private val _sessions = mutable.HashMap[Int, Session]()
private val _cursors = mutable.HashMap[Int, Cursor]()
private var _nextHandle = 1
// Keep references to returned strings so they aren't GC'd before C reads them
private var _lastReturnedString: Array[Byte] = Array.empty
private var _lastErrorBytes: Array[Byte] = Array(0)

private def newHandle(): Int =
  val h = _nextHandle
  _nextHandle += 1
  h

private var _lastError: String = ""

private def withError[T](default: T)(f: => T): T =
  try
    _lastError = ""
    f
  catch
    case e: Throwable =>
      _lastError = Option(e.getMessage).getOrElse(e.getClass.getName)
      default

// Return a C string that stays valid until the next call.
// We pin the byte array in _lastReturnedString to prevent GC.
private def returnString(s: String): CString =
  val bytes = (s + "\u0000").getBytes("UTF-8")
  _lastReturnedString = bytes
  bytes.at(0).asInstanceOf[CString]

private def returnError(): CString =
  val bytes = (_lastError + "\u0000").getBytes("UTF-8")
  _lastErrorBytes = bytes
  bytes.at(0).asInstanceOf[CString]

// ── Database lifecycle ─────────────────────────────────────────────

@exported("petradb_open")
def petradb_open(): Int = withError(0) {
  val db = new MemoryDB()
  val h = newHandle()
  _dbs(h) = db
  h
}

@exported("petradb_open_persistent")
def petradb_open_persistent(path: CString): Int = withError(0) {
  val p = fromCString(path)
  val db =
    if io.github.edadma.cross_platform.exists(p) then PersistentDB.open(p)
    else PersistentDB.create(p, 4096)
  val h = newHandle()
  _dbs(h) = db
  h
}

// ── Function context and value handles ──────────────────────────────

private class FuncContext:
  var result: Value = NullValue()
  var error: Option[String] = None
  var userData: Ptr[Byte] = null

private val _values = mutable.HashMap[Int, Value]()
private val _contexts = mutable.HashMap[Int, FuncContext]()

// ── Value accessors (for function arguments) ───────────────────────

@exported("petradb_value_int")
def petradb_value_int(valueHandle: Int): Int =
  _values.get(valueHandle).map(v => if v.isNull then 0 else v.intValue).getOrElse(0)

@exported("petradb_value_int64")
def petradb_value_int64(valueHandle: Int): Long =
  _values.get(valueHandle).map(v => if v.isNull then 0L else v.longValue).getOrElse(0L)

@exported("petradb_value_double")
def petradb_value_double(valueHandle: Int): Double =
  _values.get(valueHandle).map(v => if v.isNull then 0.0 else v.doubleValue).getOrElse(0.0)

@exported("petradb_value_text")
def petradb_value_text(valueHandle: Int): CString =
  _values.get(valueHandle) match
    case Some(v) if !v.isNull => returnString(v.string)
    case _                    => null

@exported("petradb_value_type")
def petradb_value_type(valueHandle: Int): Int =
  _values.get(valueHandle) match
    case Some(v) if v.isNull  => 5 // NULL
    case Some(_: NumberValue) => 1 // INTEGER
    case Some(_: TextValue)   => 3 // TEXT
    case Some(_: BooleanValue) => 1 // INTEGER
    case Some(_: ByteaValue)  => 4 // BLOB
    case Some(_)              => 3 // TEXT fallback
    case None                 => 5

@exported("petradb_value_is_null")
def petradb_value_is_null(valueHandle: Int): Int =
  _values.get(valueHandle).map(v => if v.isNull then 1 else 0).getOrElse(1)

// ── Result setters (for function return value) ─────────────────────

@exported("petradb_result_int")
def petradb_result_int(ctxHandle: Int, value: Int): Unit =
  _contexts.get(ctxHandle).foreach(_.result = NumberValue(value))

@exported("petradb_result_int64")
def petradb_result_int64(ctxHandle: Int, value: Long): Unit =
  _contexts.get(ctxHandle).foreach(_.result = NumberValue(value.toInt))

@exported("petradb_result_double")
def petradb_result_double(ctxHandle: Int, value: Double): Unit =
  _contexts.get(ctxHandle).foreach(_.result = NumberValue(value))

@exported("petradb_result_text")
def petradb_result_text(ctxHandle: Int, value: CString): Unit =
  _contexts.get(ctxHandle).foreach { ctx =>
    if value == null then ctx.result = NullValue()
    else ctx.result = TextValue(fromCString(value))
  }

@exported("petradb_result_null")
def petradb_result_null(ctxHandle: Int): Unit =
  _contexts.get(ctxHandle).foreach(_.result = NullValue())

@exported("petradb_result_error")
def petradb_result_error(ctxHandle: Int, msg: CString): Unit =
  _contexts.get(ctxHandle).foreach(_.error = Some(fromCString(msg)))

@exported("petradb_user_data")
def petradb_user_data(ctxHandle: Int): Ptr[Byte] =
  _contexts.get(ctxHandle).map(_.userData).getOrElse(null)

// ── Register function ──────────────────────────────────────────────

@exported("petradb_create_function")
def petradb_create_function(dbHandle: Int, name: CString, nArgs: Int, userData: Ptr[Byte],
    callback: CFuncPtr3[Int, Int, Ptr[Int], Unit]): Int = withError(-1) {
  _dbs.get(dbHandle) match
    case Some(db) =>
      val funcName = fromCString(name)
      val pinnedUserData = userData
      db.registerScalarFunction(funcName, { args =>
        // Create context
        val ctxHandle = newHandle()
        val ctx = new FuncContext()
        ctx.userData = pinnedUserData
        _contexts(ctxHandle) = ctx

        // Create value handles for arguments
        val argHandles = stackalloc[Int](args.length)
        for i <- args.indices do
          val vh = newHandle()
          _values(vh) = args(i)
          argHandles(i) = vh

        // Call the native function
        callback(ctxHandle, args.length, argHandles)

        // Clean up value handles
        for i <- args.indices do
          _values.remove(argHandles(i))

        // Get result and clean up context
        val result = ctx.result
        val err = ctx.error
        _contexts.remove(ctxHandle)

        err match
          case Some(msg) => sys.error(msg)
          case None      => result
      })
      0
    case None =>
      _lastError = "invalid database handle"
      -1
}

@exported("petradb_close")
def petradb_close(dbHandle: Int): Int = withError(-1) {
  _dbs.remove(dbHandle) match
    case Some(db) =>
      _sessions.filterInPlace((_, s) => s.db ne db)
      db.close()
      0
    case None =>
      _lastError = "invalid database handle"
      -1
}

// ── Session (connection) ───────────────────────────────────────────

@exported("petradb_connect")
def petradb_connect(dbHandle: Int): Int = withError(0) {
  _dbs.get(dbHandle) match
    case Some(db) =>
      val session = db.connect()
      val h = newHandle()
      _sessions(h) = session
      h
    case None =>
      _lastError = "invalid database handle"
      0
}

// ── Execute (no results) ───────────────────────────────────────────

@exported("petradb_exec")
def petradb_exec(sessionHandle: Int, sql: CString): Int = withError(-1) {
  _sessions.get(sessionHandle) match
    case Some(session) =>
      given Session = session
      val results = executeSQL(fromCString(sql))
      results.lastOption match
        case Some(UpdateResult(n))    => n
        case Some(DeleteResult(n))    => n
        case Some(InsertResult(_, t)) => t.data.length
        case Some(CopyResult(n))      => n
        case Some(_)                  => 0
        case None                     => 0
    case None =>
      _lastError = "invalid session handle"
      -1
}

// ── Cursor (prepared query) ────────────────────────────────────────

@exported("petradb_prepare")
def petradb_prepare(sessionHandle: Int, sql: CString): Int = withError(0) {
  _sessions.get(sessionHandle) match
    case Some(session) =>
      val cursor = session.openCursor(fromCString(sql))
      val h = newHandle()
      _cursors(h) = cursor
      h
    case None =>
      _lastError = "invalid session handle"
      0
}

@exported("petradb_step")
def petradb_step(cursorHandle: Int): Int = withError(-1) {
  _cursors.get(cursorHandle) match
    case Some(cursor) => if cursor.step() then 1 else 0
    case None =>
      _lastError = "invalid cursor handle"
      -1
}

@exported("petradb_finalize")
def petradb_finalize(cursorHandle: Int): Int = withError(-1) {
  _cursors.remove(cursorHandle) match
    case Some(cursor) =>
      cursor.close()
      0
    case None =>
      _lastError = "invalid cursor handle"
      -1
}

// ── Column metadata ────────────────────────────────────────────────

@exported("petradb_column_count")
def petradb_column_count(cursorHandle: Int): Int = withError(-1) {
  _cursors.get(cursorHandle) match
    case Some(cursor) => cursor.columnCount
    case None =>
      _lastError = "invalid cursor handle"
      -1
}

@exported("petradb_column_name")
def petradb_column_name(cursorHandle: Int, index: Int): CString = withError(null) {
  _cursors.get(cursorHandle) match
    case Some(cursor) => returnString(cursor.columnName(index))
    case None =>
      _lastError = "invalid cursor handle"
      null
}

// ── Column value accessors ─────────────────────────────────────────

@exported("petradb_column_type")
def petradb_column_type(cursorHandle: Int, index: Int): Int = withError(0) {
  _cursors.get(cursorHandle) match
    case Some(cursor) =>
      if cursor.columnIsNull(index) then 5
      else cursor.columnValue(index) match
        case _: NumberValue  => 1
        case _: TextValue    => 3
        case _: BooleanValue => 1
        case _: ByteaValue   => 4
        case _: ArrayValue   => 3
        case _               => 3
    case None =>
      _lastError = "invalid cursor handle"
      0
}

@exported("petradb_column_int")
def petradb_column_int(cursorHandle: Int, index: Int): Int = withError(0) {
  _cursors.get(cursorHandle) match
    case Some(cursor) =>
      if cursor.columnIsNull(index) then 0
      else cursor.columnValue(index) match
        case BooleanValue(b) => if b then 1 else 0
        case v               => v.intValue
    case None =>
      _lastError = "invalid cursor handle"
      0
}

@exported("petradb_column_int64")
def petradb_column_int64(cursorHandle: Int, index: Int): Long = withError(0L) {
  _cursors.get(cursorHandle) match
    case Some(cursor) =>
      if cursor.columnIsNull(index) then 0L
      else cursor.columnLong(index)
    case None =>
      _lastError = "invalid cursor handle"
      0L
}

@exported("petradb_column_double")
def petradb_column_double(cursorHandle: Int, index: Int): Double = withError(0.0) {
  _cursors.get(cursorHandle) match
    case Some(cursor) =>
      if cursor.columnIsNull(index) then 0.0
      else cursor.columnDouble(index)
    case None =>
      _lastError = "invalid cursor handle"
      0.0
}

@exported("petradb_column_text")
def petradb_column_text(cursorHandle: Int, index: Int): CString = withError(null) {
  _cursors.get(cursorHandle) match
    case Some(cursor) =>
      if cursor.columnIsNull(index) then null
      else returnString(cursor.columnText(index))
    case None =>
      _lastError = "invalid cursor handle"
      null
}

@exported("petradb_column_blob")
def petradb_column_blob(cursorHandle: Int, index: Int): Ptr[Byte] = withError(null) {
  _cursors.get(cursorHandle) match
    case Some(cursor) =>
      if cursor.columnIsNull(index) then null
      else cursor.columnValue(index) match
        case ByteaValue(data) =>
          _lastReturnedString = data
          data.at(0)
        case ArrayValue(data) =>
          val bytes = data.map(_.intValue.toByte).toArray
          _lastReturnedString = bytes
          bytes.at(0)
        case v =>
          val bytes = v.string.getBytes("UTF-8")
          _lastReturnedString = bytes
          bytes.at(0)
    case None =>
      _lastError = "invalid cursor handle"
      null
}

@exported("petradb_column_bytes")
def petradb_column_bytes(cursorHandle: Int, index: Int): Int = withError(0) {
  _cursors.get(cursorHandle) match
    case Some(cursor) =>
      if cursor.columnIsNull(index) then 0
      else cursor.columnValue(index) match
        case ByteaValue(data) => data.length
        case ArrayValue(data) => data.length
        case v                => v.string.getBytes("UTF-8").length
    case None =>
      _lastError = "invalid cursor handle"
      0
}

@exported("petradb_column_is_null")
def petradb_column_is_null(cursorHandle: Int, index: Int): Int = withError(-1) {
  _cursors.get(cursorHandle) match
    case Some(cursor) => if cursor.columnIsNull(index) then 1 else 0
    case None =>
      _lastError = "invalid cursor handle"
      -1
}

// ── Error reporting ────────────────────────────────────────────────

@exported("petradb_errmsg")
def petradb_errmsg(): CString = returnError()

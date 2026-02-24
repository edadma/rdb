package io.github.edadma.petradb.jdbc

import io.github.edadma.petradb.Result

import java.sql.{SQLFeatureNotSupportedException, SQLWarning, Savepoint}
import java.util.Properties
import java.util.concurrent.Executor

abstract class AbstractConnection extends java.sql.Connection:

  def execute(sql: String): Seq[Result]
  def doClose(): Unit
  def url: String
  def username: String

  private var _closed         = false
  private var _autoCommit     = true
  private var _inTransaction  = false

  def createStatement(): java.sql.Statement = new PetraStatement(this)
  def prepareStatement(sql: String): java.sql.PreparedStatement = new PetraPreparedStatement(this, sql)

  def commit(): Unit =
    execute("COMMIT")
    _inTransaction = false
    if !_autoCommit then
      execute("BEGIN")
      _inTransaction = true

  def rollback(): Unit =
    execute("ROLLBACK")
    _inTransaction = false
    if !_autoCommit then
      execute("BEGIN")
      _inTransaction = true

  def setAutoCommit(b: Boolean): Unit =
    if b != _autoCommit then
      _autoCommit = b
      if !b then
        execute("BEGIN")
        _inTransaction = true
      else if _inTransaction then
        execute("COMMIT")
        _inTransaction = false

  def getAutoCommit: Boolean = _autoCommit
  def isClosed: Boolean      = _closed

  def close(): Unit =
    if !_closed then
      doClose()
      _closed = true

  def getMetaData(): java.sql.DatabaseMetaData = new PetraDatabaseMetaData(this)

  // ── Warnings ───────────────────────────────────────────────────────
  def getWarnings(): SQLWarning = null
  def clearWarnings(): Unit     = ()

  // ── Misc ───────────────────────────────────────────────────────────
  def nativeSQL(sql: String): String = sql
  def isReadOnly: Boolean            = false
  def setReadOnly(b: Boolean): Unit  = ()
  def getCatalog: String             = null
  def setCatalog(catalog: String): Unit = ()
  def getTransactionIsolation: Int   = java.sql.Connection.TRANSACTION_READ_COMMITTED
  def setTransactionIsolation(level: Int): Unit = ()
  def getTypeMap: java.util.Map[String, Class[?]] = java.util.Collections.emptyMap()
  def setTypeMap(map: java.util.Map[String, Class[?]]): Unit = throw SQLFeatureNotSupportedException()
  def getHoldability: Int = java.sql.ResultSet.HOLD_CURSORS_OVER_COMMIT
  def setHoldability(h: Int): Unit = throw SQLFeatureNotSupportedException()

  // ── Savepoints (unsupported) ───────────────────────────────────────
  def setSavepoint(): Savepoint              = throw SQLFeatureNotSupportedException()
  def setSavepoint(name: String): Savepoint  = throw SQLFeatureNotSupportedException()
  def rollback(sp: Savepoint): Unit          = throw SQLFeatureNotSupportedException()
  def releaseSavepoint(sp: Savepoint): Unit  = throw SQLFeatureNotSupportedException()

  // ── createStatement / prepareStatement overloads ───────────────────
  def createStatement(t: Int, c: Int): java.sql.Statement = new PetraStatement(this)
  def createStatement(t: Int, c: Int, h: Int): java.sql.Statement = new PetraStatement(this)
  def prepareStatement(sql: String, t: Int, c: Int): java.sql.PreparedStatement = new PetraPreparedStatement(this, sql)
  def prepareStatement(sql: String, t: Int, c: Int, h: Int): java.sql.PreparedStatement = new PetraPreparedStatement(this, sql)
  def prepareStatement(sql: String, autoGenKeys: Int): java.sql.PreparedStatement = new PetraPreparedStatement(this, sql)
  def prepareStatement(sql: String, colIndexes: Array[Int]): java.sql.PreparedStatement = new PetraPreparedStatement(this, sql)
  def prepareStatement(sql: String, colNames: Array[String]): java.sql.PreparedStatement = new PetraPreparedStatement(this, sql)

  // ── CallableStatement (unsupported) ───────────────────────────────
  def prepareCall(sql: String): java.sql.CallableStatement = throw SQLFeatureNotSupportedException()
  def prepareCall(sql: String, t: Int, c: Int): java.sql.CallableStatement = throw SQLFeatureNotSupportedException()
  def prepareCall(sql: String, t: Int, c: Int, h: Int): java.sql.CallableStatement = throw SQLFeatureNotSupportedException()

  // ── Large object creation (unsupported) ───────────────────────────
  def createClob(): java.sql.Clob    = throw SQLFeatureNotSupportedException()
  def createBlob(): java.sql.Blob    = throw SQLFeatureNotSupportedException()
  def createNClob(): java.sql.NClob  = throw SQLFeatureNotSupportedException()
  def createSQLXML(): java.sql.SQLXML = throw SQLFeatureNotSupportedException()

  // ── Validity ────────────────────────────────────────────────────────
  def isValid(timeout: Int): Boolean = !_closed

  // ── Client info (unsupported) ────────────────────────────────────
  def setClientInfo(name: String, value: String): Unit = throw java.sql.SQLClientInfoException()
  def setClientInfo(props: Properties): Unit           = throw java.sql.SQLClientInfoException()
  def getClientInfo(name: String): String  = null
  def getClientInfo(): Properties          = new Properties()

  // ── Arrays / Structs (unsupported) ────────────────────────────────
  def createArrayOf(typeName: String, elements: Array[AnyRef]): java.sql.Array = throw SQLFeatureNotSupportedException()
  def createStruct(typeName: String, attrs: Array[AnyRef]): java.sql.Struct    = throw SQLFeatureNotSupportedException()

  // ── Schema ─────────────────────────────────────────────────────────
  def getSchema: String            = null
  def setSchema(schema: String): Unit = ()

  // ── Network / Abort (unsupported) ─────────────────────────────────
  def abort(exec: Executor): Unit  = throw SQLFeatureNotSupportedException()
  def setNetworkTimeout(exec: Executor, ms: Int): Unit = throw SQLFeatureNotSupportedException()
  def getNetworkTimeout: Int       = 0

  // ── Wrapper ────────────────────────────────────────────────────────
  def unwrap[T](iface: Class[T]): T         = throw java.sql.SQLException("not a wrapper")
  def isWrapperFor(iface: Class[?]): Boolean = false

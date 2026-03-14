package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

/** A cursor provides row-by-row access to query results without materializing
  * all rows in memory. Used both by the programmatic API (sqlite3_step style)
  * and will later back SQL cursors (DECLARE CURSOR / FETCH / CLOSE).
  *
  * Usage:
  * {{{
  * val cursor = session.openCursor("SELECT * FROM users WHERE age > 25")
  * while cursor.step() do
  *   val name = cursor.columnText(0)
  *   val age = cursor.columnInt(1)
  * cursor.close()
  * }}}
  */
class Cursor private[engine] (
    private val process: Process,
    private val ctx: Seq[Row] = Nil,
):
  private val iter: RowIterator = process.iterator(ctx)
  private var _currentRow: Option[Row] = None
  private var _closed: Boolean = false
  private var _rowCount: Int = 0

  /** The result schema — column names, types, and count. */
  val metadata: Metadata = process.meta

  /** Number of columns in the result. */
  def columnCount: Int = metadata.width

  /** Column name by index (0-based). */
  def columnName(index: Int): String = metadata.columns(index).name

  /** Column type by index (0-based). */
  def columnType(index: Int): Type = metadata.columns(index).typ

  /** Advance to the next row.
    * @return true if a row is available, false if no more rows
    */
  def step(): Boolean =
    require(!_closed, "cursor is closed")
    if iter.hasNext then
      _currentRow = Some(iter.next())
      _rowCount += 1
      true
    else
      _currentRow = None
      false

  /** Fetch up to `n` rows at once. For future SQL FETCH support. */
  def fetch(n: Int): Seq[Row] =
    require(!_closed, "cursor is closed")
    val buf = scala.collection.mutable.ArrayBuffer[Row]()
    var i = 0
    while i < n && iter.hasNext do
      val row = iter.next()
      buf += row
      _rowCount += 1
      i += 1
    _currentRow = buf.lastOption
    buf.toSeq

  /** Skip up to `n` rows. Returns the number of rows actually skipped. */
  def move(n: Int): Int =
    require(!_closed, "cursor is closed")
    var moved = 0
    while moved < n && iter.hasNext do
      iter.next()
      _rowCount += 1
      moved += 1
    _currentRow = None
    moved

  /** The current row's raw Value at the given column index. */
  def columnValue(index: Int): Value =
    _currentRow match
      case Some(row) => row.data(index)
      case None      => throw new IllegalStateException("no current row — call step() first")

  /** Whether the current row's column is NULL. */
  def columnIsNull(index: Int): Boolean = columnValue(index).isNull

  /** Get the current row's column as an Int. */
  def columnInt(index: Int): Int = columnValue(index).intValue

  /** Get the current row's column as a Long. */
  def columnLong(index: Int): Long = columnValue(index).longValue

  /** Get the current row's column as a Double. */
  def columnDouble(index: Int): Double = columnValue(index).doubleValue

  /** Get the current row's column as a String. */
  def columnText(index: Int): String = columnValue(index).string

  /** Get the current row's column as a Boolean. */
  def columnBoolean(index: Int): Boolean = columnValue(index).asInstanceOf[BooleanValue].b

  /** Number of rows consumed so far. */
  def rowCount: Int = _rowCount

  /** Whether the cursor has been closed. */
  def isClosed: Boolean = _closed

  /** Close the cursor, releasing resources. */
  def close(): Unit =
    _closed = true
    _currentRow = None

  /** Materialize all remaining rows into a Seq. Useful for small results
    * or when you need random access.
    */
  def toSeq: Seq[Row] =
    require(!_closed, "cursor is closed")
    val buf = scala.collection.mutable.ArrayBuffer[Row]()
    while iter.hasNext do
      buf += iter.next()
      _rowCount += 1
    _currentRow = buf.lastOption
    buf.toSeq

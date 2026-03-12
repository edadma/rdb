package io.github.edadma.petradb.jdbc

import io.github.edadma.petradb.*

class PetraResultSet(table: TableValue) extends AbstractResultSet:

  private var rowIdx   = -1
  private var _wasNull = false
  private var _closed  = false

  // ── Navigation ──────────────────────────────────────────────────────
  override def next(): Boolean =
    rowIdx += 1
    rowIdx < table.data.length

  override def close(): Unit = _closed = true
  override def isClosed(): Boolean = _closed
  override def wasNull(): Boolean  = _wasNull

  // ── Metadata ────────────────────────────────────────────────────────
  override def getMetaData(): java.sql.ResultSetMetaData =
    new PetraResultSetMetaData(table.meta)

  override def findColumn(name: String): Int =
    table.meta.columnMap.get(name) match
      case Some((idx, _, _)) => idx + 1
      case None => throw java.sql.SQLException(s"column '$name' not found")

  // ── Core value accessor ─────────────────────────────────────────────
  private def getValue(col: Int): Value =
    if rowIdx < 0 || rowIdx >= table.data.length then
      throw java.sql.SQLException("no current row")
    val row = table.data(rowIdx)
    if col < 1 || col > row.data.length then
      throw java.sql.SQLException(s"column index $col out of range")
    row.data(col - 1)

  private def withNull[A](col: Int, ifNull: A)(f: Value => A): A =
    val v = getValue(col)
    if v.isNull then
      _wasNull = true
      ifNull
    else
      _wasNull = false
      f(v)

  // ── getString ───────────────────────────────────────────────────────
  override def getString(col: Int): String =
    withNull(col, null)(v => v.string)

  override def getString(label: String): String =
    getString(findColumn(label))

  // ── getBoolean ──────────────────────────────────────────────────────
  override def getBoolean(col: Int): Boolean =
    withNull(col, false) {
      case BooleanValue(b)   => b
      case NumberValue(_, n) => n.intValue != 0
      case v                 => v.string.trim.toLowerCase match
        case "true" | "t" | "1" | "yes" => true
        case _                           => false
    }

  override def getBoolean(label: String): Boolean = getBoolean(findColumn(label))

  // ── getByte / getShort ──────────────────────────────────────────────
  override def getByte(col: Int): Byte =
    withNull(col, 0.toByte)(v => v.byteValue)

  override def getByte(label: String): Byte = getByte(findColumn(label))

  override def getShort(col: Int): Short =
    withNull(col, 0.toShort)(v => v.shortValue)

  override def getShort(label: String): Short = getShort(findColumn(label))

  // ── getInt ──────────────────────────────────────────────────────────
  override def getInt(col: Int): Int =
    withNull(col, 0) {
      case NumberValue(_, n) => n.intValue
      case BooleanValue(b)   => if b then 1 else 0
      case v                 => v.string.trim.toInt
    }

  override def getInt(label: String): Int = getInt(findColumn(label))

  // ── getLong ─────────────────────────────────────────────────────────
  override def getLong(col: Int): Long =
    withNull(col, 0L) {
      case NumberValue(_, n) => n.longValue
      case BooleanValue(b)   => if b then 1L else 0L
      case v                 => v.string.trim.toLong
    }

  override def getLong(label: String): Long = getLong(findColumn(label))

  // ── getFloat / getDouble ────────────────────────────────────────────
  override def getFloat(col: Int): Float =
    withNull(col, 0f)(v => v.floatValue)

  override def getFloat(label: String): Float = getFloat(findColumn(label))

  override def getDouble(col: Int): Double =
    withNull(col, 0.0) {
      case NumberValue(_, n) => n.doubleValue
      case v                 => v.string.trim.toDouble
    }

  override def getDouble(label: String): Double = getDouble(findColumn(label))

  // ── getBigDecimal ───────────────────────────────────────────────────
  override def getBigDecimal(col: Int): java.math.BigDecimal =
    withNull(col, null) {
      case NumberValue(_, n) => new java.math.BigDecimal(n.toString)
      case v                 => new java.math.BigDecimal(v.string.trim)
    }

  override def getBigDecimal(label: String): java.math.BigDecimal =
    getBigDecimal(findColumn(label))

  override def getBigDecimal(col: Int, scale: Int): java.math.BigDecimal =
    val bd = getBigDecimal(col)
    if bd == null then null else bd.setScale(scale, java.math.RoundingMode.HALF_UP)

  override def getBigDecimal(label: String, scale: Int): java.math.BigDecimal =
    getBigDecimal(findColumn(label), scale)

  // ── getDate / getTimestamp ──────────────────────────────────────────
  override def getDate(col: Int): java.sql.Date =
    withNull(col, null) {
      case DateValue(d)       => java.sql.Date.valueOf(d)
      case TimestampValue(ts) => java.sql.Date.valueOf(ts.toLocalDate)
      case v                  => java.sql.Date.valueOf(v.string.trim)
    }

  override def getDate(label: String): java.sql.Date = getDate(findColumn(label))

  override def getTimestamp(col: Int): java.sql.Timestamp =
    withNull(col, null) {
      case TimestampValue(ts)   => java.sql.Timestamp.valueOf(ts)
      case TimestampTZValue(ts) => java.sql.Timestamp.from(ts.toInstant)
      case DateValue(d)         => java.sql.Timestamp.valueOf(d.atStartOfDay)
      case v                    => java.sql.Timestamp.valueOf(v.string.trim)
    }

  override def getTimestamp(label: String): java.sql.Timestamp =
    getTimestamp(findColumn(label))

  // ── getObject ───────────────────────────────────────────────────────
  override def getObject(col: Int): AnyRef =
    val v = getValue(col)
    if v.isNull then { _wasNull = true; null }
    else { _wasNull = false; valueToObject(v) }

  override def getObject(label: String): AnyRef = getObject(findColumn(label))

  // ── getRow ──────────────────────────────────────────────────────────
  override def getRow(): Int = if rowIdx >= 0 && rowIdx < table.data.length then rowIdx + 1 else 0

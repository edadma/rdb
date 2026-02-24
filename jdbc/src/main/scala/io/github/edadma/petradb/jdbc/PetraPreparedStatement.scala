package io.github.edadma.petradb.jdbc

import io.github.edadma.petradb.*

import java.sql.SQLFeatureNotSupportedException
import scala.collection.mutable

class PetraPreparedStatement(conn: AbstractConnection, sql: String)
    extends PetraStatement(conn)
    with java.sql.PreparedStatement:

  private val params = new mutable.HashMap[Int, Any]()  // 1-based index → value

  // ── Parameter formatting ────────────────────────────────────────────
  private def formatParam(idx: Int): String =
    val value = params.getOrElse(idx, throw java.sql.SQLException(s"parameter $idx not set"))
    value match
      case null                         => "NULL"
      case s: String                    => "'" + s.replace("'", "''") + "'"
      case bd: java.math.BigDecimal     => bd.toPlainString
      case b: java.lang.Boolean         => if b.booleanValue then "true" else "false"
      case n: Number                    => n.toString
      case _                            => value.toString

  private def buildSql(): String =
    val sb        = new StringBuilder
    var paramIdx  = 1
    var i         = 0
    while i < sql.length do
      if sql.charAt(i) == '?' then
        sb.append(formatParam(paramIdx))
        paramIdx += 1
      else
        sb.append(sql.charAt(i))
      i += 1
    sb.toString

  // ── No-arg execute variants ─────────────────────────────────────────
  override def execute(): Boolean =
    processResults(conn.execute(buildSql()))

  override def executeQuery(): java.sql.ResultSet =
    conn.execute(buildSql()).head match
      case QueryResult(table) => new PetraResultSet(table)
      case _ => throw java.sql.SQLException("query did not return a result set")

  override def executeUpdate(): Int =
    val results = conn.execute(buildSql())
    if results.isEmpty then 0
    else results.head match
      case UpdateResult(n)    => n
      case DeleteResult(n)    => n
      case InsertResult(_, _) => 1
      case _                  => 0

  // ── Parameter setters ───────────────────────────────────────────────
  override def clearParameters(): Unit = params.clear()

  override def setNull(paramIndex: Int, sqlType: Int): Unit               = params(paramIndex) = null
  override def setNull(paramIndex: Int, sqlType: Int, typeName: String): Unit = params(paramIndex) = null
  override def setBoolean(paramIndex: Int, x: Boolean): Unit              = params(paramIndex) = java.lang.Boolean.valueOf(x)
  override def setByte(paramIndex: Int, x: Byte): Unit                    = params(paramIndex) = java.lang.Integer.valueOf(x.toInt)
  override def setShort(paramIndex: Int, x: Short): Unit                  = params(paramIndex) = java.lang.Integer.valueOf(x.toInt)
  override def setInt(paramIndex: Int, x: Int): Unit                      = params(paramIndex) = java.lang.Integer.valueOf(x)
  override def setLong(paramIndex: Int, x: Long): Unit                    = params(paramIndex) = java.lang.Long.valueOf(x)
  override def setFloat(paramIndex: Int, x: Float): Unit                  = params(paramIndex) = java.lang.Double.valueOf(x.toDouble)
  override def setDouble(paramIndex: Int, x: Double): Unit                = params(paramIndex) = java.lang.Double.valueOf(x)
  override def setBigDecimal(paramIndex: Int, x: java.math.BigDecimal): Unit = params(paramIndex) = x
  override def setString(paramIndex: Int, x: String): Unit                = params(paramIndex) = x
  override def setObject(paramIndex: Int, x: AnyRef): Unit                = params(paramIndex) = x
  override def setObject(paramIndex: Int, x: AnyRef, targetSqlType: Int): Unit = params(paramIndex) = x
  override def setObject(paramIndex: Int, x: AnyRef, targetSqlType: Int, scaleOrLength: Int): Unit = params(paramIndex) = x

  override def setBytes(paramIndex: Int, x: Array[Byte]): Unit             = throw SQLFeatureNotSupportedException()
  override def setDate(paramIndex: Int, x: java.sql.Date): Unit            = params(paramIndex) = x.toString
  override def setDate(paramIndex: Int, x: java.sql.Date, cal: java.util.Calendar): Unit = params(paramIndex) = x.toString
  override def setTime(paramIndex: Int, x: java.sql.Time): Unit            = throw SQLFeatureNotSupportedException()
  override def setTime(paramIndex: Int, x: java.sql.Time, cal: java.util.Calendar): Unit = throw SQLFeatureNotSupportedException()
  override def setTimestamp(paramIndex: Int, x: java.sql.Timestamp): Unit  = params(paramIndex) = x.toString
  override def setTimestamp(paramIndex: Int, x: java.sql.Timestamp, cal: java.util.Calendar): Unit = params(paramIndex) = x.toString
  override def setAsciiStream(paramIndex: Int, x: java.io.InputStream, length: Int): Unit  = throw SQLFeatureNotSupportedException()
  override def setAsciiStream(paramIndex: Int, x: java.io.InputStream, length: Long): Unit = throw SQLFeatureNotSupportedException()
  override def setAsciiStream(paramIndex: Int, x: java.io.InputStream): Unit = throw SQLFeatureNotSupportedException()
  override def setUnicodeStream(paramIndex: Int, x: java.io.InputStream, length: Int): Unit = throw SQLFeatureNotSupportedException()
  override def setBinaryStream(paramIndex: Int, x: java.io.InputStream, length: Int): Unit  = throw SQLFeatureNotSupportedException()
  override def setBinaryStream(paramIndex: Int, x: java.io.InputStream, length: Long): Unit = throw SQLFeatureNotSupportedException()
  override def setBinaryStream(paramIndex: Int, x: java.io.InputStream): Unit = throw SQLFeatureNotSupportedException()
  override def setCharacterStream(paramIndex: Int, reader: java.io.Reader, length: Int): Unit  = throw SQLFeatureNotSupportedException()
  override def setCharacterStream(paramIndex: Int, reader: java.io.Reader, length: Long): Unit = throw SQLFeatureNotSupportedException()
  override def setCharacterStream(paramIndex: Int, reader: java.io.Reader): Unit = throw SQLFeatureNotSupportedException()
  override def setRef(paramIndex: Int, x: java.sql.Ref): Unit             = throw SQLFeatureNotSupportedException()
  override def setBlob(paramIndex: Int, x: java.sql.Blob): Unit           = throw SQLFeatureNotSupportedException()
  override def setBlob(paramIndex: Int, inputStream: java.io.InputStream, length: Long): Unit = throw SQLFeatureNotSupportedException()
  override def setBlob(paramIndex: Int, inputStream: java.io.InputStream): Unit = throw SQLFeatureNotSupportedException()
  override def setClob(paramIndex: Int, x: java.sql.Clob): Unit           = throw SQLFeatureNotSupportedException()
  override def setClob(paramIndex: Int, reader: java.io.Reader, length: Long): Unit = throw SQLFeatureNotSupportedException()
  override def setClob(paramIndex: Int, reader: java.io.Reader): Unit     = throw SQLFeatureNotSupportedException()
  override def setArray(paramIndex: Int, x: java.sql.Array): Unit         = throw SQLFeatureNotSupportedException()
  override def setURL(paramIndex: Int, x: java.net.URL): Unit             = throw SQLFeatureNotSupportedException()
  override def setRowId(paramIndex: Int, x: java.sql.RowId): Unit         = throw SQLFeatureNotSupportedException()
  override def setNString(paramIndex: Int, value: String): Unit            = params(paramIndex) = value
  override def setNCharacterStream(paramIndex: Int, value: java.io.Reader, length: Long): Unit = throw SQLFeatureNotSupportedException()
  override def setNCharacterStream(paramIndex: Int, value: java.io.Reader): Unit = throw SQLFeatureNotSupportedException()
  override def setNClob(paramIndex: Int, value: java.sql.NClob): Unit     = throw SQLFeatureNotSupportedException()
  override def setNClob(paramIndex: Int, reader: java.io.Reader, length: Long): Unit = throw SQLFeatureNotSupportedException()
  override def setNClob(paramIndex: Int, reader: java.io.Reader): Unit    = throw SQLFeatureNotSupportedException()
  override def setSQLXML(paramIndex: Int, xmlObject: java.sql.SQLXML): Unit = throw SQLFeatureNotSupportedException()

  override def addBatch(): Unit = throw SQLFeatureNotSupportedException()
  override def getMetaData(): java.sql.ResultSetMetaData = null
  override def getParameterMetaData(): java.sql.ParameterMetaData = throw SQLFeatureNotSupportedException()

package io.github.edadma.petradb.jdbc

import io.github.edadma.petradb.*

import java.sql.{ResultSetMetaData, Types}

class PetraResultSetMetaData(meta: Metadata) extends java.sql.ResultSetMetaData:

  def getColumnCount(): Int = meta.columns.length

  def getColumnName(col: Int): String  = meta.columns(col - 1).name
  def getColumnLabel(col: Int): String = meta.columns(col - 1).name

  def getColumnTypeName(col: Int): String = meta.columns(col - 1).typ.name

  def getColumnType(col: Int): Int = meta.columns(col - 1).typ match
    case IntegerType | SmallintType | SerialType | SmallSerialType => Types.INTEGER
    case BigintType | BigSerialType                                 => Types.BIGINT
    case TextType | _: VarcharType | _: CharType                   => Types.VARCHAR
    case BooleanType                                                => Types.BOOLEAN
    case DoubleType                                                  => Types.DOUBLE
    case _: NumericType                                             => Types.NUMERIC
    case DateType                                                   => Types.DATE
    case TimestampType | TimestampTZType                            => Types.TIMESTAMP
    case UUIDType                                                   => Types.VARCHAR
    case _: EnumType                                                => Types.VARCHAR
    case _                                                          => Types.OTHER

  def isNullable(col: Int): Int = ResultSetMetaData.columnNullable

  def getTableName(col: Int): String =
    meta.columns(col - 1).table.getOrElse("")

  def getSchemaName(col: Int): String = ""
  def getCatalogName(col: Int): String = ""
  def getColumnDisplaySize(col: Int): Int = 0
  def getPrecision(col: Int): Int = 0
  def getScale(col: Int): Int = 0
  def isAutoIncrement(col: Int): Boolean = meta.columns(col - 1).typ match
    case SerialType | BigSerialType | SmallSerialType => true
    case _                                            => false
  def isCaseSensitive(col: Int): Boolean  = true
  def isSearchable(col: Int): Boolean     = true
  def isCurrency(col: Int): Boolean       = false
  def isSigned(col: Int): Boolean         = true
  def isReadOnly(col: Int): Boolean       = false
  def isWritable(col: Int): Boolean       = true
  def isDefinitelyWritable(col: Int): Boolean = false

  def getColumnClassName(col: Int): String = meta.columns(col - 1).typ match
    case IntegerType | SmallintType | SerialType | SmallSerialType => "java.lang.Integer"
    case BigintType | BigSerialType                                 => "java.lang.Long"
    case DoubleType                                                  => "java.lang.Double"
    case _: NumericType                                             => "java.math.BigDecimal"
    case BooleanType                                                => "java.lang.Boolean"
    case DateType                                                   => "java.sql.Date"
    case TimestampType | TimestampTZType                            => "java.sql.Timestamp"
    case _                                                          => "java.lang.String"

  def unwrap[T](iface: Class[T]): T       = throw java.sql.SQLException("not a wrapper")
  def isWrapperFor(iface: Class[?]): Boolean = false

package io.github.edadma.petradb.jdbc

import io.github.edadma.petradb.*

class PetraDatabaseMetaData(conn: AbstractConnection) extends AbstractDatabaseMetaData:

  override def getDatabaseProductName(): String    = "PetraDB"
  override def getDatabaseProductVersion(): String = "1.1.0"
  override def getDriverName(): String             = "PetraDB JDBC Driver"
  override def getDriverVersion(): String          = "1.1.0"
  override def getDriverMajorVersion(): Int        = 1
  override def getDriverMinorVersion(): Int        = 0
  override def getDatabaseMajorVersion(): Int      = 1
  override def getDatabaseMinorVersion(): Int      = 1

  override def getURL(): String      = conn.url
  override def getUserName(): String = conn.username

  override def supportsTransactions(): Boolean      = true
  override def supportsMinimumSQLGrammar(): Boolean = true

  override def getIdentifierQuoteString(): String = "\""
  override def getSearchStringEscape(): String    = "\\"

  override def getConnection(): java.sql.Connection = conn

  override def getSchemas(): java.sql.ResultSet =
    val meta = Metadata(IndexedSeq(
      ColumnMetadata(None, "TABLE_SCHEM",   TextType),
      ColumnMetadata(None, "TABLE_CATALOG", TextType),
    ))
    val rows = Vector(Row(IndexedSeq(TextValue(""), NullValue()), meta, None, None))
    new PetraResultSet(TableValue(rows, meta))

  override def getSchemas(catalog: String, schemaPattern: String): java.sql.ResultSet =
    getSchemas()

  override def getTableTypes(): java.sql.ResultSet =
    val meta = Metadata(IndexedSeq(ColumnMetadata(None, "TABLE_TYPE", TextType)))
    val rows = Vector(Row(IndexedSeq(TextValue("TABLE")), meta, None, None))
    new PetraResultSet(TableValue(rows, meta))

  override def getTables(
    catalog: String, schemaPattern: String,
    tableNamePattern: String, types: Array[String],
  ): java.sql.ResultSet =
    val meta = Metadata(IndexedSeq(
      ColumnMetadata(None, "TABLE_CAT",   TextType),
      ColumnMetadata(None, "TABLE_SCHEM", TextType),
      ColumnMetadata(None, "TABLE_NAME",  TextType),
      ColumnMetadata(None, "TABLE_TYPE",  TextType),
      ColumnMetadata(None, "REMARKS",     TextType),
    ))
    val pattern = Option(tableNamePattern).filter(_ != "%")
    val rows = conn.tableNames
      .filter(n => pattern.forall(_ == n))
      .map { name =>
        Row(
          IndexedSeq(NullValue(), TextValue(""), TextValue(name), TextValue("TABLE"), TextValue("")),
          meta, None, None,
        )
      }.toVector
    new PetraResultSet(TableValue(rows, meta))

  override def getColumns(
    catalog: String, schemaPattern: String,
    tableNamePattern: String, columnNamePattern: String,
  ): java.sql.ResultSet =
    val meta = Metadata(IndexedSeq(
      ColumnMetadata(None, "TABLE_CAT",         TextType),
      ColumnMetadata(None, "TABLE_SCHEM",       TextType),
      ColumnMetadata(None, "TABLE_NAME",        TextType),
      ColumnMetadata(None, "COLUMN_NAME",       TextType),
      ColumnMetadata(None, "DATA_TYPE",         IntegerType),
      ColumnMetadata(None, "TYPE_NAME",         TextType),
      ColumnMetadata(None, "COLUMN_SIZE",       IntegerType),
      ColumnMetadata(None, "BUFFER_LENGTH",     IntegerType),
      ColumnMetadata(None, "DECIMAL_DIGITS",    IntegerType),
      ColumnMetadata(None, "NUM_PREC_RADIX",    IntegerType),
      ColumnMetadata(None, "NULLABLE",          IntegerType),
      ColumnMetadata(None, "REMARKS",           TextType),
      ColumnMetadata(None, "COLUMN_DEF",        TextType),
      ColumnMetadata(None, "SQL_DATA_TYPE",     IntegerType),
      ColumnMetadata(None, "SQL_DATETIME_SUB",  IntegerType),
      ColumnMetadata(None, "CHAR_OCTET_LENGTH", IntegerType),
      ColumnMetadata(None, "ORDINAL_POSITION",  IntegerType),
      ColumnMetadata(None, "IS_NULLABLE",       TextType),
    ))
    val tablePattern = Option(tableNamePattern).filter(_ != "%")
    val colPattern   = Option(columnNamePattern).filter(_ != "%")
    val nullable     = NumberValue(java.sql.ResultSetMetaData.columnNullable)
    val rows = conn.tableNames
      .filter(n => tablePattern.forall(_ == n))
      .flatMap { tableName =>
        conn.tableColumns(tableName).zipWithIndex.flatMap { case (col, idx) =>
          if colPattern.forall(_ == col.name) then
            Some(Row(
              IndexedSeq(
                NullValue(),                        // TABLE_CAT
                NullValue(),                        // TABLE_SCHEM
                TextValue(tableName),               // TABLE_NAME
                TextValue(col.name),                // COLUMN_NAME
                NumberValue(jdbcType(col.typ)),      // DATA_TYPE
                TextValue(col.typ.name),             // TYPE_NAME
                NumberValue(0),                      // COLUMN_SIZE
                NullValue(),                         // BUFFER_LENGTH
                NullValue(),                         // DECIMAL_DIGITS
                NumberValue(10),                     // NUM_PREC_RADIX
                nullable,                            // NULLABLE
                TextValue(""),                       // REMARKS
                NullValue(),                         // COLUMN_DEF
                NumberValue(0),                      // SQL_DATA_TYPE
                NullValue(),                         // SQL_DATETIME_SUB
                NullValue(),                         // CHAR_OCTET_LENGTH
                NumberValue(idx + 1),                // ORDINAL_POSITION
                TextValue("YES"),                    // IS_NULLABLE
              ),
              meta, None, None,
            ))
          else None
        }
      }.toVector
    new PetraResultSet(TableValue(rows, meta))

  private def jdbcType(typ: Type): Int =
    import java.sql.Types
    typ match
      case IntegerType | SmallintType | SerialType | SmallSerialType => Types.INTEGER
      case BigintType | BigSerialType                                 => Types.BIGINT
      case TextType | _: VarcharType | _: CharType                   => Types.VARCHAR
      case BooleanType                                                => Types.BOOLEAN
      case DoubleType                                                 => Types.DOUBLE
      case _: NumericType                                             => Types.NUMERIC
      case DateType                                                   => Types.DATE
      case TimestampType | TimestampTZType                            => Types.TIMESTAMP
      case UUIDType                                                   => Types.VARCHAR
      case _: EnumType                                                => Types.VARCHAR
      case _                                                          => Types.OTHER

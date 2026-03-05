package io.github.edadma.petradb.jdbc

import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

class PetraDatabaseMetaData(conn: AbstractConnection) extends AbstractDatabaseMetaData:

  override def getDatabaseProductName(): String    = "PetraDB"
  override def getDatabaseProductVersion(): String = "1.2.2"
  override def getDriverName(): String             = "PetraDB JDBC Driver"
  override def getDriverVersion(): String          = "1.2.2"
  override def getDriverMajorVersion(): Int        = 1
  override def getDriverMinorVersion(): Int        = 2
  override def getDatabaseMajorVersion(): Int      = 1
  override def getDatabaseMinorVersion(): Int      = 2

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
      ColumnMetadata(None, "TABLE_CAT",          TextType),
      ColumnMetadata(None, "TABLE_SCHEM",        TextType),
      ColumnMetadata(None, "TABLE_NAME",         TextType),
      ColumnMetadata(None, "COLUMN_NAME",        TextType),
      ColumnMetadata(None, "DATA_TYPE",          IntegerType),
      ColumnMetadata(None, "TYPE_NAME",          TextType),
      ColumnMetadata(None, "COLUMN_SIZE",        IntegerType),
      ColumnMetadata(None, "BUFFER_LENGTH",      IntegerType),
      ColumnMetadata(None, "DECIMAL_DIGITS",     IntegerType),
      ColumnMetadata(None, "NUM_PREC_RADIX",     IntegerType),
      ColumnMetadata(None, "NULLABLE",           IntegerType),
      ColumnMetadata(None, "REMARKS",            TextType),
      ColumnMetadata(None, "COLUMN_DEF",         TextType),
      ColumnMetadata(None, "SQL_DATA_TYPE",      IntegerType),
      ColumnMetadata(None, "SQL_DATETIME_SUB",   IntegerType),
      ColumnMetadata(None, "CHAR_OCTET_LENGTH",  IntegerType),
      ColumnMetadata(None, "ORDINAL_POSITION",   IntegerType),
      ColumnMetadata(None, "IS_NULLABLE",        TextType),
      ColumnMetadata(None, "IS_AUTOINCREMENT",   TextType),
      ColumnMetadata(None, "IS_GENERATEDCOLUMN", TextType),
    ))
    val tablePattern = Option(tableNamePattern).filter(_ != "%")
    val colPattern   = Option(columnNamePattern).filter(_ != "%")
    val rows = conn.tableNames
      .filter(n => tablePattern.forall(_ == n))
      .flatMap { tableName =>
        conn.tableColumnSpecs(tableName).zipWithIndex.flatMap { case (col, idx) =>
          if colPattern.forall(_ == col.name) then
            val isSerial   = col.typ == SerialType || col.typ == BigSerialType || col.typ == SmallSerialType
            val isUUID     = col.typ == UUIDType
            val isAuto     = isSerial || isUUID
            val nullable   = if col.required then java.sql.ResultSetMetaData.columnNoNulls
                             else java.sql.ResultSetMetaData.columnNullable
            val isNullStr  = if col.required then "NO" else "YES"
            val defaultStr = col.default match
              case Some(v) if !isSerial => TextValue(sqlLiteral(v))
              case _                    => NullValue()
            Some(Row(
              IndexedSeq(
                NullValue(),                                        // TABLE_CAT
                TextValue(""),                                      // TABLE_SCHEM
                TextValue(tableName),                               // TABLE_NAME
                TextValue(col.name),                                // COLUMN_NAME
                NumberValue(jdbcType(col.typ)),                     // DATA_TYPE
                TextValue(col.typ.name),                            // TYPE_NAME
                NumberValue(0),                                     // COLUMN_SIZE
                NullValue(),                                        // BUFFER_LENGTH
                NullValue(),                                        // DECIMAL_DIGITS
                NumberValue(10),                                    // NUM_PREC_RADIX
                NumberValue(nullable),                              // NULLABLE
                TextValue(""),                                      // REMARKS
                defaultStr,                                         // COLUMN_DEF
                NumberValue(0),                                     // SQL_DATA_TYPE
                NullValue(),                                        // SQL_DATETIME_SUB
                NullValue(),                                        // CHAR_OCTET_LENGTH
                NumberValue(idx + 1),                               // ORDINAL_POSITION
                TextValue(isNullStr),                               // IS_NULLABLE
                TextValue(if isAuto then "YES" else "NO"),          // IS_AUTOINCREMENT
                TextValue(if isAuto then "YES" else "NO"),          // IS_GENERATEDCOLUMN
              ),
              meta, None, None,
            ))
          else None
        }
      }.toVector
    new PetraResultSet(TableValue(rows, meta))

  override def getPrimaryKeys(
    catalog: String, schema: String, table: String,
  ): java.sql.ResultSet =
    val meta = Metadata(IndexedSeq(
      ColumnMetadata(None, "TABLE_CAT",   TextType),
      ColumnMetadata(None, "TABLE_SCHEM", TextType),
      ColumnMetadata(None, "TABLE_NAME",  TextType),
      ColumnMetadata(None, "COLUMN_NAME", TextType),
      ColumnMetadata(None, "KEY_SEQ",     IntegerType),
      ColumnMetadata(None, "PK_NAME",     TextType),
    ))
    val rows = conn.tablePrimaryKey(table) match
      case None => Vector.empty
      case Some(pk) =>
        pk.columns.zipWithIndex.map { case (colName, idx) =>
          Row(
            IndexedSeq(
              NullValue(),
              TextValue(""),
              TextValue(table),
              TextValue(colName),
              NumberValue(idx + 1),
              pk.name.map(TextValue.apply).getOrElse(NullValue()),
            ),
            meta, None, None,
          )
        }.toVector
    new PetraResultSet(TableValue(rows, meta))

  private def fkResultSetMeta = Metadata(IndexedSeq(
    ColumnMetadata(None, "PKTABLE_CAT",   TextType),
    ColumnMetadata(None, "PKTABLE_SCHEM", TextType),
    ColumnMetadata(None, "PKTABLE_NAME",  TextType),
    ColumnMetadata(None, "PKCOLUMN_NAME", TextType),
    ColumnMetadata(None, "FKTABLE_CAT",   TextType),
    ColumnMetadata(None, "FKTABLE_SCHEM", TextType),
    ColumnMetadata(None, "FKTABLE_NAME",  TextType),
    ColumnMetadata(None, "FKCOLUMN_NAME", TextType),
    ColumnMetadata(None, "KEY_SEQ",       IntegerType),
    ColumnMetadata(None, "UPDATE_RULE",   IntegerType),
    ColumnMetadata(None, "DELETE_RULE",   IntegerType),
    ColumnMetadata(None, "FK_NAME",       TextType),
    ColumnMetadata(None, "PK_NAME",       TextType),
    ColumnMetadata(None, "DEFERRABILITY", IntegerType),
  ))

  private def refActionToJdbc(action: ReferentialAction): Int = action match
    case ReferentialAction.NoAction  => java.sql.DatabaseMetaData.importedKeyNoAction
    case ReferentialAction.Restrict  => java.sql.DatabaseMetaData.importedKeyRestrict
    case ReferentialAction.Cascade   => java.sql.DatabaseMetaData.importedKeyCascade
    case ReferentialAction.SetNull   => java.sql.DatabaseMetaData.importedKeySetNull

  private def fkRow(
    meta: Metadata, pkTable: String, pkCol: String,
    fkTable: String, fkCol: String, seq: Int,
    updateRule: Int, deleteRule: Int,
    fkName: String, pkName: String,
  ): Row =
    Row(IndexedSeq(
      NullValue(),                                                   // PKTABLE_CAT
      TextValue(""),                                                 // PKTABLE_SCHEM
      TextValue(pkTable),                                            // PKTABLE_NAME
      TextValue(pkCol),                                              // PKCOLUMN_NAME
      NullValue(),                                                   // FKTABLE_CAT
      TextValue(""),                                                 // FKTABLE_SCHEM
      TextValue(fkTable),                                            // FKTABLE_NAME
      TextValue(fkCol),                                              // FKCOLUMN_NAME
      NumberValue(seq),                                              // KEY_SEQ
      NumberValue(updateRule),                                       // UPDATE_RULE
      NumberValue(deleteRule),                                       // DELETE_RULE
      if fkName.nonEmpty then TextValue(fkName) else NullValue(),    // FK_NAME
      if pkName.nonEmpty then TextValue(pkName) else NullValue(),    // PK_NAME
      NumberValue(java.sql.DatabaseMetaData.importedKeyNotDeferrable), // DEFERRABILITY
    ), meta, None, None)

  override def getImportedKeys(
    catalog: String, schema: String, table: String,
  ): java.sql.ResultSet =
    val meta = fkResultSetMeta
    val fks  = conn.tableForeignKeys(table)
    val pkNameCache = scala.collection.mutable.Map[String, String]()
    def pkName(refTable: String): String =
      pkNameCache.getOrElseUpdate(refTable,
        conn.tablePrimaryKey(refTable).flatMap(_.name).getOrElse(""))
    val rows = fks.flatMap { fk =>
      fk.columns.zip(fk.referencedColumns).zipWithIndex.map { case ((col, refCol), idx) =>
        fkRow(meta, fk.referencedTable, refCol, table, col, idx + 1,
          refActionToJdbc(fk.onUpdate), refActionToJdbc(fk.onDelete),
          fk.name.getOrElse(""), pkName(fk.referencedTable))
      }
    }.toVector
    new PetraResultSet(TableValue(rows, meta))

  override def getExportedKeys(
    catalog: String, schema: String, table: String,
  ): java.sql.ResultSet =
    val meta   = fkResultSetMeta
    val pkN    = conn.tablePrimaryKey(table).flatMap(_.name).getOrElse("")
    val rows = conn.tableNames.flatMap { childTable =>
      conn.tableForeignKeys(childTable).filter(_.referencedTable == table).flatMap { fk =>
        fk.columns.zip(fk.referencedColumns).zipWithIndex.map { case ((col, refCol), idx) =>
          fkRow(meta, table, refCol, childTable, col, idx + 1,
            refActionToJdbc(fk.onUpdate), refActionToJdbc(fk.onDelete),
            fk.name.getOrElse(""), pkN)
        }
      }
    }.toVector
    new PetraResultSet(TableValue(rows, meta))

  override def getIndexInfo(
    catalog: String, schema: String, table: String,
    unique: Boolean, approximate: Boolean,
  ): java.sql.ResultSet =
    val meta = Metadata(IndexedSeq(
      ColumnMetadata(None, "TABLE_CAT",          TextType),
      ColumnMetadata(None, "TABLE_SCHEM",        TextType),
      ColumnMetadata(None, "TABLE_NAME",         TextType),
      ColumnMetadata(None, "NON_UNIQUE",         BooleanType),
      ColumnMetadata(None, "INDEX_QUALIFIER",    TextType),
      ColumnMetadata(None, "INDEX_NAME",         TextType),
      ColumnMetadata(None, "TYPE",               IntegerType),
      ColumnMetadata(None, "ORDINAL_POSITION",   IntegerType),
      ColumnMetadata(None, "COLUMN_NAME",        TextType),
      ColumnMetadata(None, "ASC_OR_DESC",        TextType),
      ColumnMetadata(None, "CARDINALITY",        IntegerType),
      ColumnMetadata(None, "PAGES",              IntegerType),
      ColumnMetadata(None, "FILTER_CONDITION",   TextType),
    ))
    val indexes = conn.tableIndexes(table)
    val filtered = if unique then indexes.filter(_.unique) else indexes
    val rows = filtered.sortBy(_.name).flatMap { idx =>
      idx.columns.zipWithIndex.map { case (col, i) =>
        Row(IndexedSeq(
          NullValue(),                                                        // TABLE_CAT
          TextValue(""),                                                      // TABLE_SCHEM
          TextValue(table),                                                   // TABLE_NAME
          BooleanValue(!idx.unique),                                          // NON_UNIQUE
          TextValue(""),                                                      // INDEX_QUALIFIER
          TextValue(idx.name),                                                // INDEX_NAME
          NumberValue(java.sql.DatabaseMetaData.tableIndexOther.toInt),        // TYPE
          NumberValue(i + 1),                                                 // ORDINAL_POSITION
          TextValue(col),                                                     // COLUMN_NAME
          TextValue("A"),                                                     // ASC_OR_DESC
          NumberValue(0),                                                     // CARDINALITY
          NumberValue(0),                                                     // PAGES
          NullValue(),                                                        // FILTER_CONDITION
        ), meta, None, None)
      }
    }.toVector
    new PetraResultSet(TableValue(rows, meta))

  private def sqlLiteral(v: Value): String = v match
    case _: NullValue      => "NULL"
    case TextValue(s)      => s"'${s.replace("'", "''")}'"
    case BooleanValue(b)   => if b then "TRUE" else "FALSE"
    case NumberValue(_, n) => n.toString
    case TimestampValue(t) => s"'$t'"
    case DateValue(d)      => s"'$d'"
    case UUIDValue(id)     => s"'$id'"
    case EnumValue(_, _)   => s"'${v.string}'"
    case _                 => v.string

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

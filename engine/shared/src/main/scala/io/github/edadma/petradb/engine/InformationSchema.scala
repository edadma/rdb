package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import scala.collection.immutable.ArraySeq

object InformationSchema:

  private val catalog = "petradb"
  private val schema  = "public"

  private def tv(s: String): Value  = TextValue(s)
  private def nv(): Value           = NullValue()
  private def iv(n: Int): Value     = NumberValue(n)
  private def bv(b: Boolean): Value = TextValue(if b then "YES" else "NO")

  private def typeString(t: Type): String = t match
    case IntegerType     => "integer"
    case SmallintType    => "smallint"
    case BigintType      => "bigint"
    case DoubleType      => "double precision"
    case BooleanType     => "boolean"
    case TextType        => "text"
    case DateType        => "date"
    case TimeType        => "time without time zone"
    case TimeTZType      => "time with time zone"
    case TimestampType   => "timestamp without time zone"
    case TimestampTZType => "timestamp with time zone"
    case IntervalType    => "interval"
    case UUIDType        => "uuid"
    case ByteaType       => "bytea"
    case JSONType        => "json"
    case SerialType      => "integer"
    case SmallSerialType => "smallint"
    case BigSerialType   => "bigint"
    case _: VarcharType  => "character varying"
    case _: CharType     => "character"
    case _: NumericType  => "numeric"
    case _               => t.name

  private def charMaxLength(t: Type): Value = t match
    case v: VarcharType => iv(v.length)
    case c: CharType    => iv(c.length)
    case _              => nv()

  private def numericPrecision(t: Type): Value = t match
    case IntegerType | SerialType          => iv(32)
    case SmallintType | SmallSerialType    => iv(16)
    case BigintType | BigSerialType        => iv(64)
    case DoubleType                        => iv(53)
    case n: NumericType                    => iv(n.precision)
    case _                                 => nv()

  private def numericScale(t: Type): Value = t match
    case n: NumericType                                                     => iv(n.scale)
    case IntegerType | SmallintType | BigintType | SerialType | SmallSerialType | BigSerialType => iv(0)
    case _                                                                  => nv()

  def generate(tableName: String, db: DB): Process =
    tableName match
      case "tables"                   => generateTables(db)
      case "columns"                  => generateColumns(db)
      case "table_constraints"        => generateTableConstraints(db)
      case "key_column_usage"         => generateKeyColumnUsage(db)
      case "referential_constraints"  => generateReferentialConstraints(db)
      case "schemata"                 => generateSchemata(db)
      case other                      => sys.error(s"information_schema table '$other' is not supported")

  private def generateTables(db: DB): Process =
    val meta = Metadata(Vector(
      ColumnMetadata(Some("tables"), "table_catalog", TextType),
      ColumnMetadata(Some("tables"), "table_schema", TextType),
      ColumnMetadata(Some("tables"), "table_name", TextType),
      ColumnMetadata(Some("tables"), "table_type", TextType),
    ))
    val rows = db.tableNames.toVector.sorted.map { name =>
      Row(Vector(tv(catalog), tv(schema), tv(name), tv("BASE TABLE")), meta, None, None)
    }
    StaticProcess(rows.to(ArraySeq), meta)

  private def generateColumns(db: DB): Process =
    val meta = Metadata(Vector(
      ColumnMetadata(Some("columns"), "table_catalog", TextType),
      ColumnMetadata(Some("columns"), "table_schema", TextType),
      ColumnMetadata(Some("columns"), "table_name", TextType),
      ColumnMetadata(Some("columns"), "column_name", TextType),
      ColumnMetadata(Some("columns"), "ordinal_position", IntegerType),
      ColumnMetadata(Some("columns"), "column_default", TextType),
      ColumnMetadata(Some("columns"), "is_nullable", TextType),
      ColumnMetadata(Some("columns"), "data_type", TextType),
      ColumnMetadata(Some("columns"), "character_maximum_length", IntegerType),
      ColumnMetadata(Some("columns"), "numeric_precision", IntegerType),
      ColumnMetadata(Some("columns"), "numeric_scale", IntegerType),
    ))
    val rows = Vector.newBuilder[Row]
    for
      tableName <- db.tableNames.toVector.sorted
      table <- db.getTable(tableName)
      (col, idx) <- table.columns.zipWithIndex
    do rows += Row(
      Vector(
        tv(catalog),
        tv(schema),
        tv(tableName),
        tv(col.name),
        iv(idx + 1),
        col.default.map(v => tv(v.string)).getOrElse(nv()),
        bv(!col.required),
        tv(typeString(col.typ)),
        charMaxLength(col.typ),
        numericPrecision(col.typ),
        numericScale(col.typ),
      ),
      meta, None, None,
    )
    StaticProcess(rows.result().to(ArraySeq), meta)

  private def generateTableConstraints(db: DB): Process =
    val meta = Metadata(Vector(
      ColumnMetadata(Some("table_constraints"), "constraint_catalog", TextType),
      ColumnMetadata(Some("table_constraints"), "constraint_schema", TextType),
      ColumnMetadata(Some("table_constraints"), "constraint_name", TextType),
      ColumnMetadata(Some("table_constraints"), "table_catalog", TextType),
      ColumnMetadata(Some("table_constraints"), "table_schema", TextType),
      ColumnMetadata(Some("table_constraints"), "table_name", TextType),
      ColumnMetadata(Some("table_constraints"), "constraint_type", TextType),
    ))
    val rows = Vector.newBuilder[Row]
    for
      tableName <- db.tableNames.toVector.sorted
      table <- db.getTable(tableName)
    do
      for row <- constraintRows(tableName, table) do
        rows += Row(row, meta, None, None)
    StaticProcess(rows.result().to(ArraySeq), meta)

  private def constraintRows(tableName: String, table: Table): Vector[Vector[Value]] =
    val result = Vector.newBuilder[Vector[Value]]
    table.primaryKey.foreach { pk =>
      val name = pk.name.getOrElse(s"${tableName}_pkey")
      result += Vector(tv(catalog), tv(schema), tv(name), tv(catalog), tv(schema), tv(tableName), tv("PRIMARY KEY"))
    }
    for c <- table.constraints do c match
      case _: PrimaryKeySpec => // already handled above
      case u: UniqueSpec =>
        val name = u.name.getOrElse(s"${tableName}_${u.columns.mkString("_")}_key")
        result += Vector(tv(catalog), tv(schema), tv(name), tv(catalog), tv(schema), tv(tableName), tv("UNIQUE"))
      case fk: ForeignKeySpec =>
        val name = fk.name.getOrElse(s"${tableName}_${fk.columns.mkString("_")}_fkey")
        result += Vector(tv(catalog), tv(schema), tv(name), tv(catalog), tv(schema), tv(tableName), tv("FOREIGN KEY"))
      case ch: CheckSpec =>
        val name = ch.name.getOrElse(s"${tableName}_check")
        result += Vector(tv(catalog), tv(schema), tv(name), tv(catalog), tv(schema), tv(tableName), tv("CHECK"))
    // Column-level unique constraints
    for col <- table.columns if col.unique do
      result += Vector(tv(catalog), tv(schema), tv(s"${tableName}_${col.name}_key"), tv(catalog), tv(schema), tv(tableName), tv("UNIQUE"))
    // Column-level FK constraints
    val constraintFKCols = table.constraints.collect { case fk: ForeignKeySpec => fk.columns }.toSet
    for col <- table.columns if col.fk.isDefined do
      if !constraintFKCols.contains(Seq(col.name)) then
        result += Vector(tv(catalog), tv(schema), tv(s"${tableName}_${col.name}_fkey"), tv(catalog), tv(schema), tv(tableName), tv("FOREIGN KEY"))
    result.result()

  private def generateKeyColumnUsage(db: DB): Process =
    val meta = Metadata(Vector(
      ColumnMetadata(Some("key_column_usage"), "constraint_catalog", TextType),
      ColumnMetadata(Some("key_column_usage"), "constraint_schema", TextType),
      ColumnMetadata(Some("key_column_usage"), "constraint_name", TextType),
      ColumnMetadata(Some("key_column_usage"), "table_catalog", TextType),
      ColumnMetadata(Some("key_column_usage"), "table_schema", TextType),
      ColumnMetadata(Some("key_column_usage"), "table_name", TextType),
      ColumnMetadata(Some("key_column_usage"), "column_name", TextType),
      ColumnMetadata(Some("key_column_usage"), "ordinal_position", IntegerType),
    ))
    val rows = Vector.newBuilder[Row]
    for
      tableName <- db.tableNames.toVector.sorted
      table <- db.getTable(tableName)
    do
      for row <- keyColumnRows(tableName, table) do
        rows += Row(row, meta, None, None)
    StaticProcess(rows.result().to(ArraySeq), meta)

  private def keyColumnRows(tableName: String, table: Table): Vector[Vector[Value]] =
    val result = Vector.newBuilder[Vector[Value]]
    table.primaryKey.foreach { pk =>
      val name = pk.name.getOrElse(s"${tableName}_pkey")
      for (col, idx) <- pk.columns.zipWithIndex do
        result += Vector(tv(catalog), tv(schema), tv(name), tv(catalog), tv(schema), tv(tableName), tv(col), iv(idx + 1))
    }
    for c <- table.constraints do c match
      case _: PrimaryKeySpec => // already handled above
      case u: UniqueSpec =>
        val name = u.name.getOrElse(s"${tableName}_${u.columns.mkString("_")}_key")
        for (col, idx) <- u.columns.zipWithIndex do
          result += Vector(tv(catalog), tv(schema), tv(name), tv(catalog), tv(schema), tv(tableName), tv(col), iv(idx + 1))
      case fk: ForeignKeySpec =>
        val name = fk.name.getOrElse(s"${tableName}_${fk.columns.mkString("_")}_fkey")
        for (col, idx) <- fk.columns.zipWithIndex do
          result += Vector(tv(catalog), tv(schema), tv(name), tv(catalog), tv(schema), tv(tableName), tv(col), iv(idx + 1))
      case _ =>
    // Column-level unique
    for col <- table.columns if col.unique do
      result += Vector(tv(catalog), tv(schema), tv(s"${tableName}_${col.name}_key"), tv(catalog), tv(schema), tv(tableName), tv(col.name), iv(1))
    // Column-level FK
    val constraintFKCols = table.constraints.collect { case fk: ForeignKeySpec => fk.columns }.toSet
    for col <- table.columns if col.fk.isDefined do
      if !constraintFKCols.contains(Seq(col.name)) then
        result += Vector(tv(catalog), tv(schema), tv(s"${tableName}_${col.name}_fkey"), tv(catalog), tv(schema), tv(tableName), tv(col.name), iv(1))
    result.result()

  private def generateReferentialConstraints(db: DB): Process =
    val meta = Metadata(Vector(
      ColumnMetadata(Some("referential_constraints"), "constraint_catalog", TextType),
      ColumnMetadata(Some("referential_constraints"), "constraint_schema", TextType),
      ColumnMetadata(Some("referential_constraints"), "constraint_name", TextType),
      ColumnMetadata(Some("referential_constraints"), "unique_constraint_catalog", TextType),
      ColumnMetadata(Some("referential_constraints"), "unique_constraint_schema", TextType),
      ColumnMetadata(Some("referential_constraints"), "unique_constraint_name", TextType),
      ColumnMetadata(Some("referential_constraints"), "update_rule", TextType),
      ColumnMetadata(Some("referential_constraints"), "delete_rule", TextType),
    ))
    val rows = Vector.newBuilder[Row]
    for
      tableName <- db.tableNames.toVector.sorted
      table <- db.getTable(tableName)
      fk <- db.foreignKeys(table)
    do
      val name = fk.name.getOrElse(s"${tableName}_${fk.columns.mkString("_")}_fkey")
      val refName = s"${fk.referencedTable}_pkey"
      rows += Row(Vector(
        tv(catalog), tv(schema), tv(name),
        tv(catalog), tv(schema), tv(refName),
        tv(actionString(fk.onUpdate)),
        tv(actionString(fk.onDelete)),
      ), meta, None, None)
    StaticProcess(rows.result().to(ArraySeq), meta)

  private def generateSchemata(db: DB): Process =
    val meta = Metadata(Vector(
      ColumnMetadata(Some("schemata"), "catalog_name", TextType),
      ColumnMetadata(Some("schemata"), "schema_name", TextType),
      ColumnMetadata(Some("schemata"), "schema_owner", TextType),
    ))
    val rows = ArraySeq(
      Row(Vector(tv(catalog), tv("public"), tv("petradb")), meta, None, None),
      Row(Vector(tv(catalog), tv("information_schema"), tv("petradb")), meta, None, None),
    )
    StaticProcess(rows, meta)

  private def actionString(a: ReferentialAction): String = a match
    case ReferentialAction.NoAction => "NO ACTION"
    case ReferentialAction.Restrict => "RESTRICT"
    case ReferentialAction.Cascade  => "CASCADE"
    case ReferentialAction.SetNull  => "SET NULL"

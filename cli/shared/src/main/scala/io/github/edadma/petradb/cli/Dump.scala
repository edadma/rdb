package io.github.edadma.petradb.cli

import io.github.edadma.petradb.*
import io.github.edadma.cross_platform

object Dump:
  def run(path: String): Unit =
    if !cross_platform.exists(path) then
      Console.err.println(s"Error: database file not found: $path")
      cross_platform.processExit(1)

    val db = PersistentDB.open(path)

    try
      dump(db)
    finally db.close()

  def dump(db: DB): Unit =
    dumpEnums(db)
    dumpTables(db)
    dumpViews(db)

  private def dumpEnums(db: DB): Unit =
    for (name, typ) <- db.types.toSeq.sortBy(_._1) do
      typ match
        case e: EnumType =>
          val labels = e.labels.map(l => s"'${escapeSql(l)}'").mkString(", ")
          println(s"CREATE TYPE $name AS ENUM ($labels);")
          println()
        case _ =>

  private def dumpTables(db: DB): Unit =
    for tableName <- db.tableNames.toSeq.sorted do
      db.getTable(tableName).foreach { table =>
        dumpCreateTable(tableName, table)
        dumpInserts(tableName, table)
      }

  private def dumpCreateTable(tableName: String, table: Table): Unit =
    val parts = scala.collection.mutable.ArrayBuffer[String]()

    for col <- table.columns do
      val typStr    = pgTypeName(col.typ)
      val notNull   = if col.required then " NOT NULL" else ""
      val isSerial  = col.typ == SmallSerialType || col.typ == SerialType || col.typ == BigSerialType
      val defaultStr =
        if isSerial then ""
        else col.default.map(v => s" DEFAULT ${sqlLiteral(v)}").getOrElse("")
      parts += s"  ${col.name} $typStr$notNull$defaultStr"

    for spec <- table.constraints do
      spec match
        case PrimaryKeySpec(cols, _) =>
          parts += s"  PRIMARY KEY (${cols.mkString(", ")})"
        case UniqueSpec(cols, _) =>
          parts += s"  UNIQUE (${cols.mkString(", ")})"
        case ForeignKeySpec(cols, refTable, refCols, _, onDelete, onUpdate) =>
          import ReferentialAction.*
          def actionStr(a: ReferentialAction): String = a match
            case Cascade  => "CASCADE"
            case Restrict => "RESTRICT"
            case SetNull  => "SET NULL"
            case NoAction => "NO ACTION"
          val del = if onDelete != NoAction then s" ON DELETE ${actionStr(onDelete)}" else ""
          val upd = if onUpdate != NoAction then s" ON UPDATE ${actionStr(onUpdate)}" else ""
          parts += s"  FOREIGN KEY (${cols.mkString(", ")}) REFERENCES $refTable (${refCols.mkString(", ")})$del$upd"
        case _ =>

    println(s"CREATE TABLE $tableName (")
    println(parts.mkString(",\n"))
    println(");")
    println()

  private def dumpInserts(tableName: String, table: Table): Unit =
    val colNames = table.columns.map(_.name).mkString(", ")
    var hasRows  = false

    for row <- table.iterator(Nil) do
      hasRows = true
      val values = row.data.map(sqlLiteral).mkString(", ")
      println(s"INSERT INTO $tableName ($colNames) VALUES ($values);")

    if hasRows then println()

  private[cli] def pgTypeName(typ: Type): String =
    typ match
      case DoubleType          => "DOUBLE PRECISION"
      case NumericType(p, s)   => s"NUMERIC($p,$s)"
      case CharType(n)         => s"CHAR($n)"
      case ArrayColumnType(e)  => s"${pgTypeName(e)}[]"
      case SmallSerialType     => "SMALLSERIAL"
      case SerialType          => "SERIAL"
      case BigSerialType       => "BIGSERIAL"
      case _: EnumType         => typ.name
      case _                   => typ.name.toUpperCase

  private[cli] def sqlLiteral(v: Value): String =
    v match
      case _: NullValue        => "NULL"
      case TextValue(s)        => s"'${escapeSql(s)}'"
      case BooleanValue(b)     => if b then "TRUE" else "FALSE"
      case NumberValue(_, n)   => n.toString
      case TimestampValue(t)   => s"'$t'"
      case DateValue(d)        => s"'$d'"
      case TimeValue(t)        => s"'$t'"
      case IntervalValue(d)    => s"'${v.string}'"
      case TimestampTZValue(t) => s"'$t'"
      case UUIDValue(id)       => s"'$id'"
      case ByteaValue(data)    => s"E'\\\\x${data.map(b => f"${b & 0xff}%02x").mkString}'"
      case EnumValue(_, typ)   => s"'${escapeSql(v.string)}'"
      case ArrayValue(elems)   => s"ARRAY[${elems.map(sqlLiteral).mkString(", ")}]"
      case _                   => v.string

  private def dumpViews(db: DB): Unit =
    for name <- db.viewNames.toSeq.sorted do
      db.getView(name).foreach { sql =>
        println(s"CREATE VIEW $name AS $sql;")
        println()
      }

  private def escapeSql(s: String): String = s.replace("'", "''")

package io.github.edadma.petradb

import io.github.edadma.importer.Importer
import io.github.edadma.cross_platform.{exists, writeFile}

class TextDB(val path: String) extends MemoryDB:
  override val name = s"text DB: $path"

  private var loading = true

  // Register pass-through converters for types not natively known to the importer
  Importer.addConverter("array", s => Some(s))
  Importer.addConverter("json", s => Some(s))
  Importer.addConverter("jsonb", s => Some(s))
  Importer.addConverter("bytea", s => Some(s))
  Importer.addConverter("interval", s => Some(s))
  Importer.addConverter("time", s => Some(s))
  Importer.addConverter("timetz", s => Some(s))

  if exists(path) then
    val imp = Importer.importFromFile(path, doubleSpaces = true)
    given Session = this.connect()

    for e <- imp.enums do
      val labels = e.labels.map(l => s"'${l.replace("'", "''")}'").mkString(", ")
      executeSQL(s"CREATE TYPE ${e.name} AS ENUM ($labels)")

    // Restore views from comment directives
    for line <- io.github.edadma.cross_platform.readFile(path).linesIterator do
      val trimmed = line.trim
      if trimmed.startsWith("-- VIEW ") then
        val rest = trimmed.drop(8) // "-- VIEW " is 8 chars
        val asIdx = rest.indexOf(" AS ")
        if asIdx > 0 then
          val vName = rest.substring(0, asIdx)
          val vSql = rest.substring(asIdx + 4)
          views(vName) = vSql

    for t <- imp.tables do
      val colDefs = t.header.map(col => s"${col.name} ${importerTypeToSQL(col.typ)}").mkString(", ")
      executeSQL(s"CREATE TABLE ${t.name} ($colDefs)")

      if t.data.nonEmpty then
        val colNames = t.header.map(_.name).mkString(", ")
        for row <- t.data do
          val values = row.map(anyToSqlLiteral).mkString(", ")
          executeSQL(s"INSERT INTO ${t.name} ($colNames) VALUES ($values)")

  loading = false

  override protected[petradb] def onMutation(): Unit =
    if !loading then writeFile(path, buildFileContent())

  private def importerTypeToSQL(typ: String): String =
    typ.toLowerCase match
      case "text" | "varchar" | "char"   => "TEXT"
      case "integer" | "int"             => "INTEGER"
      case "smallint"                    => "SMALLINT"
      case "smallserial"                 => "SMALLSERIAL"
      case "serial"                      => "SERIAL"
      case "bigint"                      => "BIGINT"
      case "bigserial"                   => "BIGSERIAL"
      case "real" | "float" | "double"   => "DOUBLE PRECISION"
      case "numeric" | "decimal"         => "NUMERIC"
      case "date"                        => "DATE"
      case "timestamp"                   => "TIMESTAMP"
      case "timestamptz"                 => "TIMESTAMPTZ"
      case "uuid"                        => "UUID"
      case "boolean"                     => "BOOLEAN"
      case "bytea"                       => "BYTEA"
      case "interval"                    => "INTERVAL"
      case "time"                        => "TIME"
      case "timetz"                      => "TIMETZ"
      case other                         => other  // enum type names pass through

  private def anyToSqlLiteral(v: Any): String =
    if v == null then "NULL"
    else v match
      case s: String                       => s"'${s.replace("'", "''")}'"
      case i: Int                          => i.toString
      case l: Long                         => l.toString
      case d: Double                       => d.toString
      case bd: BigDecimal                  => bd.toString
      case b: Boolean                      => if b then "TRUE" else "FALSE"
      case ld: java.time.LocalDate         => s"'$ld'"
      case ldt: java.time.LocalDateTime    => s"'$ldt'"
      case odt: java.time.OffsetDateTime   => s"'$odt'"
      case other                           => s"'${other.toString.replace("'", "''")}'"

  private def textdbTypeName(typ: Type): String =
    typ match
      case _: ArrayColumnType => "text"
      case _                  => typ.name

  private def renderForImporter(v: Value): String =
    v match
      case _: NullValue => "null"
      case TextValue(s) => escapeForImporter(s)
      case _: EnumValue => escapeForImporter(v.string)
      case _            => v.string

  private def escapeForImporter(s: String): String =
    val base = s.replace("\\", "\\\\")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")
    // Escape consecutive spaces: replace all but the last space in each run with \u0020
    val sb = new StringBuilder
    var i = 0
    while i < base.length do
      if base(i) == ' ' && i + 1 < base.length && base(i + 1) == ' ' then
        sb.append("\\u0020")
      else
        sb.append(base(i))
      i += 1
    sb.toString

  private def buildFileContent(): String =
    val sb = new StringBuilder

    for (vName, vSql) <- views.toSeq.sortBy(_._1) do
      sb.append(s"-- VIEW $vName AS $vSql\n")

    for (eName, typ) <- types.toSeq.sortBy(_._1) do
      typ match
        case e: EnumType =>
          sb.append(s"$eName: ${e.labels.map(escapeForImporter).mkString(", ")}\n\n")
        case _ =>

    for tableName <- tableNames.toSeq.sorted do
      getTable(tableName).foreach { t =>
        sb.append(s"$tableName\n")

        val colHeaders = t.columns.map(c => s"${c.name}:${textdbTypeName(c.typ)}")
        val res = new io.github.edadma.table.TextTable(headerBold = false, headerUnderlined = false)
        res.noansi()
        res.headerSeq(colHeaders)

        for (col, i) <- t.columns.zipWithIndex do
          if col.typ.isNumber then res.rightAlignment(i + 1)

        for row <- t.iterator(Nil) do
          res.rowSeq(row.data.map(renderForImporter))

        sb.append(res.toString)
        sb.append("\n")
      }

    sb.toString

object TextDB:
  def open(path: String): TextDB = new TextDB(path)

package io.github.edadma.petradb.jdbc

import io.github.edadma.petradb.{Result, QueryResult, ColumnMetadata, TextValue, BooleanValue, Codecs}
import io.github.edadma.petradb.engine.{ColumnSpec, PrimaryKeySpec, ForeignKeySpec, IndexMeta, ReferentialAction}
import io.github.edadma.petradb.client
import io.github.edadma.petradb.client.{Session => ClientSession, SessionOptions}

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.*

class PetraServerConnection(
  host: String,
  port: Int,
  val username: String,
  password: String,
) extends AbstractConnection:

  private val timeout                       = 30.seconds
  private given ExecutionContext            = ExecutionContext.global
  val url: String                           = s"jdbc:petradb://$host:$port"

  private val clientSession = new ClientSession(
    SessionOptions(
      host     = host,
      port     = port,
      username = if username.nonEmpty then Some(username) else None,
      password = if password.nonEmpty then Some(password) else None,
    ),
  )

  Await.result(clientSession.connect(), timeout)

  def execute(sql: String): Seq[Result] =
    Await.result(clientSession.execute(sql), timeout)

  def doClose(): Unit =
    Await.result(clientSession.close(), timeout)

  private def quoted(name: String): String = "\"" + name.replace("\"", "\"\"") + "\""

  def tableNames: Iterable[String] =
    execute("SHOW TABLES").headOption match
      case Some(QueryResult(tv)) => tv.data.map(_.data(0).asInstanceOf[TextValue].s)
      case _                     => Iterable.empty

  def tableColumns(tableName: String): Seq[ColumnMetadata] =
    execute(s"SHOW COLUMNS ${quoted(tableName)}").headOption match
      case Some(QueryResult(tv)) =>
        tv.data.map { row =>
          val name    = row.data(0).asInstanceOf[TextValue].s
          val typeTag = row.data(1).asInstanceOf[TextValue].s
          ColumnMetadata(Some(tableName), name, Codecs.typeFromTag(typeTag))
        }.toSeq
      case _ => Seq.empty

  def tableColumnSpecs(tableName: String): Seq[ColumnSpec] =
    execute(s"SHOW COLUMNS ${quoted(tableName)}").headOption match
      case Some(QueryResult(tv)) =>
        tv.data.map { row =>
          val name       = row.data(0).asInstanceOf[TextValue].s
          val typ        = Codecs.typeFromTag(row.data(1).asInstanceOf[TextValue].s)
          val required   = row.data(2).asInstanceOf[BooleanValue].b
          val indexed    = row.data(3).asInstanceOf[BooleanValue].b
          val unique     = row.data(4).asInstanceOf[BooleanValue].b
          val fkTable    = row.data(5).asInstanceOf[TextValue].s
          val fkColumn   = row.data(6).asInstanceOf[TextValue].s
          val fkOnDelete = row.data(7).asInstanceOf[TextValue].s
          val fkOnUpdate = row.data(8).asInstanceOf[TextValue].s
          val defaultStr = row.data(9).asInstanceOf[TextValue].s
          val fk =
            if fkTable.nonEmpty then
              Some((fkTable, fkColumn,
                ReferentialAction.valueOf(fkOnDelete),
                ReferentialAction.valueOf(fkOnUpdate)))
            else None
          val default = if defaultStr.nonEmpty then Some(TextValue(defaultStr)) else None
          ColumnSpec(name, typ, required, indexed, unique, fk, default)
        }.toSeq
      case _ => Seq.empty

  def tablePrimaryKey(tableName: String): Option[PrimaryKeySpec] =
    execute(s"SHOW PRIMARY KEY ${quoted(tableName)}").headOption match
      case Some(QueryResult(tv)) if tv.data.nonEmpty =>
        val columns = tv.data.map(_.data(0).asInstanceOf[TextValue].s).toSeq
        val pkName  = tv.data.head.data(1).asInstanceOf[TextValue].s
        Some(PrimaryKeySpec(columns, if pkName.nonEmpty then Some(pkName) else None))
      case _ => None

  def tableForeignKeys(tableName: String): Seq[ForeignKeySpec] =
    execute(s"SHOW FOREIGN KEYS ${quoted(tableName)}").headOption match
      case Some(QueryResult(tv)) if tv.data.nonEmpty =>
        // Group rows by fk_name (or by (ref_table) for unnamed FKs)
        val grouped = tv.data.groupBy { row =>
          val fkName   = row.data(0).asInstanceOf[TextValue].s
          val refTable = row.data(2).asInstanceOf[TextValue].s
          if fkName.nonEmpty then fkName else s"__anon__$refTable"
        }
        grouped.values.map { rows =>
          val sorted     = rows.sortBy(_.data(6).intValue)
          val fkName     = sorted.head.data(0).asInstanceOf[TextValue].s
          val cols       = sorted.map(_.data(1).asInstanceOf[TextValue].s).toSeq
          val refTable   = sorted.head.data(2).asInstanceOf[TextValue].s
          val refCols    = sorted.map(_.data(3).asInstanceOf[TextValue].s).toSeq
          val onDelete   = ReferentialAction.valueOf(sorted.head.data(4).asInstanceOf[TextValue].s)
          val onUpdate   = ReferentialAction.valueOf(sorted.head.data(5).asInstanceOf[TextValue].s)
          ForeignKeySpec(cols, refTable, refCols, if fkName.nonEmpty then Some(fkName) else None, onDelete, onUpdate)
        }.toSeq
      case _ => Seq.empty

  def tableIndexes(tableName: String): Seq[IndexMeta] =
    execute(s"SHOW INDEXES ${quoted(tableName)}").headOption match
      case Some(QueryResult(tv)) if tv.data.nonEmpty =>
        val grouped = tv.data.groupBy(_.data(0).asInstanceOf[TextValue].s)
        grouped.map { case (idxName, rows) =>
          val sorted  = rows.sortBy(_.data(3).intValue)
          val cols    = sorted.map(_.data(1).asInstanceOf[TextValue].s).toSeq
          val unique  = sorted.head.data(2).asInstanceOf[BooleanValue].b
          IndexMeta(idxName, tableName, cols, unique)
        }.toSeq
      case _ => Seq.empty

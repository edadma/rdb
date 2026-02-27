package io.github.edadma.petradb.jdbc

import io.github.edadma.petradb.*

class PetraFileConnection(key: String, displayPath: String) extends AbstractConnection:

  val url: String      = s"jdbc:petradb:file:$displayPath"
  val username: String = ""

  private val db: DB = SharedDB.acquire(key)

  private given session: Session = db.connect()

  def execute(sql: String): Seq[Result]                  = executeSQL(sql)
  def doClose(): Unit                                    = SharedDB.release(key)
  def tableNames: Iterable[String]                       = db.tableNames
  def tableColumns(tableName: String): Seq[ColumnMetadata] =
    db.getTable(tableName).map(_.meta.columns).getOrElse(Seq.empty)
  def tableColumnSpecs(tableName: String): Seq[ColumnSpec] =
    db.getTable(tableName).map(_.columns.toSeq).getOrElse(Seq.empty)
  def tablePrimaryKey(tableName: String): Option[PrimaryKeySpec] =
    db.getTable(tableName).flatMap(_.primaryKey)

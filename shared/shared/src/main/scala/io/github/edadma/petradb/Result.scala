package io.github.edadma.petradb

trait Result

case class QueryResult(table: TableValue)                           extends Result
case class InsertResult(obj: Map[String, Value], table: TableValue) extends Result
case class CreateTableResult(table: String)                         extends Result
case class DropTableResult(table: String)                           extends Result
case class CreateIndexResult(name: String)                          extends Result
case class DropIndexResult(name: String)                            extends Result
case class DropTypeResult(name: String)                             extends Result
case class CreateTypeResult(typ: String)                            extends Result
case class UpdateResult(rows: Int)                                  extends Result
case class DeleteResult(rows: Int)                                  extends Result
case class TruncateResult(table: String)                            extends Result
case class AlterTableResult()                                       extends Result
case class ExplainResult(plan: String)                              extends Result
case class PrepareResult(name: String)    extends Result
case class DeallocateResult(name: String) extends Result
case object BeginResult    extends Result
case object CommitResult   extends Result
case object RollbackResult extends Result

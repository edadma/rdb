package io.github.edadma.rdb

trait Result

case class QueryResult(table: TableValue) extends Result
case class InsertResult(result: Map[String, Value]) extends Result
case class CreateTableResult(table: String) extends Result
case class UpdateResult(rows: Int) extends Result
case class DeleteResult(rows: Int) extends Result

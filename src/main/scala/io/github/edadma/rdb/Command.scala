package io.github.edadma.rdb

trait Command

case class QueryCommand(query: Expr) extends Command
case class InsertCommand(table: Ident, columns: Seq[Ident], rows: Seq[Seq[Expr]]) extends Command
case class CreateCommand(table: Ident, columns: Seq[ColumnDesc]) extends Command

case class ColumnDesc(name: Ident, typ: String, pk: Boolean, required: Boolean)

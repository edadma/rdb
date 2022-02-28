package io.github.edadma.rdb

trait Command

case class QueryCommand(query: Expr) extends Command
case class InsertCommand(table: Ident, columns: Seq[Ident], rows: Seq[Seq[Expr]]) extends Command
case class CreateTableCommand(table: Ident, columns: Seq[ColumnDesc]) extends Command
case class UpdateCommand(table: Ident, sets: Seq[UpdateSet], cond: Option[Expr]) extends Command
case class DeleteCommand(table: Ident, cond: Option[Expr]) extends Command

case class UpdateSet(col: Ident, value: Expr)
case class ColumnDesc(name: Ident, typ: String, pk: Boolean, required: Boolean)

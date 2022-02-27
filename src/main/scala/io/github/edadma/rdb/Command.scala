package io.github.edadma.rdb

trait Command

case class QueryCommand(query: Expr) extends Command
case class InsertCommand(table: Ident, columns: Seq[Ident], rows: Seq[Seq[Expr]]) extends Command

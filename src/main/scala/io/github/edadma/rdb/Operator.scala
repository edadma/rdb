package io.github.edadma.rdb

trait Operator extends Expr { val typ: Type = TableType }

case class AliasOperator(rel: Expr, alias: Ident) extends Operator
case class ProjectOperator(rel: Expr, projs: Seq[Expr]) extends Operator
case class TableOperator(table: Ident) extends Operator
case class SelectOperator(rel: Expr, cond: Expr) extends Operator
case class CrossOperator(rel1: Expr, rel2: Expr) extends Operator

case class ProcessOperator(proc: Process) extends Operator

package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq

trait Operator extends Expr { val typ: Type = TableType }

case class AliasOperator(rel: Expr, alias: Ident) extends Operator
case class ProjectOperator(rel: Expr, projs: ArraySeq[Expr]) extends Operator
case class TableOperator(table: Ident) extends Operator
case class SelectOperator(rel: Expr, cond: Expr) extends Operator
case class CrossOperator(rel1: Expr, rel2: Expr) extends Operator
case class InnerJoinOperator(rel1: Expr, rel2: Expr, on: Expr) extends Operator
case class LeftJoinOperator(rel1: Expr, rel2: Expr, on: Expr) extends Operator
case class OffsetOperator(rel: Expr, offset: Int) extends Operator
case class LimitOperator(rel: Expr, limit: Int) extends Operator

case class ProcessOperator(proc: Process) extends Operator

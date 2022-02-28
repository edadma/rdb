package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq

trait Operator extends Expr

case class AliasOperator(rel: Expr, alias: Ident) extends Operator
case class ProjectOperator(rel: Expr, exprs: ArraySeq[Expr]) extends Operator
case class TableOperator(table: Ident) extends Operator
case class SelectOperator(rel: Expr, cond: Expr) extends Operator
case class GroupOperator(rel: Expr, by: Seq[Expr]) extends Operator
case class SortOperator(rel: Expr, by: Seq[OrderBy]) extends Operator
case class CrossOperator(rel1: Expr, rel2: Expr) extends Operator
case class InnerJoinOperator(rel1: Expr, rel2: Expr, on: Expr) extends Operator
case class LeftJoinOperator(rel1: Expr, rel2: Expr, on: Expr) extends Operator
case class OffsetOperator(rel: Expr, offset: Int) extends Operator
case class LimitOperator(rel: Expr, limit: Int) extends Operator

case class ProcessOperator(proc: Process) extends Operator

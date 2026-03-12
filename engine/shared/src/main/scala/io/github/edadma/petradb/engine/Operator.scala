package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import scala.collection.immutable.ArraySeq

trait Operator extends Expr

case class AliasOperator(rel: Expr, alias: Ident) extends Operator
case class ColumnAliasOperator(rel: Expr, alias: Ident, columns: Seq[Ident]) extends Operator
case class ProjectOperator(rel: Expr, exprs: ArraySeq[Expr]) extends Operator
case class TableOperator(table: Ident) extends Operator
case class InformationSchemaOperator(table: Ident) extends Operator
case class SelectOperator(rel: Expr, cond: Expr) extends Operator
case class HavingOperator(rel: Expr, cond: Expr) extends Operator
case class GroupOperator(rel: Expr, by: Seq[Expr]) extends Operator
case class SortOperator(rel: Expr, by: Seq[OrderBy]) extends Operator
case class CrossOperator(rel1: Expr, rel2: Expr) extends Operator
case class InnerJoinOperator(rel1: Expr, rel2: Expr, on: Expr) extends Operator
case class LeftJoinOperator(rel1: Expr, rel2: Expr, on: Expr) extends Operator
case class RightJoinOperator(rel1: Expr, rel2: Expr, on: Expr) extends Operator
case class FullJoinOperator(rel1: Expr, rel2: Expr, on: Expr) extends Operator
case class OffsetOperator(rel: Expr, offset: Int) extends Operator
case class LimitOperator(rel: Expr, limit: Int) extends Operator
case class DistinctOperator(rel: Expr) extends Operator

case class AggregateSpec(name: String, func: AggregateFunctionInstance, args: Seq[Expr], typ: Type, filter: Option[Expr] = None)
case class AggregateOperator(rel: Expr, groupBy: Seq[Expr], aggregates: Seq[AggregateSpec]) extends Operator

sealed trait WindowFunctionKind
case object RowNumberKind extends WindowFunctionKind
case object RankKind extends WindowFunctionKind
case object DenseRankKind extends WindowFunctionKind
case class LagKind(expr: Expr, offset: Int, default: Option[Expr]) extends WindowFunctionKind
case class LeadKind(expr: Expr, offset: Int, default: Option[Expr]) extends WindowFunctionKind
case class NtileKind(buckets: Int) extends WindowFunctionKind
case class AggregateWindowKind(func: AggregateFunction, args: Seq[Expr], filter: Option[Expr]) extends WindowFunctionKind

case class WindowSpec(name: String, kind: WindowFunctionKind, partitionBy: Seq[Expr], orderBy: Seq[OrderBy], typ: Type)
case class WindowOperator(rel: Expr, windows: Seq[WindowSpec]) extends Operator

case class UnionOperator(rel1: Expr, rel2: Expr, all: Boolean) extends Operator
case class IntersectOperator(rel1: Expr, rel2: Expr) extends Operator
case class ExceptOperator(rel1: Expr, rel2: Expr) extends Operator

case class LateralCrossOperator(rel1: Expr, rel2: Expr) extends Operator

case class ProcessOperator(proc: Process) extends Operator

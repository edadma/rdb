package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq
import scala.util.parsing.input.Positional

case class Ident(name: String) extends Positional

trait Expr extends Positional:
  val typ: Type

case class ColumnExpr(col: Ident, typ: Type = UnknownType) extends Expr
case class UnaryExpr(op: String, expr: Expr, typ: Type = UnknownType) extends Expr
case class BinaryExpr(left: Expr, op: String, right: Expr, typ: Type = UnknownType) extends Expr
case class StringExpr(s: String) extends Expr { val typ: Type = StringType }
case class NumberExpr(n: Number) extends Expr { val typ: Type = NumberType }
case class NullExpr() extends Expr { val typ: Type = NullType }

trait BooleanTypeExpr extends Expr { val typ: Type = BooleanType }

case class InExpr(value: Expr, array: Expr) extends BooleanTypeExpr
case class ExistsExpr(subquery: Expr) extends BooleanTypeExpr
case class BetweenExpr(value: Expr, op: String, lower: Expr, upper: Expr) extends BooleanTypeExpr

trait UnknownTypeExpr extends Expr { val typ: Type = UnknownType }

case class ApplyExpr(func: Ident, args: Seq[Expr]) extends UnknownTypeExpr

case class ScalarFunctionExpr(f: ScalarFunction, args: Seq[Expr], typ: Type) extends Expr
case class AggregateFunctionExpr(f: AggregateFunction, arg: Expr, typ: Type) extends Expr

case class SQLInArrayExpr(value: Expr, op: String, exprs: List[Expr]) extends BooleanTypeExpr
case class SQLInQueryExpr(value: Expr, op: String, query: SQLSelectExpr) extends BooleanTypeExpr
case class SQLSelectExpr(
    exprs: ArraySeq[Expr],
    from: Seq[Expr],
    where: Option[Expr],
    offset: Option[Int],
    limit: Option[Int]
) extends Expr { val typ: Type = TableType }

case class SubqueryExpr(subquery: SQLSelectExpr, typ: Type = UnknownType) extends Expr

case class StarExpr() extends UnknownTypeExpr
case class TableStarExpr(table: Ident) extends UnknownTypeExpr

package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq
import scala.util.parsing.input.Positional

case class Ident(name: String) extends Positional

trait Expr extends Positional:
  var typ: Type | Null = null

case class ColumnExpr(col: Ident) extends Expr
case class UnaryExpr(op: String, expr: Expr) extends Expr
case class BinaryExpr(left: Expr, op: String, right: Expr) extends Expr
case class BooleanExpr(b: Boolean) extends Expr { typ = BooleanType }
case class StringExpr(s: String) extends Expr { typ = StringType }
case class NumberExpr(n: Number) extends Expr { typ = NumberType }
case class NullExpr() extends Expr { typ = NullType }

case class InExpr(value: Expr, array: Expr) extends Expr
case class ExistsExpr(subquery: Expr) extends Expr
case class BetweenExpr(value: Expr, op: String, lower: Expr, upper: Expr) extends Expr

case class ApplyExpr(func: Ident, args: Seq[Expr]) extends Expr

case class ScalarFunctionExpr(f: ScalarFunction, args: Seq[Expr]) extends Expr
case class AggregateFunctionExpr(f: AggregateFunction, arg: Expr) extends Expr

case class SQLInArrayExpr(value: Expr, op: String, exprs: List[Expr]) extends Expr
case class SQLInQueryExpr(value: Expr, op: String, query: SQLSelectExpr) extends Expr
case class SQLSelectExpr(
    exprs: ArraySeq[Expr],
    from: Seq[Expr],
    where: Option[Expr],
    offset: Option[Int],
    limit: Option[Int]
) extends Expr

case class SubqueryExpr(subquery: SQLSelectExpr) extends Expr

case class StarExpr() extends Expr
case class TableStarExpr(table: Ident) extends Expr

case class Pair(k: String, v: Expr)

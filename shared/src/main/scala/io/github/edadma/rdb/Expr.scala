package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq
import scala.util.parsing.input.{Position, Positional}

case class Ident(name: String) extends Positional

trait Expr extends Positional:
  var typ: Type | Null = null

case class ColumnExpr(col: Ident) extends Expr
case class UnaryExpr(op: String, expr: Expr) extends Expr
case class BinaryExpr(left: Expr, op: String, right: Expr) extends Expr
case class BooleanExpr(b: Boolean) extends Expr { typ = BooleanType }
case class StringExpr(s: String) extends Expr { typ = TextType }
case class NumberExpr(n: Number) extends Expr { typ = NumberType }
case class NullExpr() extends Expr { typ = NullType }

case class InExpr(value: Expr, array: Expr) extends Expr
case class ExistsExpr(subquery: Expr) extends Expr
case class BetweenExpr(value: Expr, op: String, lower: Expr, upper: Expr) extends Expr

case class ApplyExpr(func: Ident, args: Seq[Expr]) extends Expr

case class ScalarFunctionExpr(f: ScalarFunction, args: Seq[Expr]) extends Expr
case class AggregateFunctionExpr(f: AggregateFunction, arg: Expr) extends Expr

case class InSeqExpr(value: Expr, op: String, exprs: Seq[Expr]) extends Expr
case class InQueryExpr(value: Expr, op: String, query: Expr) extends Expr

case class SubqueryExpr(subquery: Expr) extends Expr

case class StarExpr() extends Expr
case class TableStarExpr(table: Ident) extends Expr
case class ObjectExpr(properties: Seq[(Ident, Expr)]) extends Expr
case class ArrayExpr(elems: Seq[Expr]) extends Expr
case class TableConstructorExpr(query: Expr) extends Expr

case class Count(pos: Position, count: Int)

case class SQLSelectExpr(
    exprs: ArraySeq[Expr],
    from: Option[Seq[Expr]],
    where: Option[Expr],
    groupBy: Option[Seq[Expr]],
    orderBy: Option[Seq[OrderBy]],
    offset: Option[Count],
    limit: Option[Count]
) extends Expr

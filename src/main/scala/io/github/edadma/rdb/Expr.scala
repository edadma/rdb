package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq
import scala.util.parsing.input.Position

type Pos = Position | Null

object Ident:
  def apply(name: String): Ident = Ident(null, name)

case class Ident(pos: Pos, name: String)

trait Expr:
  val pos: Pos
  val typ: Type

case class ColumnExpr(pos: Pos, col: Ident, typ: Type = UnknownType) extends Expr
case class UnaryExpr(pos: Pos, op: String, expr: Expr, typ: Type = UnknownType) extends Expr
case class BinaryExpr(pos: Pos, left: Expr, op: String, right: Expr, typ: Type = UnknownType) extends Expr
case class StringExpr(pos: Pos, s: String) extends Expr { val typ: Type = StringType }
case class NumberExpr(pos: Pos, n: Number) extends Expr { val typ: Type = NumberType }
case class NullExpr(pos: Pos) extends Expr { val typ: Type = NullType }

trait BooleanTypeExpr extends Expr { val typ: Type = BooleanType }

case class InExpr(pos: Pos, value: Expr, array: Expr) extends BooleanTypeExpr
case class ExistsExpr(pos: Pos, subquery: Expr) extends BooleanTypeExpr
case class BetweenExpr(pos: Pos, value: Expr, lower: Expr, upper: Expr) extends BooleanTypeExpr

trait UnknownTypeExpr extends Expr { val typ: Type = UnknownType }

case class ApplyExpr(pos: Pos, func: Ident, args: Seq[Expr]) extends UnknownTypeExpr

case class ScalarFunctionExpr(pos: Pos, f: ScalarFunction, args: Seq[Expr], typ: Type) extends Expr
case class AggregateFunctionExpr(pos: Pos, f: AggregateFunction, arg: Expr, typ: Type) extends Expr

case class SQLSelectExpr(
    pos: Pos,
    exprs: ArraySeq[Expr],
    from: Seq[Expr],
    where: Option[Expr],
    offset: Option[Int],
    limit: Option[Int]
) extends Expr { val typ: Type = TableType }

case class SubqueryExpr(pos: Pos, subquery: SQLSelectExpr, typ: Type = UnknownType) extends Expr

case class StarExpr(pos: Pos) extends UnknownTypeExpr
case class TableStarExpr(pos: Pos, table: Ident) extends UnknownTypeExpr

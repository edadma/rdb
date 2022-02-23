package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq

type Pos = Int | Null

case class Ident(pos: Pos, name: String)

trait Expr:
  val typ: Type

case class ColumnExpr(col: Ident, typ: Type = UnknownType) extends Expr
case class UnaryExpr(op: String, pos: Pos, expr: Expr, typ: Type = UnknownType) extends Expr
case class BinaryExpr(lp: Pos, left: Expr, op: String, rp: Pos, right: Expr, typ: Type = UnknownType) extends Expr
case class StringExpr(s: String, pos: Pos) extends Expr { val typ: Type = StringType }
case class NumberExpr(n: Number, pos: Pos) extends Expr { val typ: Type = NumberType }
case class NullExpr(pos: Pos) extends Expr { val typ: Type = NullType }

trait BooleanTypeExpr extends Expr { val typ: Type = BooleanType }

case class InExpr(value: Expr, array: Expr) extends BooleanTypeExpr
case class ExistsExpr(subquery: Expr) extends BooleanTypeExpr
case class BetweenExpr(value: Expr, lower: Expr, upper: Expr) extends BooleanTypeExpr

trait UnknownTypeExpr extends Expr { val typ: Type = UnknownType }

case class ApplyExpr(func: Ident, args: Seq[Expr]) extends UnknownTypeExpr

case class ScalarFunctionExpr(f: ScalarFunction, args: Seq[Expr], typ: Type) extends Expr
case class AggregateFunctionExpr(f: AggregateFunction, arg: Expr, typ: Type) extends Expr

case class SelectExpr(
    exprs: ArraySeq[Expr],
    from: Seq[Expr],
    where: Option[Expr],
    offset: Option[Int],
    limit: Option[Int]
) extends Expr { val typ: Type = TableType }

case class StarExpr(pos: Pos) extends UnknownTypeExpr
case class TableStarExpr(table: Ident) extends UnknownTypeExpr

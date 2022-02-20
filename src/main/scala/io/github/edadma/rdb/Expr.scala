package io.github.edadma.rdb

type Pos = Int

case class Ident(s: String, pos: Option[Pos] = None)

trait Expr:
  val typ: Type

case class VariableExpr(name: Ident, typ: Type) extends Expr
case class UnaryExpr(op: String, expr: Expr, typ: Type) extends Expr
case class BinaryExpr(left: Expr, op: String, right: Expr, typ: Type) extends Expr
case class StringExpr(s: String, pos: Option[Pos] = None) extends Expr { val typ: Type = StringType }
case class NumberExpr(n: Number, pos: Option[Pos] = None) extends Expr { val typ: Type = NumberType }

trait UnknownTypeExpr extends Expr

case object StarExpr extends UnknownTypeExpr
case class TableStarExpr(table: Ident) extends UnknownTypeExpr

case class ApplyExpr(name: Ident, args: Seq[Expr]) extends UnknownTypeExpr

case class ScalarFunctionExpr(f: ScalarFunction, typ: Type) extends Expr

trait TableTypeExpr extends Expr { val typ: Type = TableType }

case class AliasExpr(expr: Expr, alias: Ident) extends TableTypeExpr

// SQL

case class SQLSelectExpr(exprs: Seq[Expr], from: Seq[TableExpr], where: Option[Expr]) extends TableTypeExpr

// relational

case class OperatorExpr(oper: Operator) extends TableTypeExpr
case class TableExpr(name: Ident) extends TableTypeExpr
case class SelectExpr(relation: Expr, cond: Expr) extends TableTypeExpr

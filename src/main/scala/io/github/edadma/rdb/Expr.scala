package io.github.edadma.rdb

type Pos = Int

case class Ident(s: String, pos: Option[Pos] = None)

trait Expr

case class VariableExpr(name: Ident) extends Expr
case class UnaryExpr(op: String, expr: Expr) extends Expr
case class BinaryExpr(left: Expr, op: String, right: Expr) extends Expr
case class StringExpr(s: String, pos: Option[Pos] = None) extends Expr
case class NumberExpr(n: Number, pos: Option[Pos] = None) extends Expr

case object StarExpr extends Expr
case class TableStarExpr(table: Ident) extends Expr

case class ApplyExpr(name: Ident, args: Seq[Expr]) extends Expr

case class AliasExpr(expr: Expr, alias: Ident) extends Expr

// SQL

case class SQLSelectExpr(exprs: Seq[Expr], from: Seq[TableExpr], where: Option[Expr])

// relational

case class OperatorExpr(oper: Operator) extends Expr
case class TableExpr(name: Ident) extends Expr
case class SelectExpr(relation: Expr, cond: Expr) extends Expr

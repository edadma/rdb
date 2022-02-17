package io.github.edadma.rdb

type Pos = Int

case class Ident(s: String, pos: Pos)

trait Expr

case class BinaryExpr(left: Expr, op: String, right: Expr) extends Expr
case class StringExpr(s: String) extends Expr

case object StarExpr extends Expr
case class TableStarExpr(table: Ident) extends Expr

// SQL

case class SQLSelectExpr(exprs: Seq[Expr], from: Seq[TableExpr], where: Option[Expr])

// relational

case class TableExpr(name: Ident, alias: Option[Ident]) extends Expr
case class SelectExpr(relation: Expr, cond: Expr) extends Expr

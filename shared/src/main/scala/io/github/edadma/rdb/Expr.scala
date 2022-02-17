package io.github.edadma.rdb

type Pos = Int

case class Ident(s: String, pos: Pos)

trait Expr

case class TableExpr(name: Ident) extends Expr

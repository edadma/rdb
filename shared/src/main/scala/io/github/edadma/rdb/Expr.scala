package io.github.edadma.rdb

type Pos = Int

case class Ident(s: String, pos: Pos)

trait Expr

case class BaseRelationExpr(name: Ident) extends Expr

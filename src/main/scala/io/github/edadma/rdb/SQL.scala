package io.github.edadma.rdb

object SQL:
  val query: Expr => Expr =
    case SelectExpr(exprs, from, where) =>
      from(0)

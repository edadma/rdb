package io.github.edadma.rdb

object SQL:
  val query: Expr => Expr =
    case SQLSelectExpr(exprs, from, where) => from(0)

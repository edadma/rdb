package io.github.edadma.rdb

def rewrite(expr: Expr): Expr =
  expr match
    case ApplyExpr(name, args)          =>
    case BinaryExpr(left, op, right, _) =>
    case SQLSelectExpr(oper, _)         =>
    case _                              => expr

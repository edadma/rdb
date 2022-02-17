package io.github.edadma.rdb

def eval(expr: Expr, row: Row, ctx: Seq[Row]): Value =
  expr match
    case IntExpr(n, pos) => IntValue(n).pos(pos)

def beval(expr: Expr, row: Row, ctx: Seq[Row]): Boolean = eval(expr, row, ctx).asInstanceOf[BooleanValue].b

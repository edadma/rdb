package io.github.edadma.rdb

def eval(expr: Expr, ctx: List[Row]): Value =
  expr match
    case IntExpr(n, pos) => IntValue(n).pos(pos)

def beval(expr: Expr, ctx: List[Row]): Boolean = eval(expr, ctx).asInstanceOf[BooleanValue].b

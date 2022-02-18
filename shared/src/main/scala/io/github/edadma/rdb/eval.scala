package io.github.edadma.rdb

import io.github.edadma.dal.{BasicDAL, IntType, DoubleType, TypedNumber, Type => DType}

private val PLUS = Symbol("+")

def eval(expr: Expr, row: Row, ctx: Seq[Row]): Value =
  expr match
    case NumberExpr(n: Int, pos)    => NumberValue(IntType, n).pos(pos)
    case NumberExpr(n: Double, pos) => NumberValue(DoubleType, n).pos(pos)
    case StringExpr(s, pos)         => StringValue(s).pos(pos)
    case BinaryExpr(left, op, right) =>
      val l = neval(left, row, ctx)
      val r = neval(right, row, ctx)

      op match
        case "+" => BasicDAL.compute(PLUS, l, r, NumberValue.from)

def beval(expr: Expr, row: Row, ctx: Seq[Row]): Boolean = eval(expr, row, ctx).asInstanceOf[BooleanValue].b

def neval(expr: Expr, row: Row, ctx: Seq[Row]): NumberValue = eval(expr, row, ctx).asInstanceOf[NumberValue]

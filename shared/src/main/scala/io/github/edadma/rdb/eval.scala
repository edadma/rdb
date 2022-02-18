package io.github.edadma.rdb

import io.github.edadma.dal.{BasicDAL, IntType, DoubleType, TypedNumber, Type => DType}

private val PLUS = Symbol("+")
private val MINUS = Symbol("-")
private val LT = Symbol("<")
private val GT = Symbol(">")
private val LTE = Symbol("<=")
private val GTE = Symbol(">=")

def eval(expr: Expr, row: Row, ctx: Seq[Row]): Value =
  expr match
    case NumberExpr(n: Int, pos)    => NumberValue(IntType, n).pos(pos)
    case NumberExpr(n: Double, pos) => NumberValue(DoubleType, n).pos(pos)
    case StringExpr(s, pos)         => StringValue(s).pos(pos)
    case VariableExpr(_, name)      => row.data(row.meta.columnIndices(name.s))
    case BinaryExpr(left, op @ ("+" | "-"), right) =>
      val l = neval(left, row, ctx)
      val r = neval(right, row, ctx)

      op match
        case "+" => BasicDAL.compute(PLUS, l, r, NumberValue.from)
        case "-" => BasicDAL.compute(MINUS, l, r, NumberValue.from)
    case BinaryExpr(left, op @ ("<" | ">" | "<=" | ">="), right) =>
      val l = neval(left, row, ctx)
      val r = neval(right, row, ctx)

      BooleanValue(
        op match
          case "<"  => BasicDAL.relate(LT, l, r)
          case ">"  => BasicDAL.relate(GT, l, r)
          case "<=" => BasicDAL.relate(LTE, l, r)
          case ">=" => BasicDAL.relate(GTE, l, r)
      )
    case BinaryExpr(left, op @ ("=" | "!="), right) =>
      val l = eval(left, row, ctx)
      val r = eval(right, row, ctx)

      BooleanValue(
        op match
          case "="  => l == r
          case "!=" => l != r
      )

def beval(expr: Expr, row: Row, ctx: Seq[Row]): Boolean = eval(expr, row, ctx).asInstanceOf[BooleanValue].b

def neval(expr: Expr, row: Row, ctx: Seq[Row]): NumberValue = eval(expr, row, ctx).asInstanceOf[NumberValue]

package io.github.edadma.rdb

import io.github.edadma.dal.{BasicDAL, DoubleType, IntType, TypedNumber, Type as DType}

import scala.annotation.tailrec

private val PLUS = Symbol("+")
private val MINUS = Symbol("-")
private val LT = Symbol("<")
private val GT = Symbol(">")
private val LTE = Symbol("<=")
private val GTE = Symbol(">=")

@tailrec
def lookup(v: String, ctx: Seq[Row]): Option[Value] =
  ctx match
    case Nil => None
    case hd :: tl =>
      hd.meta.columnIndices get v match
        case None      => lookup(v, tl)
        case Some(res) => Some(hd.data(res))

def eval(expr: Expr, ctx: Seq[Row]): Value =
  expr match
    case QueryExpr(operator)        => TableValue(operator.iterator(ctx).toSeq, operator.meta)
    case NumberExpr(n: Int, pos)    => NumberValue(IntType, n).pos(pos)
    case NumberExpr(n: Double, pos) => NumberValue(DoubleType, n).pos(pos)
    case StringExpr(s, pos)         => StringValue(s).pos(pos)
    case VariableExpr(table, name) =>
      val v =
        table match
          case None    => name.s
          case Some(t) => s"$t.name"

      lookup(v, ctx) match
        case None      => sys.error(s"variable '$v' not found")
        case Some(res) => res
    case BinaryExpr(left, op @ ("+" | "-"), right) =>
      val l = neval(left, ctx)
      val r = neval(right, ctx)

      op match
        case "+" => BasicDAL.compute(PLUS, l, r, NumberValue.from)
        case "-" => BasicDAL.compute(MINUS, l, r, NumberValue.from)
    case BinaryExpr(left, op @ ("<" | ">" | "<=" | ">="), right) =>
      val l = neval(left, ctx)
      val r = neval(right, ctx)

      BooleanValue(
        op match
          case "<"  => BasicDAL.relate(LT, l, r)
          case ">"  => BasicDAL.relate(GT, l, r)
          case "<=" => BasicDAL.relate(LTE, l, r)
          case ">=" => BasicDAL.relate(GTE, l, r)
      )
    case BinaryExpr(left, op @ ("=" | "!="), right) =>
      val l = eval(left, ctx)
      val r = eval(right, ctx)

      BooleanValue(
        op match
          case "="  => l == r
          case "!=" => l != r
      )

def beval(expr: Expr, ctx: Seq[Row]): Boolean = eval(expr, ctx).asInstanceOf[BooleanValue].b

def neval(expr: Expr, ctx: Seq[Row]): NumberValue = eval(expr, ctx).asInstanceOf[NumberValue]

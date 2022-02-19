package io.github.edadma.rdb

import io.github.edadma.dal.{BasicDAL, DoubleType, IntType, TypedNumber, Type as DType}

import scala.annotation.tailrec

private val PLUS = Symbol("+")
private val MINUS = Symbol("-")
private val LT = Symbol("<")
private val GT = Symbol(">")
private val LTE = Symbol("<=")
private val GTE = Symbol(">=")

def eval(expr: Expr, ctx: Seq[Row]): Value =
  expr match
    case UnaryExpr("EXISTS", expr)  => BooleanValue(aleval(expr, ctx).nonEmpty)
    case OperatorExpr(operator)     => TableValue(operator.iterator(ctx).toSeq, operator.meta)
    case NumberExpr(n: Int, pos)    => NumberValue(IntType, n).pos(pos)
    case NumberExpr(n: Double, pos) => NumberValue(DoubleType, n).pos(pos)
    case StringExpr(s, pos)         => StringValue(s).pos(pos)
    case VariableExpr(Ident(name, pos)) =>
      @tailrec
      def lookup(name: String, ctx: Seq[Row]): Option[Value] =
        ctx match
          case Nil => None
          case hd :: tl =>
            hd.meta.columnMap get name match
              case None              => lookup(name, tl)
              case Some((idx, _, _)) => Some(hd.data(idx))

      lookup(name, ctx) match
        case None      => sys.error(s"variable '$name' not found")
        case Some(res) => res
    case BinaryExpr(left, op @ ("+" | "-"), right) =>
      val l = neval(left, ctx)
      val r = neval(right, ctx)

      op match
        case "+" => BasicDAL.compute(PLUS, l, r, NumberValue.from)
        case "-" => BasicDAL.compute(MINUS, l, r, NumberValue.from)
    case BinaryExpr(left, "IN", right) =>
      val v = eval(left, ctx)
      val a = aleval(right, ctx)

      BooleanValue(a contains v)
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

def aleval(expr: Expr, ctx: Seq[Row]): ArrayLikeValue = eval(expr, ctx).asInstanceOf[ArrayLikeValue]

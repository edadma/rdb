package io.github.edadma.rdb

import io.github.edadma.dal.{BasicDAL, DoubleType, IntType, TypedNumber, Type as DType}

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

private val PLUS = Symbol("+")
private val MINUS = Symbol("-")
private val LT = Symbol("<")
private val GT = Symbol(">")
private val LTE = Symbol("<=")
private val GTE = Symbol(">=")

enum AggregateMode:
  case Result, Accumulate, Disallow

def eval(expr: Expr, ctx: Seq[Row], mode: AggregateMode): Value =
  expr match
    case AggregateFunctionExpr(f, arg, _) =>
      f.acc(eval(arg, ctx, mode))

      if mode == AggregateMode.Result then f.result
      else NullValue
    case ScalarFunctionExpr(f, args, _) => f.func(args map (e => eval(e, ctx, mode)))
    case UnaryExpr("EXISTS", expr, _)   => BooleanValue(aleval(expr, ctx, mode).nonEmpty)
    case ProcessOperator(proc)          => TableValue(proc.iterator(ctx) to ArraySeq, proc.meta)
    case NumberExpr(n: Int, pos)        => NumberValue(IntType, n).pos(pos)
    case NumberExpr(n: Double, pos)     => NumberValue(DoubleType, n).pos(pos)
    case StringExpr(s, pos)             => StringValue(s).pos(pos)
    case NullExpr(pos)                  => NullValue
    case ColumnExpr(Ident(name, pos), _) =>
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
    case BinaryExpr(left, op @ ("+" | "-"), right, _) =>
      val l = neval(left, ctx, mode)
      val r = neval(right, ctx, mode)

      op match
        case "+" => BasicDAL.compute(PLUS, l, r, NumberValue.from)
        case "-" => BasicDAL.compute(MINUS, l, r, NumberValue.from)
    case BinaryExpr(left, "IN", right, _) =>
      val v = eval(left, ctx, mode)
      val a = aleval(right, ctx, mode)

      BooleanValue(a contains v)
    case BinaryExpr(left, op @ ("<" | ">" | "<=" | ">="), right, _) =>
      val l = neval(left, ctx, mode)
      val r = neval(right, ctx, mode)

      BooleanValue(
        op match
          case "<"  => BasicDAL.relate(LT, l, r)
          case ">"  => BasicDAL.relate(GT, l, r)
          case "<=" => BasicDAL.relate(LTE, l, r)
          case ">=" => BasicDAL.relate(GTE, l, r)
      )
    case BinaryExpr(left, op @ ("=" | "!="), right, _) =>
      val l = eval(left, ctx, mode)
      val r = eval(right, ctx, mode)

      BooleanValue(
        op match
          case "="  => l == r
          case "!=" => l != r
      )

def beval(expr: Expr, ctx: Seq[Row]): Boolean =
//  println((expr, eval(expr, ctx).asInstanceOf[BooleanValue].b, ctx))
  eval(expr, ctx, AggregateMode.Disallow).asInstanceOf[BooleanValue].b

def neval(expr: Expr, ctx: Seq[Row], mode: AggregateMode): NumberValue = eval(expr, ctx, mode).asInstanceOf[NumberValue]

def aleval(expr: Expr, ctx: Seq[Row], mode: AggregateMode): ArrayLikeValue =
  eval(expr, ctx, mode).asInstanceOf[ArrayLikeValue]

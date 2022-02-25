package io.github.edadma.rdb

import io.github.edadma.dal.{BasicDAL, DoubleType, IntType, TypedNumber, Type as DType}

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

def eval(expr: Expr, ctx: Seq[Row], mode: AggregateMode): Value =
  expr match
    case AggregateFunctionExpr(f, arg) =>
      mode match
        case AggregateMode.Return => f.result
        case AggregateMode.Accumulate =>
          f.acc(eval(arg, ctx, mode))
          NullValue()
        case AggregateMode.AccumulateReturn =>
          f.acc(eval(arg, ctx, mode))
          f.result
        case AggregateMode.Disallow => sys.error(s"aggregates not allowed here: $expr")
    case ScalarFunctionExpr(f, args) => f.func(args map (e => eval(e, ctx, mode)))
    case ProcessOperator(proc)       => TableValue(proc.iterator(ctx) to ArraySeq, proc.meta)
    case e @ NumberExpr(n: Int)      => NumberValue(IntType, n).setPos(e.pos)
    case e @ NumberExpr(n: Double)   => NumberValue(DoubleType, n).setPos(e.pos)
    case e @ StringExpr(s)           => TextValue(s).setPos(e.pos)
    case e @ NullExpr()              => NullValue().setPos(e.pos)
    case e @ StarExpr()              => StarValue().setPos(e.pos)
    case c @ ColumnExpr(Ident(name)) =>
      @tailrec
      def lookup(name: String, ctx: Seq[Row]): Option[Value] =
        ctx match
          case Nil => None
          case hd :: tl =>
            hd.meta.columnMap get name match
              case None              => lookup(name, tl)
              case Some((idx, _, _)) => Some(hd.data(idx))

      lookup(name, ctx) match
        case None      => problem(c, s"'$name' not found")
        case Some(res) => res
    case InExpr(value, array) =>
      val v = eval(value, ctx, mode)
      val a = aleval(array, ctx, mode)

      BooleanValue(a contains v)
    case ExistsExpr(expr)       => BooleanValue(aleval(expr, ctx, mode).nonEmpty)
    case UnaryExpr("-", expr)   => BasicDAL.negate(neval(expr, ctx, mode), NumberValue.from)
    case UnaryExpr("NOT", expr) => BooleanValue(!beval(expr, ctx))
    case BinaryExpr(left, "||", right) =>
      val l = teval(left, ctx, mode)
      val r = teval(right, ctx, mode)

      TextValue(l.s ++ r.s)
    case BinaryExpr(left, op @ ("AND" | "OR"), right) =>
      val or = op == "OR"

      if or ^ !beval(left, ctx) then BooleanValue(or)
      else BooleanValue(beval(right, ctx))
    case BinaryExpr(left, op @ ("+" | "-" | "*" | "/"), right) =>
      val l = neval(left, ctx, mode)
      val r = neval(right, ctx, mode)

      op match
        case "+" => BasicDAL.compute(PLUS, l, r, NumberValue.from)
        case "-" => BasicDAL.compute(MINUS, l, r, NumberValue.from)
        case "*" => BasicDAL.compute(TIMES, l, r, NumberValue.from)
        case "/" => BasicDAL.compute(DIVIDE, l, r, NumberValue.from)
    case BinaryExpr(left, op @ ("<" | ">" | "<=" | ">="), right) =>
      val l = neval(left, ctx, mode)
      val r = neval(right, ctx, mode)

      BooleanValue(
        op match
          case "<"  => l < r
          case ">"  => l > r
          case "<=" => l <= r
          case ">=" => l >= r
      )
    case BinaryExpr(left, op @ ("=" | "!="), right) =>
      val l = eval(left, ctx, mode)
      val r = eval(right, ctx, mode)

      BooleanValue(
        op match
          case "="  => l == r
          case "!=" => l != r
      )

def beval(expr: Expr, ctx: Seq[Row]): Boolean = eval(expr, ctx, AggregateMode.Disallow).asInstanceOf[BooleanValue].b

def neval(expr: Expr, ctx: Seq[Row], mode: AggregateMode): NumberValue = eval(expr, ctx, mode).asInstanceOf[NumberValue]

def teval(expr: Expr, ctx: Seq[Row], mode: AggregateMode): TextValue = eval(expr, ctx, mode).asText

def aleval(expr: Expr, ctx: Seq[Row], mode: AggregateMode): ArrayLikeValue =
  eval(expr, ctx, mode).asInstanceOf[ArrayLikeValue]

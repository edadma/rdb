package io.github.edadma.rdb

import io.github.edadma.dal.{BasicDAL, DoubleType, IntType, TypedNumber, Type as DType}

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

//import pprint.*

def eval(expr: Expr, ctx: Seq[Row], mode: AggregateMode): Value =
  expr match
    case TableConstructorExpr(expr) => aleval(expr, ctx, mode)
    case AggregateFunctionExpr(f, arg) =>
      mode match
        case AggregateMode.Return => f.result
        case AggregateMode.Accumulate =>
          f.acc(eval(arg, ctx, mode))
          NULL
        case AggregateMode.AccumulateReturn =>
          f.acc(eval(arg, ctx, mode))

          val res = f.result

          f.init()
          res
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
    case InSeqExpr(value, op, exprs) =>
      val v = eval(value, ctx, mode)

      BooleanValue((op contains "NOT") ^ (exprs exists (e => eval(e, ctx, mode) == v)))
    case ExistsExpr(expr)       => BooleanValue(aleval(expr, ctx, mode).nonEmpty)
    case UnaryExpr("-", expr)   => BasicDAL.negate(neval(expr, ctx, mode), NumberValue.from)
    case UnaryExpr("NOT", expr) => BooleanValue(!beval(expr, ctx))
    case UnaryExpr(op @ ("IS NULL" | "IS NOT NULL"), expr) =>
      BooleanValue((op contains "NOT") ^ eval(expr, ctx, mode).isNull)
    case BinaryExpr(left, "||", right) =>
      val l = teval(left, ctx, mode)
      val r = teval(right, ctx, mode)

      TextValue(l.s ++ r.s)
    case BinaryExpr(left, op @ ("AND" | "OR"), right) =>
      val or = op == "OR"

      if or ^ !beval(left, ctx) then BooleanValue(or)
      else BooleanValue(beval(right, ctx))
    case BinaryExpr(left, op @ ("LIKE" | "ILIKE" | "NOT LIKE" | "NOT ILIKE"), right) =>
      def like(s: String, pattern: String, casesensitive: Boolean = true): Boolean =
        var sp = 0
        var pp = 0
        val choices = new mutable.Stack[ChoicePoint]

        case class ChoicePoint(sp: Int, pp: Int)

        def move(): Unit = {
          sp += 1
          pp += 1
        }

        def choice: Boolean =
          if (choices nonEmpty) {
            val ChoicePoint(nsp, npp) = choices.pop()

            sp = nsp
            pp = npp
            true
          } else false

        while (sp < s.length || pp < pattern.length) {
          if (pp == pattern.length && !choice)
            return false
          else
            pattern(pp) match {
              case '%' =>
                if (pp == pattern.length - 1)
                  return true

                if (sp < s.length - 1)
                  choices push ChoicePoint(sp + 1, pp)

                pp += 1
              case '_' => move()
              case c =>
                if (c == '\\')
                  pp += 1

                if (
                  sp < s.length && ((casesensitive && s(sp) == pattern(pp)) || (!casesensitive && s(
                    sp
                  ).toLower == pattern(pp).toLower))
                )
                  move()
                else if (!choice)
                  return false
            }
        }

        true

      val s = teval(left, ctx, mode).s
      val p = teval(right, ctx, mode).s
      val res = like(s, p, op contains "ILIKE")

      BooleanValue((op startsWith "NOT") ^ res)
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

def neval(expr: Expr, ctx: Seq[Row], mode: AggregateMode): NumberValue =
  val v = eval(expr, ctx, mode)

  if v.vtyp != NumberType then problem(expr, "a number was expected")

  v.asInstanceOf[NumberValue]

def teval(expr: Expr, ctx: Seq[Row], mode: AggregateMode): TextValue = eval(expr, ctx, mode).toText

def aleval(expr: Expr, ctx: Seq[Row], mode: AggregateMode): ArrayLikeValue =
  eval(expr, ctx, mode).asInstanceOf[ArrayLikeValue]
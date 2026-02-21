package io.github.edadma.rdb

import io.github.edadma.dal.{BasicDAL, DoubleType, IntType, TypedNumber, Type as DType}
import java.time.Duration

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.language.postfixOps

private[rdb] var currentParams: IndexedSeq[Value] = IndexedSeq.empty

def eval(expr: Expr, ctx: Seq[Row]): Value =
  expr match
    case ParameterExpr(index) =>
      if index < 1 || index > currentParams.length then
        problem(expr, s"parameter $$$index is not bound (have ${currentParams.length} parameters)")
      currentParams(index - 1)
    case CastExpr(expr, targetType)    => targetType.convert(eval(expr, ctx))
    case AliasExpr(expr, _)            => eval(expr, ctx)
    case VariableInstanceExpr(v)       => v.value
    case TableConstructorExpr(expr)    => aleval(expr, ctx)
    case AggregateFunctionExpr(_, _)   => sys.error(s"aggregate function not resolved by rewriter: $expr")
    case ScalarFunctionExpr(f, args)        => f.func(args map (e => eval(e, ctx)))
    case ProcessOperator(proc)              => TableValue(proc.iterator(ctx) to ArraySeq, proc.meta)
    case e @ NumberExpr(n: Int)             => NumberValue(IntType, n).setPos(e.pos)
    case e @ NumberExpr(n: Double)          => NumberValue(DoubleType, n).setPos(e.pos)
    case e @ StringExpr(s)                  => TextValue(s).setPos(e.pos)
    case e @ NullExpr()                     => NullValue().setPos(e.pos)
    case e @ BooleanExpr(b)                 => BooleanValue(b).setPos(e.pos)
    case e @ StarExpr()                     => StarValue().setPos(e.pos)
    case c @ ColumnExpr(table, Ident(name)) =>
      val lookupName = table.map(t => s"${t.name}.$name").getOrElse(name)

      @tailrec
      def lookup(name: String, ctx: Seq[Row]): Option[Value] =
        ctx match
          case Nil      => None
          case hd :: tl =>
            hd.meta.columnMap get name match
              case None              => lookup(name, tl)
              case Some((idx, _, _)) => Some(hd.data(idx))

      lookup(lookupName, ctx) match
        case None      => problem(c, s"'$lookupName' not found")
        case Some(res) => res
    case InSeqExpr(value, op, exprs) =>
      val v = eval(value, ctx)

      BooleanValue(op.contains("NOT") ^ (exprs exists (e => eval(e, ctx) == v)))
    case InQueryExpr(value, op, query) =>
      val v   = eval(value, ctx)
      val res = teval(query, ctx)

      if res.meta.width != 1 then problem(query, "sub-query must return rows of one column")

      BooleanValue(op.contains("NOT") ^ (res.data exists (_.data.head == v)))
    case SubqueryExpr(query) =>
      val res = teval(query, ctx)

      if res.isEmpty then problem(query, "sub-query returned empty result")
      else if res.length > 1 then problem(query, "sub-query returned more than one row")
      else if res.data.head.data.length != 1 then problem(query, "sub-query must return a row of one column")

      res.data.head.data.head
    case ExistsExpr(expr)                                  => BooleanValue(aleval(expr, ctx).nonEmpty)
    case UnaryExpr("-", expr)                              => BasicDAL.negate(neval(expr, ctx), NumberValue.from)
    case UnaryExpr("NOT", expr)                            => BooleanValue(!beval(expr, ctx))
    case UnaryExpr(op @ ("IS NULL" | "IS NOT NULL"), expr) =>
      BooleanValue(op.contains("NOT") ^ eval(expr, ctx).isNull)
    case BinaryExpr(left, "||", right) =>
      val l = seval(left, ctx)
      val r = seval(right, ctx)

      TextValue(l ++ r)
    case BinaryExpr(left, op @ ("AND" | "OR"), right) =>
      val or = op == "OR"

      if or ^ !beval(left, ctx) then BooleanValue(or)
      else BooleanValue(beval(right, ctx))
    case BinaryExpr(left, op @ ("LIKE" | "ILIKE" | "NOT LIKE" | "NOT ILIKE"), right) =>
      def like(s: String, pattern: String, casesensitive: Boolean = true): Boolean =
        var sp      = 0
        var pp      = 0
        val choices = new mutable.Stack[ChoicePoint]

        case class ChoicePoint(sp: Int, pp: Int)

        def move(): Unit = {
          sp += 1
          pp += 1
        }

        def choice: Boolean =
          if (choices.nonEmpty) {
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
              case c   =>
                if (c == '\\')
                  pp += 1

                if (
                  sp < s.length && ((casesensitive && s(sp) == pattern(pp)) || (!casesensitive && s(
                    sp,
                  ).toLower == pattern(pp).toLower))
                )
                  move()
                else if (!choice)
                  return false
            }
        }

        true

      val s   = seval(left, ctx)
      val p   = seval(right, ctx)
      val res = like(s, p, !op.contains("ILIKE"))

      BooleanValue(op.contains("NOT") ^ res)
    case BinaryExpr(left, op @ ("+" | "-" | "*" | "/" | "%"), right) =>
      val l = eval(left, ctx)
      val r = eval(right, ctx)

      (l, op, r) match
        // number op number (existing behavior)
        case (ln: NumberValue, "+", rn: NumberValue) => BasicDAL.compute(PLUS, ln, rn, NumberValue.from)
        case (ln: NumberValue, "-", rn: NumberValue) => BasicDAL.compute(MINUS, ln, rn, NumberValue.from)
        case (ln: NumberValue, "*", rn: NumberValue) => BasicDAL.compute(TIMES, ln, rn, NumberValue.from)
        case (ln: NumberValue, "/", rn: NumberValue) => BasicDAL.compute(DIVIDE, ln, rn, NumberValue.from)
        case (NumberValue(_, a), "%", NumberValue(_, b)) => NumberValue(a.doubleValue % b.doubleValue)
        // date +/- int (days)
        case (DateValue(d), "+", NumberValue(_, n))  => DateValue(d.plusDays(n.longValue))
        case (DateValue(d), "-", NumberValue(_, n))  => DateValue(d.minusDays(n.longValue))
        // date - date => integer (days between)
        case (DateValue(d1), "-", DateValue(d2))     => NumberValue(java.time.temporal.ChronoUnit.DAYS.between(d2, d1).toInt)
        // date + interval => timestamp
        case (DateValue(d), "+", IntervalValue(dur))   => TimestampValue(d.atStartOfDay.plus(dur))
        case (IntervalValue(dur), "+", DateValue(d))   => TimestampValue(d.atStartOfDay.plus(dur))
        // timestamp +/- interval => timestamp
        case (TimestampValue(t), "+", IntervalValue(dur)) => TimestampValue(t.plus(dur))
        case (TimestampValue(t), "-", IntervalValue(dur)) => TimestampValue(t.minus(dur))
        // timestamp - timestamp => interval
        case (TimestampValue(t1), "-", TimestampValue(t2)) => IntervalValue(Duration.between(t2, t1))
        // interval +/- interval => interval
        case (IntervalValue(d1), "+", IntervalValue(d2)) => IntervalValue(d1.plus(d2))
        case (IntervalValue(d1), "-", IntervalValue(d2)) => IntervalValue(d1.minus(d2))
        // interval * number / number * interval
        case (IntervalValue(d), "*", NumberValue(_, n))  => IntervalValue(d.multipliedBy(n.longValue))
        case (NumberValue(_, n), "*", IntervalValue(d))  => IntervalValue(d.multipliedBy(n.longValue))
        case (IntervalValue(d), "/", NumberValue(_, n))  => IntervalValue(d.dividedBy(n.longValue))
        // timestamptz +/- interval
        case (TimestampTZValue(t), "+", IntervalValue(dur)) => TimestampTZValue(t.plus(dur))
        case (TimestampTZValue(t), "-", IntervalValue(dur)) => TimestampTZValue(t.minus(dur))
        // timestamptz - timestamptz => interval
        case (TimestampTZValue(t1), "-", TimestampTZValue(t2)) => IntervalValue(Duration.between(t2, t1))
        case _ => problem(left, s"cannot apply '$op' to ${l.vtyp.name} and ${r.vtyp.name}")
    case BinaryExpr(left, op @ ("<" | ">" | "<=" | ">="), right) =>
      val l = eval(left, ctx)
      val r = eval(right, ctx)

      BooleanValue(
        op match
          case "<"  => l < r
          case ">"  => l > r
          case "<=" => l <= r
          case ">=" => l >= r,
      )
    case BinaryExpr(left, op @ ("=" | "!="), right) =>
      val l = eval(left, ctx)
      val r = eval(right, ctx)

      BooleanValue(
        op match
          case "="  => l.compare(r) == 0
          case "!=" => l.compare(r) != 0,
      )
    case ObjectExpr(properties) =>
      val keys = new mutable.HashSet[String]

      ObjectValue(properties map { case (id @ Ident(k), v) =>
        if keys(k) then problem(id, s"duplicate property key: $k")

        k -> eval(v, ctx)
      })
    case ArrayExpr(elems)     => ArrayValue(elems map (e => eval(e, ctx)) toIndexedSeq)
    case CaseExpr(whens, els) =>
      whens find { case When(when, _) => beval(when, ctx) } match
        case None =>
          els match
            case None    => NullValue()
            case Some(e) => eval(e, ctx)
        case Some(When(_, expr)) => eval(expr, ctx)

def beval(expr: Expr, ctx: Seq[Row]): Boolean =
  eval(expr, ctx).asInstanceOf[BooleanValue].b

def neval(expr: Expr, ctx: Seq[Row]): NumberValue =
  val v = eval(expr, ctx)

  if v.vtyp != NumberType then problem(expr, "a number was expected")

  v.asInstanceOf[NumberValue]

def seval(expr: Expr, ctx: Seq[Row]): String = eval(expr, ctx).string

def teval(expr: Expr, ctx: Seq[Row]): TableValue = eval(expr, ctx).asInstanceOf[TableValue]

def aleval(expr: Expr, ctx: Seq[Row]): ArrayLikeValue =
  eval(expr, ctx).asInstanceOf[ArrayLikeValue]

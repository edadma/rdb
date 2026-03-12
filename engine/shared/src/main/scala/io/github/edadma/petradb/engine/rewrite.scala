package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import scala.collection.mutable
import scala.util.parsing.input.Position

class AggregateCollector:
  private val specs = mutable.ArrayBuffer[AggregateSpec]()
  private val seen = mutable.Map[String, String]() // canonical key → column name
  private var counter = 0

  private def canonicalKey(funcName: String, args: Seq[Expr], filter: Option[Expr]): String =
    val filterStr = filter.map(f => s" FILTER($f)").getOrElse("")
    s"$funcName(${args.mkString(", ")})$filterStr"

  def collect(expr: Expr): Expr =
    expr match
      case AggregateFunctionExpr(f, args, filter) =>
        val key = canonicalKey(f.name, args, filter)
        val colName = seen.getOrElseUpdate(key, {
          counter += 1
          val name = s"_agg_$counter"
          specs += AggregateSpec(name, f, args, expr.typ.asInstanceOf[Type], filter)
          name
        })
        ColumnExpr(None, Ident(colName)) setType expr.typ
      case AliasExpr(inner, alias)       => AliasExpr(collect(inner), alias)
      case CastExpr(inner, t)            => CastExpr(collect(inner), t) setType expr.typ
      case UnaryExpr(op, inner)          => UnaryExpr(op, collect(inner)) setType expr.typ
      case BinaryExpr(l, op, r)          => BinaryExpr(collect(l), op, collect(r)) setType expr.typ
      case ScalarFunctionExpr(f, args)   => ScalarFunctionExpr(f, args.map(collect))
      case CaseExpr(whens, els) =>
        CaseExpr(
          whens.map { case When(w, e) => When(collect(w), collect(e)) },
          els.map(collect),
        )
      case _: WindowExpr => expr // window expressions handle their own aggregation
      case _ => expr

  def result: Seq[AggregateSpec] = specs.toSeq

  def hasAggregates: Boolean = specs.nonEmpty

def aggregate(expr: Expr): Boolean =
  expr match
    case _: AggregateFunctionExpr    => true
    case _: WindowExpr               => false // window functions are not aggregates
    case AliasExpr(expr, _)          => aggregate(expr)
    case CastExpr(expr, _)           => aggregate(expr)
    case ScalarFunctionExpr(_, args) => args exists aggregate
    case UnaryExpr(_, expr)          => aggregate(expr)
    case BinaryExpr(left, _, right)  => aggregate(left) | aggregate(right)
    case CaseExpr(whens, els) =>
      whens.exists { case When(w, e) => aggregate(w) || aggregate(e) } ||
        els.exists(aggregate)
    case _                           => false

def window(expr: Expr): Boolean =
  expr match
    case _: WindowExpr               => true
    case AliasExpr(expr, _)          => window(expr)
    case CastExpr(expr, _)           => window(expr)
    case UnaryExpr(_, expr)          => window(expr)
    case BinaryExpr(left, _, right)  => window(left) || window(right)
    case ScalarFunctionExpr(_, args) => args.exists(window)
    case CaseExpr(whens, els) =>
      whens.exists { case When(w, e) => window(w) || window(e) } ||
        els.exists(window)
    case _                           => false

private val windowOnlyFunctions = Set("row_number", "rank", "dense_rank", "lag", "lead", "ntile")

class WindowCollector:
  private val specs = mutable.ArrayBuffer[WindowSpec]()
  private val seen = mutable.Map[String, String]()
  private var counter = 0

  private def canonicalKey(kind: WindowFunctionKind, partBy: Seq[Expr], ordBy: Seq[OrderBy]): String =
    s"$kind OVER (${partBy.mkString(", ")}; ${ordBy.mkString(", ")})"

  def collect(expr: Expr): Expr =
    expr match
      case w @ WindowExpr(_, partBy, ordBy, frame) =>
        val kind = w.func match
          case ApplyExpr(Ident(name), args, _) =>
            name.toLowerCase match
              case "row_number"  => RowNumberKind
              case "rank"        => RankKind
              case "dense_rank"  => DenseRankKind
              case "lag" =>
                val offset = if args.length >= 2 then eval(args(1), Nil).intValue else 1
                val default = args.lift(2)
                LagKind(args.head, offset, default)
              case "lead" =>
                val offset = if args.length >= 2 then eval(args(1), Nil).intValue else 1
                val default = args.lift(2)
                LeadKind(args.head, offset, default)
              case "ntile" =>
                val buckets = eval(args.head, Nil).intValue
                NtileKind(buckets)
              case _ => sys.error(s"unresolved window function: $name")
          case AggregateFunctionExpr(f, args, filter) =>
            AggregateWindowKind(aggregateFunction(f.name), args, filter)
          case other => sys.error(s"unexpected window function expression: $other")
        val key = canonicalKey(kind, partBy, ordBy)
        val colName = seen.getOrElseUpdate(key, {
          counter += 1
          val name = s"_win_$counter"
          specs += WindowSpec(name, kind, partBy, ordBy, w.typ.asInstanceOf[Type], frame)
          name
        })
        ColumnExpr(None, Ident(colName)) setType w.typ
      case AliasExpr(inner, alias)       => AliasExpr(collect(inner), alias)
      case CastExpr(inner, t)            => CastExpr(collect(inner), t) setType expr.typ
      case UnaryExpr(op, inner)          => UnaryExpr(op, collect(inner)) setType expr.typ
      case BinaryExpr(l, op, r)          => BinaryExpr(collect(l), op, collect(r)) setType expr.typ
      case ScalarFunctionExpr(f, args)   => ScalarFunctionExpr(f, args.map(collect))
      case CaseExpr(whens, els) =>
        CaseExpr(
          whens.map { case When(w, e) => When(collect(w), collect(e)) },
          els.map(collect),
        )
      case _ => expr

  def result: Seq[WindowSpec] = specs.toSeq
  def hasWindows: Boolean = specs.nonEmpty

def resolveAliases(expr: Expr, aliases: Map[String, Expr]): Expr =
  expr match
    case ColumnExpr(None, Ident(name)) =>
      aliases.get(name) match
        case Some(underlying) => underlying
        case None             => expr
    case BinaryExpr(l, op, r) =>
      BinaryExpr(resolveAliases(l, aliases), op, resolveAliases(r, aliases)) setType expr.typ
    case UnaryExpr(op, e) =>
      UnaryExpr(op, resolveAliases(e, aliases)) setType expr.typ
    case ScalarFunctionExpr(f, args) =>
      ScalarFunctionExpr(f, args.map(a => resolveAliases(a, aliases)))
    case CastExpr(e, t) =>
      CastExpr(resolveAliases(e, aliases), t) setType expr.typ
    case _ => expr

private def evalCountExpr(pos: Position, expr: Expr, label: String)(using session: Session): Int =
  val v = eval(rewrite(expr), Nil)
  v match
    case NumberValue(_, n) => n.intValue
    case other             => throw ExecutionException(pos, s"$label must be an integer, got: $other")

def rewrite(expr: Expr)(using session: Session): Expr =
  expr match
    case _ if expr.typ != null              => expr
    case CastExpr(expr, targetType)         => CastExpr(rewrite(expr), targetType) setType targetType
    case AliasExpr(expr, alias) => AliasExpr(rewrite(expr), alias)
    case SubqueryExpr(query)    => SubqueryExpr(rewrite(query))
    case CaseExpr(whens, els)   =>
      CaseExpr(whens map { case When(when, expr) => When(rewrite(when), rewrite(expr)) }, els map rewrite)
    case InSeqExpr(value, op, exprs)       => InSeqExpr(rewrite(value), op, exprs map rewrite)
    case InQueryExpr(value, op, array)     => InQueryExpr(rewrite(value), op, rewrite(array))
    case QuantifiedCompareExpr(value, op, quantifier, expr) =>
      QuantifiedCompareExpr(rewrite(value), op, quantifier, rewrite(expr))
    case TableConstructorExpr(expr)        => TableConstructorExpr(rewrite(expr))
    case WindowExpr(ApplyExpr(id @ Ident(func), args, filter), partBy, ordBy, frame) =>
      val rwPartBy = partBy.map(rewrite)
      val rwOrdBy = ordBy.map { case OrderBy(f, d, n) => OrderBy(rewrite(f), d, n) }
      func.toLowerCase match
        case "row_number" =>
          if args.nonEmpty then throw ParseException(id.pos, "ROW_NUMBER takes no arguments")
          WindowExpr(ApplyExpr(id, Nil, None), rwPartBy, rwOrdBy, frame) setType NumberType
        case "rank" =>
          if args.nonEmpty then throw ParseException(id.pos, "RANK takes no arguments")
          WindowExpr(ApplyExpr(id, Nil, None), rwPartBy, rwOrdBy, frame) setType NumberType
        case "dense_rank" =>
          if args.nonEmpty then throw ParseException(id.pos, "DENSE_RANK takes no arguments")
          WindowExpr(ApplyExpr(id, Nil, None), rwPartBy, rwOrdBy, frame) setType NumberType
        case "lag" =>
          if args.isEmpty || args.length > 3 then throw ParseException(id.pos, "LAG requires 1 to 3 arguments")
          val rwArgs = args.map(rewrite)
          WindowExpr(ApplyExpr(id, rwArgs, None), rwPartBy, rwOrdBy, frame) setType rwArgs.head.typ
        case "lead" =>
          if args.isEmpty || args.length > 3 then throw ParseException(id.pos, "LEAD requires 1 to 3 arguments")
          val rwArgs = args.map(rewrite)
          WindowExpr(ApplyExpr(id, rwArgs, None), rwPartBy, rwOrdBy, frame) setType rwArgs.head.typ
        case "ntile" =>
          if args.length != 1 then throw ParseException(id.pos, "NTILE requires exactly 1 argument")
          val rwArgs = args.map(rewrite)
          WindowExpr(ApplyExpr(id, rwArgs, None), rwPartBy, rwOrdBy, frame) setType NumberType
        case _ =>
          aggregateFunction get func.toLowerCase match
            case Some(f) =>
              val (instance, typ) = f.instantiate
              val rwArgs = args.map(rewrite)
              val rwFilter = filter.map(rewrite)
              WindowExpr(AggregateFunctionExpr(instance, rwArgs, rwFilter), rwPartBy, rwOrdBy, frame) setType typ
            case None =>
              scalarFunction get func.toLowerCase match
                case Some(_) => throw ParseException(id.pos, s"scalar function '$func' cannot be used as a window function")
                case None    => throw UndefinedReferenceException(id.pos, s"unknown function '$func'")
    case ApplyExpr(id @ Ident(func), args, filter) =>
      func.toLowerCase match
        case "generate_series" =>
          if filter.isDefined then throw ParseException(id.pos, "FILTER is not allowed on generate_series")
          val rwArgs = args map rewrite
          ProcessOperator(GenerateSeriesProcess(rwArgs(0), rwArgs(1), rwArgs.lift(2)))
        case "nextval" =>
          if filter.isDefined then throw ParseException(id.pos, "FILTER is not allowed on scalar functions")
          ScalarFunctionExpr(
            ScalarFunction("nextval", { case Seq(nameVal) =>
              val seqName = nameVal.string
              val seq = session.db.getSequence(seqName).getOrElse(sys.error(s"relation \"$seqName\" does not exist"))
              val v = seq.nextval()
              session.sequenceValues(seqName) = v
              session.lastSequenceUsed = Some(seqName)
              NumberValue(v.toInt)
            }, NumberType),
            args map rewrite,
          )
        case "currval" =>
          if filter.isDefined then throw ParseException(id.pos, "FILTER is not allowed on scalar functions")
          ScalarFunctionExpr(
            ScalarFunction("currval", { case Seq(nameVal) =>
              val seqName = nameVal.string
              if !session.db.hasSequence(seqName) then sys.error(s"relation \"$seqName\" does not exist")
              val v = session.sequenceValues.getOrElse(seqName, sys.error(s"currval of sequence \"$seqName\" is not yet defined in this session"))
              NumberValue(v.toInt)
            }, NumberType),
            args map rewrite,
          )
        case "setval" =>
          if filter.isDefined then throw ParseException(id.pos, "FILTER is not allowed on scalar functions")
          ScalarFunctionExpr(
            ScalarFunction("setval", { case params =>
              val seqName = params.head.string
              val value = params(1).longValue
              val isCalled = if params.length > 2 then params(2).asInstanceOf[BooleanValue].b else true
              val seq = session.db.getSequence(seqName).getOrElse(sys.error(s"relation \"$seqName\" does not exist"))
              seq.setval(value, isCalled)
              session.sequenceValues(seqName) = value
              session.lastSequenceUsed = Some(seqName)
              NumberValue(value.toInt)
            }, NumberType),
            args map rewrite,
          )
        case "lastval" =>
          if filter.isDefined then throw ParseException(id.pos, "FILTER is not allowed on scalar functions")
          ScalarFunctionExpr(
            ScalarFunction("lastval", { case Seq() =>
              val seqName = session.lastSequenceUsed.getOrElse(sys.error("lastval is not yet defined in this session"))
              NumberValue(session.sequenceValues(seqName).toInt)
            }, NumberType),
            Seq.empty,
          )
        case _ =>
          scalarFunction get func.toLowerCase match
            case None =>
              aggregateFunction get func.toLowerCase match
                case None    => throw UndefinedReferenceException(id.pos, s"unknown function '$func'")
                case Some(f) =>
                  val (instance, typ) = f.instantiate
                  AggregateFunctionExpr(instance, args map rewrite, filter map rewrite) setType typ
            case Some(f) =>
              if filter.isDefined then throw ParseException(id.pos, "FILTER is not allowed on scalar functions")
              ScalarFunctionExpr(f, args map rewrite)
    case VariableExpr(id @ Ident(name)) =>
      scalarVariable get name match
        case None    => throw UndefinedReferenceException(id.pos, s"unknown variable '$name'")
        case Some(v) => VariableInstanceExpr(v.instance)
    case ExistsExpr(subquery) => ExistsExpr(rewrite(subquery)) setType BooleanType
    case UnaryExpr(op, expr)  =>
      val e = rewrite(expr)

      UnaryExpr(op, e) setType e.typ
    case BinaryExpr(left, op @ ("+" | "-" | "*" | "/" | "^" | "AND" | "OR" | "->" | "#>" | "||"), right) =>
      val l = rewrite(left)
      val r = rewrite(right)

      BinaryExpr(l, op, r) setType l.typ
    case BinaryExpr(left, op @ ("->>" | "#>>"), right) =>
      BinaryExpr(rewrite(left), op, rewrite(right)) setType TextType
    case BinaryExpr(left, op @ ("<=" | ">=" | "!=" | "=" | "<" | ">" | "LIKE" | "ILIKE" | "@>" | "<@" | "&&" | "?" | "?|" | "?&" | "IS DISTINCT FROM" | "IS NOT DISTINCT FROM"), right) =>
      BinaryExpr(rewrite(left), op, rewrite(right)) setType BooleanType
    case OverlapsExpr(s1, e1, s2, e2) =>
      BinaryExpr(
        BinaryExpr(rewrite(s1), "<", rewrite(e2)) setType BooleanType,
        "AND",
        BinaryExpr(rewrite(s2), "<", rewrite(e1)) setType BooleanType,
      ) setType BooleanType
    case BetweenExpr(value, op, lower, upper) =>
      val v = rewrite(value)
      val l = rewrite(lower)
      val r = rewrite(upper)

      op match
        case "BETWEEN" =>
          BinaryExpr(
            BinaryExpr(lower, "<=", value) setType BooleanType,
            "AND",
            BinaryExpr(value, "<=", upper) setType BooleanType,
          ) setType BooleanType
        case "NOT BETWEEN" =>
          BinaryExpr(
            BinaryExpr(value, "<", lower) setType BooleanType,
            "OR",
            BinaryExpr(value, ">", upper) setType BooleanType,
          ) setType BooleanType
        case "BETWEEN SYMMETRIC" =>
          // (value BETWEEN lower AND upper) OR (value BETWEEN upper AND lower)
          BinaryExpr(
            BinaryExpr(
              BinaryExpr(lower, "<=", value) setType BooleanType,
              "AND",
              BinaryExpr(value, "<=", upper) setType BooleanType,
            ) setType BooleanType,
            "OR",
            BinaryExpr(
              BinaryExpr(upper, "<=", value) setType BooleanType,
              "AND",
              BinaryExpr(value, "<=", lower) setType BooleanType,
            ) setType BooleanType,
          ) setType BooleanType
        case "NOT BETWEEN SYMMETRIC" =>
          // NOT ((value BETWEEN lower AND upper) OR (value BETWEEN upper AND lower))
          UnaryExpr("NOT",
            BinaryExpr(
              BinaryExpr(
                BinaryExpr(lower, "<=", value) setType BooleanType,
                "AND",
                BinaryExpr(value, "<=", upper) setType BooleanType,
              ) setType BooleanType,
              "OR",
              BinaryExpr(
                BinaryExpr(upper, "<=", value) setType BooleanType,
                "AND",
                BinaryExpr(value, "<=", lower) setType BooleanType,
              ) setType BooleanType,
            ) setType BooleanType,
          ) setType BooleanType
    case SQLSelectExpr(exprs, None, where, groupBy, having, orderBy, offset, limit, _) =>
      if where.isDefined then throw ParseException(where.get.pos, "WHERE clause not allowed here")
      if groupBy.isDefined then throw ParseException(where.get.pos, "GROUP BY clause not allowed here")
      if having.isDefined then throw ParseException(where.get.pos, "HAVING clause not allowed here")
      if orderBy.isDefined then throw ParseException(where.get.pos, "ORDER BY clause not allowed here")
      if offset.isDefined then throw ParseException(offset.get.pos, "OFFSET clause not allowed here")
      if limit.isDefined then throw ParseException(limit.get.pos, "LIMIT clause not allowed here")

      val rewritten_projs = exprs map rewrite

      ProcessOperator(ProjectProcess(SingleProcess, rewritten_projs))
    case LateralExpr(rel) => rewrite(rel)
    case SQLSelectExpr(exprs, Some(from), where, groupBy, having, orderBy, offset, limit, distinct) =>
      def isLateral(e: Expr): Boolean = e match
        case LateralExpr(_)                            => true
        case AliasOperator(LateralExpr(_), _)          => true
        case ColumnAliasOperator(LateralExpr(_), _, _) => true
        case _                                         => false

      def stripLateral(e: Expr): Expr = e match
        case LateralExpr(rel)                            => rel
        case AliasOperator(LateralExpr(rel), a)          => AliasOperator(rel, a)
        case ColumnAliasOperator(LateralExpr(rel), a, c) => ColumnAliasOperator(rel, a, c)
        case other                                       => other

      val rewrittenFrom = from.map {
        case e if isLateral(e) =>
          e match
            case LateralExpr(rel)                            => LateralExpr(rewrite(rel))
            case AliasOperator(LateralExpr(rel), a)          => AliasOperator(LateralExpr(rewrite(rel)), a)
            case ColumnAliasOperator(LateralExpr(rel), a, c) => ColumnAliasOperator(LateralExpr(rewrite(rel)), a, c)
            case _                                           => rewrite(e)
        case e => rewrite(e)
      }

      val r = rewrittenFrom.reduceLeft { (left, right) =>
        if isLateral(right) then LateralCrossOperator(left, stripLateral(right))
        else CrossOperator(left, right)
      }
      val r1 =
        where match
          case Some(cond) => SelectOperator(r, rewrite(cond))
          case None       => r

      val rewrittenExprs = exprs map rewrite
      val isGrouped = groupBy.isDefined || (rewrittenExprs exists aggregate)

      val r_ordered =
        if isGrouped then
          // Build alias map for resolving HAVING/ORDER BY references
          val aliasMap: Map[String, Expr] = rewrittenExprs.collect {
            case AliasExpr(underlying, Ident(name)) => name -> underlying
          }.toMap

          // Create collector and collect aggregates from SELECT exprs
          val collector = new AggregateCollector

          val collectedExprs = rewrittenExprs.map(collector.collect)

          // Collect aggregates from HAVING (resolve aliases first)
          val collectedHaving = having.map { cond =>
            val rewritten = rewrite(cond)
            val resolved = resolveAliases(rewritten, aliasMap)
            collector.collect(resolved)
          }

          // Collect aggregates from ORDER BY (resolve ordinals and aliases first)
          val collectedOrderBy = orderBy.map { os =>
            os.map { case OrderBy(f, d, n) =>
              val ordinalResolved = f match
                case NumberExpr(idx: Int) if idx >= 1 && idx <= rewrittenExprs.length =>
                  rewrittenExprs(idx - 1) match
                    case AliasExpr(inner, _) => inner
                    case other               => other
                case _ => rewrite(f)
              val resolved = resolveAliases(ordinalResolved, aliasMap)
              OrderBy(collector.collect(resolved), d, n)
            }
          }

          val groupByExprs = groupBy.map(_.map(rewrite)).getOrElse(Nil)

          // Build: source → AggregateOperator → HAVING → [WINDOW] → ORDER BY → PROJECT
          val r2 = AggregateOperator(r1, groupByExprs, collector.result)
          val r3 =
            collectedHaving match
              case Some(cond) => HavingOperator(r2, cond)
              case None       => r2

          // Window function collection on post-aggregate expressions
          val winCollector = new WindowCollector
          val winCollectedExprs = collectedExprs.map(winCollector.collect)
          val winCollectedOrderBy = collectedOrderBy.map(_.map { case OrderBy(f, d, n) => OrderBy(winCollector.collect(f), d, n) })
          val r3w = if winCollector.hasWindows then WindowOperator(r3, winCollector.result) else r3

          val r4 =
            winCollectedOrderBy match
              case Some(os) => SortOperator(r3w, os)
              case None     => r3w
          exprs match
            case Seq(StarExpr()) => r4
            case _               => ProjectOperator(r4, winCollectedExprs)
        else
          // Non-grouped path: [WINDOW] → ORDER BY → PROJECT
          val hasWindows = rewrittenExprs.exists(window)

          if hasWindows then
            val winCollector = new WindowCollector
            val winCollectedExprs = rewrittenExprs.map(winCollector.collect)
            val winCollectedOrderBy = orderBy.map { os =>
              os.map { case OrderBy(f, d, n) =>
                val resolved = f match
                  case NumberExpr(idx: Int) if idx >= 1 && idx <= rewrittenExprs.length =>
                    rewrittenExprs(idx - 1) match
                      case AliasExpr(inner, _) => inner
                      case other               => other
                  case _ => rewrite(f)
                OrderBy(winCollector.collect(resolved), d, n)
              }
            }
            val r1w = WindowOperator(r1, winCollector.result)
            val r2 =
              winCollectedOrderBy match
                case None     => r1w
                case Some(os) => SortOperator(r1w, os)
            exprs match
              case Seq(StarExpr()) => r2
              case _               => ProjectOperator(r2, winCollectedExprs)
          else
            val r2 =
              orderBy match
                case None     => r1
                case Some(os) => SortOperator(r1, os map { case OrderBy(f, d, n) =>
                  val resolved = f match
                    case NumberExpr(idx: Int) if idx >= 1 && idx <= rewrittenExprs.length =>
                      rewrittenExprs(idx - 1) match
                        case AliasExpr(inner, _) => inner
                        case other               => other
                    case _ => rewrite(f)
                  OrderBy(resolved, d, n)
                })
            val r3 =
              exprs match
                case Seq(StarExpr()) => r2
                case _               => ProjectOperator(r2, rewrittenExprs)
            having match
              case Some(cond) => HavingOperator(r3, rewrite(cond))
              case None       => r3

      val r_distinct = if distinct then DistinctOperator(r_ordered) else r_ordered
      val r5 =
        offset match
          case Some(Count(pos, expr)) =>
            val count = evalCountExpr(pos, expr, "offset")
            if count < 0 then throw ExecutionException(pos, s"offset should be non-negative: $count")
            OffsetOperator(r_distinct, count)
          case None => r_distinct
      val r6 =
        limit match
          case Some(Count(pos, expr)) =>
            val count = evalCountExpr(pos, expr, "limit")
            if count < 0 then throw ExecutionException(pos, s"limit should be non-negative: $count")
            LimitOperator(r5, count)
          case None => r5

      rewrite(r6)

    case SetOperationExpr(op, left, right) =>
      val l = rewrite(left)
      val r = rewrite(right)
      op match
        case "UNION"     => rewrite(UnionOperator(l, r, all = false))
        case "UNION ALL" => rewrite(UnionOperator(l, r, all = true))
        case "INTERSECT" => rewrite(IntersectOperator(l, r))
        case "EXCEPT"    => rewrite(ExceptOperator(l, r))
    case CompoundQueryExpr(inner, orderBy, offset, limit) =>
      val r = rewrite(inner)
      val r1 =
        orderBy match
          case None     => r
          case Some(os) => SortOperator(r, os map { case OrderBy(f, d, n) => OrderBy(rewrite(f), d, n) })
      val r2 =
        offset match
          case Some(Count(pos, expr)) =>
            val count = evalCountExpr(pos, expr, "offset")
            if count < 0 then throw ExecutionException(pos, s"offset should be non-negative: $count")
            OffsetOperator(r1, count)
          case None => r1
      val r3 =
        limit match
          case Some(Count(pos, expr)) =>
            val count = evalCountExpr(pos, expr, "limit")
            if count < 0 then throw ExecutionException(pos, s"limit should be non-negative: $count")
            LimitOperator(r2, count)
          case None => r2
      rewrite(r3)
    case UnionOperator(rel1, rel2, all) =>
      ProcessOperator(UnionProcess(procRewrite(rel1), procRewrite(rel2), all))
    case IntersectOperator(rel1, rel2) =>
      ProcessOperator(IntersectProcess(procRewrite(rel1), procRewrite(rel2)))
    case ExceptOperator(rel1, rel2) =>
      ProcessOperator(ExceptProcess(procRewrite(rel1), procRewrite(rel2)))
    case SortOperator(rel, by)             => ProcessOperator(SortProcess(procRewrite(rel), by))
    case AggregateOperator(rel, groupBy, aggregates) =>
      ProcessOperator(AggregateProcess(procRewrite(rel), groupBy, aggregates))
    case WindowOperator(rel, windows) =>
      ProcessOperator(WindowProcess(procRewrite(rel), windows))
    case OffsetOperator(rel, offset)       => ProcessOperator(DropProcess(procRewrite(rel), offset))
    case LimitOperator(rel, limit)         => ProcessOperator(TakeProcess(procRewrite(rel), limit))
    case DistinctOperator(rel)             => ProcessOperator(DistinctProcess(procRewrite(rel)))
    case LateralCrossOperator(rel1, rel2) =>
      ProcessOperator(LateralCrossProcess(procRewrite(rel1), procRewrite(rel2)))
    case InnerJoinOperator(rel1, rel2, on) if isLateralExpr(rel2) =>
      ProcessOperator(SeqScanProcess(
        LateralCrossProcess(procRewrite(rel1), procRewrite(stripLateralExpr(rel2))),
        rewrite(on)))
    case LeftJoinOperator(rel1, rel2, on) if isLateralExpr(rel2) =>
      ProcessOperator(LeftLateralJoinProcess(
        procRewrite(rel1), procRewrite(stripLateralExpr(rel2)), rewrite(on)))
    case InnerJoinOperator(rel1, rel2, on) =>
      val proc1 = procRewrite(rel1)
      val proc2 = procRewrite(rel2)
      val rwOn = rewrite(on)
      tryIndexJoin(proc1, proc2, rwOn, isLeft = false, isRight = false)
        .map(ProcessOperator(_))
        .orElse(tryHashJoin(proc1, proc2, rwOn, "INNER").map(ProcessOperator(_)))
        .getOrElse(ProcessOperator(SeqScanProcess(CrossProcess(proc1, proc2), rwOn)))
    case LeftJoinOperator(rel1, rel2, on) =>
      val proc1 = procRewrite(rel1)
      val proc2 = procRewrite(rel2)
      val rwOn = rewrite(on)
      tryIndexJoin(proc1, proc2, rwOn, isLeft = true, isRight = false)
        .map(ProcessOperator(_))
        .orElse(tryHashJoin(proc1, proc2, rwOn, "LEFT").map(ProcessOperator(_)))
        .getOrElse(ProcessOperator(LeftCrossJoinProcess(proc1, proc2, rwOn)))
    case RightJoinOperator(rel1, rel2, on) =>
      val proc1 = procRewrite(rel1)
      val proc2 = procRewrite(rel2)
      val rwOn = rewrite(on)
      tryIndexJoin(proc1, proc2, rwOn, isLeft = false, isRight = true)
        .map(ProcessOperator(_))
        .orElse(tryHashJoin(proc1, proc2, rwOn, "RIGHT").map(ProcessOperator(_)))
        .getOrElse(ProcessOperator(RightCrossJoinProcess(proc1, proc2, rwOn)))
    case FullJoinOperator(rel1, rel2, on) =>
      val proc1 = procRewrite(rel1)
      val proc2 = procRewrite(rel2)
      val rwOn = rewrite(on)
      tryHashJoin(proc1, proc2, rwOn, "FULL")
        .map(ProcessOperator(_))
        .getOrElse(ProcessOperator(FullCrossJoinProcess(proc1, proc2, rwOn)))
    case AliasOperator(rel, Ident(alias)) => ProcessOperator(AliasProcess(procRewrite(rel), alias))
    case ColumnAliasOperator(rel, Ident(alias), columns) =>
      ProcessOperator(ColumnAliasProcess(procRewrite(rel), alias, columns.map(_.name)))
    case TableOperator(id @ Ident(name))  =>
      session.getTable(name) match
        case Some(t) => ProcessOperator(t)
        case None =>
          session.db.getView(name) match
            case Some(sql) =>
              val viewQuery = SQLParser.parseQuery(sql)
              rewrite(viewQuery)
            case None => throw UndefinedReferenceException(id.pos, s"table '$name' not found")
    case InformationSchemaOperator(id @ Ident(name)) =>
      try ProcessOperator(InformationSchema.generate(name, session.db))
      catch case e: RuntimeException => throw UndefinedReferenceException(id.pos, e.getMessage)
    case ProjectOperator(rel, projs) =>
      val rewritten_projs = projs map rewrite
      val rewritten_proc  = procRewrite(rel)

      ProcessOperator(ProjectProcess(rewritten_proc, rewritten_projs))
    case CrossOperator(rel1, rel2) if isLateralExpr(rel2) =>
      ProcessOperator(LateralCrossProcess(procRewrite(rel1), procRewrite(stripLateralExpr(rel2))))
    case CrossOperator(rel1, rel2) => ProcessOperator(CrossProcess(procRewrite(rel1), procRewrite(rel2)))
    case SelectOperator(rel, cond) =>
      val proc = procRewrite(rel)
      val rwCond = rewrite(cond)
      proc match
        case table: Table =>
          tryIndexScan(table, rwCond) match
            case Some(p) => ProcessOperator(p)
            case None    => ProcessOperator(SeqScanProcess(proc, rwCond))
        case _ => ProcessOperator(SeqScanProcess(proc, rwCond))
    case HavingOperator(rel, cond) => ProcessOperator(HavingProcess(procRewrite(rel), rewrite(cond)))
    case ValuesExpr(rows) =>
      val rewrittenRows = rows.map(_.map(rewrite))
      val width         = rewrittenRows.head.length
      ProcessOperator(ValuesProcess(rewrittenRows, width))
    case _: ValueExpr => expr
    // todo: ColumnExpr, VariableExpr
    case _ => expr

private def isLateralExpr(e: Expr): Boolean = e match
  case LateralExpr(_)                            => true
  case AliasOperator(LateralExpr(_), _)          => true
  case ColumnAliasOperator(LateralExpr(_), _, _) => true
  case _                                         => false

private def stripLateralExpr(e: Expr): Expr = e match
  case LateralExpr(rel)                            => rel
  case AliasOperator(LateralExpr(rel), a)          => AliasOperator(rel, a)
  case ColumnAliasOperator(LateralExpr(rel), a, c) => ColumnAliasOperator(rel, a, c)
  case other                                       => other

private def flattenAnd(expr: Expr): Seq[Expr] =
  expr match
    case BinaryExpr(l, "AND", r) => flattenAnd(l) ++ flattenAnd(r)
    case other                   => Seq(other)

private def findIndex(table: Table, colName: String): Option[(TableIndex, Boolean)] =
  table.tableIndexes.values.find { idx =>
    idx.meta.columns.length == 1 && idx.meta.columns.head == colName
  }.map(idx => (idx, idx.meta.unique))

private def isColumnOf(table: Table, expr: Expr): Option[String] =
  expr match
    case ColumnExpr(None, Ident(name)) if table.hasColumn(name)          => Some(name)
    case ColumnExpr(Some(Ident(t)), Ident(name)) if t == table.name && table.hasColumn(name) => Some(name)
    case _                                                                => None

private def isNonColumnExpr(table: Table, expr: Expr): Boolean =
  isColumnOf(table, expr).isEmpty

def tryIndexScan(table: Table, cond: Expr)(using Session): Option[Process] =
  val conjuncts = flattenAnd(cond)

  // Try composite index: collect all col = expr equalities, find best multi-column index with longest contiguous prefix
  def tryComposite: Option[Process] =
    // Build map: column name → (value expression, conjunct index) from equality conjuncts
    val colMap = mutable.Map[String, (Expr, Int)]()
    conjuncts.zipWithIndex.foreach { case (conj, idx) =>
      conj match
        case BinaryExpr(left, "=", right) =>
          isColumnOf(table, left).foreach { col =>
            if isNonColumnExpr(table, right) && !colMap.contains(col) then colMap(col) = (right, idx)
          }
          isColumnOf(table, right).foreach { col =>
            if isNonColumnExpr(table, left) && !colMap.contains(col) then colMap(col) = (left, idx)
          }
        case _ =>
    }
    if colMap.isEmpty then return None

    // For each multi-column index, compute longest contiguous prefix covered by equalities
    val candidates = table.tableIndexes.values.toSeq.filter(_.meta.columns.length > 1).flatMap { idx =>
      val prefix = idx.meta.columns.takeWhile(c => colMap.contains(c))
      if prefix.length >= 2 then // only worth it for 2+ columns matched
        val keyExprs = prefix.map(c => colMap(c)._1)
        val usedIndices = prefix.map(c => colMap(c)._2).toSet
        Some((idx, keyExprs, usedIndices, prefix.length, idx.meta.unique))
      else None
    }
    if candidates.isEmpty then return None

    // Score: more prefix columns first, then unique > non-unique
    val (bestIdx, keyExprs, usedIndices, _, _) = candidates.sortBy { case (_, _, _, prefixLen, unique) =>
      (-prefixLen, if unique then 0 else 1)
    }.head

    val residualConjuncts = conjuncts.zipWithIndex.collect { case (c, i) if !usedIndices.contains(i) => c }
    val residual = residualConjuncts.reduceLeftOption((a, b) => BinaryExpr(a, "AND", b) setType BooleanType)
    Some(IndexScanProcess(table, bestIdx, PointLookup(keyExprs), residual))

  // Try to find a single-column equality match: col = expr or expr = col
  def tryEquality: Option[Process] =
    val found = conjuncts.zipWithIndex.flatMap { case (conj, idx) =>
      conj match
        case BinaryExpr(left, "=", right) =>
          isColumnOf(table, left).flatMap(col => findIndex(table, col).filter(_ => isNonColumnExpr(table, right)).map((ti, _) => (ti, right, idx)))
            .orElse(isColumnOf(table, right).flatMap(col => findIndex(table, col).filter(_ => isNonColumnExpr(table, left)).map((ti, _) => (ti, left, idx))))
        case _ => None
    }.sortBy { case (idx, _, _) => if idx.meta.unique then 0 else 1 }.headOption

    found.map { case (idx, keyExpr, usedIdx) =>
      val residualConjuncts = conjuncts.zipWithIndex.collect { case (c, i) if i != usedIdx => c }
      val residual = residualConjuncts.reduceLeftOption((a, b) => BinaryExpr(a, "AND", b) setType BooleanType)
      IndexScanProcess(table, idx, PointLookup(Seq(keyExpr)), residual)
    }

  // Try to find a range match: lower <= col AND col <= upper (from BETWEEN rewrite)
  def tryRange: Option[Process] =
    val leComps = conjuncts.zipWithIndex.flatMap { case (conj, idx) =>
      conj match
        case BinaryExpr(lower, "<=", colExpr) =>
          isColumnOf(table, colExpr).flatMap(col => findIndex(table, col).map((ti, _) => (col, ti, lower, idx)))
        case _ => None
    }
    val geComps = conjuncts.zipWithIndex.flatMap { case (conj, idx) =>
      conj match
        case BinaryExpr(colExpr, "<=", upper) =>
          isColumnOf(table, colExpr).flatMap(col => findIndex(table, col).map((ti, _) => (col, ti, upper, idx)))
        case _ => None
    }
    val rangeMatch = for
      (col1, idx1, lower, li) <- leComps.headOption
      (col2, idx2, upper, ui) <- geComps.find((c, _, _, i) => c == col1 && i != li)
    yield (idx1, lower, upper, li, ui)

    rangeMatch.map { case (idx, lower, upper, li, ui) =>
      val residualConjuncts = conjuncts.zipWithIndex.collect { case (c, i) if i != li && i != ui => c }
      val residual = residualConjuncts.reduceLeftOption((a, b) => BinaryExpr(a, "AND", b) setType BooleanType)
      IndexScanProcess(table, idx, RangeLookup(Seq(lower), Seq(upper)), residual)
    }

  def tryInList: Option[Process] =
    conjuncts.zipWithIndex.flatMap { case (conj, idx) =>
      conj match
        case InSeqExpr(col, op, exprs) if !op.contains("NOT") =>
          isColumnOf(table, col).flatMap(colName =>
            findIndex(table, colName).map { case (tableIdx, _) =>
              val residualConjuncts = conjuncts.zipWithIndex.collect { case (c, i) if i != idx => c }
              val residual = residualConjuncts.reduceLeftOption((a, b) => BinaryExpr(a, "AND", b) setType BooleanType)
              IndexScanProcess(table, tableIdx, MultiPointLookup(exprs.map(e => Seq(e))), residual)
            }
          )
        case InQueryExpr(col, op, query) if !op.contains("NOT") =>
          isColumnOf(table, col).flatMap(colName =>
            findIndex(table, colName).map { case (tableIdx, _) =>
              val residualConjuncts = conjuncts.zipWithIndex.collect { case (c, i) if i != idx => c }
              val residual = residualConjuncts.reduceLeftOption((a, b) => BinaryExpr(a, "AND", b) setType BooleanType)
              IndexScanProcess(table, tableIdx, InQueryLookup(query), residual)
            }
          )
        case _ => None
    }.headOption

  tryComposite.orElse(tryEquality).orElse(tryInList).orElse(tryRange)

private def extractTable(proc: Process): Option[(Table, Metadata)] =
  proc match
    case t: Table                           => Some((t, t.meta))
    case AliasProcess(t: Table, _)          => Some((t, proc.meta))
    case ColumnAliasProcess(t: Table, _, _) => Some((t, proc.meta))
    case _                                  => None

private def columnOfMeta(meta: Metadata, expr: Expr): Option[String] =
  expr match
    case ColumnExpr(None, Ident(name))           => if meta.columnMap.contains(name) then Some(name) else None
    case ColumnExpr(Some(Ident(t)), Ident(name)) => if meta.columnMap.contains(s"$t.$name") then Some(name) else None
    case _                                       => None

private def columnIndexOfMeta(meta: Metadata, expr: Expr): Option[Int] =
  expr match
    case ColumnExpr(None, Ident(name))           => meta.columnMap.get(name).map(_._1)
    case ColumnExpr(Some(Ident(t)), Ident(name)) => meta.columnMap.get(s"$t.$name").map(_._1)
    case _                                       => None

private def tryHashJoin(
    left: Process,
    right: Process,
    cond: Expr,
    joinType: String,
)(using Session): Option[Process] =
  val conjuncts = flattenAnd(cond)
  val leftMeta = left.meta
  val rightMeta = right.meta

  case class EquiPair(leftIdx: Int, rightIdx: Int, conjIdx: Int)

  val equiPairs = mutable.ArrayBuffer[EquiPair]()
  val otherIndices = mutable.Set[Int]()

  conjuncts.zipWithIndex.foreach { case (conj, idx) =>
    conj match
      case BinaryExpr(l, "=", r) =>
        val pair = for
          li <- columnIndexOfMeta(leftMeta, l)
          ri <- columnIndexOfMeta(rightMeta, r)
        yield EquiPair(li, ri, idx)

        pair.orElse {
          for
            ri <- columnIndexOfMeta(rightMeta, l)
            li <- columnIndexOfMeta(leftMeta, r)
          yield EquiPair(li, ri, idx)
        } match
          case Some(ep) => equiPairs += ep
          case None     => otherIndices += idx
      case _ =>
        otherIndices += idx
  }

  if equiPairs.isEmpty then return None

  val buildKeys = equiPairs.map(_.leftIdx).toSeq
  val probeKeys = equiPairs.map(_.rightIdx).toSeq
  val residualConj = conjuncts.zipWithIndex.collect { case (c, i) if otherIndices.contains(i) => c }
  val residual = residualConj.reduceLeftOption((a, b) => BinaryExpr(a, "AND", b) setType BooleanType)

  val proc = joinType match
    case "INNER" => HashJoinProcess(left, right, buildKeys, probeKeys, residual)
    case "LEFT"  => LeftHashJoinProcess(left, right, buildKeys, probeKeys, residual)
    case "RIGHT" => RightHashJoinProcess(left, right, buildKeys, probeKeys, residual)
    case "FULL"  => FullHashJoinProcess(left, right, buildKeys, probeKeys, residual)

  Some(proc)

private def tryIndexJoin(
    left: Process,
    right: Process,
    cond: Expr,
    isLeft: Boolean,
    isRight: Boolean,
)(using Session): Option[Process] =
  val conjuncts = flattenAnd(cond)
  val leftMeta = left.meta
  val rightMeta = right.meta

  case class EquiPair(leftExpr: Expr, rightExpr: Expr, leftCol: String, rightCol: String, conjIdx: Int)

  val equiPairs = mutable.ArrayBuffer[EquiPair]()
  val otherIndices = mutable.Set[Int]()

  conjuncts.zipWithIndex.foreach { case (conj, idx) =>
    conj match
      case BinaryExpr(l, "=", r) =>
        val pair = for
          lc <- columnOfMeta(leftMeta, l)
          rc <- columnOfMeta(rightMeta, r)
        yield EquiPair(l, r, lc, rc, idx)

        pair.orElse {
          for
            rc <- columnOfMeta(rightMeta, l)
            lc <- columnOfMeta(leftMeta, r)
          yield EquiPair(r, l, lc, rc, idx)
        } match
          case Some(ep) => equiPairs += ep
          case None     => otherIndices += idx
      case _ =>
        otherIndices += idx
  }

  if equiPairs.isEmpty then return None

  def tryIndexedInner(
      innerTable: Table,
      getInnerCol: EquiPair => String,
      getOuterKeyExpr: EquiPair => Expr,
  ): Option[(TableIndex, Seq[Expr], Set[Int])] =
    val colMap = mutable.Map[String, (Expr, Int)]()
    equiPairs.foreach { ep =>
      val col = getInnerCol(ep)
      if !colMap.contains(col) then colMap(col) = (getOuterKeyExpr(ep), ep.conjIdx)
    }

    val compositeResult = innerTable.tableIndexes.values.toSeq.filter(_.meta.columns.length > 1).flatMap { idx =>
      val prefix = idx.meta.columns.takeWhile(c => colMap.contains(c))
      if prefix.length >= 2 then
        val keyExprs = prefix.map(c => colMap(c)._1)
        val usedIndices = prefix.map(c => colMap(c)._2).toSet
        Some((idx, keyExprs, usedIndices, prefix.length, idx.meta.unique))
      else None
    }.sortBy { case (_, _, _, prefixLen, unique) =>
      (-prefixLen, if unique then 0 else 1)
    }.headOption.map { case (idx, keyExprs, usedIndices, _, _) =>
      (idx, keyExprs, usedIndices)
    }

    compositeResult.orElse {
      equiPairs.flatMap { ep =>
        val col = getInnerCol(ep)
        findIndex(innerTable, col).map { case (idx, unique) =>
          (idx, Seq(getOuterKeyExpr(ep)), Set(ep.conjIdx), unique)
        }
      }.sortBy { case (_, _, _, unique) => if unique then 0 else 1 }
        .headOption.map { case (idx, keyExprs, usedIndices, _) =>
          (idx, keyExprs, usedIndices)
        }
    }

  def buildProcess(
      outerProc: Process,
      innerTable: Table,
      innerMeta: Metadata,
      idx: TableIndex,
      keyExprs: Seq[Expr],
      usedConjIndices: Set[Int],
      outerIsLeft: Boolean,
  ): Process =
    val residualConj = conjuncts.zipWithIndex.collect { case (c, i) if !usedConjIndices.contains(i) => c }
    val residual = residualConj.reduceLeftOption((a, b) => BinaryExpr(a, "AND", b) setType BooleanType)

    if isLeft then
      LeftIndexNestedLoopJoinProcess(outerProc, innerTable, idx, keyExprs, innerMeta, residual)
    else if isRight then
      RightIndexNestedLoopJoinProcess(outerProc, innerTable, idx, keyExprs, innerMeta, residual)
    else
      IndexNestedLoopJoinProcess(outerProc, innerTable, idx, keyExprs, innerMeta, residual, outerIsLeft)

  val tryRight = if !isRight then
    extractTable(right).flatMap { case (rightTable, rightOuterMeta) =>
      tryIndexedInner(rightTable, _.rightCol, _.leftExpr).map { case (idx, keyExprs, usedIndices) =>
        buildProcess(left, rightTable, rightOuterMeta, idx, keyExprs, usedIndices, outerIsLeft = true)
      }
    }
  else None

  tryRight.orElse {
    if !isLeft then
      extractTable(left).flatMap { case (leftTable, leftOuterMeta) =>
        tryIndexedInner(leftTable, _.leftCol, _.rightExpr).map { case (idx, keyExprs, usedIndices) =>
          buildProcess(right, leftTable, leftOuterMeta, idx, keyExprs, usedIndices, outerIsLeft = false)
        }
      }
    else None
  }

def procRewrite(expr: Expr)(using session: Session): Process = rewrite(expr).asInstanceOf[ProcessOperator].proc

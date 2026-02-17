package io.github.edadma.rdb

import scala.collection.mutable

class AggregateCollector:
  private val specs = mutable.ArrayBuffer[AggregateSpec]()
  private val seen = mutable.Map[String, String]() // canonical key → column name
  private var counter = 0

  private def canonicalKey(funcName: String, arg: Expr): String =
    s"$funcName(${arg.toString})"

  def collect(expr: Expr): Expr =
    expr match
      case AggregateFunctionExpr(f, arg) =>
        val key = canonicalKey(f.name, arg)
        val colName = seen.getOrElseUpdate(key, {
          counter += 1
          val name = s"_agg_$counter"
          specs += AggregateSpec(name, f, arg, expr.typ.asInstanceOf[Type])
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
      case _ => expr

  def result: Seq[AggregateSpec] = specs.toSeq

  def hasAggregates: Boolean = specs.nonEmpty

def aggregate(expr: Expr): Boolean =
  expr match
    case _: AggregateFunctionExpr    => true
    case AliasExpr(expr, _)          => aggregate(expr)
    case CastExpr(expr, _)           => aggregate(expr)
    case ScalarFunctionExpr(_, args) => args exists aggregate
    case UnaryExpr(_, expr)          => aggregate(expr)
    case BinaryExpr(left, _, right)  => aggregate(left) | aggregate(right)
    case CaseExpr(whens, els) =>
      whens.exists { case When(w, e) => aggregate(w) || aggregate(e) } ||
        els.exists(aggregate)
    case _                           => false

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

def rewrite(expr: Expr)(using db: DB): Expr =
  expr match
    case _ if expr.typ != null              => expr
    case CastExpr(expr, targetType)         => CastExpr(rewrite(expr), targetType) setType targetType
    case AliasExpr(expr, alias) => AliasExpr(rewrite(expr), alias)
    case SubqueryExpr(query)    => SubqueryExpr(rewrite(query))
    case CaseExpr(whens, els)   =>
      CaseExpr(whens map { case When(when, expr) => When(rewrite(when), rewrite(expr)) }, els map rewrite)
    case InSeqExpr(value, op, exprs)       => InSeqExpr(rewrite(value), op, exprs map rewrite)
    case InQueryExpr(value, op, array)     => InQueryExpr(rewrite(value), op, rewrite(array))
    case TableConstructorExpr(expr)        => TableConstructorExpr(rewrite(expr))
    case ApplyExpr(id @ Ident(func), args) =>
      scalarFunction get func.toLowerCase match
        case None =>
          aggregateFunction get func.toLowerCase match
            case None                        => problem(id, s"unknown function '$func'")
            case Some(f) if args.length != 1 => problem(id, "aggregate function takes one argument")
            case Some(f)                     =>
              val (instance, typ) = f.instantiate

              AggregateFunctionExpr(instance, rewrite(args.head)) setType typ
        case Some(f) => ScalarFunctionExpr(f, args map rewrite)
    case VariableExpr(id @ Ident(name)) =>
      scalarVariable get name match
        case None    => problem(id, s"unknown variable '$name'")
        case Some(v) => VariableInstanceExpr(v.instance)
    case ExistsExpr(subquery) => ExistsExpr(rewrite(subquery)) setType BooleanType
    case UnaryExpr(op, expr)  =>
      val e = rewrite(expr)

      UnaryExpr(op, e) setType e.typ
    case BinaryExpr(left, op @ ("+" | "-" | "*" | "/" | "AND" | "OR"), right) =>
      val l = rewrite(left)
      val r = rewrite(right)

      BinaryExpr(l, op, r) setType l.typ
    case BinaryExpr(left, op @ ("<=" | ">=" | "!=" | "=" | "<" | ">" | "LIKE" | "ILIKE"), right) =>
      BinaryExpr(rewrite(left), op, rewrite(right)) setType BooleanType
    case BetweenExpr(value, op, lower, upper) =>
      val v = rewrite(value)
      val l = rewrite(lower)
      val r = rewrite(upper)

      (if op == "BETWEEN" then BinaryExpr(lower, "<=", value) setType BooleanType
       else BinaryExpr(value, "<", lower) setType BooleanType) match
        case leftCond =>
          (if op == "BETWEEN" then BinaryExpr(value, "<=", upper) setType BooleanType
           else BinaryExpr(value, ">", upper) setType BooleanType) match
            case rightCond =>
              BinaryExpr(leftCond, if op == "BETWEEN" then "AND" else "OR", rightCond) setType BooleanType
    case SQLSelectExpr(exprs, None, where, groupBy, having, orderBy, offset, limit, _) =>
      if where.isDefined then problem(where.get, "WHERE clause not allowed here")
      if groupBy.isDefined then problem(where.get, "GROUP BY clause not allowed here")
      if having.isDefined then problem(where.get, "HAVING clause not allowed here")
      if orderBy.isDefined then problem(where.get, "ORDER BY clause not allowed here")
      if offset.isDefined then problem(offset.get.pos, "OFFSET clause not allowed here")
      if limit.isDefined then problem(limit.get.pos, "LIMIT clause not allowed here")

      val rewritten_projs = exprs map rewrite

      ProcessOperator(ProjectProcess(SingleProcess, rewritten_projs))
    case SQLSelectExpr(exprs, Some(from), where, groupBy, having, orderBy, offset, limit, distinct) =>
      def cross(es: Seq[Expr]): Expr =
        es match
          case Seq(e)  => e
          case e :: tl => CrossOperator(e, cross(tl))

      val r  = cross(from map rewrite)
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

          // Collect aggregates from ORDER BY (resolve aliases first)
          val collectedOrderBy = orderBy.map { os =>
            os.map { case OrderBy(f, d, n) =>
              val rewritten = rewrite(f)
              val resolved = resolveAliases(rewritten, aliasMap)
              OrderBy(collector.collect(resolved), d, n)
            }
          }

          val groupByExprs = groupBy.map(_.map(rewrite)).getOrElse(Nil)

          // Build: source → AggregateOperator → HAVING → ORDER BY → PROJECT
          val r2 = AggregateOperator(r1, groupByExprs, collector.result)
          val r3 =
            collectedHaving match
              case Some(cond) => HavingOperator(r2, cond)
              case None       => r2
          val r4 =
            collectedOrderBy match
              case Some(os) => SortOperator(r3, os)
              case None     => r3
          exprs match
            case Seq(StarExpr()) => r4
            case _               => ProjectOperator(r4, collectedExprs)
        else
          // Non-grouped path: ORDER BY → PROJECT
          val r2 =
            orderBy match
              case None     => r1
              case Some(os) => SortOperator(r1, os map { case OrderBy(f, d, n) => OrderBy(rewrite(f), d, n) })
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
          case Some(Count(pos, count)) =>
            if count < 0 then problem(pos, s"offset should be non-negative: $count")

            OffsetOperator(r_distinct, count)
          case None => r_distinct
      val r6 =
        limit match
          case Some(Count(pos, count)) =>
            if count < 1 then problem(pos, s"limit should be positive: $count")

            LimitOperator(r5, count)
          case None => r5

      rewrite(r6)

    case SortOperator(rel, by)             => ProcessOperator(SortProcess(procRewrite(rel), by))
    case AggregateOperator(rel, groupBy, aggregates) =>
      ProcessOperator(AggregateProcess(procRewrite(rel), groupBy, aggregates))
    case OffsetOperator(rel, offset)       => ProcessOperator(DropProcess(procRewrite(rel), offset))
    case LimitOperator(rel, limit)         => ProcessOperator(TakeProcess(procRewrite(rel), limit))
    case DistinctOperator(rel)             => ProcessOperator(DistinctProcess(procRewrite(rel)))
    case InnerJoinOperator(rel1, rel2, on) =>
      ProcessOperator(FilterProcess(CrossProcess(procRewrite(rel1), procRewrite(rel2)), rewrite(on)))
    case LeftJoinOperator(rel1, rel2, on) =>
      ProcessOperator(LeftCrossJoinProcess(procRewrite(rel1), procRewrite(rel2), rewrite(on)))
    case RightJoinOperator(rel1, rel2, on) =>
      ProcessOperator(RightCrossJoinProcess(procRewrite(rel1), procRewrite(rel2), rewrite(on)))
    case FullJoinOperator(rel1, rel2, on) =>
      ProcessOperator(FullCrossJoinProcess(procRewrite(rel1), procRewrite(rel2), rewrite(on)))
    case AliasOperator(rel, Ident(alias)) => ProcessOperator(AliasProcess(procRewrite(rel), alias))
    case TableOperator(id @ Ident(name))  =>
      db.getTable(name) match
        case Some(t) => ProcessOperator(t)
        case None    => problem(id, s"table '$name' not found")
    case ProjectOperator(rel, projs) =>
      val rewritten_projs = projs map rewrite
      val rewritten_proc  = procRewrite(rel)

      ProcessOperator(ProjectProcess(rewritten_proc, rewritten_projs))
    case CrossOperator(rel1, rel2) => ProcessOperator(CrossProcess(procRewrite(rel1), procRewrite(rel2)))
    case SelectOperator(rel, cond) => ProcessOperator(FilterProcess(procRewrite(rel), rewrite(cond)))
    case HavingOperator(rel, cond) => ProcessOperator(HavingProcess(procRewrite(rel), rewrite(cond)))
    // todo: ColumnExpr, VariableExpr
    case _ => expr

def procRewrite(expr: Expr)(using db: DB): Process = rewrite(expr).asInstanceOf[ProcessOperator].proc

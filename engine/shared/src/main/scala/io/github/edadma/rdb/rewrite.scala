package io.github.edadma.rdb

import scala.collection.mutable

class AggregateCollector:
  private val specs = mutable.ArrayBuffer[AggregateSpec]()
  private val seen = mutable.Map[String, String]() // canonical key → column name
  private var counter = 0

  private def canonicalKey(funcName: String, args: Seq[Expr]): String =
    s"$funcName(${args.mkString(", ")})"

  def collect(expr: Expr): Expr =
    expr match
      case AggregateFunctionExpr(f, args) =>
        val key = canonicalKey(f.name, args)
        val colName = seen.getOrElseUpdate(key, {
          counter += 1
          val name = s"_agg_$counter"
          specs += AggregateSpec(name, f, args, expr.typ.asInstanceOf[Type])
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
    case QuantifiedCompareExpr(value, op, quantifier, expr) =>
      QuantifiedCompareExpr(rewrite(value), op, quantifier, rewrite(expr))
    case TableConstructorExpr(expr)        => TableConstructorExpr(rewrite(expr))
    case ApplyExpr(id @ Ident(func), args) =>
      scalarFunction get func.toLowerCase match
        case None =>
          aggregateFunction get func.toLowerCase match
            case None    => problem(id, s"unknown function '$func'")
            case Some(f) =>
              val (instance, typ) = f.instantiate

              AggregateFunctionExpr(instance, args map rewrite) setType typ
        case Some(f) => ScalarFunctionExpr(f, args map rewrite)
    case VariableExpr(id @ Ident(name)) =>
      scalarVariable get name match
        case None    => problem(id, s"unknown variable '$name'")
        case Some(v) => VariableInstanceExpr(v.instance)
    case ExistsExpr(subquery) => ExistsExpr(rewrite(subquery)) setType BooleanType
    case UnaryExpr(op, expr)  =>
      val e = rewrite(expr)

      UnaryExpr(op, e) setType e.typ
    case BinaryExpr(left, op @ ("+" | "-" | "*" | "/" | "AND" | "OR" | "->" | "#>" | "||"), right) =>
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
      if where.isDefined then problem(where.get, "WHERE clause not allowed here")
      if groupBy.isDefined then problem(where.get, "GROUP BY clause not allowed here")
      if having.isDefined then problem(where.get, "HAVING clause not allowed here")
      if orderBy.isDefined then problem(where.get, "ORDER BY clause not allowed here")
      if offset.isDefined then problem(offset.get.pos, "OFFSET clause not allowed here")
      if limit.isDefined then problem(limit.get.pos, "LIMIT clause not allowed here")

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
          case Some(Count(pos, count)) =>
            if count < 0 then problem(pos, s"offset should be non-negative: $count")
            OffsetOperator(r1, count)
          case None => r1
      val r3 =
        limit match
          case Some(Count(pos, count)) =>
            if count < 1 then problem(pos, s"limit should be positive: $count")
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
      ProcessOperator(SeqScanProcess(CrossProcess(procRewrite(rel1), procRewrite(rel2)), rewrite(on)))
    case LeftJoinOperator(rel1, rel2, on) =>
      ProcessOperator(LeftCrossJoinProcess(procRewrite(rel1), procRewrite(rel2), rewrite(on)))
    case RightJoinOperator(rel1, rel2, on) =>
      ProcessOperator(RightCrossJoinProcess(procRewrite(rel1), procRewrite(rel2), rewrite(on)))
    case FullJoinOperator(rel1, rel2, on) =>
      ProcessOperator(FullCrossJoinProcess(procRewrite(rel1), procRewrite(rel2), rewrite(on)))
    case AliasOperator(rel, Ident(alias)) => ProcessOperator(AliasProcess(procRewrite(rel), alias))
    case ColumnAliasOperator(rel, Ident(alias), columns) =>
      ProcessOperator(ColumnAliasProcess(procRewrite(rel), alias, columns.map(_.name)))
    case TableOperator(id @ Ident(name))  =>
      db.getTable(name) match
        case Some(t) => ProcessOperator(t)
        case None    => problem(id, s"table '$name' not found")
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

def tryIndexScan(table: Table, cond: Expr)(using DB): Option[Process] =
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

def procRewrite(expr: Expr)(using db: DB): Process = rewrite(expr).asInstanceOf[ProcessOperator].proc

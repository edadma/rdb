package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import io.github.edadma.dal.BasicDAL

import scala.language.postfixOps

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait Process:
  def iterator(ctx: Seq[Row]): RowIterator
  def meta: Metadata

  protected def validateColumns(expr: Expr, m: Metadata): Unit =
    expr match
      case ColumnExpr(table, id @ Ident(name)) =>
        val lookupName = table.map(t => s"${t.name}.$name").getOrElse(name)
        if !m.columnMap.contains(lookupName) then
          // If qualified with a table not in our metadata, it may be a correlated outer reference — skip
          val isOuterRef = table.exists(t => !m.columns.exists(c => c.table.contains(t.name)))
          if !isOuterRef then
            throw UndefinedReferenceException(id.pos, s"column '$lookupName' does not exist")
      case UnaryExpr(_, e)                  => validateColumns(e, m)
      case BinaryExpr(l, _, r)              => validateColumns(l, m); validateColumns(r, m)
      case BetweenExpr(v, _, lo, hi)        => validateColumns(v, m); validateColumns(lo, m); validateColumns(hi, m)
      case CaseExpr(whens, els)             => whens.foreach(w => { validateColumns(w.when, m); validateColumns(w.expr, m) }); els.foreach(e => validateColumns(e, m))
      case InSeqExpr(v, _, es)              => validateColumns(v, m); es.foreach(e => validateColumns(e, m))
      case InQueryExpr(v, _, _)             => validateColumns(v, m)
      case AliasExpr(e, _)                  => validateColumns(e, m)
      case CastExpr(e, _)                   => validateColumns(e, m)
      case ScalarFunctionExpr(_, args)       => args.foreach(e => validateColumns(e, m))
      case AggregateFunctionExpr(_, args, filter) => args.foreach(e => validateColumns(e, m)); filter.foreach(f => validateColumns(f, m))
      case WindowExpr(func, partBy, ordBy, _) => validateColumns(func, m); partBy.foreach(e => validateColumns(e, m)); ordBy.foreach { case OrderBy(f, _, _) => validateColumns(f, m) }
      case _                                => // literals, subqueries, etc.

type RowIterator = Iterator[Row]

case object SingleProcess extends Process:
  val meta: Metadata = Metadata(Vector.empty)

  def iterator(ctx: Seq[Row]): RowIterator = Iterator(
    Row(Vector.empty, meta, None, None),
  )

case class SeqScanProcess(input: Process, cond: Expr) extends Process:
  val meta: Metadata = input.meta
  validateColumns(cond, meta)

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx).filter(row => beval(cond, row +: ctx))

sealed trait IndexLookup
case class PointLookup(keyExprs: Seq[Expr]) extends IndexLookup
case class RangeLookup(lowerExprs: Seq[Expr], upperExprs: Seq[Expr]) extends IndexLookup
case class MultiPointLookup(keyExprsList: Seq[Seq[Expr]]) extends IndexLookup
case class InQueryLookup(query: Expr) extends IndexLookup

case class IndexScanProcess(table: Table, index: TableIndex, lookup: IndexLookup, residual: Option[Expr]) extends Process:
  val meta: Metadata = table.meta
  def iterator(ctx: Seq[Row]): RowIterator =
    val baseIter = lookup match
      case PointLookup(keyExprs) =>
        val key = keyExprs.map(e => eval(e, ctx)).toIndexedSeq
        table.indexPointScan(index, key).getOrElse(table.iterator(ctx))
      case RangeLookup(lower, upper) =>
        val lo = lower.map(e => eval(e, ctx)).toIndexedSeq
        val hi = upper.map(e => eval(e, ctx)).toIndexedSeq
        table.indexRangeScan(index, lo, hi).getOrElse(table.iterator(ctx))
      case MultiPointLookup(keyExprsList) =>
        keyExprsList.iterator.flatMap { keyExprs =>
          val key = keyExprs.map(e => eval(e, ctx)).toIndexedSeq
          table.indexPointScan(index, key).getOrElse(Iterator.empty)
        }
      case InQueryLookup(query) =>
        val res = teval(query, ctx)
        if res.meta.width != 1 then throw ExecutionException(query.pos, "sub-query must return rows of one column")
        res.data.map(_.data.head).distinct.iterator.flatMap { v =>
          table.indexPointScan(index, IndexedSeq(v)).getOrElse(Iterator.empty)
        }
    residual match
      case Some(cond) => baseIter.filter(row => beval(cond, row +: ctx))
      case None       => baseIter

case class HavingProcess(input: Process, cond: Expr) extends Process:
  val meta: Metadata = input.meta
  validateColumns(cond, meta)

  def iterator(ctx: Seq[Row]): RowIterator =
    input.iterator(ctx).filter(row => beval(cond, row +: ctx))

case class AggregateProcess(input: Process, groupBy: Seq[Expr], aggregates: Seq[AggregateSpec]) extends Process:
  val meta: Metadata =
    val aggColumns = aggregates.map(spec => ColumnMetadata(None, spec.name, spec.typ))
    Metadata(input.meta.columns ++ aggColumns)
  for expr <- groupBy do validateColumns(expr, input.meta)

  def iterator(ctx: Seq[Row]): RowIterator =
    val rows = input.iterator(ctx).toVector

    if groupBy.isEmpty then
      // Ungrouped aggregate: treat entire input as one group
      for spec <- aggregates do spec.func.init()

      for row <- rows do
        val rowCtx = row +: ctx
        for spec <- aggregates do
          if spec.filter.forall(f => beval(f, rowCtx)) then
            spec.func.acc(spec.args.map(a => eval(a, rowCtx)))

      val aggValues = aggregates.map(_.func.result).toVector
      // Even if rows is empty, emit one row (COUNT→0, SUM→0, etc.)
      val baseData = if rows.isEmpty then Vector.fill(input.meta.width)(NULL) else rows.last.data
      Iterator(Row(baseData ++ aggValues, meta, None, None))
    else
      // Grouped aggregate
      val grouped = mutable.LinkedHashMap[Seq[Value], ArrayBuffer[Row]]()

      for row <- rows do
        val key = groupBy.map(e => eval(e, row +: ctx))
        grouped.getOrElseUpdate(key, ArrayBuffer.empty) += row

      grouped.iterator.map { case (_, group) =>
        for spec <- aggregates do spec.func.init()

        for row <- group do
          val rowCtx = row +: ctx
          for spec <- aggregates do
            if spec.filter.forall(f => beval(f, rowCtx)) then
              spec.func.acc(spec.args.map(a => eval(a, rowCtx)))

        val aggValues = aggregates.map(_.func.result).toVector
        Row(group.last.data ++ aggValues, meta, None, None)
      }

case class WindowProcess(input: Process, windows: Seq[WindowSpec]) extends Process:
  val meta: Metadata =
    val winColumns = windows.map(spec => ColumnMetadata(None, spec.name, spec.typ))
    Metadata(input.meta.columns ++ winColumns)

  private final class SeqOrdering(ords: Seq[Ordering[Value]]) extends Ordering[Seq[Value]]:
    def compare(xs: Seq[Value], ys: Seq[Value]): Int =
      val x   = xs.iterator
      val y   = ys.iterator
      val ord = ords.iterator
      while (x.hasNext && y.hasNext && ord.hasNext)
        val res = ord.next().compare(x.next(), y.next())
        if (res != 0) return res
      0

  private def sameOrderByValues(a: Row, b: Row, orderBy: Seq[OrderBy], ctx: Seq[Row]): Boolean =
    orderBy.forall { ob =>
      val va = eval(ob.f, a +: ctx)
      val vb = eval(ob.f, b +: ctx)
      (va.isNull && vb.isNull) || (!va.isNull && !vb.isNull && va.compare(vb) == 0)
    }

  def iterator(ctx: Seq[Row]): RowIterator =
    val rows = input.iterator(ctx).toVector
    if rows.isEmpty then return Iterator.empty

    val rowCount = rows.length
    val winValues = Array.ofDim[Value](rowCount, windows.length)

    for (spec, winIdx) <- windows.zipWithIndex do
      // Partition rows, preserving original indices
      val partitioned: Map[Seq[Value], Vector[(Row, Int)]] =
        if spec.partitionBy.isEmpty then
          Map(Nil -> rows.zipWithIndex)
        else
          rows.zipWithIndex.groupBy { case (row, _) =>
            spec.partitionBy.map(e => eval(e, row +: ctx))
          }

      for (_, partition) <- partitioned do
        // Sort within partition
        val sorted =
          if spec.orderBy.isEmpty then partition
          else
            val orderings = spec.orderBy.map { case OrderBy(_, asc, nullsFirst) =>
              (asc, nullsFirst) match
                case (false, false) => Nulls.first.reverse
                case (false, true)  => Nulls.last.reverse
                case (true, false)  => Nulls.last
                case (true, true)   => Nulls.first
            }
            val ordering = new SeqOrdering(orderings)
            partition.sortBy { case (row, _) =>
              spec.orderBy.map(ob => eval(ob.f, row +: ctx))
            }(using ordering)

        spec.kind match
          case RowNumberKind =>
            for ((_, origIdx), rank) <- sorted.zipWithIndex do
              winValues(origIdx)(winIdx) = NumberValue(rank + 1)

          case RankKind =>
            var rank = 1
            for i <- sorted.indices do
              if i > 0 && !sameOrderByValues(sorted(i)._1, sorted(i - 1)._1, spec.orderBy, ctx) then
                rank = i + 1
              winValues(sorted(i)._2)(winIdx) = NumberValue(rank)

          case DenseRankKind =>
            var rank = 1
            for i <- sorted.indices do
              if i > 0 && !sameOrderByValues(sorted(i)._1, sorted(i - 1)._1, spec.orderBy, ctx) then
                rank += 1
              winValues(sorted(i)._2)(winIdx) = NumberValue(rank)

          case LagKind(expr, offset, default) =>
            for i <- sorted.indices do
              val sourceIdx = i - offset
              val value =
                if sourceIdx >= 0 then eval(expr, sorted(sourceIdx)._1 +: ctx)
                else default.map(d => eval(d, sorted(i)._1 +: ctx)).getOrElse(NullValue())
              winValues(sorted(i)._2)(winIdx) = value

          case LeadKind(expr, offset, default) =>
            for i <- sorted.indices do
              val sourceIdx = i + offset
              val value =
                if sourceIdx < sorted.length then eval(expr, sorted(sourceIdx)._1 +: ctx)
                else default.map(d => eval(d, sorted(i)._1 +: ctx)).getOrElse(NullValue())
              winValues(sorted(i)._2)(winIdx) = value

          case NtileKind(buckets) =>
            val n = sorted.length
            val base = n / buckets
            val remainder = n % buckets
            var tile = 1
            var count = 0
            val tileSize = if remainder > 0 then base + 1 else base
            var currentTileSize = tileSize
            for i <- sorted.indices do
              if count >= currentTileSize && tile < buckets then
                tile += 1
                count = 0
                currentTileSize = if tile <= remainder then base + 1 else base
              winValues(sorted(i)._2)(winIdx) = NumberValue(tile)
              count += 1

          case AggregateWindowKind(aggFactory, args, filter) =>
            spec.frame match
              case Some(FrameSpec(start, end)) =>
                // Frame-based: compute per-row aggregate over the frame window
                for i <- sorted.indices do
                  val frameStart = start match
                    case UnboundedPreceding => 0
                    case CurrentRow         => i
                    case Preceding(n)      => math.max(0, i - n)
                    case Following(n)      => math.min(sorted.length - 1, i + n)
                    case UnboundedFollowing => sorted.length - 1
                  val frameEnd = end match
                    case UnboundedFollowing => sorted.length - 1
                    case CurrentRow         => i
                    case Following(n)      => math.min(sorted.length - 1, i + n)
                    case Preceding(n)      => math.max(0, i - n)
                    case UnboundedPreceding => 0
                  val (inst, _) = aggFactory.instantiate
                  inst.init()
                  for j <- frameStart to frameEnd do
                    val rowCtx = sorted(j)._1 +: ctx
                    if filter.forall(f => beval(f, rowCtx)) then
                      inst.acc(args.map(a => eval(a, rowCtx)))
                  winValues(sorted(i)._2)(winIdx) = inst.result
              case None =>
                // No frame: aggregate over entire partition
                val (instance, _) = aggFactory.instantiate
                instance.init()
                for (row, _) <- sorted do
                  val rowCtx = row +: ctx
                  if filter.forall(f => beval(f, rowCtx)) then
                    instance.acc(args.map(a => eval(a, rowCtx)))
                val result = instance.result
                for (_, origIdx) <- sorted do
                  winValues(origIdx)(winIdx) = result

    rows.iterator.zipWithIndex.map { case (row, idx) =>
      Row(row.data ++ winValues(idx).toVector, meta, None, None)
    }

case class ProjectProcess(input: Process, fields: IndexedSeq[Expr]) extends Process:
  private val metaCtx = Seq(input.meta)

  // Expand TableStarExpr into individual ColumnExpr for each column of the named table
  private val expandedFields: IndexedSeq[Expr] = fields.flatMap {
    case ts @ TableStarExpr(Ident(tableName)) =>
      val matching = input.meta.columns.filter(c => c.table.contains(tableName))
      if matching.isEmpty then throw UndefinedReferenceException(ts.pos, s"table '$tableName' not found in FROM clause")
      matching.map(c => ColumnExpr(Some(Ident(tableName)), Ident(c.name)).setPos(ts.pos))
    case other => IndexedSeq(other)
  }

  @tailrec
  private def lookup(name: String, ctx: Seq[Metadata]): Option[(Type, Option[String])] =
    ctx match
      case Nil      => None
      case hd :: tl =>
        hd.columnMap get name match
          case None                => lookup(name, tl)
          case Some((_, typ, tab)) => Some((typ, tab))

  val meta: Metadata =
    Metadata(expandedFields.zipWithIndex map {
      case (AliasExpr(expr, alias), _)             => ColumnMetadata(None, alias.name, expr.typ)
      case (c @ ColumnExpr(table, Ident(name)), _) =>
        val lookupName = table.map(t => s"${t.name}.$name").getOrElse(name)

        lookup(lookupName, metaCtx) match
          case None             => throw UndefinedReferenceException(c.pos, s"'$lookupName' not found")
          case Some((typ, tab)) => ColumnMetadata(tab, name, typ)
      case (expr: Expr, _) => ColumnMetadata(None, exprToSQL(expr), expr.typ)
    })

  def iterator(ctx: Seq[Row]): RowIterator =
    input
      .iterator(ctx)
      .map(row =>
        val projected = expandedFields.map(f => eval(f, row +: ctx))
        Row(projected, meta, None, None),
      )

case class AliasProcess(input: Process, alias: String) extends Process:
  val meta: Metadata = Metadata(input.meta.columns map (_.copy(table = Some(alias))))

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx).map(_.copy(meta = meta))

case class ColumnAliasProcess(input: Process, alias: String, columns: Seq[String]) extends Process:
  val meta: Metadata =
    require(columns.length == input.meta.columns.length,
      s"column alias count (${columns.length}) doesn't match column count (${input.meta.columns.length})")
    Metadata(columns.zip(input.meta.columns).map { (name, col) =>
      ColumnMetadata(Some(alias), name, col.typ)
    }.toIndexedSeq)

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx).map(_.copy(meta = meta))

case class DistinctProcess(input: Process) extends Process:
  val meta: Metadata = input.meta

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx).distinctBy(_.data)

object Nulls:
  val first: Ordering[Value] =
    (x: Value, y: Value) =>
      if x.isNull && y.isNull then 0
      else if x.isNull then -1
      else if y.isNull then 1
      else x compare y

  val last: Ordering[Value] =
    (x: Value, y: Value) =>
      if x.isNull && y.isNull then 0
      else if x.isNull then 1
      else if y.isNull then -1
      else x compare y

case class OrderBy(f: Expr, asc: Boolean, nullsFirst: Boolean)

case class SortProcess(input: Process, by: Seq[OrderBy]) extends Process:
  val meta: Metadata = input.meta

  private final class SeqOrdering(ords: Seq[Ordering[Value]]) extends Ordering[Seq[Value]]:
    def compare(xs: Seq[Value], ys: Seq[Value]): Int =
      val x   = xs.iterator
      val y   = ys.iterator
      val ord = ords.iterator

      while (x.hasNext && y.hasNext && ord.hasNext)
        val res = ord.next().compare(x.next(), y.next())

        if (res != 0) return res

      0

  def iterator(ctx: Seq[Row]): RowIterator =
    val data      = input.iterator(ctx) to ArraySeq
    val fs        = by map { case OrderBy(f, _, _) => f }

    // Validate ORDER BY column references eagerly (even on 0/1 rows)
    for f <- fs do validateColumns(f, meta)

    val orderings =
      by map { case OrderBy(_, asc, nullsFirst) =>
        (asc, nullsFirst) match
          case (false, false) => Nulls.first.reverse
          case (false, true)  => Nulls.last.reverse
          case (true, false)  => Nulls.last
          case (true, true)   => Nulls.first
      }
    val ordering              = new SeqOrdering(orderings)
    val sorted: ArraySeq[Row] = data.sortBy(row =>
      fs map {
        case NumberExpr(n: Int) if n >= 1 && n <= meta.width => row.data(n - 1)
        case f                                               => eval(f, row +: ctx)
      },
    )(using ordering)

    sorted.iterator

case class TakeProcess(input: Process, n: Int) extends Process:
  val meta: Metadata = input.meta

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx) take n

class DropProcess(input: Process, n: Int) extends Process:
  val meta: Metadata = input.meta

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx) drop n

case class StaticProcess(data: ArraySeq[Row], meta: Metadata) extends Process:
  def iterator(ctx: Seq[Row]): RowIterator = data.iterator

case class UnionProcess(input1: Process, input2: Process, all: Boolean) extends Process:
  val meta: Metadata = input1.meta

  def iterator(ctx: Seq[Row]): RowIterator =
    val combined = input1.iterator(ctx) ++ input2.iterator(ctx).map(row => Row(row.data, meta, None, None))
    if all then combined else combined.distinctBy(_.data)

case class IntersectProcess(input1: Process, input2: Process) extends Process:
  val meta: Metadata = input1.meta

  def iterator(ctx: Seq[Row]): RowIterator =
    val rightSet = input2.iterator(ctx).map(_.data).toSet
    input1.iterator(ctx).filter(row => rightSet.contains(row.data)).distinctBy(_.data)

case class ExceptProcess(input1: Process, input2: Process) extends Process:
  val meta: Metadata = input1.meta

  def iterator(ctx: Seq[Row]): RowIterator =
    val rightSet = input2.iterator(ctx).map(_.data).toSet
    input1.iterator(ctx).filter(row => !rightSet.contains(row.data)).distinctBy(_.data)

case class ValuesProcess(rows: Seq[Seq[Expr]], width: Int) extends Process:
  val meta: Metadata = Metadata((1 to width).map(i => ColumnMetadata(None, s"column$i", AnyType)).toIndexedSeq)

  def iterator(ctx: Seq[Row]): RowIterator =
    rows.iterator.map { exprs =>
      if exprs.length != width then sys.error(s"VALUES row has ${exprs.length} columns, expected $width")
      Row(exprs.map(e => eval(e, ctx)).toIndexedSeq, meta, None, None)
    }

case class LateralCrossProcess(input1: Process, input2: Process) extends Process:
  val meta: Metadata = Metadata(input1.meta.columns ++ input2.meta.columns)

  def iterator(ctx: Seq[Row]): RowIterator =
    input1.iterator(ctx).flatMap { leftRow =>
      input2.iterator(leftRow +: ctx).map { rightRow =>
        Row(leftRow.data ++ rightRow.data, meta, None, None)
      }
    }

case class LeftLateralJoinProcess(input1: Process, input2: Process, cond: Expr) extends Process:
  val meta: Metadata = Metadata(input1.meta.columns ++ input2.meta.columns)

  def iterator(ctx: Seq[Row]): RowIterator =
    input1.iterator(ctx).flatMap { leftRow =>
      val matches = input2.iterator(leftRow +: ctx)
        .map(rightRow => Row(leftRow.data ++ rightRow.data, meta, None, None))
        .filter(row => beval(cond, row +: ctx))
      if matches.isEmpty then
        Iterator(Row(leftRow.data ++ Vector.fill(input2.meta.width)(NULL), meta, None, None))
      else matches
    }

case class CrossProcess(input1: Process, input2: Process) extends Process:
  val meta: Metadata = Metadata(input1.meta.columns ++ input2.meta.columns)

  def iterator(ctx: Seq[Row]): RowIterator =
    for
      x <- input1.iterator(ctx)
      y <- input2.iterator(ctx)
    yield Row(x.data ++ y.data, meta, None, None)

case class LeftCrossJoinProcess(input1: Process, input2: Process, cond: Expr) extends Process:
  val meta: Metadata = Metadata(input1.meta.columns ++ input2.meta.columns)

  def iterator(ctx: Seq[Row]): RowIterator =
    input1.iterator(ctx).flatMap { x =>
      val matches =
        input2.iterator(ctx) map (y => Row(x.data ++ y.data, meta, None, None)) filter (row => beval(cond, row +: ctx))

      if matches.isEmpty then Iterator(Row(x.data ++ Seq.fill(input2.meta.width)(NULL), meta, None, None))
      else matches
    }

case class RightCrossJoinProcess(input1: Process, input2: Process, cond: Expr) extends Process:
  val meta: Metadata = Metadata(input1.meta.columns ++ input2.meta.columns)

  def iterator(ctx: Seq[Row]): RowIterator =
    input2.iterator(ctx).flatMap { y =>
      val matches =
        input1.iterator(ctx) map (x => Row(x.data ++ y.data, meta, None, None)) filter (row => beval(cond, row +: ctx))

      if matches.isEmpty then Iterator(Row(Vector.fill(input1.meta.width)(NULL) ++ y.data, meta, None, None))
      else matches
    }

case class FullCrossJoinProcess(input1: Process, input2: Process, cond: Expr) extends Process:
  val meta: Metadata = Metadata(input1.meta.columns ++ input2.meta.columns)

  def iterator(ctx: Seq[Row]): RowIterator =
    val leftRows = input1.iterator(ctx).to(ArraySeq)
    val rightRows = input2.iterator(ctx).to(ArraySeq)
    val rightMatched = mutable.Set[Int]()

    val leftResults = leftRows.iterator.flatMap { x =>
      var matched = false
      val matches = rightRows.zipWithIndex.iterator.flatMap { case (y, idx) =>
        val row = Row(x.data ++ y.data, meta, None, None)
        if beval(cond, row +: ctx) then
          matched = true
          rightMatched += idx
          Iterator(row)
        else Iterator.empty
      }.to(ArraySeq)

      if matched then matches.iterator
      else Iterator(Row(x.data ++ Vector.fill(input2.meta.width)(NULL), meta, None, None))
    }

    val rightUnmatched = rightRows.zipWithIndex.iterator.flatMap { case (y, idx) =>
      if rightMatched.contains(idx) then Iterator.empty
      else Iterator(Row(Vector.fill(input1.meta.width)(NULL) ++ y.data, meta, None, None))
    }

    leftResults ++ rightUnmatched

case class HashJoinProcess(build: Process, probe: Process, buildKeys: Seq[Int], probeKeys: Seq[Int], residual: Option[Expr])
    extends Process:
  val meta: Metadata = Metadata(build.meta.columns ++ probe.meta.columns)

  def iterator(ctx: Seq[Row]): RowIterator =
    val buildRows = build.iterator(ctx).toVector
    val hashTable = mutable.HashMap[Vector[Value], ArrayBuffer[Row]]()
    for row <- buildRows do
      val key = buildKeys.map(row.data(_)).toVector
      hashTable.getOrElseUpdate(key, ArrayBuffer()) += row
    probe.iterator(ctx).flatMap { probeRow =>
      val key = probeKeys.map(probeRow.data(_)).toVector
      hashTable.getOrElse(key, ArrayBuffer.empty).iterator.flatMap { buildRow =>
        val combined = Row(buildRow.data ++ probeRow.data, meta, None, None)
        residual match
          case Some(cond) => if beval(cond, combined +: ctx) then Iterator(combined) else Iterator.empty
          case None       => Iterator(combined)
      }
    }

case class LeftHashJoinProcess(build: Process, probe: Process, buildKeys: Seq[Int], probeKeys: Seq[Int], residual: Option[Expr])
    extends Process:
  val meta: Metadata = Metadata(build.meta.columns ++ probe.meta.columns)

  def iterator(ctx: Seq[Row]): RowIterator =
    val probeRows = probe.iterator(ctx).toVector
    val hashTable = mutable.HashMap[Vector[Value], ArrayBuffer[Row]]()
    for row <- probeRows do
      val key = probeKeys.map(row.data(_)).toVector
      hashTable.getOrElseUpdate(key, ArrayBuffer()) += row
    build.iterator(ctx).flatMap { buildRow =>
      val key = buildKeys.map(buildRow.data(_)).toVector
      val matches = hashTable.getOrElse(key, ArrayBuffer.empty).iterator.flatMap { probeRow =>
        val combined = Row(buildRow.data ++ probeRow.data, meta, None, None)
        residual match
          case Some(cond) => if beval(cond, combined +: ctx) then Iterator(combined) else Iterator.empty
          case None       => Iterator(combined)
      }.to(ArraySeq)
      if matches.isEmpty then Iterator(Row(buildRow.data ++ Vector.fill(probe.meta.width)(NULL), meta, None, None))
      else matches.iterator
    }

case class RightHashJoinProcess(build: Process, probe: Process, buildKeys: Seq[Int], probeKeys: Seq[Int], residual: Option[Expr])
    extends Process:
  val meta: Metadata = Metadata(build.meta.columns ++ probe.meta.columns)

  def iterator(ctx: Seq[Row]): RowIterator =
    val buildRows = build.iterator(ctx).toVector
    val hashTable = mutable.HashMap[Vector[Value], ArrayBuffer[Row]]()
    for row <- buildRows do
      val key = buildKeys.map(row.data(_)).toVector
      hashTable.getOrElseUpdate(key, ArrayBuffer()) += row
    probe.iterator(ctx).flatMap { probeRow =>
      val key = probeKeys.map(probeRow.data(_)).toVector
      val matches = hashTable.getOrElse(key, ArrayBuffer.empty).iterator.flatMap { buildRow =>
        val combined = Row(buildRow.data ++ probeRow.data, meta, None, None)
        residual match
          case Some(cond) => if beval(cond, combined +: ctx) then Iterator(combined) else Iterator.empty
          case None       => Iterator(combined)
      }.to(ArraySeq)
      if matches.isEmpty then Iterator(Row(Vector.fill(build.meta.width)(NULL) ++ probeRow.data, meta, None, None))
      else matches.iterator
    }

case class FullHashJoinProcess(build: Process, probe: Process, buildKeys: Seq[Int], probeKeys: Seq[Int], residual: Option[Expr])
    extends Process:
  val meta: Metadata = Metadata(build.meta.columns ++ probe.meta.columns)

  def iterator(ctx: Seq[Row]): RowIterator =
    val buildRows = build.iterator(ctx).toVector
    val hashTable = mutable.HashMap[Vector[Value], ArrayBuffer[(Row, Int)]]()
    for (row, idx) <- buildRows.zipWithIndex do
      val key = buildKeys.map(row.data(_)).toVector
      hashTable.getOrElseUpdate(key, ArrayBuffer()) += ((row, idx))
    val buildMatched = mutable.BitSet()

    val probeResults = probe.iterator(ctx).flatMap { probeRow =>
      val key = probeKeys.map(probeRow.data(_)).toVector
      val matches = hashTable.getOrElse(key, ArrayBuffer.empty).iterator.flatMap { case (buildRow, buildIdx) =>
        val combined = Row(buildRow.data ++ probeRow.data, meta, None, None)
        residual match
          case Some(cond) =>
            if beval(cond, combined +: ctx) then
              buildMatched += buildIdx
              Iterator(combined)
            else Iterator.empty
          case None =>
            buildMatched += buildIdx
            Iterator(combined)
      }.to(ArraySeq)
      if matches.isEmpty then Iterator(Row(Vector.fill(build.meta.width)(NULL) ++ probeRow.data, meta, None, None))
      else matches.iterator
    }.to(ArraySeq)

    val buildUnmatched = buildRows.zipWithIndex.iterator.flatMap { case (buildRow, idx) =>
      if buildMatched.contains(idx) then Iterator.empty
      else Iterator(Row(buildRow.data ++ Vector.fill(probe.meta.width)(NULL), meta, None, None))
    }

    probeResults.iterator ++ buildUnmatched

case class IndexNestedLoopJoinProcess(
    outer: Process,
    table: Table,
    index: TableIndex,
    outerKeyExprs: Seq[Expr],
    innerMeta: Metadata,
    residual: Option[Expr],
    outerIsLeft: Boolean,
) extends Process:
  val meta: Metadata =
    if outerIsLeft then Metadata(outer.meta.columns ++ innerMeta.columns)
    else Metadata(innerMeta.columns ++ outer.meta.columns)

  def iterator(ctx: Seq[Row]): RowIterator =
    outer.iterator(ctx).flatMap { outerRow =>
      val key = outerKeyExprs.map(e => eval(e, outerRow +: ctx)).toIndexedSeq
      val innerIter = table.indexPointScan(index, key).getOrElse(Iterator.empty)
      val matches = innerIter.flatMap { innerRow =>
        val combined =
          if outerIsLeft then Row(outerRow.data ++ innerRow.data, meta, None, None)
          else Row(innerRow.data ++ outerRow.data, meta, None, None)
        residual match
          case Some(cond) => if beval(cond, combined +: ctx) then Iterator(combined) else Iterator.empty
          case None       => Iterator(combined)
      }
      matches
    }

case class LeftIndexNestedLoopJoinProcess(
    outer: Process,
    table: Table,
    index: TableIndex,
    outerKeyExprs: Seq[Expr],
    innerMeta: Metadata,
    residual: Option[Expr],
) extends Process:
  val meta: Metadata = Metadata(outer.meta.columns ++ innerMeta.columns)

  def iterator(ctx: Seq[Row]): RowIterator =
    outer.iterator(ctx).flatMap { outerRow =>
      val key = outerKeyExprs.map(e => eval(e, outerRow +: ctx)).toIndexedSeq
      val innerIter = table.indexPointScan(index, key).getOrElse(Iterator.empty)
      val matches = innerIter.flatMap { innerRow =>
        val combined = Row(outerRow.data ++ innerRow.data, meta, None, None)
        residual match
          case Some(cond) => if beval(cond, combined +: ctx) then Iterator(combined) else Iterator.empty
          case None       => Iterator(combined)
      }
      if matches.isEmpty then
        Iterator(Row(outerRow.data ++ Vector.fill(innerMeta.width)(NULL), meta, None, None))
      else matches
    }

case class RightIndexNestedLoopJoinProcess(
    outer: Process,
    table: Table,
    index: TableIndex,
    outerKeyExprs: Seq[Expr],
    innerMeta: Metadata,
    residual: Option[Expr],
) extends Process:
  val meta: Metadata = Metadata(innerMeta.columns ++ outer.meta.columns)

  def iterator(ctx: Seq[Row]): RowIterator =
    outer.iterator(ctx).flatMap { outerRow =>
      val key = outerKeyExprs.map(e => eval(e, outerRow +: ctx)).toIndexedSeq
      val innerIter = table.indexPointScan(index, key).getOrElse(Iterator.empty)
      val matches = innerIter.flatMap { innerRow =>
        val combined = Row(innerRow.data ++ outerRow.data, meta, None, None)
        residual match
          case Some(cond) => if beval(cond, combined +: ctx) then Iterator(combined) else Iterator.empty
          case None       => Iterator(combined)
      }
      if matches.isEmpty then
        Iterator(Row(Vector.fill(innerMeta.width)(NULL) ++ outerRow.data, meta, None, None))
      else matches
    }

case class GenerateSeriesProcess(startExpr: Expr, stopExpr: Expr, stepExpr: Option[Expr]) extends Process:
  val meta: Metadata = Metadata(Vector(ColumnMetadata(None, "generate_series", NumberType)))

  def iterator(ctx: Seq[Row]): RowIterator =
    val start = neval(startExpr, ctx).value.longValue
    val stop = neval(stopExpr, ctx).value.longValue
    val step = stepExpr.map(e => neval(e, ctx).value.longValue).getOrElse(1L)
    if step == 0 then Iterator.empty
    else if step > 0 then
      Iterator.iterate(start)(_ + step).takeWhile(_ <= stop).map { i =>
        Row(Vector(NumberValue(i.toInt)), meta, None, None)
      }
    else
      Iterator.iterate(start)(_ + step).takeWhile(_ >= stop).map { i =>
        Row(Vector(NumberValue(i.toInt)), meta, None, None)
      }

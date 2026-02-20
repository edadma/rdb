package io.github.edadma.rdb

import io.github.edadma.dal.BasicDAL

import scala.language.postfixOps

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait Process:
  def iterator(ctx: Seq[Row]): RowIterator
  def meta: Metadata

type RowIterator = Iterator[Row]

case object SingleProcess extends Process:
  val meta: Metadata = Metadata(Vector.empty)

  def iterator(ctx: Seq[Row]): RowIterator = Iterator(
    Row(Vector.empty, meta, None, None),
  )

case class SeqScanProcess(input: Process, cond: Expr) extends Process:
  val meta: Metadata = input.meta

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
        if res.meta.width != 1 then problem(query, "sub-query must return rows of one column")
        res.data.map(_.data.head).distinct.iterator.flatMap { v =>
          table.indexPointScan(index, IndexedSeq(v)).getOrElse(Iterator.empty)
        }
    residual match
      case Some(cond) => baseIter.filter(row => beval(cond, row +: ctx))
      case None       => baseIter

case class HavingProcess(input: Process, cond: Expr) extends Process:
  val meta: Metadata = input.meta

  def iterator(ctx: Seq[Row]): RowIterator =
    input.iterator(ctx).filter(row => beval(cond, row +: ctx))

case class AggregateProcess(input: Process, groupBy: Seq[Expr], aggregates: Seq[AggregateSpec]) extends Process:
  val meta: Metadata =
    val aggColumns = aggregates.map(spec => ColumnMetadata(None, spec.name, spec.typ))
    Metadata(input.meta.columns ++ aggColumns)

  def iterator(ctx: Seq[Row]): RowIterator =
    val rows = input.iterator(ctx).toVector

    if groupBy.isEmpty then
      // Ungrouped aggregate: treat entire input as one group
      for spec <- aggregates do spec.func.init()

      for row <- rows do
        val rowCtx = row +: ctx
        for spec <- aggregates do spec.func.acc(spec.args.map(a => eval(a, rowCtx)))

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
          for spec <- aggregates do spec.func.acc(spec.args.map(a => eval(a, rowCtx)))

        val aggValues = aggregates.map(_.func.result).toVector
        Row(group.last.data ++ aggValues, meta, None, None)
      }

case class ProjectProcess(input: Process, fields: IndexedSeq[Expr]) extends Process:
  private val ctx = Seq(input.meta)

  @tailrec
  private def lookup(name: String, ctx: Seq[Metadata]): Option[(Type, Option[String])] =
    ctx match
      case Nil      => None
      case hd :: tl =>
        hd.columnMap get name match
          case None                => lookup(name, tl)
          case Some((_, typ, tab)) => Some((typ, tab))

  val meta: Metadata =
    Metadata(fields.zipWithIndex map {
      case (AliasExpr(expr, alias), _)             => ColumnMetadata(None, alias.name, expr.typ)
      case (c @ ColumnExpr(table, Ident(name)), _) =>
        val lookupName = table.map(t => s"${t.name}.$name").getOrElse(name)

        lookup(lookupName, ctx) match
          case None             => problem(c, s"'$lookupName' not found")
          case Some((typ, tab)) => ColumnMetadata(tab, name, typ)
      case (expr: Expr, idx) => ColumnMetadata(None, s"col_${idx + 1}", expr.typ)
    })

  def iterator(ctx: Seq[Row]): RowIterator =
    input
      .iterator(ctx)
      .map(row =>
        val projected = fields.map(f => eval(f, row +: ctx))
        Row(projected, meta, None, None),
      )

case class AliasProcess(input: Process, alias: String) extends Process:
  val meta: Metadata = Metadata(input.meta.columns map (_.copy(table = Some(alias))))

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx).map(_.copy(meta = meta))

case class DistinctProcess(input: Process) extends Process:
  val meta: Metadata = input.meta

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx).distinctBy(_.data)

object Nulls:
  val first: Ordering[Value] =
    (x: Value, y: Value) =>
      if x.isNull then -1
      else if y.isNull then 1
      else x compare y

  val last: Ordering[Value] =
    (x: Value, y: Value) =>
      if x.isNull then 1
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

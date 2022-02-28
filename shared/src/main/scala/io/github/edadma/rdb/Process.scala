package io.github.edadma.rdb

import io.github.edadma.dal.BasicDAL
//import pprint.pprintln

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

  def iterator(ctx: Seq[Row]): RowIterator = Iterator(Row(Vector.empty, meta, None, AggregateMode.AccumulateReturn))

case class FilterProcess(input: Process, cond: Expr) extends Process:
  val meta: Metadata = input.meta

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx).filter(row => beval(cond, row +: ctx))

case class UngroupedProcess(input: Process, column: Boolean) extends Process:
  val meta: Metadata = input.meta

  def iterator(ctx: Seq[Row]): RowIterator =
    val rows = input.iterator(ctx) to ArrayBuffer // todo: do this without buffering table

    if column then
      rows
        .map(_.copy(mode = AggregateMode.Accumulate))
        .iterator ++ rows.map(_.copy(mode = AggregateMode.Return)).iterator
    else
      for (i <- 0 until (rows.length - 1))
        rows(i) = rows(i).copy(mode = AggregateMode.Accumulate)

      rows(rows.length - 1) = rows(rows.length - 1).copy(mode = AggregateMode.AccumulateReturn)
      rows.iterator

case class ProjectProcess(input: Process, fields: IndexedSeq[Expr] /*, metactx: Seq[Metadata]*/ ) extends Process:
  private val ctx = Seq(input.meta) // input.meta +: metactx

  @tailrec
  private def lookup(name: String, ctx: Seq[Metadata]): Option[(Type, Option[String])] =
    ctx match
      case Nil => None
      case hd :: tl =>
        hd.columnMap get name match
          case None                => lookup(name, tl)
          case Some((_, typ, tab)) => Some((typ, tab))

  val meta: Metadata =
    Metadata(fields.zipWithIndex map {
      case (c @ ColumnExpr(Ident(name)), idx) =>
        lookup(name, ctx) match
          case None             => problem(c, s"'$name' not found")
          case Some((typ, tab)) => ColumnMetadata(tab, name, typ)
      case (expr: Expr, idx) => ColumnMetadata(None, s"col_${idx + 1}", expr.typ)
    })

  def iterator(ctx: Seq[Row]): RowIterator =
    input
      .iterator(ctx)
      .flatMap(row =>
        val projected =
          fields
            .map(f => eval(f, row +: ctx, row.mode))

        row.mode match
          case AggregateMode.Return | AggregateMode.AccumulateReturn => Iterator(Row(projected, meta, None))
          case _                                                     => Iterator.empty
      )

case class AliasProcess(input: Process, alias: String) extends Process:
  val meta: Metadata = Metadata(input.meta.columns map (_.copy(table = Some(alias))))

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx).map(_.copy(meta = meta))

case class DistinctProcess(input: Process) extends Process:
  val meta: Metadata = input.meta

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx).distinctBy(_.data)

case class GroupProcess(input: Process, by: Seq[Expr]) extends Process:
  val meta: Metadata = input.meta

  def iterator(ctx: Seq[Row]): RowIterator =
    val data = input.iterator(ctx) to mutable.ArraySeq
    val groups = (data groupBy (row => by map (f => eval(f, row +: ctx, AggregateMode.Disallow))) values) toSeq

    for (g <- groups)
      for (i <- 0 until (g.length - 1))
        g(i) = g(i).copy(mode = AggregateMode.Accumulate)

      g(g.length - 1) = g.last.copy(mode = AggregateMode.AccumulateReturn)

    Iterator.concat(groups: _*)

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
      val x = xs.iterator
      val y = ys.iterator
      val ord = ords.iterator

      while (x.hasNext && y.hasNext && ord.hasNext)
        val res = ord.next().compare(x.next(), y.next())

        if (res != 0) return res

      0

  def iterator(ctx: Seq[Row]): RowIterator =
    val data = input.iterator(ctx) to ArraySeq
    val fs = by map { case OrderBy(f, _, _) => f }
    val orderings =
      by map { case OrderBy(_, asc, nullsFirst) =>
        (asc, nullsFirst) match
          case (false, false) => Nulls.first.reverse
          case (false, true)  => Nulls.last.reverse
          case (true, false)  => Nulls.last
          case (true, true)   => Nulls.first
      }
    val ordering = new SeqOrdering(orderings)
    val sorted: ArraySeq[Row] = data.sortBy(row => fs map (f => eval(f, row +: ctx, AggregateMode.Disallow)))(ordering)

    sorted.iterator

case class TakeProcess(input: Process, n: Int) extends Process:
  val meta: Metadata = input.meta

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx) take n

class DropProcess(input: Process, n: Int) extends Process:
  val meta: Metadata = input.meta

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx) drop n

case class CrossProcess(input1: Process, input2: Process) extends Process:
  val meta: Metadata = Metadata(input1.meta.columns ++ input2.meta.columns)

  def iterator(ctx: Seq[Row]): RowIterator =
    for
      x <- input1.iterator(ctx)
      y <- input2.iterator(ctx)
    yield Row(x.data ++ y.data, meta, None)

case class LeftCrossJoinProcess(input1: Process, input2: Process, cond: Expr) extends Process:
  val meta: Metadata = Metadata(input1.meta.columns ++ input2.meta.columns)

  def iterator(ctx: Seq[Row]): RowIterator =
    input1.iterator(ctx).flatMap { x =>
      val matches =
        input2.iterator(ctx) map (y => Row(x.data ++ y.data, meta, None)) filter (row => beval(cond, row +: ctx))

      if matches.isEmpty then Iterator(Row(x.data ++ Seq.fill(input2.meta.width)(NULL), meta, None))
      else matches
    }

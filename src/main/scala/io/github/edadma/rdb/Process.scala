package io.github.edadma.rdb

import scala.annotation.tailrec

trait Process:
  def iterator(ctx: Seq[Row]): RowIterator
  def meta: Metadata

type RowIterator = Iterator[Row]

class FilterProcess(input: Process, cond: Expr) extends Process:
  val meta: Metadata = input.meta

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx).filter(row => beval(cond, row +: ctx))

class ProjectProcess(input: Process, fields: IndexedSeq[Expr] /*, metactx: Seq[Metadata]*/ ) extends Process:
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
      case (ColumnExpr(Ident(name, pos), _), idx) =>
        lookup(name, ctx) match
          case None             => sys.error(s"variable '$name' not found")
          case Some((typ, tab)) => ColumnMetadata(tab, name, typ)
      case (expr: Expr, idx) => ColumnMetadata(None, s"${idx + 1}", expr.typ)
    })

  def iterator(ctx: Seq[Row]): RowIterator =
    input.iterator(ctx).map(row => Row(fields.map(f => eval(f, row +: ctx)), meta))

class AliasProcess(input: Process, alias: String) extends Process:
  val meta: Metadata = Metadata(input.meta.columns map (_.copy(table = Some(alias))))

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx).map(_.copy(meta = meta))

class DistinctProcess(input: Process) extends Process:
  val meta: Metadata = input.meta

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx).distinctBy(_.data)

class TakeProcess(input: Process, n: Int) extends Process:
  val meta: Metadata = input.meta

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx) take n

class DropProcess(input: Process, n: Int) extends Process:
  val meta: Metadata = input.meta

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx) drop n

class CrossProcess(input1: Process, input2: Process) extends Process:
  val meta: Metadata = Metadata(input1.meta.columns ++ input2.meta.columns)

  def iterator(ctx: Seq[Row]): RowIterator =
    for
      x <- input1.iterator(ctx)
      y <- input2.iterator(ctx)
    yield Row(x.data ++ y.data, meta)

class LeftCrossJoinProcess(input1: Process, input2: Process, cond: Expr) extends Process:
  val meta: Metadata = Metadata(input1.meta.columns ++ input2.meta.columns)

  def iterator(ctx: Seq[Row]): RowIterator =
    input1.iterator(ctx).flatMap { x =>
      val matches = input2.iterator(ctx) map (y => Row(x.data ++ y.data, meta)) filter (row => beval(cond, row +: ctx))

      if matches.isEmpty then Iterator(Row(x.data ++ Seq.fill(input2.meta.width)(NullValue), meta))
      else matches
    }

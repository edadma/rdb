package io.github.edadma.rdb

import scala.annotation.tailrec
//import pprint.pprintln

trait Operator:
  def iterator(ctx: Seq[Row]): RowIterator
  def meta: Metadata

type RowIterator = Iterator[Row]

//case class ScanStep(tab: Table) extends Step:
//  val meta: RowMeta = tab.meta
//
//  def iterator: RowIterator =
//    new RowIterator:
//      var idx = 0
//
//      def hasNext: Boolean = idx < tab.size
//
//      def next: Row =
//        val res = tab row idx
//
//        idx += 1
//        res

//class CollectOperator(input: Operator) extends Operator:
//  val data: Seq[Row] = input.iterator(ctx).toSeq
//  val meta: Metadata = input.meta
//
//  def iterator(ctx: Seq[Row]): RowIterator = data.iterator
//
//  def value: TableValue = TableValue(input.toSeq, input.meta)

class FilterOperator(input: Operator, cond: Expr) extends Operator:
  val meta: Metadata = input.meta

  def iterator(ctx: Seq[Row]): RowIterator =
    println(("filter meta", meta))
    println(("filter ctx", ctx))
    println(("filter cond", cond))
    input.iterator(ctx).filter(row => beval(cond, row +: ctx))

class ProjectOperator(input: Operator, fields: IndexedSeq[Expr], metactx: Seq[Metadata]) extends Operator:
  private val ctx = input.meta +: metactx

  @tailrec
  private def lookup(name: String, ctx: Seq[Metadata]): Option[(Type, Option[String])] =
    ctx match
      case Nil => None
      case hd :: tl =>
        hd.columnMap get name match
          case None                => lookup(name, tl)
          case Some((_, typ, tab)) => Some((typ, tab))

  val meta: Metadata =
    Metadata(fields map { case VariableExpr(Ident(name, pos)) =>
      lookup(name, ctx) match
        case None             => sys.error(s"variable '$name' not found")
        case Some((typ, tab)) => ColumnMetadata(tab, name, typ)
    })

  def iterator(ctx: Seq[Row]): RowIterator =
    input.iterator(ctx).map(row => Row(fields.map(f => eval(f, row +: ctx)), meta))

class AliasOperator(input: Operator, alias: String) extends Operator:
  require(input.meta.singleTable, s"row data not single table: ${input.meta}")

  val meta: Metadata = Metadata(input.meta.columns map (_.copy(table = Some(alias))))

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx).map(_.copy(meta = meta))

class CrossOperator(input1: Operator, input2: Operator) extends Operator:
  val meta: Metadata = Metadata(input1.meta.columns ++ input2.meta.columns)

  def iterator(ctx: Seq[Row]): RowIterator =
    for
      x <- input1.iterator(ctx)
      y <- input2.iterator(ctx)
    yield Row(x.data ++ y.data, meta)
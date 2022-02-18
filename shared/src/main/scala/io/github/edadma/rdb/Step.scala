package io.github.edadma.rdb

import scala.collection.mutable.ArrayBuffer

trait Step

//case class ScanStep(tab: Table) extends Step with RowIterable:
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

class CollectStep(input: RowIterable) extends Step with RowIterable:
  val data: Seq[Row] = input.toSeq
  val meta: Metadata = input.meta

  def iterator: RowIterator = data.iterator

  def value: TableValue = TableValue(input.toSeq, input.meta)

class FilterStep(input: RowIterable, cond: Expr, ctx: () => Seq[Row]) extends Step with RowIterable:
  val meta: Metadata = input.meta

  def iterator: RowIterator = input.iterator.filter(row => beval(cond, row, ctx()))

class CrossStep(input1: RowIterable, input2: RowIterable) extends Step with RowIterable:
  val meta: Metadata = Metadata(input1.meta.columns ++ input2.meta.columns)

  def iterator: RowIterator =
    (for { x <- input1.iterator; y <- input2 } yield (x, y)) map ((row1, row2) => Row(row1.data ++ row2.data, meta))

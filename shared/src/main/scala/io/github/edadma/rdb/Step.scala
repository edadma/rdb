package io.github.edadma.rdb

import scala.collection.mutable.ArrayBuffer

trait Step

case class ScanStep(tab: Table) extends Step with RowIterable:
  val meta: RowMeta = tab.meta

  def iterator: RowIterator =
    new RowIterator:
      var idx = 0

      def hasNext: Boolean = idx < tab.size

      def next: Row =
        val res = tab row idx

        idx += 1
        res

case class CollectStep(input: RowIterable) extends Step with RowIterable:
  val data: Seq[Row] = input.toSeq
  val meta: RowMeta = input.meta

  def iterator: RowIterator = data.iterator

  def value: TableValue = TableValue(input.toSeq, input.meta)

case class FilterStep(input: RowIterable, cond: Expr, ctx: () => Seq[Row]) extends Step with RowIterable:
  val meta: RowMeta = input.meta

  def iterator: RowIterator = input.iterator.filter(row => beval(cond, row, ctx()))

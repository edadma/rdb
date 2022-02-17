package io.github.edadma.rdb

import scala.collection.mutable.ArrayBuffer

trait Step

case class ScanStep(tab: Table) extends Step with RowIterable:
  def meta: RowMeta = tab.meta

  def iterator: RowIterator =
    new RowIterator:
      var idx = 0

      def hasNext: Boolean = idx < tab.size

      def next(): Row =
        val res = tab row idx

        idx += 1
        res

case class CollectStep(rows: RowIterable) extends Step with RowIterable:
  val data: Seq[Row] = rows.toSeq
  val meta: RowMeta = rows.meta

  def iterator: RowIterator = data.iterator

  def value: TableValue = TableValue(rows.toSeq, rows.meta)

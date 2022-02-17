package io.github.edadma.rdb

import scala.collection.mutable.ArrayBuffer

trait Step

class ScanStep(tab: Table) extends Step with RowIterable:
  def meta: RowMeta = tab.meta

  def iterator: RowIterator =
    new RowIterator:
      var idx = 0

      def hasNext: Boolean = idx < tab.size

      def next(): Row =
        val res = tab row idx

        idx += 1
        res

class CollectStep(rows: RowIterable) extends Step:
  def value = TableValue(rows.toSeq, rows.meta)

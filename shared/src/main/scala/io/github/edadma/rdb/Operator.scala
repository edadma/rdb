package io.github.edadma.rdb

trait Operator

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

class CollectOperator(input: RowIterable) extends Operator with RowIterable:
  val data: Seq[Row] = input.toSeq
  val meta: Metadata = input.meta

  def iterator: RowIterator = data.iterator

  def value: TableValue = TableValue(input.toSeq, input.meta)

class FilterOperator(input: RowIterable, cond: Expr, ctx: () => Seq[Row]) extends Operator with RowIterable:
  val meta: Metadata = input.meta

  def iterator: RowIterator = input.iterator.filter(row => beval(cond, row, ctx()))

class AliasOperator(input: RowIterable, alias: String) extends Operator with RowIterable:
  require(input.meta.singleTable, s"row data not single table: ${input.meta}")

  val meta: Metadata = Metadata(input.meta.columns map (_.copy(table = Some(alias))))

  def iterator: RowIterator = input.iterator

class CrossOperator(input1: RowIterable, input2: RowIterable) extends Operator with RowIterable:
  val meta: Metadata = Metadata(input1.meta.columns ++ input2.meta.columns)

  def iterator: RowIterator =
    for
      x <- input1.iterator
      y <- input2
    yield Row(x.data ++ y.data, meta)

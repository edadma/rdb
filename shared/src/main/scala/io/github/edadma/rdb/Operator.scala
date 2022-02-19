package io.github.edadma.rdb

trait Operator extends RowIterable

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

//class CollectOperator(input: RowIterable) extends Operator:
//  val data: Seq[Row] = input.iterator(ctx).toSeq
//  val meta: Metadata = input.meta
//
//  def iterator(ctx: Seq[Row]): RowIterator = data.iterator
//
//  def value: TableValue = TableValue(input.toSeq, input.meta)

class FilterOperator(input: RowIterable, cond: Expr) extends Operator:
  val meta: Metadata = input.meta

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx).filter(row => beval(cond, ctx))

class AliasOperator(input: RowIterable, alias: String) extends Operator:
  require(input.meta.singleTable, s"row data not single table: ${input.meta}")

  val meta: Metadata = Metadata(input.meta.columns map (_.copy(table = Some(alias))))

  def iterator(ctx: Seq[Row]): RowIterator = input.iterator(ctx)

class CrossOperator(input1: RowIterable, input2: RowIterable) extends Operator:
  val meta: Metadata = Metadata(input1.meta.columns ++ input2.meta.columns)

  def iterator(ctx: Seq[Row]): RowIterator =
    for
      x <- input1.iterator(ctx)
      y <- input2.iterator(ctx)
    yield Row(x.data ++ y.data, meta)

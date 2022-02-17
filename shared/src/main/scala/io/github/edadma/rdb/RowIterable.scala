package io.github.edadma.rdb

type Row = Seq[Value]

trait RowIterable extends Iterable[Row]:
  def iterator: RowIterator
  def meta: TableMeta

trait RowIterator extends Iterator[Row]
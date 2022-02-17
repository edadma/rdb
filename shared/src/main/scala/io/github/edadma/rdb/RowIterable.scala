package io.github.edadma.rdb

trait RowIterable extends Iterable[Row]:
  def iterator: RowIterator
  def meta: RowMeta

trait RowIterator extends Iterator[Row]
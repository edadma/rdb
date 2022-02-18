package io.github.edadma.rdb

trait RowIterable extends Iterable[Row]:
  def iterator: RowIterator
  def meta: Metadata

type RowIterator = Iterator[Row]

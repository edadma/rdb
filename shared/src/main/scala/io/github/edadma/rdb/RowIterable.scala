package io.github.edadma.rdb

trait RowIterable: // extends Iterable[Row]:
  def iterator(ctx: Seq[Row]): RowIterator
  def meta: Metadata

type RowIterator = Iterator[Row]

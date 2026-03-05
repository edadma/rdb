package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

object ValueSeqOrdering extends Ordering[IndexedSeq[Value]]:
  def compare(a: IndexedSeq[Value], b: IndexedSeq[Value]): Int =
    val len = math.min(a.length, b.length)
    var i = 0

    while i < len do
      val cmp = a(i).compare(b(i))
      if cmp != 0 then return cmp
      i += 1

    a.length - b.length

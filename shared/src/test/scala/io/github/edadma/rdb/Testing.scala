package io.github.edadma.rdb

import pprint.*

trait Testing:
  def test(sql: String): String =
    implicit val db: DB = MemoryDB("test")

    PPrinter.BlackWhite(executeSQL(sql)).toString :+ '\n'

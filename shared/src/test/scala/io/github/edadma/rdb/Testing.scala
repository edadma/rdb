package io.github.edadma.rdb

import pprint.*

trait Testing:
  def test(sql: String): String =
    implicit val db: DB = new MemoryDB

    PPrinter.BlackWhite(executeSQL(sql)).toString :+ '\n'

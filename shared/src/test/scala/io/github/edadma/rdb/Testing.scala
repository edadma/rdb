package io.github.edadma.rdb

import pprint.*

trait Testing:
  def test(sql: String): String =
    given DB = new MemoryDB

    PPrinter.BlackWhite(executeSQL(sql)).toString :+ '\n'

  def query(sql: String): TableValue =
    given DB = new MemoryDB

    executeSQL(sql).collect { case QueryResult(t) => t }.last

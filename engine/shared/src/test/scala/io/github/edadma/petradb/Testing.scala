package io.github.edadma.petradb

import pprint.*

trait Testing:
  def test(sql: String): String =
    given Session = new MemoryDB().connect()

    PPrinter.BlackWhite(executeSQL(sql)).toString :+ '\n'

  def query(sql: String): TableValue =
    given Session = new MemoryDB().connect()

    executeSQL(sql).collect { case QueryResult(t) => t }.last

  def results(sql: String): Seq[io.github.edadma.petradb.Result] =
    given Session = new MemoryDB().connect()

    executeSQL(sql)

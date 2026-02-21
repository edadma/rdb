package io.github.edadma.rdb

import pprint.*
import java.io.{ByteArrayOutputStream, PrintStream}

trait Testing:
  def suppressStderr[A](block: => A): A =
    Console.withErr(new PrintStream(new ByteArrayOutputStream()))(block)

  def test(sql: String): String =
    given Session = new MemoryDB().connect()

    PPrinter.BlackWhite(executeSQL(sql)).toString :+ '\n'

  def query(sql: String): TableValue =
    given Session = new MemoryDB().connect()

    executeSQL(sql).collect { case QueryResult(t) => t }.last

  def results(sql: String): Seq[io.github.edadma.rdb.Result] =
    given Session = new MemoryDB().connect()

    executeSQL(sql)

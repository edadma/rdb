package io.github.edadma.rdb.cli

import mainargs.{main, arg, Flag, ParserForMethods}
import io.github.edadma.rdb.*
import scala.io.Source

object Main:
  @main
  def run(
      @arg(short = 'm', doc = "Use in-memory database")
      memory: Flag = Flag(false),
      @arg(short = 'e', doc = "Execute SQL and exit")
      execute: Seq[String] = Seq.empty,
      @arg(short = 'f', doc = "Execute SQL file and exit")
      file: Seq[String] = Seq.empty,
      @arg(doc = "Read SQL from stdin and exit")
      stdin: Flag = Flag(false),
      @arg(doc = "Database file path")
      path: Option[String] = None,
  ): Unit =
    val db: DB =
      if memory.value || path.isEmpty then new MemoryDB
      else
        val p = path.get
        val f = new java.io.File(p)
        if f.exists() then PersistentDB.open(p)
        else PersistentDB.create(p, 4096)

    val rl   = PlatformReadLine.create()
    val repl = new Repl(db, rl)

    val batch = execute.nonEmpty || file.nonEmpty || stdin.value

    if batch then
      for f <- file do repl.executeFile(f)
      if stdin.value then
        val sql = Source.stdin.mkString
        if sql.trim.nonEmpty then repl.executeSql(sql)
      for sql <- execute do repl.executeSql(sql)
      rl.close()
    else
      println("rdb — interactive SQL shell")
      println("Type \\q to quit, \\dt to list tables, \\d <table> to describe a table.")
      println()
      repl.run()

  @main
  def dump(
      @arg(doc = "Path to the database file")
      path: String,
  ): Unit =
    Dump.run(path)

  def main(args: Array[String]): Unit =
    val normalizedArgs = args.map(a => if a == "-h" then "--help" else a)
    ParserForMethods(this).runOrExit(normalizedArgs.toIndexedSeq, allowPositional = true)

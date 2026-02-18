package io.github.edadma.rdb.cli

import mainargs.{main, arg, Flag, ParserForClass, TokensReader}
import io.github.edadma.rdb.*
import scala.io.Source

@main
case class Config(
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
)

object Main:
  def main(args: Array[String]): Unit =
    val config = ParserForClass[Config].constructOrExit(args.toIndexedSeq)

    val db: DB =
      if config.memory.value || config.path.isEmpty then new MemoryDB
      else
        val path = config.path.get
        val f    = new java.io.File(path)
        if f.exists() then PersistentDB.open(path)
        else PersistentDB.create(path, 4096)

    val rl   = PlatformReadLine.create()
    val repl = new Repl(db, rl)

    val batch = config.execute.nonEmpty || config.file.nonEmpty || config.stdin.value

    if batch then
      for f <- config.file do repl.executeFile(f)
      if config.stdin.value then
        val sql = Source.stdin.mkString
        if sql.trim.nonEmpty then repl.executeSql(sql)
      for sql <- config.execute do repl.executeSql(sql)
      rl.close()
    else
      println("rdb — interactive SQL shell")
      println("Type \\q to quit, \\dt to list tables, \\d <table> to describe a table.")
      println()
      repl.run()

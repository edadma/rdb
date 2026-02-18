package io.github.edadma.rdb.cli

import mainargs.{main, arg, Flag, ParserForClass, TokensReader}
import io.github.edadma.rdb.*

@main
case class Config(
    @arg(short = 'm', doc = "Use in-memory database")
    memory: Flag = Flag(false),
    @arg(short = 'f', doc = "Execute SQL file then enter REPL")
    file: Seq[String] = Seq.empty,
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

    println("rdb — interactive SQL shell")
    println("Type \\q to quit, \\dt to list tables, \\d <table> to describe a table.")
    println()

    for f <- config.file do repl.executeFile(f)

    repl.run()

package io.github.edadma.rdb.cli

import io.github.edadma.rdb.*

object Main:
  def main(args: Array[String]): Unit =
    val positional = args.filterNot(_.startsWith("--"))
    val db: DB =
      if args.contains("--memory") || positional.isEmpty then new MemoryDB
      else
        val path = positional.head
        val file = new java.io.File(path)
        if file.exists() then PersistentDB.open(path)
        else PersistentDB.create(path, 4096)

    val rl   = PlatformReadLine.create()
    val repl = new Repl(db, rl)

    println("rdb — interactive SQL shell")
    println("Type \\q to quit, \\dt to list tables, \\d <table> to describe a table.")
    println()

    // Execute any additional file arguments
    if positional.length > 1 then
      for file <- positional.tail do
        repl.executeFile(file)

    repl.run()

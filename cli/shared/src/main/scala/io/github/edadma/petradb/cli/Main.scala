package io.github.edadma.petradb.cli

import mainargs.{main, arg, Flag, ParserForMethods}
import io.github.edadma.petradb.*
import io.github.edadma.cross_platform

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
        if p.endsWith(".ptxt") then TextDB.open(p)
        else if cross_platform.exists(p) then PersistentDB.open(p)
        else PersistentDB.create(p, 4096)

    val session = db.connect()
    val repl    = new PlatformRepl(session)

    val batch = execute.nonEmpty || file.nonEmpty || stdin.value

    if batch then
      for f <- file do repl.executeFile(f)
      if stdin.value then
        repl.readStdin() match
          case Some(sql) => repl.executeSql(sql)
          case None      => Console.err.println("--stdin is not supported on this platform")
      for sql <- execute do repl.executeSql(sql)
    else
      println("PetraDB — interactive SQL shell")
      println("Type \\q to quit, \\dt to list tables, \\d <table> to describe a table.")
      println()
      repl.run()

  @main
  def dump(
      @arg(doc = "Path to the database file")
      path: String,
  ): Unit =
    Dump.run(path)

  private val subcommands = Set("run", "dump")

  def main(args: Array[String]): Unit =
    val realArgs = cross_platform.processArgs(args.toSeq).toArray
    val normalizedArgs = realArgs.map(a => if a == "-h" then "--help" else a)
    val effective =
      if normalizedArgs.isEmpty then
        Array("run")
      else if subcommands.contains(normalizedArgs.head) then
        normalizedArgs
      else if !normalizedArgs.head.startsWith("-") then
        Array("run", "--path") ++ normalizedArgs
      else
        Array("run") ++ normalizedArgs
    ParserForMethods(this).runOrExit(effective.toIndexedSeq, allowPositional = true)

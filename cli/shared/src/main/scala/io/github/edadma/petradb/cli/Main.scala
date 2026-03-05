package io.github.edadma.petradb.cli

import mainargs.{main, arg, Flag, ParserForMethods}
import io.github.edadma.petradb
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*
import io.github.edadma.petradb.client.SessionOptions
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
      @arg(doc = "Server host")
      host: Option[String] = None,
      @arg(doc = "Server port")
      port: Option[Int] = None,
      @arg(doc = "Username")
      user: Option[String] = None,
      @arg(doc = "Password")
      password: Option[String] = None,
  ): Unit =
    host match
      case Some(h) =>
        val options = SessionOptions(h, port.getOrElse(DefaultPort), user, password)
        connectAndRun(options, execute, file, stdin.value)
      case None =>
        val db: DB =
          if memory.value || path.isEmpty then new MemoryDB
          else
            val p = path.get
            if p.endsWith(".ptxt") then TextDB.open(p)
            else if cross_platform.exists(p) then PersistentDB.open(p)
            else PersistentDB.create(p, 4096)
        startRepl(db.connect(), execute, file, stdin.value)

  def startRepl(session: petradb.Session, execute: Seq[String], file: Seq[String], stdin: Boolean): Unit =
    val repl = new PlatformRepl(session)
    val batch = execute.nonEmpty || file.nonEmpty || stdin

    if batch then
      for f <- file do repl.executeFile(f) {}
      if stdin then
        repl.readStdin() match
          case Some(sql) => repl.executeSql(sql) {}
          case None      => Console.err.println("--stdin is not supported on this platform")
      for sql <- execute do repl.executeSql(sql) {}
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

  def mainWithArgs(args: Array[String]): Unit =
    runParsed(args.map(a => if a == "-h" then "--help" else a))

  def main(args: Array[String]): Unit =
    mainWithArgs(cross_platform.processArgs(args.toSeq).toArray)

  private def runParsed(normalizedArgs: Array[String]): Unit =
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

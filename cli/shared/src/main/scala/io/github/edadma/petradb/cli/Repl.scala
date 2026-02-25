package io.github.edadma.petradb.cli

import io.github.edadma.petradb.*
import io.github.edadma.cross_platform.readFile

abstract class Repl(val session: Session):
  val prompt     = "petra> "
  val contPrompt = "  -> "

  def run(): Unit

  def readStdin(): Option[String] = None

  def handleMeta(input: String): Boolean =
    MetaCommand.parse(input) match
      case MetaCommand.Quit =>
        false
      case MetaCommand.ListTables =>
        Output.listTables(session.db)
        true
      case MetaCommand.DescribeTable(name) =>
        Output.describeTable(session.db, name)
        true
      case MetaCommand.Include(path) =>
        executeFile(path)
        true
      case MetaCommand.DumpSchema =>
        Dump.dump(session.db)
        true
      case MetaCommand.Unknown(cmd) =>
        println(s"Unknown command: $cmd")
        println("Available: \\dt  \\d <table>  \\dump  \\i <file>  \\q")
        true

  def collectAndExecute(first: String, readLine: String => Option[String]): Unit =
    val buf = new StringBuilder(first)
    if !first.endsWith(";") then
      var done = false
      while !done do
        readLine(contPrompt) match
          case None => done = true
          case Some(cont) =>
            buf.append("\n").append(cont)
            if cont.trim.endsWith(";") then done = true
    executeSql(buf.toString)

  def executeSql(sql: String): Unit =
    given Session = session
    try
      val results = executeSQL(sql)
      results.foreach(Output.printResult)
    catch
      case e: PetraException =>
        val pos = e.pos
        if pos == null || pos.line == 0 then
          Console.err.println(e.getMessage)
        else if pos.line == 1 then
          Console.err.println(s"${e.getMessage}\n${pos.longString}")
        else
          Console.err.println(s"${pos.line}: ${e.getMessage}\n${pos.longString}")
      case e: Exception => Console.err.println(s"Error: ${e.getMessage}")

  def executeFile(path: String): Unit =
    try
      val sql = readFile(path)
      executeSql(sql)
    catch
      case e: Exception => println(s"Error reading file: ${e.getMessage}")

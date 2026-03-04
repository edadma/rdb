package io.github.edadma.petradb.cli

import io.github.edadma.petradb.*
import io.github.edadma.cross_platform.readFile

import scala.concurrent.ExecutionContext
import scala.util.{Success, Failure}

abstract class Repl(val session: Session):
  val prompt     = "petra> "
  val contPrompt = "  -> "
  var timing     = false

  given ExecutionContext = ExecutionContext.parasitic

  def run(): Unit

  def readStdin(): Option[String] = None

  private def printElapsed(start: Long): Unit =
    val elapsed = System.currentTimeMillis() - start
    println(s"Time: ${elapsed / 1000.0} s")

  def handleMeta(input: String)(onDone: Boolean => Unit): Unit =
    MetaCommand.parse(input) match
      case MetaCommand.Quit =>
        onDone(false)
      case MetaCommand.ListTables =>
        Output.listTables(session.db)
        onDone(true)
      case MetaCommand.ListViews =>
        Output.listViews(session.db)
        onDone(true)
      case MetaCommand.DescribeTable(name) =>
        Output.describeTable(session.db, name)
        onDone(true)
      case MetaCommand.Include(path) =>
        executeFile(path) { onDone(true) }
      case MetaCommand.DumpSchema =>
        Dump.dump(session.db)
        onDone(true)
      case MetaCommand.Copy(args) =>
        executeSql(s"COPY $args;") { onDone(true) }
      case MetaCommand.Timing =>
        timing = !timing
        println(s"Timing is ${if timing then "on" else "off"}.")
        onDone(true)
      case MetaCommand.Unknown(cmd) =>
        println(s"Unknown command: $cmd")
        println("Available: \\copy <args>  \\d <table>  \\dt  \\dump  \\dv  \\i <file>  \\q  \\timing")
        onDone(true)

  def collectAndExecute(first: String, readLine: String => Option[String])(onDone: => Unit): Unit =
    val buf = new StringBuilder(first)
    if !first.endsWith(";") then
      var done = false
      while !done do
        readLine(contPrompt) match
          case None => done = true
          case Some(cont) =>
            buf.append("\n").append(cont)
            if cont.trim.endsWith(";") then done = true
    executeSql(buf.toString)(onDone)

  def executeSql(sql: String)(onDone: => Unit): Unit =
    val start = if timing then System.currentTimeMillis() else 0L
    session.execute(sql).onComplete {
      case Success(results) =>
        results.foreach(Output.printResult)
        if timing then printElapsed(start)
        onDone
      case Failure(e: PetraException) =>
        val pos = e.pos
        if pos == null || pos.line == 0 then
          Console.err.println(e.getMessage)
        else if pos.line == 1 then
          Console.err.println(s"${e.getMessage}\n${pos.longString}")
        else
          Console.err.println(s"${pos.line}: ${e.getMessage}\n${pos.longString}")
        if timing then printElapsed(start)
        onDone
      case Failure(e) =>
        Console.err.println(s"Error: ${e.getMessage}")
        if timing then printElapsed(start)
        onDone
    }

  def executeFile(path: String)(onDone: => Unit): Unit =
    try
      val sql = readFile(path)
      executeSql(sql)(onDone)
    catch
      case e: Exception =>
        println(s"Error reading file: ${e.getMessage}")
        onDone

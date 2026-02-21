package io.github.edadma.rdb.cli

import io.github.edadma.rdb.*
import scala.io.Source

class Repl(session: Session, rl: ReadLine):
  private val prompt     = "rdb> "
  private val contPrompt = "  -> "

  def run(): Unit =
    var running = true

    while running do
      rl.readLine(prompt) match
        case None => running = false
        case Some(line) =>
          val trimmed = line.trim
          if trimmed.nonEmpty then
            rl.addHistory(line)
            if trimmed.startsWith("\\") then
              running = handleMeta(trimmed)
            else
              collectAndExecute(trimmed)

    rl.close()

  private def handleMeta(input: String): Boolean =
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

  private def collectAndExecute(first: String): Unit =
    val buf = new StringBuilder(first)

    if !first.endsWith(";") then
      var done = false
      while !done do
        rl.readLine(contPrompt) match
          case None => done = true
          case Some(cont) =>
            rl.addHistory(cont)
            buf.append("\n").append(cont)
            if cont.trim.endsWith(";") then done = true

    executeSql(buf.toString)

  def executeSql(sql: String): Unit =
    given Session = session
    try
      val results = executeSQL(sql)
      results.foreach(Output.printResult)
    catch
      case e: Exception => println(s"Error: ${e.getMessage}")

  def executeFile(path: String): Unit =
    try
      val source = Source.fromFile(path)
      try
        val sql = source.mkString
        executeSql(sql)
      finally source.close()
    catch
      case e: java.io.FileNotFoundException => println(s"File not found: $path")
      case e: Exception                     => println(s"Error reading file: ${e.getMessage}")

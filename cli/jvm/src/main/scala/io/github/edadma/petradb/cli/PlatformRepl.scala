package io.github.edadma.petradb.cli

import io.github.edadma.petradb.Session
import org.jline.reader.{LineReader, LineReaderBuilder, EndOfFileException, UserInterruptException}
import org.jline.terminal.TerminalBuilder
import java.nio.file.Paths

class PlatformRepl(session: Session) extends Repl(session):
  private val terminal = TerminalBuilder.builder().system(true).build()
  private val historyPath = Paths.get(System.getProperty("user.home"), ".petradb_history")
  private val reader = LineReaderBuilder.builder()
    .terminal(terminal)
    .variable(LineReader.HISTORY_FILE, historyPath)
    .build()

  override def readStdin(): Option[String] =
    val s = scala.io.Source.stdin.mkString
    if s.trim.nonEmpty then Some(s) else None

  private def jlineReadLine(p: String): Option[String] =
    try Some(reader.readLine(p))
    catch
      case _: EndOfFileException     => None
      case _: UserInterruptException => None

  def run(): Unit =
    var running = true
    while running do
      jlineReadLine(prompt) match
        case None => running = false
        case Some(line) =>
          val trimmed = line.trim
          if trimmed.nonEmpty then
            if trimmed.startsWith("\\") then
              running = handleMeta(trimmed)
            else
              collectAndExecute(trimmed, jlineReadLine)
    terminal.close()

package io.github.edadma.rdb.cli

import org.jline.reader.{LineReader, LineReaderBuilder, EndOfFileException, UserInterruptException}
import org.jline.terminal.TerminalBuilder
import org.jline.reader.impl.history.DefaultHistory

import java.nio.file.Paths

class JLineReadLine extends ReadLine:
  private val terminal = TerminalBuilder.builder().system(true).build()
  private val historyPath = Paths.get(System.getProperty("user.home"), ".rdb_history")
  private val reader = LineReaderBuilder.builder()
    .terminal(terminal)
    .variable(LineReader.HISTORY_FILE, historyPath)
    .build()

  def readLine(prompt: String): Option[String] =
    try Some(reader.readLine(prompt))
    catch
      case _: EndOfFileException      => None
      case _: UserInterruptException  => None

  def addHistory(line: String): Unit = () // JLine handles history automatically

  def close(): Unit = terminal.close()

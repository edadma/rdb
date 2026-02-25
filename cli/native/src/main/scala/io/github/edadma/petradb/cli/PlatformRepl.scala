package io.github.edadma.petradb.cli

import io.github.edadma.petradb.Session
import io.github.edadma.readline

class PlatformRepl(session: Session) extends Repl(session):
  override def readStdin(): Option[String] =
    val s = scala.io.Source.stdin.mkString
    if s.trim.nonEmpty then Some(s) else None

  private def nativeReadLine(p: String): Option[String] =
    readline.readline(p) match
      case null => None
      case line => Some(line)

  def run(): Unit =
    var running = true
    while running do
      nativeReadLine(prompt) match
        case None => running = false
        case Some(line) =>
          val trimmed = line.trim
          if trimmed.nonEmpty then
            readline.add_history(line)
            if trimmed.startsWith("\\") then
              running = handleMeta(trimmed)
            else
              collectAndExecute(trimmed, nativeReadLine)

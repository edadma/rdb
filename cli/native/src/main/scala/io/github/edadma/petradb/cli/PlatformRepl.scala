package io.github.edadma.petradb.cli

import io.github.edadma.petradb.Session
import io.github.edadma.readline
import scala.concurrent.{Await, Promise => SPromise}
import scala.concurrent.duration.Duration

class PlatformRepl(session: Session) extends Repl(session):
  private val historyFile = System.getProperty("user.home") + "/.petradb_history"

  override def readStdin(): Option[String] =
    val s = scala.io.Source.stdin.mkString
    if s.trim.nonEmpty then Some(s) else None

  private def nativeReadLine(p: String): Option[String] =
    readline.readline(p) match
      case null => None
      case line => Some(line)

  def run(): Unit =
    readline.read_history(historyFile)
    var running = true
    while running do
      nativeReadLine(prompt) match
        case None => running = false
        case Some(line) =>
          val trimmed = line.trim
          if trimmed.nonEmpty then
            readline.add_history(line)
            if trimmed.startsWith("\\") then
              val p = SPromise[Boolean]()
              handleMeta(trimmed)(result => p.success(result))
              running = Await.result(p.future, Duration.Inf)
            else
              val p = SPromise[Unit]()
              collectAndExecute(trimmed, nativeReadLine)(p.success(()))
              Await.result(p.future, Duration.Inf)
    readline.write_history(historyFile)

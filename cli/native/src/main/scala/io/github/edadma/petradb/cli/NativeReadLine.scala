package io.github.edadma.petradb.cli

import io.github.edadma.readline

class NativeReadLine extends ReadLine:
  def readLine(prompt: String): Option[String] =
    readline.readline(prompt) match
      case null => None
      case line => Some(line)

  def addHistory(line: String): Unit = readline.add_history(line)

  def close(): Unit = ()

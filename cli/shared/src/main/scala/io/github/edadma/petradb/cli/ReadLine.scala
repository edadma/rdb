package io.github.edadma.petradb.cli

trait ReadLine:
  def readLine(prompt: String): Option[String]
  def addHistory(line: String): Unit
  def close(): Unit

package io.github.edadma.petradb.cli

object PlatformReadLine:
  def create(): ReadLine = new JLineReadLine

package io.github.edadma.rdb.cli

object PlatformReadLine:
  def create(): ReadLine = new NativeReadLine

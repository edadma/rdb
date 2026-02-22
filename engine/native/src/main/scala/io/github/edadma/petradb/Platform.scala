package io.github.edadma.petradb

import io.github.edadma.libuuid.generateRandomString

object Platform:
  def randomUUID: String = generateRandomString

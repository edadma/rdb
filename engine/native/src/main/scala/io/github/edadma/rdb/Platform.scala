package io.github.edadma.rdb

import io.github.edadma.libuuid.generateRandomString

object Platform:
  def randomUUID: String = generateRandomString

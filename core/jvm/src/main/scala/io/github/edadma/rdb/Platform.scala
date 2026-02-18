package io.github.edadma.rdb

import java.util.UUID

object Platform:
  def randomUUID: String = UUID.randomUUID.toString

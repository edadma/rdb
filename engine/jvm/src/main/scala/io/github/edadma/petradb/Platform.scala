package io.github.edadma.petradb

import java.util.UUID

object Platform:
  def randomUUID: String = UUID.randomUUID.toString

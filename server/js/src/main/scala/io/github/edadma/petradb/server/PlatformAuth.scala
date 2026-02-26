package io.github.edadma.petradb.server

object PlatformAuth:
  def checkBasic(header: String, auth: BasicAuth): Boolean = false

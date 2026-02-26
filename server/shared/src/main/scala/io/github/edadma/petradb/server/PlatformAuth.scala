package io.github.edadma.petradb.server

import scala.util.Try

object PlatformAuth:
  def checkBasic(header: String, auth: BasicAuth): Boolean =
    if !header.startsWith("Basic ") then return false
    Try {
      val decoded = new String(PlatformCrypto.base64Decode(header.drop(6)), "UTF-8")
      decoded.split(":", 2) match
        case Array(username, password) =>
          auth.users.get(username).exists(hash => PasswordHash.verifyPassword(password, hash))
        case _ => false
    }.getOrElse(false)

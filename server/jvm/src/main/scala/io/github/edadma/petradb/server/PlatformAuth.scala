package io.github.edadma.petradb.server

import org.mindrot.jbcrypt.BCrypt
import scala.util.Try

object PlatformAuth:
  def checkBasic(header: String, auth: BasicAuth): Boolean =
    if !header.startsWith("Basic ") then return false
    Try {
      val decoded = new String(java.util.Base64.getDecoder.decode(header.drop(6)))
      decoded.split(":", 2) match
        case Array(username, password) =>
          auth.users.get(username).exists(hash => BCrypt.checkpw(password, hash))
        case _ => false
    }.getOrElse(false)

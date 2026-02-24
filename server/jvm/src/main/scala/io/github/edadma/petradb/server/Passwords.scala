package io.github.edadma.petradb.server

import org.mindrot.jbcrypt.BCrypt
import scala.util.Try

object Passwords:
  def check(password: String, hash: String): Boolean =
    Try(BCrypt.checkpw(password, hash)).getOrElse(false)

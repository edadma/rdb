package io.github.edadma.petradb.server

import toml.*
import toml.derivation.auto.*
import io.github.edadma.cross_platform.{exists, readFile}

sealed trait AuthConfig
case object NoAuth                               extends AuthConfig
case class BasicAuth(users: Map[String, String]) extends AuthConfig

case class ServerConfig(auth: AuthConfig)

object ServerConfig:
  val unrestricted: ServerConfig = ServerConfig(NoAuth)

  private case class UserEntry(username: String, password: String)
  private case class TomlConfig(auth: Option[String] = None, users: Option[List[UserEntry]] = None)

  def fromFile(path: String): ServerConfig =
    if !exists(path) then sys.error(s"Config file not found: $path")
    Toml.parseAs[TomlConfig](readFile(path)) match
      case Left((_, msg)) => sys.error(s"Config parse error: $msg")
      case Right(cfg) =>
        cfg.auth.getOrElse("none") match
          case "none" => ServerConfig(NoAuth)
          case "basic" =>
            val users = cfg.users.getOrElse(Nil).map(u => u.username -> u.password).toMap
            ServerConfig(BasicAuth(users))
          case other => sys.error(s"Unknown auth mode: $other")

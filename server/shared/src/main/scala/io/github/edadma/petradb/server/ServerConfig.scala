package io.github.edadma.petradb.server

import toml.*
import toml.derivation.auto.*
import io.github.edadma.cross_platform.{exists, readFile}

sealed trait AuthConfig
case object NoAuth                               extends AuthConfig
case class BasicAuth(users: Map[String, String]) extends AuthConfig

sealed trait CorsConfig
case object CorsAllowAll                  extends CorsConfig
case class CorsAllowOrigin(origin: String) extends CorsConfig
case object NoCors                        extends CorsConfig

case class ServerConfig(auth: AuthConfig, cors: CorsConfig = CorsAllowAll)

object ServerConfig:
  val unrestricted: ServerConfig = ServerConfig(NoAuth, CorsAllowAll)

  private case class UserEntry(username: String, password: String)
  private case class CorsSection(origin: Option[String] = None)
  private case class TomlConfig(
    auth: Option[String] = None,
    users: Option[List[UserEntry]] = None,
    cors: Option[CorsSection] = None,
  )

  def fromFile(path: String): ServerConfig =
    if !exists(path) then sys.error(s"Config file not found: $path")
    Toml.parseAs[TomlConfig](readFile(path)) match
      case Left((_, msg)) => sys.error(s"Config parse error: $msg")
      case Right(cfg) =>
        val authConfig = cfg.auth.getOrElse("none") match
          case "none" => NoAuth
          case "basic" =>
            val users = cfg.users.getOrElse(Nil).map(u => u.username -> u.password).toMap
            BasicAuth(users)
          case other => sys.error(s"Unknown auth mode: $other")

        val corsConfig = cfg.cors match
          case None => CorsAllowAll
          case Some(section) =>
            section.origin.getOrElse("*") match
              case "*"    => CorsAllowAll
              case "none" => NoCors
              case o      => CorsAllowOrigin(o)

        ServerConfig(authConfig, corsConfig)

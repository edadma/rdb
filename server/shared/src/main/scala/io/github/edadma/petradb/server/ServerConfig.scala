package io.github.edadma.petradb.server

import io.github.edadma.yaml.readFromString
import io.github.edadma.cross_platform.{exists, readFile}

sealed trait AuthConfig
case object NoAuth                                    extends AuthConfig
case class BasicAuth(users: Map[String, String])      extends AuthConfig // username -> bcrypt hash

case class ServerConfig(auth: AuthConfig)

object ServerConfig:
  val unrestricted: ServerConfig = ServerConfig(NoAuth)

  def fromFile(path: String): ServerConfig =
    if !exists(path) then sys.error(s"Config file not found: $path")
    val node = readFromString(readFile(path))
    node.getStringOption("auth") match
      case None | Some("none") => ServerConfig(NoAuth)
      case Some("basic") =>
        val users =
          if node.contains("users") then
            node.getSeq("users").map { u =>
              u.getString("username") -> u.getString("password")
            }.toMap
          else Map.empty[String, String]
        ServerConfig(BasicAuth(users))
      case Some(other) =>
        sys.error(s"Unknown auth mode: $other")

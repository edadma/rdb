package io.github.edadma.petradb.client

import io.github.edadma.fetch.*
import io.github.edadma.petradb.*
import io.github.edadma.petradb.Codecs.given
import upickle.default.*
import scala.concurrent.{Future, ExecutionContext}

case class SessionOptions(
  host: String = "localhost",
  port: Int = 3000,
  username: Option[String] = None,
  password: Option[String] = None,
)

class Session(options: SessionOptions = SessionOptions()):

  private val baseUrl                   = s"http://${options.host}:${options.port}"
  private var sessionId: Option[String] = None

  private def authHeader: Map[String, String] =
    (for u <- options.username; p <- options.password yield {
      val encoded = java.util.Base64.getEncoder.encodeToString(s"$u:$p".getBytes("UTF-8"))
      Map("Authorization" -> s"Basic $encoded")
    }).getOrElse(Map.empty)

  private def sessionHeaders: Map[String, String] =
    Map("Content-Type" -> "application/octet-stream") ++
      sessionId.map("X-Session-Id" -> _) ++
      authHeader

  private def baseHeaders: Map[String, String] = authHeader

  def execute(sql: String)(implicit ec: ExecutionContext): Future[Seq[Result]] =
    fetch(
      s"$baseUrl/sql",
      "POST",
      body    = Some(sql),
      headers = sessionHeaders,
    ).flatMap { res =>
      if res.ok then
        Future.successful(readBinary[Seq[Result]](res.body))
      else
        Future.failed(new RuntimeException(res.bodyAsString))
    }

  def connect()(implicit ec: ExecutionContext): Future[String] =
    fetch(s"$baseUrl/session", "POST", headers = baseHeaders).flatMap { res =>
      if res.ok then
        val id = readBinary[Map[String, String]](res.body).apply("sessionId")
        sessionId = Some(id)
        Future.successful(id)
      else
        Future.failed(new RuntimeException(s"Failed to create session: ${res.bodyAsString}"))
    }

  def close()(implicit ec: ExecutionContext): Future[Unit] =
    sessionId match
      case None => Future.unit
      case Some(id) =>
        fetch(s"$baseUrl/session/$id", "DELETE", headers = baseHeaders).flatMap { res =>
          sessionId = None
          if res.ok then Future.unit
          else Future.failed(new RuntimeException(res.bodyAsString))
        }

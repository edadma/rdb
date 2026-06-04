package io.github.edadma.petradb.client

import io.github.edadma.fetch.*
import io.github.edadma.petradb.*
import io.github.edadma.petradb.Codecs.given
import upickle.default.*
import scala.concurrent.{Future, ExecutionContext}

case class SessionOptions(
  host: String = "localhost",
  port: Int = DefaultPort,
  username: Option[String] = None,
  password: Option[String] = None,
)

class Session(options: SessionOptions = SessionOptions()) extends io.github.edadma.petradb.Session:

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

  def execute(sql: String)(using ec: ExecutionContext): Future[Seq[Result]] =
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

  /** Bind parameters are not yet carried by the HTTP protocol; the server only accepts a raw SQL
    * string. Supporting `$1`-style binds over HTTP requires extending the `/sql` endpoint to accept
    * a parameter payload. Use the embedded engine `Session` for parameterized queries for now.
    */
  def execute(sql: String, params: Seq[Value])(using ec: ExecutionContext): Future[Seq[Result]] =
    Future.failed(
      new UnsupportedOperationException(
        "parameterized execute is not supported over the HTTP client yet; use the embedded engine Session",
      ),
    )

  def connect()(using ec: ExecutionContext): Future[String] =
    fetch(s"$baseUrl/session", "POST", headers = baseHeaders).flatMap { res =>
      if res.ok then
        val id = readBinary[Map[String, String]](res.body).apply("sessionId")
        sessionId = Some(id)
        Future.successful(id)
      else
        Future.failed(new RuntimeException(s"Failed to create session: ${res.bodyAsString}"))
    }

  def close()(using ec: ExecutionContext): Future[Unit] =
    sessionId match
      case None => Future.unit
      case Some(id) =>
        fetch(s"$baseUrl/session/$id", "DELETE", headers = baseHeaders).flatMap { res =>
          sessionId = None
          if res.ok then Future.unit
          else Future.failed(new RuntimeException(res.bodyAsString))
        }

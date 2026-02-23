package io.github.edadma.petradb.client

import io.github.edadma.petradb.*
import scala.concurrent.{Future, ExecutionContext}

case class SessionOptions(
  host: String = "localhost",
  port: Int = 3000,
  rowMode: String = "object",
)

class Session(options: SessionOptions = SessionOptions()):

  private val baseUrl               = s"http://${options.host}:${options.port}"
  private var sessionId: Option[String] = None

  def execute(sql: String, rowMode: String = options.rowMode)(implicit ec: ExecutionContext): Future[Seq[Result]] =
    val body    = ujson.write(ujson.Obj("sql" -> sql, "rowMode" -> rowMode))
    val headers = sessionId.map(id => Map("X-Session-Id" -> id)).getOrElse(Map.empty)
    platformPost(s"$baseUrl/sql", body, headers).map { response =>
      if response.status >= 400 then
        val msg = ujson.read(response.body).obj.get("error").map(_.str).getOrElse(response.body)
        throw new RuntimeException(msg)
      ResponseParser.parseResponse(response.body, rowMode)
    }

  def connect()(implicit ec: ExecutionContext): Future[String] =
    platformPost(s"$baseUrl/session", "", Map.empty).map { response =>
      if response.status >= 400 then
        throw new RuntimeException(s"Failed to create session: ${response.body}")
      val id = ujson.read(response.body)("sessionId").str
      sessionId = Some(id)
      id
    }

  def close()(implicit ec: ExecutionContext): Future[Unit] =
    sessionId match
      case None     => Future.successful(())
      case Some(id) =>
        platformDelete(s"$baseUrl/session/$id", Map.empty).map { _ =>
          sessionId = None
        }

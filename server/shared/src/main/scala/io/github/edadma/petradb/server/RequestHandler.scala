package io.github.edadma.petradb.server

import io.github.edadma.petradb.{executeSQL, Session}
import zio.json.*

object RequestHandler:
  case class HandlerResponse(status: Int, body: String, contentType: String = "application/json; charset=UTF-8")

  def handleSql(
    sessionMgr: SessionManager,
    sessionId: Option[String],
    requestBody: String,
  ): HandlerResponse =
    val sqlRequest = requestBody.fromJson[SqlRequest] match
      case Left(err) =>
        return HandlerResponse(400, ErrorResponse(s"Invalid JSON: $err").toJson)
      case Right(req) => req

    val session = sessionId match
      case Some(id) => sessionMgr.getSession(id)
      case None     => sessionMgr.transientSession()

    val results =
      try executeSQL(sqlRequest.sql)(using session)
      catch
        case e: Exception =>
          return HandlerResponse(400, ErrorResponse("SQL error", Some(e.getMessage)).toJson)

    val rowMode = sqlRequest.rowMode.getOrElse("object")
    val resultJsons = ResultSerializer.serializeResults(results, rowMode)
    HandlerResponse(200, resultJsons.toJson)

  def handleCreateSession(sessionMgr: SessionManager): HandlerResponse =
    val (id, _) = sessionMgr.createSession()
    HandlerResponse(200, s"""{"sessionId":"$id"}""")

  def handleCloseSession(sessionMgr: SessionManager, sessionId: String): HandlerResponse =
    if sessionMgr.closeSession(sessionId) then
      HandlerResponse(200, """{"ok":true}""")
    else
      HandlerResponse(404, ErrorResponse("Session not found").toJson)

  def handleHealth(): HandlerResponse =
    HandlerResponse(200, """{"status":"ok"}""")

package io.github.edadma.petradb.server

import io.github.edadma.petradb.{executeSQL, Session, PetraException, ParseException, TypeException, UndefinedReferenceException, SchemaException, ConstraintException}
import io.github.edadma.petradb.Codecs.given
import upickle.default.*

import scala.util.Try
import org.mindrot.jbcrypt.BCrypt

object RequestHandler:
  case class HandlerResponse(
    status: Int,
    body: Array[Byte],
    contentType: String = "application/octet-stream",
    extraHeaders: Map[String, String] = Map.empty,
  )

  private def errorResponse(status: Int, message: String): HandlerResponse =
    HandlerResponse(status, message.getBytes("UTF-8"), "text/plain; charset=UTF-8")

  def checkAuth(authHeader: Option[String], auth: AuthConfig): Boolean =
    auth match
      case NoAuth => true
      case BasicAuth(users) =>
        authHeader match
          case Some(header) if header.startsWith("Basic ") =>
            Try {
              val decoded = new String(java.util.Base64.getDecoder.decode(header.drop(6)))
              decoded.split(":", 2) match
                case Array(username, password) =>
                  users.get(username).exists(hash => checkPassword(password, hash))
                case _ => false
            }.getOrElse(false)
          case _ => false

  def handleUnauthorized(): HandlerResponse =
    HandlerResponse(
      401,
      "Unauthorized".getBytes("UTF-8"),
      "text/plain; charset=UTF-8",
      Map("WWW-Authenticate" -> """Basic realm="PetraDB""""),
    )

  private def checkPassword(password: String, hash: String): Boolean =
    Try(BCrypt.checkpw(password, hash)).getOrElse(false)

  def handleSql(
    sessionMgr: SessionManager,
    sessionId: Option[String],
    requestBody: String,
  ): HandlerResponse =
    val session = sessionId match
      case Some(id) => sessionMgr.getSession(id)
      case None     => sessionMgr.transientSession()

    val results =
      try executeSQL(requestBody)(using session)
      catch
        case e: (ParseException | TypeException | UndefinedReferenceException) =>
          return errorResponse(400, e.getMessage)
        case e: (SchemaException | ConstraintException) =>
          return errorResponse(409, e.getMessage)
        case e: PetraException =>
          return errorResponse(500, e.getMessage)
        case e: Exception =>
          return errorResponse(500, e.getMessage)

    HandlerResponse(200, writeBinary(results.toSeq))

  def handleCreateSession(sessionMgr: SessionManager): HandlerResponse =
    val (id, _) = sessionMgr.createSession()
    HandlerResponse(200, writeBinary(Map("sessionId" -> id)))

  def handleCloseSession(sessionMgr: SessionManager, sessionId: String): HandlerResponse =
    if sessionMgr.closeSession(sessionId) then
      HandlerResponse(200, writeBinary(Map("ok" -> "true")))
    else
      errorResponse(404, "Session not found")

  def handleHealth(): HandlerResponse =
    HandlerResponse(200, """{"status":"ok"}""".getBytes("UTF-8"), "application/json; charset=UTF-8")

package io.github.edadma.petradb.server

import io.github.edadma.petradb.{executeSQL, Session, PetraException, ParseException, TypeException, UndefinedReferenceException, SchemaException, ConstraintException}
import io.github.edadma.petradb.Codecs.given
import upickle.default.*

object RequestHandler:
  case class HandlerResponse(
    status: Int,
    body: Array[Byte],
    contentType: String = "application/octet-stream",
    extraHeaders: Map[String, String] = Map.empty,
  )

  private def errorResponse(status: Int, message: String): HandlerResponse =
    HandlerResponse(status, message.getBytes("UTF-8"), "text/plain; charset=UTF-8")

  private def corsHeaders(cors: CorsConfig): Map[String, String] =
    cors match
      case NoCors => Map.empty
      case CorsAllowAll =>
        Map(
          "Access-Control-Allow-Origin"  -> "*",
          "Access-Control-Allow-Methods" -> "GET, POST, DELETE, OPTIONS",
          "Access-Control-Allow-Headers" -> "Content-Type, Authorization, X-Session-Id",
        )
      case CorsAllowOrigin(origin) =>
        Map(
          "Access-Control-Allow-Origin"  -> origin,
          "Access-Control-Allow-Methods" -> "GET, POST, DELETE, OPTIONS",
          "Access-Control-Allow-Headers" -> "Content-Type, Authorization, X-Session-Id",
        )

  def checkAuth(authHeader: Option[String], auth: AuthConfig): Boolean =
    auth match
      case NoAuth        => true
      case ba: BasicAuth => authHeader.exists(h => PlatformAuth.checkBasic(h, ba))

  def handleUnauthorized(): HandlerResponse =
    HandlerResponse(
      401,
      "Unauthorized".getBytes("UTF-8"),
      "text/plain; charset=UTF-8",
      Map("WWW-Authenticate" -> """Basic realm="PetraDB""""),
    )

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

  def dispatch(
    method: String,
    path: String,
    body: String,
    authHeader: Option[String],
    sessionId: Option[String],
    sessionMgr: SessionManager,
    auth: AuthConfig,
    cors: CorsConfig = CorsAllowAll,
  ): HandlerResponse =
    val ch = corsHeaders(cors)

    if method == "OPTIONS" then
      return HandlerResponse(204, Array.emptyByteArray, extraHeaders = ch)

    val response =
      if path != "/health" && !checkAuth(authHeader, auth) then handleUnauthorized()
      else (method, path) match
        case ("POST", "/sql")     => handleSql(sessionMgr, sessionId, body)
        case ("POST", "/session") => handleCreateSession(sessionMgr)
        case ("DELETE", p) if p.startsWith("/session/") =>
          handleCloseSession(sessionMgr, p.stripPrefix("/session/"))
        case ("GET", "/health") => handleHealth()
        case _ =>
          HandlerResponse(404, """{"error":"Not found"}""".getBytes("UTF-8"), "application/json; charset=UTF-8")

    response.copy(extraHeaders = ch ++ response.extraHeaders)

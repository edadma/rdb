package io.github.edadma.petradb.server

import io.github.edadma.petradb.DB
import io.github.edadma.microserve.*

class PetraServer(
  val loop: EventLoop,
  db: DB,
  host: String = "127.0.0.1",
  port: Int = 5432,
  auth: AuthConfig = NoAuth,
):
  private val sessionMgr = new SessionManager(db)
  private var server: Server = null

  def start(onListening: () => Unit = () => ()): Unit =
    server = createServer(loop) { (req, res) =>
      val result =
        if req.path != "/health" && !RequestHandler.checkAuth(req.get("Authorization"), auth) then
          RequestHandler.handleUnauthorized()
        else (req.method, req.path) match
          case ("POST", "/sql") =>
            val sessionId = req.get("X-Session-Id")
            RequestHandler.handleSql(sessionMgr, sessionId, req.bodyString)

          case ("POST", "/session") =>
            RequestHandler.handleCreateSession(sessionMgr)

          case ("DELETE", path) if path.startsWith("/session/") =>
            val id = path.stripPrefix("/session/")
            RequestHandler.handleCloseSession(sessionMgr, id)

          case ("GET", "/health") =>
            RequestHandler.handleHealth()

          case _ =>
            RequestHandler.HandlerResponse(404, """{"error":"Not found"}""".getBytes("UTF-8"), "application/json; charset=UTF-8")

      val r = result.extraHeaders.foldLeft(
        res.status(result.status).set("Content-Type", result.contentType)
      ) { case (acc, (k, v)) => acc.set(k, v) }
      r.end(result.body)
    }

    server.listen(port, host)(onListening)

  def actualPort: Int = server.actualPort

  def stop(onDrain: () => Unit = () => ()): Unit =
    sessionMgr.closeAll()
    db.close()
    if server != null then server.close(onDrain)

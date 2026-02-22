package io.github.edadma.petradb.server

import io.github.edadma.microserve.*

class PetraServer(
  val loop: EventLoop,
  host: String = "127.0.0.1",
  port: Int = 5432,
):
  private val sessionMgr = new SessionManager
  private var server: Server = null

  def start(onListening: () => Unit = () => ()): Unit =
    server = createServer(loop) { (req, res) =>
      val result = (req.method, req.path) match
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
          RequestHandler.HandlerResponse(404, """{"error":"Not found"}""")

      res.status(result.status).sendJson(result.body)
    }

    server.listen(port, host)(onListening)

  def actualPort: Int = server.actualPort

  def stop(onDrain: () => Unit = () => ()): Unit =
    sessionMgr.closeAll()
    if server != null then server.close(onDrain)

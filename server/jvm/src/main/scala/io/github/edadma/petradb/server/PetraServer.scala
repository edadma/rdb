package io.github.edadma.petradb.server

import io.github.edadma.petradb.{DB, DefaultPort}
import io.github.edadma.microserve.*

class PetraServer(
  val loop: EventLoop,
  db: DB,
  host: String = "127.0.0.1",
  port: Int = DefaultPort,
  auth: AuthConfig = NoAuth,
):
  private val sessionMgr = new SessionManager(db)
  private var server: Server = null

  def start(onListening: () => Unit = () => ()): Unit =
    server = createServer(loop) { (req, res) =>
      val result = RequestHandler.dispatch(
        method = req.method,
        path = req.path,
        body = req.bodyString,
        authHeader = req.get("Authorization"),
        sessionId = req.get("X-Session-Id"),
        sessionMgr = sessionMgr,
        auth = auth,
      )

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

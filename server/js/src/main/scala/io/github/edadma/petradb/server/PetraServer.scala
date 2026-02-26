package io.github.edadma.petradb.server

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.compiletime.uninitialized
import io.github.edadma.petradb.DB

class PetraServer(
  db: DB,
  host: String = "127.0.0.1",
  port: Int = 5432,
  auth: AuthConfig = NoAuth,
):
  private val sessionMgr = new SessionManager(db)
  private var server: NodeHttpServer = uninitialized

  private def toBuffer(bytes: Array[Byte]): Uint8Array =
    NodeBuffer.from(js.Array(bytes.map(_.toInt & 0xff)*))

  def start(onListening: () => Unit = () => ()): Unit =
    server = NodeHttp.createServer { (req, res) =>
      val chunks = js.Array[String]()

      req.on("data", { (chunk: js.Any) =>
        chunks.push(chunk.toString)
      })

      req.on("end", { (_: js.Any) =>
        val body = chunks.mkString
        val authHeader = req.headers.get("authorization").map(_.toString)
        val path = req.url

        val result =
          if path != "/health" && !RequestHandler.checkAuth(authHeader, auth) then
            RequestHandler.handleUnauthorized()
          else (req.method, path) match
            case ("POST", "/sql") =>
              val sessionId = req.headers.get("x-session-id").map(_.toString)
              RequestHandler.handleSql(sessionMgr, sessionId, body)

            case ("POST", "/session") =>
              RequestHandler.handleCreateSession(sessionMgr)

            case ("DELETE", p) if p.startsWith("/session/") =>
              val id = p.stripPrefix("/session/")
              RequestHandler.handleCloseSession(sessionMgr, id)

            case ("GET", "/health") =>
              RequestHandler.handleHealth()

            case _ =>
              RequestHandler.HandlerResponse(404, """{"error":"Not found"}""".getBytes("UTF-8"), "application/json; charset=UTF-8")

        val headers = js.Dictionary(
          "Content-Type" -> result.contentType,
        )
        result.extraHeaders.foreach { case (k, v) => headers(k) = v }

        res.writeHead(result.status, headers)
        res.end(toBuffer(result.body))
      })
    }

    server.listen(port, host, () => onListening())

  def stop(onDrain: () => Unit = () => ()): Unit =
    sessionMgr.closeAll()
    db.close()
    if server != null then server.close(() => onDrain())

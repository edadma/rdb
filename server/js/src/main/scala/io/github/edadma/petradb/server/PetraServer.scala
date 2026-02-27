package io.github.edadma.petradb.server

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.compiletime.uninitialized
import io.github.edadma.petradb.{DB, DefaultPort}

class PetraServer(
  db: DB,
  host: String = "127.0.0.1",
  port: Int = DefaultPort,
  auth: AuthConfig = NoAuth,
  cors: CorsConfig = CorsAllowAll,
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

        val result = RequestHandler.dispatch(
          method = req.method,
          path = { val idx = req.url.indexOf('?'); if idx < 0 then req.url else req.url.substring(0, idx) },
          body = body,
          authHeader = req.headers.get("authorization").map(_.toString),
          sessionId = req.headers.get("x-session-id").map(_.toString),
          sessionMgr = sessionMgr,
          auth = auth,
          cors = cors,
        )

        val headers = js.Dictionary(
          "Content-Type" -> result.contentType,
        )
        result.extraHeaders.foreach { case (k, v) => headers(k) = v }

        res.writeHead(result.status, headers)
        res.end(toBuffer(result.body))
      })
    }

    server.listen(port, host, () => onListening())

  def actualPort: Int = server.address().port.asInstanceOf[Int]

  def stop(onDrain: () => Unit = () => ()): Unit =
    sessionMgr.closeAll()
    db.close()
    if server != null then server.close(() => onDrain())

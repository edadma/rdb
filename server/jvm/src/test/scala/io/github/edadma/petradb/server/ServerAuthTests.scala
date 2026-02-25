package io.github.edadma.petradb.server

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.github.edadma.petradb.MemoryDB
import io.github.edadma.microserve.EventLoop
import org.mindrot.jbcrypt.BCrypt

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

class ServerAuthTests extends AnyFreeSpec with Matchers:
  private val client = HttpClient.newHttpClient()

  // bcrypt hashes generated once for the test suite
  private val aliceHash = BCrypt.hashpw("alice123", BCrypt.gensalt())
  private val bobHash   = BCrypt.hashpw("bob456",   BCrypt.gensalt())

  private def withServerAuth(auth: AuthConfig)(test: Int => Unit): Unit =
    val loop   = new EventLoop
    val db     = new MemoryDB
    val server = new PetraServer(loop, db, port = 0, auth = auth)
    server.start()
    val port   = server.actualPort
    val thread = new Thread(() => loop.run())
    thread.setDaemon(true)
    thread.start()
    Thread.sleep(100)
    try test(port)
    finally
      server.stop(() => loop.stop())
      thread.join(3000)

  private def withTempConfig(content: String)(test: String => Unit): Unit =
    val f = java.io.File.createTempFile("petradb-config-", ".yaml")
    f.deleteOnExit()
    java.nio.file.Files.writeString(f.toPath, content)
    try test(f.getAbsolutePath)
    finally f.delete()

  private def basicAuthHeader(username: String, password: String): Map[String, String] =
    val encoded = java.util.Base64.getEncoder.encodeToString(s"$username:$password".getBytes("UTF-8"))
    Map("Authorization" -> s"Basic $encoded")

  private def postSql(port: Int, sql: String, headers: Map[String, String] = Map.empty): (Int, Array[Byte]) =
    var builder = HttpRequest.newBuilder()
      .uri(URI.create(s"http://127.0.0.1:$port/sql"))
      .header("Content-Type", "application/octet-stream")
      .POST(HttpRequest.BodyPublishers.ofString(sql))
    headers.foreach((k, v) => builder = builder.header(k, v))
    val res = client.send(builder.build(), HttpResponse.BodyHandlers.ofByteArray())
    (res.statusCode(), res.body())

  private def get(port: Int, path: String, headers: Map[String, String] = Map.empty): (Int, Array[Byte]) =
    var builder = HttpRequest.newBuilder()
      .uri(URI.create(s"http://127.0.0.1:$port$path"))
      .GET()
    headers.foreach((k, v) => builder = builder.header(k, v))
    val res = client.send(builder.build(), HttpResponse.BodyHandlers.ofByteArray())
    (res.statusCode(), res.body())

  "no auth config — all requests succeed" in withServerAuth(NoAuth) { port =>
    val (status, _) = postSql(port, "SELECT 1")
    status shouldBe 200
  }

  "auth: none from config file — all requests succeed" in {
    withTempConfig("""auth = "none"""") { path =>
      val cfg = ServerConfig.fromFile(path)
      cfg.auth shouldBe NoAuth
      withServerAuth(cfg.auth) { port =>
        val (status, _) = postSql(port, "SELECT 1")
        status shouldBe 200
      }
    }
  }

  "basic auth — no Authorization header returns 401" in {
    withServerAuth(BasicAuth(Map("alice" -> aliceHash))) { port =>
      val (status, body) = postSql(port, "SELECT 1")
      status shouldBe 401
      new String(body, "UTF-8") shouldBe "Unauthorized"
    }
  }

  "basic auth — wrong password returns 401" in {
    withServerAuth(BasicAuth(Map("alice" -> aliceHash))) { port =>
      val (status, _) = postSql(port, "SELECT 1", basicAuthHeader("alice", "wrongpassword"))
      status shouldBe 401
    }
  }

  "basic auth — wrong username returns 401" in {
    withServerAuth(BasicAuth(Map("alice" -> aliceHash))) { port =>
      val (status, _) = postSql(port, "SELECT 1", basicAuthHeader("eve", "alice123"))
      status shouldBe 401
    }
  }

  "basic auth — correct credentials return 200" in {
    withServerAuth(BasicAuth(Map("alice" -> aliceHash))) { port =>
      val (status, _) = postSql(port, "SELECT 1", basicAuthHeader("alice", "alice123"))
      status shouldBe 200
    }
  }

  "basic auth — health endpoint is exempt from auth" in {
    withServerAuth(BasicAuth(Map("alice" -> aliceHash))) { port =>
      val (status, body) = get(port, "/health")
      status shouldBe 200
      new String(body, "UTF-8") shouldBe """{"status":"ok"}"""
    }
  }

  "basic auth — multiple users can each authenticate" in {
    val users = Map("alice" -> aliceHash, "bob" -> bobHash)
    withServerAuth(BasicAuth(users)) { port =>
      val (statusA, _) = postSql(port, "SELECT 1", basicAuthHeader("alice", "alice123"))
      val (statusB, _) = postSql(port, "SELECT 1", basicAuthHeader("bob", "bob456"))
      val (statusC, _) = postSql(port, "SELECT 1", basicAuthHeader("alice", "bob456"))
      statusA shouldBe 200
      statusB shouldBe 200
      statusC shouldBe 401
    }
  }

  "basic auth — config loaded from file with users" in {
    withTempConfig(
      s"""auth = "basic"
         |
         |[[users]]
         |username = "alice"
         |password = "$aliceHash"
         |
         |[[users]]
         |username = "bob"
         |password = "$bobHash"
         |""".stripMargin
    ) { path =>
      val cfg = ServerConfig.fromFile(path)
      cfg.auth match
        case BasicAuth(users) =>
          users.keySet shouldBe Set("alice", "bob")
        case _ => fail("expected BasicAuth")

      withServerAuth(cfg.auth) { port =>
        val (s1, _) = postSql(port, "SELECT 1", basicAuthHeader("alice", "alice123"))
        val (s2, _) = postSql(port, "SELECT 1", basicAuthHeader("bob", "bob456"))
        val (s3, _) = postSql(port, "SELECT 1")
        s1 shouldBe 200
        s2 shouldBe 200
        s3 shouldBe 401
      }
    }
  }

  "missing config file fails with clear error" in {
    val ex = intercept[RuntimeException] {
      ServerConfig.fromFile("/nonexistent/path/config.yaml")
    }
    ex.getMessage should include("not found")
  }

  "basic auth — empty users map rejects all credentials" in {
    withServerAuth(BasicAuth(Map.empty)) { port =>
      val (status, _) = postSql(port, "SELECT 1", basicAuthHeader("alice", "alice123"))
      status shouldBe 401
    }
  }

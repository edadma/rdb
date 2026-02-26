package io.github.edadma.petradb.server

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.github.edadma.petradb.MemoryDB
import io.github.edadma.cross_platform.{createTempFile, writeFile, deleteFile}

class ServerAuthTests extends AnyFreeSpec with Matchers:
  private val aliceHash = PasswordHash.hashPassword("alice123")
  private val bobHash   = PasswordHash.hashPassword("bob456")

  private def dispatch(auth: AuthConfig)(method: String, path: String, body: String, headers: Map[String, String] = Map.empty): (Int, Array[Byte]) =
    val db = new MemoryDB
    val sessionMgr = new SessionManager(db)
    try
      val result = RequestHandler.dispatch(
        method = method,
        path = path,
        body = body,
        authHeader = headers.get("Authorization"),
        sessionId = headers.get("X-Session-Id"),
        sessionMgr = sessionMgr,
        auth = auth,
      )
      (result.status, result.body)
    finally db.close()

  private def basicAuthHeader(username: String, password: String): Map[String, String] =
    val encoded = PlatformCrypto.base64Encode(s"$username:$password".getBytes("UTF-8"))
    Map("Authorization" -> s"Basic $encoded")

  private def postSql(auth: AuthConfig, sql: String, headers: Map[String, String] = Map.empty): (Int, Array[Byte]) =
    dispatch(auth)("POST", "/sql", sql, headers)

  private def getHealth(auth: AuthConfig, headers: Map[String, String] = Map.empty): (Int, Array[Byte]) =
    dispatch(auth)("GET", "/health", "", headers)

  "no auth config — all requests succeed" in {
    val (status, _) = postSql(NoAuth, "SELECT 1")
    status shouldBe 200
  }

  "auth: none from config file — all requests succeed" in {
    val path = createTempFile("petradb-config-", ".toml")
    writeFile(path, """auth = "none"""")
    try
      val cfg = ServerConfig.fromFile(path)
      cfg.auth shouldBe NoAuth
      val (status, _) = postSql(cfg.auth, "SELECT 1")
      status shouldBe 200
    finally deleteFile(path)
  }

  "basic auth — no Authorization header returns 401" in {
    val (status, body) = postSql(BasicAuth(Map("alice" -> aliceHash)), "SELECT 1")
    status shouldBe 401
    new String(body, "UTF-8") shouldBe "Unauthorized"
  }

  "basic auth — wrong password returns 401" in {
    val (status, _) = postSql(BasicAuth(Map("alice" -> aliceHash)), "SELECT 1", basicAuthHeader("alice", "wrongpassword"))
    status shouldBe 401
  }

  "basic auth — wrong username returns 401" in {
    val (status, _) = postSql(BasicAuth(Map("alice" -> aliceHash)), "SELECT 1", basicAuthHeader("eve", "alice123"))
    status shouldBe 401
  }

  "basic auth — correct credentials return 200" in {
    val (status, _) = postSql(BasicAuth(Map("alice" -> aliceHash)), "SELECT 1", basicAuthHeader("alice", "alice123"))
    status shouldBe 200
  }

  "basic auth — health endpoint is exempt from auth" in {
    val (status, body) = getHealth(BasicAuth(Map("alice" -> aliceHash)))
    status shouldBe 200
    new String(body, "UTF-8") shouldBe """{"status":"ok"}"""
  }

  "basic auth — multiple users can each authenticate" in {
    val users = Map("alice" -> aliceHash, "bob" -> bobHash)
    val auth = BasicAuth(users)
    val (statusA, _) = postSql(auth, "SELECT 1", basicAuthHeader("alice", "alice123"))
    val (statusB, _) = postSql(auth, "SELECT 1", basicAuthHeader("bob", "bob456"))
    val (statusC, _) = postSql(auth, "SELECT 1", basicAuthHeader("alice", "bob456"))
    statusA shouldBe 200
    statusB shouldBe 200
    statusC shouldBe 401
  }

  "basic auth — config loaded from file with users" in {
    val path = createTempFile("petradb-config-", ".toml")
    writeFile(
      path,
      s"""auth = "basic"
         |
         |[[users]]
         |username = "alice"
         |password = "$aliceHash"
         |
         |[[users]]
         |username = "bob"
         |password = "$bobHash"
         |""".stripMargin,
    )
    try
      val cfg = ServerConfig.fromFile(path)
      cfg.auth match
        case BasicAuth(users) =>
          users.keySet shouldBe Set("alice", "bob")
        case _ => fail("expected BasicAuth")

      val auth = cfg.auth
      val (s1, _) = postSql(auth, "SELECT 1", basicAuthHeader("alice", "alice123"))
      val (s2, _) = postSql(auth, "SELECT 1", basicAuthHeader("bob", "bob456"))
      val (s3, _) = postSql(auth, "SELECT 1")
      s1 shouldBe 200
      s2 shouldBe 200
      s3 shouldBe 401
    finally deleteFile(path)
  }

  "missing config file fails with clear error" in {
    val ex = intercept[RuntimeException] {
      ServerConfig.fromFile("/nonexistent/path/config.yaml")
    }
    ex.getMessage should include("not found")
  }

  "basic auth — empty users map rejects all credentials" in {
    val (status, _) = postSql(BasicAuth(Map.empty), "SELECT 1", basicAuthHeader("alice", "alice123"))
    status shouldBe 401
  }

package io.github.edadma.petradb.server

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.github.edadma.petradb.engine.MemoryDB
import io.github.edadma.cross_platform.{createTempFile, writeFile, deleteFile}

class SessionLimitTests extends AnyFreeSpec with Matchers:
  private def withSessionMgr[A](maxSessions: Int)(f: (SessionManager, MemoryDB) => A): A =
    val db = new MemoryDB
    val mgr = new SessionManager(db, maxSessions)
    try f(mgr, db)
    finally db.close()

  private def dispatch(mgr: SessionManager, method: String, path: String, body: String = "", sessionId: Option[String] = None, auth: AuthConfig = NoAuth, authHeader: Option[String] = None): (Int, Array[Byte]) =
    val result = RequestHandler.dispatch(
      method = method,
      path = path,
      body = body,
      authHeader = authHeader,
      sessionId = sessionId,
      sessionMgr = mgr,
      auth = auth,
    )
    (result.status, result.body)

  private def basicAuthHeader(username: String, password: String): Option[String] =
    val encoded = PlatformCrypto.base64Encode(s"$username:$password".getBytes("UTF-8"))
    Some(s"Basic $encoded")

  "createSession returns None when at capacity" in {
    withSessionMgr(2) { (mgr, _) =>
      mgr.createSession() shouldBe defined
      mgr.createSession() shouldBe defined
      mgr.createSession() shouldBe None
    }
  }

  "getSession with unknown ID returns None when full" in {
    withSessionMgr(1) { (mgr, _) =>
      mgr.createSession() shouldBe defined
      mgr.getSession("unknown-id") shouldBe None
    }
  }

  "getSession with existing ID works when full" in {
    withSessionMgr(1) { (mgr, _) =>
      val Some((id, _)) = mgr.createSession(): @unchecked
      mgr.isFull shouldBe true
      mgr.getSession(id) shouldBe defined
    }
  }

  "closing a session frees a slot" in {
    withSessionMgr(1) { (mgr, _) =>
      val Some((id, _)) = mgr.createSession(): @unchecked
      mgr.createSession() shouldBe None
      mgr.closeSession(id) shouldBe true
      mgr.createSession() shouldBe defined
    }
  }

  "maxSessions 0 means unlimited" in {
    withSessionMgr(0) { (mgr, _) =>
      for _ <- 1 to 100 do mgr.createSession() shouldBe defined
      mgr.isFull shouldBe false
    }
  }

  "POST /session returns 503 when at capacity" in {
    withSessionMgr(1) { (mgr, _) =>
      val (s1, _) = dispatch(mgr, "POST", "/session")
      s1 shouldBe 200
      val (s2, body) = dispatch(mgr, "POST", "/session")
      s2 shouldBe 503
      new String(body, "UTF-8") shouldBe "Too many sessions"
    }
  }

  "POST /sql with unknown session ID returns 503 when full" in {
    withSessionMgr(1) { (mgr, _) =>
      mgr.createSession() shouldBe defined
      val (status, body) = dispatch(mgr, "POST", "/sql", "SELECT 1", sessionId = Some("nonexistent"))
      status shouldBe 503
      new String(body, "UTF-8") shouldBe "Too many sessions"
    }
  }

  "POST /sql with existing session ID succeeds when full" in {
    withSessionMgr(1) { (mgr, _) =>
      val Some((id, _)) = mgr.createSession(): @unchecked
      val (status, _) = dispatch(mgr, "POST", "/sql", "SELECT 1", sessionId = Some(id))
      status shouldBe 200
    }
  }

  "POST /sql without session ID uses transient session even when full" in {
    withSessionMgr(1) { (mgr, _) =>
      mgr.createSession() shouldBe defined
      val (status, _) = dispatch(mgr, "POST", "/sql", "SELECT 1")
      status shouldBe 200
    }
  }

  "config parsing — max_sessions from TOML" in {
    val path = createTempFile("petradb-config-", ".toml")
    writeFile(path, "[sessions]\nmax_sessions = 500\n")
    try
      val cfg = ServerConfig.fromFile(path)
      cfg.maxSessions shouldBe 500
    finally deleteFile(path)
  }

  "config parsing — no sessions section defaults to 0" in {
    val path = createTempFile("petradb-config-", ".toml")
    writeFile(path, "")
    try
      val cfg = ServerConfig.fromFile(path)
      cfg.maxSessions shouldBe 0
    finally deleteFile(path)
  }

  private val testHash = PasswordHash.hashPassword("secret")
  private val testAuth = BasicAuth(Map("admin" -> testHash))
  private val validCreds = basicAuthHeader("admin", "secret")

  "basic auth — missing credentials returns 401 before 503 when full" in {
    withSessionMgr(1) { (mgr, _) =>
      mgr.createSession() shouldBe defined
      val (status, body) = dispatch(mgr, "POST", "/session", auth = testAuth)
      status shouldBe 401
      new String(body, "UTF-8") shouldBe "Unauthorized"
    }
  }

  "basic auth — valid credentials returns 503 when full" in {
    withSessionMgr(1) { (mgr, _) =>
      mgr.createSession() shouldBe defined
      val (status, body) = dispatch(mgr, "POST", "/session", auth = testAuth, authHeader = validCreds)
      status shouldBe 503
      new String(body, "UTF-8") shouldBe "Too many sessions"
    }
  }

  "basic auth — valid credentials returns 200 when not full" in {
    withSessionMgr(2) { (mgr, _) =>
      val (status, _) = dispatch(mgr, "POST", "/session", auth = testAuth, authHeader = validCreds)
      status shouldBe 200
    }
  }

  "basic auth — sql with unknown session returns 401 without credentials when full" in {
    withSessionMgr(1) { (mgr, _) =>
      mgr.createSession() shouldBe defined
      val (status, _) = dispatch(mgr, "POST", "/sql", "SELECT 1", sessionId = Some("bad-id"), auth = testAuth)
      status shouldBe 401
    }
  }

  "basic auth — sql with unknown session returns 503 with valid credentials when full" in {
    withSessionMgr(1) { (mgr, _) =>
      mgr.createSession() shouldBe defined
      val (status, body) = dispatch(mgr, "POST", "/sql", "SELECT 1", sessionId = Some("bad-id"), auth = testAuth, authHeader = validCreds)
      status shouldBe 503
      new String(body, "UTF-8") shouldBe "Too many sessions"
    }
  }

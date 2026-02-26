package io.github.edadma.petradb.server

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.github.edadma.petradb.*
import io.github.edadma.petradb.Codecs.given
import upickle.default.*

class ServerTests extends AnyFreeSpec with Matchers:
  private def withHandler(test: (SessionManager, (String, String, String, Map[String, String]) => (Int, Array[Byte])) => Unit): Unit =
    val db = new MemoryDB
    val sessionMgr = new SessionManager(db)
    def dispatch(method: String, path: String, body: String, headers: Map[String, String] = Map.empty): (Int, Array[Byte]) =
      val result = RequestHandler.dispatch(
        method = method,
        path = path,
        body = body,
        authHeader = headers.get("Authorization"),
        sessionId = headers.get("X-Session-Id"),
        sessionMgr = sessionMgr,
        auth = NoAuth,
      )
      (result.status, result.body)
    try test(sessionMgr, dispatch)
    finally db.close()

  private def get(dispatch: (String, String, String, Map[String, String]) => (Int, Array[Byte]), path: String): (Int, Array[Byte]) =
    dispatch("GET", path, "", Map.empty)

  private def postSql(
    dispatch: (String, String, String, Map[String, String]) => (Int, Array[Byte]),
    sql: String,
    headers: Map[String, String] = Map.empty,
  ): (Int, Seq[Result]) =
    val (status, body) = dispatch("POST", "/sql", sql, headers)
    val results = if status == 200 then readBinary[Seq[Result]](body) else Seq.empty
    (status, results)

  private def postSqlRaw(
    dispatch: (String, String, String, Map[String, String]) => (Int, Array[Byte]),
    sql: String,
    headers: Map[String, String] = Map.empty,
  ): (Int, Array[Byte]) =
    dispatch("POST", "/sql", sql, headers)

  private def postSession(dispatch: (String, String, String, Map[String, String]) => (Int, Array[Byte])): (Int, Map[String, String]) =
    val (status, body) = dispatch("POST", "/session", "", Map.empty)
    val map = if status == 200 then readBinary[Map[String, String]](body) else Map.empty
    (status, map)

  private def deleteSession(dispatch: (String, String, String, Map[String, String]) => (Int, Array[Byte]), sessionId: String): (Int, Array[Byte]) =
    dispatch("DELETE", s"/session/$sessionId", "", Map.empty)

  "health check" in withHandler { (_, d) =>
    val (status, body) = get(d, "/health")
    status shouldBe 200
    new String(body, "UTF-8") shouldBe """{"status":"ok"}"""
  }

  "404 for unknown endpoint" in withHandler { (_, d) =>
    val (status, body) = get(d, "/unknown")
    status shouldBe 404
    new String(body, "UTF-8") should include("Not found")
  }

  "SQL error" in withHandler { (_, d) =>
    val (status, _) = postSqlRaw(d, "SELECT * FROM nonexistent")
    status shouldBe 400
  }

  "create table and select" in withHandler { (_, d) =>
    val (s1, r1) = postSql(d, "CREATE TABLE t (id INT, name TEXT)")
    s1 shouldBe 200
    r1 shouldBe Seq(CreateTableResult("t"))

    postSql(d, "INSERT INTO t (id, name) VALUES (1, 'Alice')")

    val (s2, r2) = postSql(d, "SELECT * FROM t")
    s2 shouldBe 200
    r2.head match
      case QueryResult(table) =>
        table.data.length shouldBe 1
        table.data(0).getString("name") shouldBe "Alice"
        table.data(0).getInt("id") shouldBe 1
      case other => fail(s"unexpected: $other")
  }

  "multi-statement" in withHandler { (_, d) =>
    val (status, results) = postSql(d, "CREATE TABLE t (id INT); INSERT INTO t VALUES (1); SELECT * FROM t")
    status shouldBe 200
    results.length shouldBe 3
    results(0) shouldBe CreateTableResult("t")
    results(1) match { case InsertResult(_, _) => case other => fail(s"unexpected: $other") }
    results(2) match { case QueryResult(_) => case other => fail(s"unexpected: $other") }
  }

  "session management" in withHandler { (_, d) =>
    val (s1, b1) = postSession(d)
    s1 shouldBe 200
    val sessionId = b1("sessionId")

    postSql(d, "CREATE TABLE t (id INT)", Map("X-Session-Id" -> sessionId))
    val (s2, _) = postSql(d, "SELECT * FROM t", Map("X-Session-Id" -> sessionId))
    s2 shouldBe 200

    val (s3, _) = deleteSession(d, sessionId)
    s3 shouldBe 200

    val (s4, _) = deleteSession(d, sessionId)
    s4 shouldBe 404
  }

  "data types round-trip" in withHandler { (_, d) =>
    postSql(d, "CREATE TABLE t (i INT, s TEXT, b BOOLEAN)")
    postSql(d, "INSERT INTO t (i, s, b) VALUES (42, 'hello', true)")
    postSql(d, "INSERT INTO t (i, s, b) VALUES (NULL, NULL, false)")

    val (status, results) = postSql(d, "SELECT * FROM t ORDER BY i")
    status shouldBe 200
    val table = results.head.asInstanceOf[QueryResult].table
    table.data.length shouldBe 2

    table.data(0)("i") shouldBe NullValue()
    table.data(0)("s") shouldBe NullValue()
    table.data(0)("b") shouldBe BooleanValue(false)

    table.data(1).getInt("i") shouldBe 42
    table.data(1).getString("s") shouldBe "hello"
    table.data(1).getBoolean("b") shouldBe true
  }

  "transaction rollback" in withHandler { (_, d) =>
    val (_, b) = postSession(d)
    val sessionId = b("sessionId")
    val hdrs = Map("X-Session-Id" -> sessionId)

    postSql(d, "CREATE TABLE t (id INT)", hdrs)

    postSql(d, "BEGIN", hdrs)
    postSql(d, "INSERT INTO t VALUES (1)", hdrs)
    postSql(d, "ROLLBACK", hdrs)

    val (_, r1) = postSql(d, "SELECT * FROM t", hdrs)
    r1.head.asInstanceOf[QueryResult].table.data.length shouldBe 0

    postSql(d, "BEGIN", hdrs)
    postSql(d, "INSERT INTO t VALUES (2)", hdrs)
    postSql(d, "COMMIT", hdrs)

    val (_, r2) = postSql(d, "SELECT * FROM t", hdrs)
    r2.head.asInstanceOf[QueryResult].table.data.length shouldBe 1
    r2.head.asInstanceOf[QueryResult].table.data(0).getInt("id") shouldBe 2
  }

  "session isolation — transaction rollback visibility" in withHandler { (_, d) =>
    val (_, bA) = postSession(d)
    val hdrsA = Map("X-Session-Id" -> bA("sessionId"))
    val (_, bB) = postSession(d)
    val hdrsB = Map("X-Session-Id" -> bB("sessionId"))

    postSql(d, "CREATE TABLE t (id INT, name TEXT)", hdrsA)

    postSql(d, "BEGIN", hdrsA)
    postSql(d, "INSERT INTO t VALUES (1, 'tentative')", hdrsA)
    postSql(d, "ROLLBACK", hdrsA)

    val (_, rAfterRollback) = postSql(d, "SELECT * FROM t", hdrsB)
    rAfterRollback.head.asInstanceOf[QueryResult].table.data.length shouldBe 0

    postSql(d, "BEGIN", hdrsA)
    postSql(d, "INSERT INTO t VALUES (1, 'committed')", hdrsA)
    postSql(d, "COMMIT", hdrsA)

    val (_, rAfterCommit) = postSql(d, "SELECT * FROM t", hdrsB)
    val rows = rAfterCommit.head.asInstanceOf[QueryResult].table.data
    rows.length shouldBe 1
    rows(0).getString("name") shouldBe "committed"
  }

  "session isolation — independent transactions" in withHandler { (_, d) =>
    val (_, bA) = postSession(d)
    val hdrsA = Map("X-Session-Id" -> bA("sessionId"))
    val (_, bB) = postSession(d)
    val hdrsB = Map("X-Session-Id" -> bB("sessionId"))

    postSql(d, "CREATE TABLE t (id INT)", hdrsA)

    postSql(d, "BEGIN", hdrsA)
    postSql(d, "INSERT INTO t VALUES (1)", hdrsA)

    postSql(d, "BEGIN", hdrsB)
    postSql(d, "INSERT INTO t VALUES (2)", hdrsB)

    postSql(d, "COMMIT", hdrsB)
    postSql(d, "ROLLBACK", hdrsA)

    val (_, r) = postSql(d, "SELECT * FROM t ORDER BY id", hdrsA)
    val rows = r.head.asInstanceOf[QueryResult].table.data
    rows.length shouldBe 1
    rows(0).getInt("id") shouldBe 2
  }

  "auto-created sessions via X-Session-Id" in withHandler { (_, d) =>
    val hdrs = Map("X-Session-Id" -> "my-custom-id")

    val (s1, _) = postSql(d, "CREATE TABLE t (id INT)", hdrs)
    s1 shouldBe 200

    val (s2, _) = postSql(d, "INSERT INTO t VALUES (1)", hdrs)
    s2 shouldBe 200

    val (s3, r3) = postSql(d, "SELECT * FROM t", hdrs)
    s3 shouldBe 200
    r3.head.asInstanceOf[QueryResult].table.data.length shouldBe 1
  }

  "stateless requests — prepared statement isolation" in withHandler { (_, d) =>
    val (s1, _) = postSql(d, "PREPARE p AS SELECT 1")
    s1 shouldBe 200

    val (s2, _) = postSqlRaw(d, "EXECUTE p")
    s2 shouldBe 400
  }

  "enum value round-trip" in withHandler { (_, d) =>
    val (_, b) = postSession(d)
    val hdrs = Map("X-Session-Id" -> b("sessionId"))

    postSql(d, "CREATE TYPE color AS ENUM ('red', 'green', 'blue')", hdrs)
    postSql(d, "CREATE TABLE t (id INT, c color)", hdrs)
    postSql(d, "INSERT INTO t VALUES (1, 'green')", hdrs)

    val (status, results) = postSql(d, "SELECT * FROM t", hdrs)
    status shouldBe 200
    val row = results.head.asInstanceOf[QueryResult].table.data(0)
    row("c").string shouldBe "green"
  }

  "DELETE /session/ with empty ID" in withHandler { (_, d) =>
    val (status, _) = deleteSession(d, "")
    status shouldBe 404
  }

  "timestamp and date round-trip" in withHandler { (_, d) =>
    val (_, b) = postSession(d)
    val hdrs = Map("X-Session-Id" -> b("sessionId"))

    postSql(d, "CREATE TABLE t (id INT, d DATE, ts TIMESTAMP)", hdrs)
    postSql(d, "INSERT INTO t VALUES (1, '2024-06-15', '2024-06-15 14:30:00')", hdrs)

    val (status, results) = postSql(d, "SELECT * FROM t", hdrs)
    status shouldBe 200
    val row = results.head.asInstanceOf[QueryResult].table.data(0)
    row("d").string should include("2024-06-15")
    row("ts").string should include("2024-06-15")
    row("ts").string should include("14:30")
  }

  "UUID round-trip" in withHandler { (_, d) =>
    val (_, b) = postSession(d)
    val hdrs = Map("X-Session-Id" -> b("sessionId"))

    postSql(d, "CREATE TABLE t (id UUID DEFAULT gen_random_uuid(), name TEXT)", hdrs)
    postSql(d, "INSERT INTO t (name) VALUES ('test')", hdrs)

    val (status, results) = postSql(d, "SELECT * FROM t", hdrs)
    status shouldBe 200
    val row = results.head.asInstanceOf[QueryResult].table.data(0)
    row("id") shouldBe a[UUIDValue]
    row("id").string should fullyMatch regex "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
  }

  "JSON/JSONB round-trip" in withHandler { (_, d) =>
    val (_, b) = postSession(d)
    val hdrs = Map("X-Session-Id" -> b("sessionId"))

    postSql(d, "CREATE TABLE t (id INT, data JSONB)", hdrs)
    postSql(d, """INSERT INTO t VALUES (1, '{"name": "Alice", "scores": [10, 20]}')""", hdrs)

    val (status, results) = postSql(d, "SELECT * FROM t", hdrs)
    status shouldBe 200
    val row = results.head.asInstanceOf[QueryResult].table.data(0)
    row("data") shouldBe a[ObjectValue]
    val obj = row("data").asInstanceOf[ObjectValue]
    obj.get("name") shouldBe Some(TextValue("Alice"))
    obj.get("scores").get.asInstanceOf[ArrayValue].data.length shouldBe 2
  }

  "response is always Seq[Result]" in withHandler { (_, d) =>
    val (s1, r1) = postSql(d, "SELECT 1")
    s1 shouldBe 200
    r1.length shouldBe 1
    r1.head shouldBe a[QueryResult]

    val (_, b) = postSession(d)
    val hdrs = Map("X-Session-Id" -> b("sessionId"))
    val (s2, r2) = postSql(d, "BEGIN", hdrs)
    s2 shouldBe 200
    r2.head shouldBe BeginResult
    postSql(d, "ROLLBACK", hdrs)
  }

  "UPDATE returns UpdateResult" in withHandler { (_, d) =>
    postSql(d, "CREATE TABLE t (id INT, name TEXT)")
    postSql(d, "INSERT INTO t VALUES (1, 'a'), (2, 'b'), (3, 'c')")

    val (status, results) = postSql(d, "UPDATE t SET name = 'x' WHERE id < 3")
    status shouldBe 200
    results.head shouldBe UpdateResult(2)
  }

  "DELETE returns DeleteResult" in withHandler { (_, d) =>
    postSql(d, "CREATE TABLE t (id INT)")
    postSql(d, "INSERT INTO t VALUES (1), (2), (3)")

    val (status, results) = postSql(d, "DELETE FROM t WHERE id = 1")
    status shouldBe 200
    results.head shouldBe DeleteResult(1)
  }

  "INSERT returns InsertResult" in withHandler { (_, d) =>
    postSql(d, "CREATE TABLE t (id SERIAL, name TEXT)")

    val (status, results) = postSql(d, "INSERT INTO t (name) VALUES ('Alice')")
    status shouldBe 200
    results.head match
      case InsertResult(obj, table) =>
        obj should contain key "id"
        table.data.length shouldBe 1
      case other => fail(s"unexpected: $other")
  }

  "TRUNCATE returns TruncateResult" in withHandler { (_, d) =>
    postSql(d, "CREATE TABLE t (id INT)")
    postSql(d, "INSERT INTO t VALUES (1)")

    val (status, results) = postSql(d, "TRUNCATE TABLE t")
    status shouldBe 200
    results.head shouldBe TruncateResult("t")
  }

  "CREATE/DROP TYPE returns correct Result" in withHandler { (_, d) =>
    val (_, b) = postSession(d)
    val hdrs = Map("X-Session-Id" -> b("sessionId"))

    val (s1, r1) = postSql(d, "CREATE TYPE mood AS ENUM ('happy', 'sad')", hdrs)
    s1 shouldBe 200
    r1.head shouldBe CreateTypeResult("mood")

    val (s2, r2) = postSql(d, "DROP TYPE mood", hdrs)
    s2 shouldBe 200
    r2.head shouldBe DropTypeResult("mood")
  }

  "CREATE/DROP INDEX returns correct Result" in withHandler { (_, d) =>
    val (_, b) = postSession(d)
    val hdrs = Map("X-Session-Id" -> b("sessionId"))

    postSql(d, "CREATE TABLE t (id INT, name TEXT)", hdrs)
    val (s1, r1) = postSql(d, "CREATE INDEX idx_name ON t (name)", hdrs)
    s1 shouldBe 200
    r1.head shouldBe CreateIndexResult("idx_name")

    val (s2, r2) = postSql(d, "DROP INDEX idx_name", hdrs)
    s2 shouldBe 200
    r2.head shouldBe DropIndexResult("idx_name")
  }

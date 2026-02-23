package io.github.edadma.petradb.server

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.github.edadma.petradb.*
import io.github.edadma.petradb.Codecs.given
import io.github.edadma.microserve.EventLoop
import upickle.default.*

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

class ServerTests extends AnyFreeSpec with Matchers:
  private val client = HttpClient.newHttpClient()

  private def withServer(test: Int => Unit): Unit =
    val loop   = new EventLoop
    val db     = new MemoryDB
    val server = new PetraServer(loop, db, port = 0)
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

  private def get(port: Int, path: String): (Int, Array[Byte]) =
    val req = HttpRequest.newBuilder()
      .uri(URI.create(s"http://127.0.0.1:$port$path"))
      .GET()
      .build()
    val res = client.send(req, HttpResponse.BodyHandlers.ofByteArray())
    (res.statusCode(), res.body())

  private def postSql(port: Int, sql: String, headers: Map[String, String] = Map.empty): (Int, Seq[Result]) =
    var builder = HttpRequest.newBuilder()
      .uri(URI.create(s"http://127.0.0.1:$port/sql"))
      .header("Content-Type", "application/octet-stream")
      .POST(HttpRequest.BodyPublishers.ofString(sql))
    headers.foreach((k, v) => builder = builder.header(k, v))
    val res = client.send(builder.build(), HttpResponse.BodyHandlers.ofByteArray())
    val results =
      if res.statusCode() == 200 then readBinary[Seq[Result]](res.body())
      else Seq.empty
    (res.statusCode(), results)

  private def postSqlRaw(port: Int, sql: String, headers: Map[String, String] = Map.empty): (Int, Array[Byte]) =
    var builder = HttpRequest.newBuilder()
      .uri(URI.create(s"http://127.0.0.1:$port/sql"))
      .header("Content-Type", "application/octet-stream")
      .POST(HttpRequest.BodyPublishers.ofString(sql))
    headers.foreach((k, v) => builder = builder.header(k, v))
    val res = client.send(builder.build(), HttpResponse.BodyHandlers.ofByteArray())
    (res.statusCode(), res.body())

  private def postSession(port: Int): (Int, Map[String, String]) =
    val req = HttpRequest.newBuilder()
      .uri(URI.create(s"http://127.0.0.1:$port/session"))
      .POST(HttpRequest.BodyPublishers.noBody())
      .build()
    val res = client.send(req, HttpResponse.BodyHandlers.ofByteArray())
    val body = if res.statusCode() == 200 then readBinary[Map[String, String]](res.body()) else Map.empty
    (res.statusCode(), body)

  private def deleteSession(port: Int, sessionId: String): (Int, Array[Byte]) =
    val req = HttpRequest.newBuilder()
      .uri(URI.create(s"http://127.0.0.1:$port/session/$sessionId"))
      .DELETE()
      .build()
    val res = client.send(req, HttpResponse.BodyHandlers.ofByteArray())
    (res.statusCode(), res.body())

  "health check" in withServer { port =>
    val (status, body) = get(port, "/health")
    status shouldBe 200
    new String(body, "UTF-8") shouldBe """{"status":"ok"}"""
  }

  "404 for unknown endpoint" in withServer { port =>
    val (status, body) = get(port, "/unknown")
    status shouldBe 404
    new String(body, "UTF-8") should include("Not found")
  }

  "SQL error" in withServer { port =>
    val (status, _) = postSqlRaw(port, "SELECT * FROM nonexistent")
    status shouldBe 400
  }

  "create table and select" in withServer { port =>
    val (s1, r1) = postSql(port, "CREATE TABLE t (id INT, name TEXT)")
    s1 shouldBe 200
    r1 shouldBe Seq(CreateTableResult("t"))

    postSql(port, "INSERT INTO t (id, name) VALUES (1, 'Alice')")

    val (s2, r2) = postSql(port, "SELECT * FROM t")
    s2 shouldBe 200
    r2.head match
      case QueryResult(table) =>
        table.data.length shouldBe 1
        table.data(0).getString("name") shouldBe "Alice"
        table.data(0).getInt("id") shouldBe 1
      case other => fail(s"unexpected: $other")
  }

  "multi-statement" in withServer { port =>
    val (status, results) = postSql(port, "CREATE TABLE t (id INT); INSERT INTO t VALUES (1); SELECT * FROM t")
    status shouldBe 200
    results.length shouldBe 3
    results(0) shouldBe CreateTableResult("t")
    results(1) match { case InsertResult(_, _) => case other => fail(s"unexpected: $other") }
    results(2) match { case QueryResult(_) => case other => fail(s"unexpected: $other") }
  }

  "session management" in withServer { port =>
    val (s1, b1) = postSession(port)
    s1 shouldBe 200
    val sessionId = b1("sessionId")

    postSql(port, "CREATE TABLE t (id INT)", Map("X-Session-Id" -> sessionId))
    val (s2, _) = postSql(port, "SELECT * FROM t", Map("X-Session-Id" -> sessionId))
    s2 shouldBe 200

    val (s3, _) = deleteSession(port, sessionId)
    s3 shouldBe 200

    val (s4, _) = deleteSession(port, sessionId)
    s4 shouldBe 404
  }

  "data types round-trip" in withServer { port =>
    postSql(port, "CREATE TABLE t (i INT, s TEXT, b BOOLEAN)")
    postSql(port, "INSERT INTO t (i, s, b) VALUES (42, 'hello', true)")
    postSql(port, "INSERT INTO t (i, s, b) VALUES (NULL, NULL, false)")

    val (status, results) = postSql(port, "SELECT * FROM t ORDER BY i")
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

  "transaction rollback" in withServer { port =>
    val (_, b) = postSession(port)
    val sessionId = b("sessionId")
    val hdrs = Map("X-Session-Id" -> sessionId)

    postSql(port, "CREATE TABLE t (id INT)", hdrs)

    postSql(port, "BEGIN", hdrs)
    postSql(port, "INSERT INTO t VALUES (1)", hdrs)
    postSql(port, "ROLLBACK", hdrs)

    val (_, r1) = postSql(port, "SELECT * FROM t", hdrs)
    r1.head.asInstanceOf[QueryResult].table.data.length shouldBe 0

    postSql(port, "BEGIN", hdrs)
    postSql(port, "INSERT INTO t VALUES (2)", hdrs)
    postSql(port, "COMMIT", hdrs)

    val (_, r2) = postSql(port, "SELECT * FROM t", hdrs)
    r2.head.asInstanceOf[QueryResult].table.data.length shouldBe 1
    r2.head.asInstanceOf[QueryResult].table.data(0).getInt("id") shouldBe 2
  }

  "session isolation — transaction rollback visibility" in withServer { port =>
    val (_, bA) = postSession(port)
    val hdrsA = Map("X-Session-Id" -> bA("sessionId"))
    val (_, bB) = postSession(port)
    val hdrsB = Map("X-Session-Id" -> bB("sessionId"))

    postSql(port, "CREATE TABLE t (id INT, name TEXT)", hdrsA)

    postSql(port, "BEGIN", hdrsA)
    postSql(port, "INSERT INTO t VALUES (1, 'tentative')", hdrsA)
    postSql(port, "ROLLBACK", hdrsA)

    val (_, rAfterRollback) = postSql(port, "SELECT * FROM t", hdrsB)
    rAfterRollback.head.asInstanceOf[QueryResult].table.data.length shouldBe 0

    postSql(port, "BEGIN", hdrsA)
    postSql(port, "INSERT INTO t VALUES (1, 'committed')", hdrsA)
    postSql(port, "COMMIT", hdrsA)

    val (_, rAfterCommit) = postSql(port, "SELECT * FROM t", hdrsB)
    val rows = rAfterCommit.head.asInstanceOf[QueryResult].table.data
    rows.length shouldBe 1
    rows(0).getString("name") shouldBe "committed"
  }

  "session isolation — independent transactions" in withServer { port =>
    val (_, bA) = postSession(port)
    val hdrsA = Map("X-Session-Id" -> bA("sessionId"))
    val (_, bB) = postSession(port)
    val hdrsB = Map("X-Session-Id" -> bB("sessionId"))

    postSql(port, "CREATE TABLE t (id INT)", hdrsA)

    postSql(port, "BEGIN", hdrsA)
    postSql(port, "INSERT INTO t VALUES (1)", hdrsA)

    postSql(port, "BEGIN", hdrsB)
    postSql(port, "INSERT INTO t VALUES (2)", hdrsB)

    postSql(port, "COMMIT", hdrsB)
    postSql(port, "ROLLBACK", hdrsA)

    val (_, r) = postSql(port, "SELECT * FROM t ORDER BY id", hdrsA)
    val rows = r.head.asInstanceOf[QueryResult].table.data
    rows.length shouldBe 1
    rows(0).getInt("id") shouldBe 2
  }

  "auto-created sessions via X-Session-Id" in withServer { port =>
    val hdrs = Map("X-Session-Id" -> "my-custom-id")

    val (s1, _) = postSql(port, "CREATE TABLE t (id INT)", hdrs)
    s1 shouldBe 200

    val (s2, _) = postSql(port, "INSERT INTO t VALUES (1)", hdrs)
    s2 shouldBe 200

    val (s3, r3) = postSql(port, "SELECT * FROM t", hdrs)
    s3 shouldBe 200
    r3.head.asInstanceOf[QueryResult].table.data.length shouldBe 1
  }

  "stateless requests — prepared statement isolation" in withServer { port =>
    val (s1, _) = postSql(port, "PREPARE p AS SELECT 1")
    s1 shouldBe 200

    val (s2, _) = postSqlRaw(port, "EXECUTE p")
    s2 shouldBe 400
  }

  "enum value round-trip" in withServer { port =>
    val (_, b) = postSession(port)
    val hdrs = Map("X-Session-Id" -> b("sessionId"))

    postSql(port, "CREATE TYPE color AS ENUM ('red', 'green', 'blue')", hdrs)
    postSql(port, "CREATE TABLE t (id INT, c color)", hdrs)
    postSql(port, "INSERT INTO t VALUES (1, 'green')", hdrs)

    val (status, results) = postSql(port, "SELECT * FROM t", hdrs)
    status shouldBe 200
    val row = results.head.asInstanceOf[QueryResult].table.data(0)
    row("c").string shouldBe "green"
  }

  "DELETE /session/ with empty ID" in withServer { port =>
    val (status, _) = deleteSession(port, "")
    status shouldBe 404
  }

  "timestamp and date round-trip" in withServer { port =>
    val (_, b) = postSession(port)
    val hdrs = Map("X-Session-Id" -> b("sessionId"))

    postSql(port, "CREATE TABLE t (id INT, d DATE, ts TIMESTAMP)", hdrs)
    postSql(port, "INSERT INTO t VALUES (1, '2024-06-15', '2024-06-15 14:30:00')", hdrs)

    val (status, results) = postSql(port, "SELECT * FROM t", hdrs)
    status shouldBe 200
    val row = results.head.asInstanceOf[QueryResult].table.data(0)
    row("d").string should include("2024-06-15")
    row("ts").string should include("2024-06-15")
    row("ts").string should include("14:30")
  }

  "UUID round-trip" in withServer { port =>
    val (_, b) = postSession(port)
    val hdrs = Map("X-Session-Id" -> b("sessionId"))

    postSql(port, "CREATE TABLE t (id UUID DEFAULT gen_random_uuid(), name TEXT)", hdrs)
    postSql(port, "INSERT INTO t (name) VALUES ('test')", hdrs)

    val (status, results) = postSql(port, "SELECT * FROM t", hdrs)
    status shouldBe 200
    val row = results.head.asInstanceOf[QueryResult].table.data(0)
    row("id") shouldBe a[UUIDValue]
    row("id").string should fullyMatch regex "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
  }

  "JSON/JSONB round-trip" in withServer { port =>
    val (_, b) = postSession(port)
    val hdrs = Map("X-Session-Id" -> b("sessionId"))

    postSql(port, "CREATE TABLE t (id INT, data JSONB)", hdrs)
    postSql(port, """INSERT INTO t VALUES (1, '{"name": "Alice", "scores": [10, 20]}')""", hdrs)

    val (status, results) = postSql(port, "SELECT * FROM t", hdrs)
    status shouldBe 200
    val row = results.head.asInstanceOf[QueryResult].table.data(0)
    row("data") shouldBe a[ObjectValue]
    val obj = row("data").asInstanceOf[ObjectValue]
    obj.get("name") shouldBe Some(TextValue("Alice"))
    obj.get("scores").get.asInstanceOf[ArrayValue].data.length shouldBe 2
  }

  "response is always Seq[Result]" in withServer { port =>
    val (s1, r1) = postSql(port, "SELECT 1")
    s1 shouldBe 200
    r1.length shouldBe 1
    r1.head shouldBe a[QueryResult]

    val (_, b) = postSession(port)
    val hdrs = Map("X-Session-Id" -> b("sessionId"))
    val (s2, r2) = postSql(port, "BEGIN", hdrs)
    s2 shouldBe 200
    r2.head shouldBe BeginResult
    postSql(port, "ROLLBACK", hdrs)
  }

  "UPDATE returns UpdateResult" in withServer { port =>
    postSql(port, "CREATE TABLE t (id INT, name TEXT)")
    postSql(port, "INSERT INTO t VALUES (1, 'a'), (2, 'b'), (3, 'c')")

    val (status, results) = postSql(port, "UPDATE t SET name = 'x' WHERE id < 3")
    status shouldBe 200
    results.head shouldBe UpdateResult(2)
  }

  "DELETE returns DeleteResult" in withServer { port =>
    postSql(port, "CREATE TABLE t (id INT)")
    postSql(port, "INSERT INTO t VALUES (1), (2), (3)")

    val (status, results) = postSql(port, "DELETE FROM t WHERE id = 1")
    status shouldBe 200
    results.head shouldBe DeleteResult(1)
  }

  "INSERT returns InsertResult" in withServer { port =>
    postSql(port, "CREATE TABLE t (id SERIAL, name TEXT)")

    val (status, results) = postSql(port, "INSERT INTO t (name) VALUES ('Alice')")
    status shouldBe 200
    results.head match
      case InsertResult(obj, table) =>
        obj should contain key "id"
        table.data.length shouldBe 1
      case other => fail(s"unexpected: $other")
  }

  "TRUNCATE returns TruncateResult" in withServer { port =>
    postSql(port, "CREATE TABLE t (id INT)")
    postSql(port, "INSERT INTO t VALUES (1)")

    val (status, results) = postSql(port, "TRUNCATE TABLE t")
    status shouldBe 200
    results.head shouldBe TruncateResult("t")
  }

  "CREATE/DROP TYPE returns correct Result" in withServer { port =>
    val (_, b) = postSession(port)
    val hdrs = Map("X-Session-Id" -> b("sessionId"))

    val (s1, r1) = postSql(port, "CREATE TYPE mood AS ENUM ('happy', 'sad')", hdrs)
    s1 shouldBe 200
    r1.head shouldBe CreateTypeResult("mood")

    val (s2, r2) = postSql(port, "DROP TYPE mood", hdrs)
    s2 shouldBe 200
    r2.head shouldBe DropTypeResult("mood")
  }

  "CREATE/DROP INDEX returns correct Result" in withServer { port =>
    val (_, b) = postSession(port)
    val hdrs = Map("X-Session-Id" -> b("sessionId"))

    postSql(port, "CREATE TABLE t (id INT, name TEXT)", hdrs)
    val (s1, r1) = postSql(port, "CREATE INDEX idx_name ON t (name)", hdrs)
    s1 shouldBe 200
    r1.head shouldBe CreateIndexResult("idx_name")

    val (s2, r2) = postSql(port, "DROP INDEX idx_name", hdrs)
    s2 shouldBe 200
    r2.head shouldBe DropIndexResult("idx_name")
  }

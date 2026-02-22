package io.github.edadma.petradb.server

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.github.edadma.petradb.MemoryDB
import io.github.edadma.microserve.EventLoop
import zio.json.*
import zio.json.ast.Json

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

class ServerTests extends AnyFreeSpec with Matchers:
  private val client = HttpClient.newHttpClient()

  private def withServer(test: Int => Unit): Unit =
    val loop = new EventLoop
    val db = new MemoryDB
    val server = new PetraServer(loop, db, port = 0)
    server.start()
    val port = server.actualPort
    val thread = new Thread(() => loop.run())
    thread.setDaemon(true)
    thread.start()
    Thread.sleep(100)
    try test(port)
    finally
      server.stop(() => loop.stop())
      thread.join(3000)

  private def get(port: Int, path: String): (Int, String) =
    val req = HttpRequest.newBuilder()
      .uri(URI.create(s"http://127.0.0.1:$port$path"))
      .GET()
      .build()
    val res = client.send(req, HttpResponse.BodyHandlers.ofString())
    (res.statusCode(), res.body())

  private def post(port: Int, path: String, body: String, headers: Map[String, String] = Map.empty): (Int, String) =
    var builder = HttpRequest.newBuilder()
      .uri(URI.create(s"http://127.0.0.1:$port$path"))
      .header("Content-Type", "application/json")
      .POST(HttpRequest.BodyPublishers.ofString(body))
    headers.foreach((k, v) => builder = builder.header(k, v))
    val req = builder.build()
    val res = client.send(req, HttpResponse.BodyHandlers.ofString())
    (res.statusCode(), res.body())

  private def delete(port: Int, path: String): (Int, String) =
    val req = HttpRequest.newBuilder()
      .uri(URI.create(s"http://127.0.0.1:$port$path"))
      .DELETE()
      .build()
    val res = client.send(req, HttpResponse.BodyHandlers.ofString())
    (res.statusCode(), res.body())

  private def parseJson(s: String): Json = s.fromJson[Json].toOption.get

  private def jsonArr(json: Json): IndexedSeq[Json] = json match
    case Json.Arr(elements) => elements.toIndexedSeq
    case other              => fail(s"Expected JSON array, got: $other")

  private def jsonObj(json: Json): Map[String, Json] = json match
    case Json.Obj(fields) => fields.toMap
    case other            => fail(s"Expected JSON object, got: $other")

  "health check" in withServer { port =>
    val (status, body) = get(port, "/health")
    status shouldBe 200
    body shouldBe """{"status":"ok"}"""
  }

  "404 for unknown endpoint" in withServer { port =>
    val (status, body) = get(port, "/unknown")
    status shouldBe 404
    val obj = jsonObj(parseJson(body))
    obj("error") shouldBe Json.Str("Not found")
  }

  "invalid JSON" in withServer { port =>
    val (status, body) = post(port, "/sql", "not json")
    status shouldBe 400
    val obj = jsonObj(parseJson(body))
    obj("error").asInstanceOf[Json.Str].value should startWith("Invalid JSON:")
  }

  "SQL error" in withServer { port =>
    val (status, body) = post(port, "/sql", """{"sql":"SELECT * FROM nonexistent"}""")
    status shouldBe 400
    val obj = jsonObj(parseJson(body))
    obj("error") shouldBe Json.Str("SQL error")
    obj should contain key "detail"
  }

  "create table and select" in withServer { port =>
    val (s1, b1) = post(port, "/sql", """{"sql":"CREATE TABLE t (id INT, name TEXT)"}""")
    s1 shouldBe 200
    val r1 = jsonArr(parseJson(b1))
    r1.length shouldBe 1
    jsonObj(r1.head)("command") shouldBe Json.Str("create table")
    jsonObj(r1.head)("table") shouldBe Json.Str("t")

    val (s2, _) = post(port, "/sql", """{"sql":"INSERT INTO t (id, name) VALUES (1, 'Alice')"}""")
    s2 shouldBe 200

    val (s3, b3) = post(port, "/sql", """{"sql":"SELECT * FROM t"}""")
    s3 shouldBe 200
    val r3 = jsonArr(parseJson(b3))
    r3.length shouldBe 1
    val selectResult = jsonObj(r3.head)
    selectResult("command") shouldBe Json.Str("select")

    val rows = jsonArr(selectResult("rows"))
    rows.length shouldBe 1
    val row = jsonObj(rows.head)
    row("id") shouldBe Json.Num(new java.math.BigDecimal(1))
    row("name") shouldBe Json.Str("Alice")

    val fields = jsonArr(selectResult("fields"))
    fields.length shouldBe 2
  }

  "array row mode" in withServer { port =>
    post(port, "/sql", """{"sql":"CREATE TABLE t (id INT, name TEXT)"}""")
    post(port, "/sql", """{"sql":"INSERT INTO t (id, name) VALUES (1, 'Alice')"}""")

    val (status, body) = post(port, "/sql", """{"sql":"SELECT * FROM t","rowMode":"array"}""")
    status shouldBe 200
    val results = jsonArr(parseJson(body))
    val selectResult = jsonObj(results.head)
    selectResult("command") shouldBe Json.Str("select")

    val rows = jsonArr(selectResult("rows"))
    rows.length shouldBe 1
    val row = jsonArr(rows.head)
    row(0) shouldBe Json.Num(new java.math.BigDecimal(1))
    row(1) shouldBe Json.Str("Alice")
  }

  "multi-statement" in withServer { port =>
    val sql = "CREATE TABLE t (id INT); INSERT INTO t VALUES (1); SELECT * FROM t"
    val (status, body) = post(port, "/sql", s"""{"sql":"$sql"}""")
    status shouldBe 200
    val results = jsonArr(parseJson(body))
    results.length shouldBe 3
    jsonObj(results(0))("command") shouldBe Json.Str("create table")
    jsonObj(results(1))("command") shouldBe Json.Str("insert")
    jsonObj(results(2))("command") shouldBe Json.Str("select")
  }

  "session management" in withServer { port =>
    val (s1, b1) = post(port, "/session", "")
    s1 shouldBe 200
    val sessionId = jsonObj(parseJson(b1))("sessionId").asInstanceOf[Json.Str].value

    post(port, "/sql", """{"sql":"CREATE TABLE t (id INT)"}""", Map("X-Session-Id" -> sessionId))
    val (s2, b2) = post(port, "/sql", """{"sql":"SELECT * FROM t"}""", Map("X-Session-Id" -> sessionId))
    s2 shouldBe 200

    val (s3, b3) = delete(port, s"/session/$sessionId")
    s3 shouldBe 200
    jsonObj(parseJson(b3))("ok") shouldBe Json.Bool(true)

    val (s4, _) = delete(port, s"/session/$sessionId")
    s4 shouldBe 404
  }

  "data types in JSON" in withServer { port =>
    post(port, "/sql", """{"sql":"CREATE TABLE t (i INT, s TEXT, b BOOLEAN)"}""")
    post(port, "/sql", """{"sql":"INSERT INTO t (i, s, b) VALUES (42, 'hello', true)"}""")
    post(port, "/sql", """{"sql":"INSERT INTO t (i, s, b) VALUES (NULL, NULL, false)"}""")

    val (status, body) = post(port, "/sql", """{"sql":"SELECT * FROM t ORDER BY i"}""")
    status shouldBe 200
    val results = jsonArr(parseJson(body))
    val rows = jsonArr(jsonObj(results.head)("rows"))
    rows.length shouldBe 2

    val row1 = jsonObj(rows.head)
    row1("i") shouldBe Json.Null
    row1("s") shouldBe Json.Null
    row1("b") shouldBe Json.Bool(false)

    val row2 = jsonObj(rows(1))
    row2("i") shouldBe Json.Num(new java.math.BigDecimal(42))
    row2("s") shouldBe Json.Str("hello")
    row2("b") shouldBe Json.Bool(true)
  }

  "transaction rollback" in withServer { port =>
    val (_, sb) = post(port, "/session", "")
    val sessionId = jsonObj(parseJson(sb))("sessionId").asInstanceOf[Json.Str].value
    val hdrs = Map("X-Session-Id" -> sessionId)

    post(port, "/sql", """{"sql":"CREATE TABLE t (id INT)"}""", hdrs)

    // Begin, insert, then rollback — data should disappear
    post(port, "/sql", """{"sql":"BEGIN"}""", hdrs)
    post(port, "/sql", """{"sql":"INSERT INTO t VALUES (1)"}""", hdrs)
    post(port, "/sql", """{"sql":"ROLLBACK"}""", hdrs)

    val (_, bAfter) = post(port, "/sql", """{"sql":"SELECT * FROM t"}""", hdrs)
    val rowsAfter = jsonArr(jsonObj(jsonArr(parseJson(bAfter)).head)("rows"))
    rowsAfter.length shouldBe 0

    // Begin, insert, then commit — data should persist
    post(port, "/sql", """{"sql":"BEGIN"}""", hdrs)
    post(port, "/sql", """{"sql":"INSERT INTO t VALUES (2)"}""", hdrs)
    post(port, "/sql", """{"sql":"COMMIT"}""", hdrs)

    val (_, bCommit) = post(port, "/sql", """{"sql":"SELECT * FROM t"}""", hdrs)
    val rowsCommit = jsonArr(jsonObj(jsonArr(parseJson(bCommit)).head)("rows"))
    rowsCommit.length shouldBe 1
    jsonObj(rowsCommit.head)("id") shouldBe Json.Num(new java.math.BigDecimal(2))
  }

  // ── 1. Session Isolation — Transaction Rollback Visibility ──────

  "session isolation — transaction rollback visibility" in withServer { port =>
    val (_, sb1) = post(port, "/session", "")
    val idA = jsonObj(parseJson(sb1))("sessionId").asInstanceOf[Json.Str].value
    val hdrsA = Map("X-Session-Id" -> idA)

    val (_, sb2) = post(port, "/session", "")
    val idB = jsonObj(parseJson(sb2))("sessionId").asInstanceOf[Json.Str].value
    val hdrsB = Map("X-Session-Id" -> idB)

    // Session A: create table
    post(port, "/sql", """{"sql":"CREATE TABLE t (id INT, name TEXT)"}""", hdrsA)

    // Session A: begin, insert, rollback
    post(port, "/sql", """{"sql":"BEGIN"}""", hdrsA)
    post(port, "/sql", """{"sql":"INSERT INTO t VALUES (1, 'tentative')"}""", hdrsA)
    post(port, "/sql", """{"sql":"ROLLBACK"}""", hdrsA)

    // Session B: should see 0 rows (rollback undid the insert)
    val (_, bAfterRollback) = post(port, "/sql", """{"sql":"SELECT * FROM t"}""", hdrsB)
    val rowsAfterRollback = jsonArr(jsonObj(jsonArr(parseJson(bAfterRollback)).head)("rows"))
    rowsAfterRollback.length shouldBe 0

    // Session A: begin, insert, commit
    post(port, "/sql", """{"sql":"BEGIN"}""", hdrsA)
    post(port, "/sql", """{"sql":"INSERT INTO t VALUES (1, 'committed')"}""", hdrsA)
    post(port, "/sql", """{"sql":"COMMIT"}""", hdrsA)

    // Session B: should now see 1 row
    val (_, bAfterCommit) = post(port, "/sql", """{"sql":"SELECT * FROM t"}""", hdrsB)
    val rowsAfterCommit = jsonArr(jsonObj(jsonArr(parseJson(bAfterCommit)).head)("rows"))
    rowsAfterCommit.length shouldBe 1
    jsonObj(rowsAfterCommit.head)("name") shouldBe Json.Str("committed")
  }

  // ── 2. Session Isolation — Independent Transactions ─────────────

  "session isolation — independent transactions" in withServer { port =>
    val (_, sb1) = post(port, "/session", "")
    val idA = jsonObj(parseJson(sb1))("sessionId").asInstanceOf[Json.Str].value
    val hdrsA = Map("X-Session-Id" -> idA)

    val (_, sb2) = post(port, "/session", "")
    val idB = jsonObj(parseJson(sb2))("sessionId").asInstanceOf[Json.Str].value
    val hdrsB = Map("X-Session-Id" -> idB)

    // Session A: create table (outside transaction)
    post(port, "/sql", """{"sql":"CREATE TABLE t (id INT)"}""", hdrsA)

    // Session A: begin + insert
    post(port, "/sql", """{"sql":"BEGIN"}""", hdrsA)
    post(port, "/sql", """{"sql":"INSERT INTO t VALUES (1)"}""", hdrsA)

    // Session B: begin + insert
    post(port, "/sql", """{"sql":"BEGIN"}""", hdrsB)
    post(port, "/sql", """{"sql":"INSERT INTO t VALUES (2)"}""", hdrsB)

    // Session B: commit (id=2 persists)
    post(port, "/sql", """{"sql":"COMMIT"}""", hdrsB)

    // Session A: rollback (id=1 undone)
    post(port, "/sql", """{"sql":"ROLLBACK"}""", hdrsA)

    // Only B's committed row should remain
    val (_, body) = post(port, "/sql", """{"sql":"SELECT * FROM t ORDER BY id"}""", hdrsA)
    val rows = jsonArr(jsonObj(jsonArr(parseJson(body)).head)("rows"))
    rows.length shouldBe 1
    jsonObj(rows.head)("id") shouldBe Json.Num(new java.math.BigDecimal(2))
  }

  // ── 3. Auto-Created Sessions via X-Session-Id ──────────────────

  "auto-created sessions via X-Session-Id" in withServer { port =>
    val hdrs = Map("X-Session-Id" -> "my-custom-id")

    // First request auto-creates session
    val (s1, _) = post(port, "/sql", """{"sql":"CREATE TABLE t (id INT)"}""", hdrs)
    s1 shouldBe 200

    // Second request shares state with same session
    val (s2, _) = post(port, "/sql", """{"sql":"INSERT INTO t VALUES (1)"}""", hdrs)
    s2 shouldBe 200

    // Third request reads back data
    val (s3, b3) = post(port, "/sql", """{"sql":"SELECT * FROM t"}""", hdrs)
    s3 shouldBe 200
    val rows = jsonArr(jsonObj(jsonArr(parseJson(b3)).head)("rows"))
    rows.length shouldBe 1
  }

  // ── 4. Stateless Requests — Prepared Statement Isolation ────────

  "stateless requests — prepared statement isolation" in withServer { port =>
    // First transient request: prepare a statement
    val (s1, _) = post(port, "/sql", """{"sql":"PREPARE p AS SELECT 1"}""")
    s1 shouldBe 200

    // Second transient request: execute should fail (different session)
    val (s2, b2) = post(port, "/sql", """{"sql":"EXECUTE p"}""")
    s2 shouldBe 400
    val obj = jsonObj(parseJson(b2))
    obj("error") shouldBe Json.Str("SQL error")
  }

  // ── 5. EnumValue Serialization ─────────────────────────────────

  "enum value serialization" in withServer { port =>
    val (_, sb) = post(port, "/session", "")
    val sessionId = jsonObj(parseJson(sb))("sessionId").asInstanceOf[Json.Str].value
    val hdrs = Map("X-Session-Id" -> sessionId)

    post(port, "/sql", """{"sql":"CREATE TYPE color AS ENUM ('red', 'green', 'blue')"}""", hdrs)
    post(port, "/sql", """{"sql":"CREATE TABLE t (id INT, c color)"}""", hdrs)
    post(port, "/sql", """{"sql":"INSERT INTO t VALUES (1, 'green')"}""", hdrs)

    val (status, body) = post(port, "/sql", """{"sql":"SELECT * FROM t"}""", hdrs)
    status shouldBe 200
    val rows = jsonArr(jsonObj(jsonArr(parseJson(body)).head)("rows"))
    rows.length shouldBe 1
    val row = jsonObj(rows.head)
    row("c") shouldBe Json.Str("green")
  }

  // ── 6. DELETE /session/ with Empty ID ───────────────────────────

  "DELETE /session/ with empty ID" in withServer { port =>
    val (status, _) = delete(port, "/session/")
    status shouldBe 404
  }

  // ── 7. Timestamp and Date Serialization ─────────────────────────

  "timestamp and date serialization" in withServer { port =>
    val (_, sb) = post(port, "/session", "")
    val sessionId = jsonObj(parseJson(sb))("sessionId").asInstanceOf[Json.Str].value
    val hdrs = Map("X-Session-Id" -> sessionId)

    post(port, "/sql", """{"sql":"CREATE TABLE t (id INT, d DATE, ts TIMESTAMP)"}""", hdrs)
    post(port, "/sql", """{"sql":"INSERT INTO t VALUES (1, '2024-06-15', '2024-06-15 14:30:00')"}""", hdrs)

    val (status, body) = post(port, "/sql", """{"sql":"SELECT * FROM t"}""", hdrs)
    status shouldBe 200
    val rows = jsonArr(jsonObj(jsonArr(parseJson(body)).head)("rows"))
    rows.length shouldBe 1
    val row = jsonObj(rows.head)

    // Date serializes as ISO string
    row("d") shouldBe a[Json.Str]
    row("d").asInstanceOf[Json.Str].value should include("2024-06-15")

    // Timestamp serializes as ISO string
    row("ts") shouldBe a[Json.Str]
    row("ts").asInstanceOf[Json.Str].value should include("2024-06-15")
    row("ts").asInstanceOf[Json.Str].value should include("14:30")
  }

  // ── 8. UUID Serialization ───────────────────────────────────────

  "UUID serialization" in withServer { port =>
    val (_, sb) = post(port, "/session", "")
    val sessionId = jsonObj(parseJson(sb))("sessionId").asInstanceOf[Json.Str].value
    val hdrs = Map("X-Session-Id" -> sessionId)

    post(port, "/sql", """{"sql":"CREATE TABLE t (id UUID DEFAULT gen_random_uuid(), name TEXT)"}""", hdrs)
    post(port, "/sql", """{"sql":"INSERT INTO t (name) VALUES ('test')"}""", hdrs)

    val (status, body) = post(port, "/sql", """{"sql":"SELECT * FROM t"}""", hdrs)
    status shouldBe 200
    val rows = jsonArr(jsonObj(jsonArr(parseJson(body)).head)("rows"))
    rows.length shouldBe 1
    val row = jsonObj(rows.head)

    row("id") shouldBe a[Json.Str]
    val uuidStr = row("id").asInstanceOf[Json.Str].value
    uuidStr should fullyMatch regex "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
  }

  // ── 9. JSON/JSONB Round-Trip ────────────────────────────────────

  "JSON/JSONB round-trip" in withServer { port =>
    val (_, sb) = post(port, "/session", "")
    val sessionId = jsonObj(parseJson(sb))("sessionId").asInstanceOf[Json.Str].value
    val hdrs = Map("X-Session-Id" -> sessionId)

    post(port, "/sql", """{"sql":"CREATE TABLE t (id INT, data JSONB)"}""", hdrs)
    post(port, "/sql", """{"sql":"INSERT INTO t VALUES (1, '{\"name\": \"Alice\", \"scores\": [10, 20]}')"}""", hdrs)

    val (status, body) = post(port, "/sql", """{"sql":"SELECT * FROM t"}""", hdrs)
    status shouldBe 200
    val rows = jsonArr(jsonObj(jsonArr(parseJson(body)).head)("rows"))
    rows.length shouldBe 1
    val row = jsonObj(rows.head)

    // data should be a JSON object, not a string
    row("data") shouldBe a[Json.Obj]
    val data = jsonObj(row("data"))
    data("name") shouldBe Json.Str("Alice")

    val scores = jsonArr(data("scores"))
    scores.length shouldBe 2
    scores(0) shouldBe Json.Num(new java.math.BigDecimal(10))
    scores(1) shouldBe Json.Num(new java.math.BigDecimal(20))
  }

  // ── 10. Multiple Results Array Encoding ─────────────────────────

  "response is always a JSON array" in withServer { port =>
    // Single SELECT — response is array of length 1
    val (s1, b1) = post(port, "/sql", """{"sql":"SELECT 1"}""")
    s1 shouldBe 200
    val r1 = jsonArr(parseJson(b1))
    r1.length shouldBe 1
    jsonObj(r1.head)("command") shouldBe Json.Str("select")

    // BEGIN — response is array of length 1 with command "begin"
    val (_, sb) = post(port, "/session", "")
    val sessionId = jsonObj(parseJson(sb))("sessionId").asInstanceOf[Json.Str].value
    val hdrs = Map("X-Session-Id" -> sessionId)

    val (s2, b2) = post(port, "/sql", """{"sql":"BEGIN"}""", hdrs)
    s2 shouldBe 200
    val r2 = jsonArr(parseJson(b2))
    r2.length shouldBe 1
    jsonObj(r2.head)("command") shouldBe Json.Str("begin")

    // Clean up the transaction
    post(port, "/sql", """{"sql":"ROLLBACK"}""", hdrs)
  }

  // ── Response Format Alignment Tests ──────────────────────────────

  "UPDATE returns rowCount" in withServer { port =>
    post(port, "/sql", """{"sql":"CREATE TABLE t (id INT, name TEXT)"}""")
    post(port, "/sql", """{"sql":"INSERT INTO t VALUES (1, 'a'), (2, 'b'), (3, 'c')"}""")

    val (status, body) = post(port, "/sql", """{"sql":"UPDATE t SET name = 'x' WHERE id < 3"}""")
    status shouldBe 200
    val result = jsonObj(jsonArr(parseJson(body)).head)
    result("command") shouldBe Json.Str("update")
    result("rowCount") shouldBe Json.Num(new java.math.BigDecimal(2))
    result should not contain key("rows")
  }

  "DELETE returns rowCount" in withServer { port =>
    post(port, "/sql", """{"sql":"CREATE TABLE t (id INT)"}""")
    post(port, "/sql", """{"sql":"INSERT INTO t VALUES (1), (2), (3)"}""")

    val (status, body) = post(port, "/sql", """{"sql":"DELETE FROM t WHERE id = 1"}""")
    status shouldBe 200
    val result = jsonObj(jsonArr(parseJson(body)).head)
    result("command") shouldBe Json.Str("delete")
    result("rowCount") shouldBe Json.Num(new java.math.BigDecimal(1))
    result should not contain key("rows")
  }

  "INSERT includes rows and fields" in withServer { port =>
    post(port, "/sql", """{"sql":"CREATE TABLE t (id SERIAL, name TEXT)"}""")

    val (status, body) = post(port, "/sql", """{"sql":"INSERT INTO t (name) VALUES ('Alice')"}""")
    status shouldBe 200
    val result = jsonObj(jsonArr(parseJson(body)).head)
    result("command") shouldBe Json.Str("insert")

    // result contains generated serial
    val insertResult = jsonObj(result("result"))
    insertResult("id") shouldBe a[Json.Num]

    // rows array with the inserted auto-generated values
    val rows = jsonArr(result("rows"))
    rows.length shouldBe 1

    // fields array with column metadata for generated columns
    val fields = jsonArr(result("fields"))
    fields.length should be >= 1
    jsonObj(fields(0))("name") shouldBe Json.Str("id")
  }

  "TRUNCATE command string and table field" in withServer { port =>
    post(port, "/sql", """{"sql":"CREATE TABLE t (id INT)"}""")
    post(port, "/sql", """{"sql":"INSERT INTO t VALUES (1)"}""")

    val (status, body) = post(port, "/sql", """{"sql":"TRUNCATE TABLE t"}""")
    status shouldBe 200
    val result = jsonObj(jsonArr(parseJson(body)).head)
    result("command") shouldBe Json.Str("truncate table")
    result("table") shouldBe Json.Str("t")
  }

  "CREATE TYPE response shape" in withServer { port =>
    val (_, sb) = post(port, "/session", "")
    val sessionId = jsonObj(parseJson(sb))("sessionId").asInstanceOf[Json.Str].value
    val hdrs = Map("X-Session-Id" -> sessionId)

    val (status, body) = post(port, "/sql", """{"sql":"CREATE TYPE mood AS ENUM ('happy', 'sad')"}""", hdrs)
    status shouldBe 200
    val result = jsonObj(jsonArr(parseJson(body)).head)
    result("command") shouldBe Json.Str("create type")
    result("type") shouldBe Json.Str("mood")
    result should not contain key("name")
  }

  "DROP TYPE response shape" in withServer { port =>
    val (_, sb) = post(port, "/session", "")
    val sessionId = jsonObj(parseJson(sb))("sessionId").asInstanceOf[Json.Str].value
    val hdrs = Map("X-Session-Id" -> sessionId)

    post(port, "/sql", """{"sql":"CREATE TYPE mood AS ENUM ('happy', 'sad')"}""", hdrs)
    val (status, body) = post(port, "/sql", """{"sql":"DROP TYPE mood"}""", hdrs)
    status shouldBe 200
    val result = jsonObj(jsonArr(parseJson(body)).head)
    result("command") shouldBe Json.Str("drop type")
    result("type") shouldBe Json.Str("mood")
    result should not contain key("name")
  }

  "CREATE INDEX response shape" in withServer { port =>
    val (_, sb) = post(port, "/session", "")
    val sessionId = jsonObj(parseJson(sb))("sessionId").asInstanceOf[Json.Str].value
    val hdrs = Map("X-Session-Id" -> sessionId)

    post(port, "/sql", """{"sql":"CREATE TABLE t (id INT, name TEXT)"}""", hdrs)
    val (status, body) = post(port, "/sql", """{"sql":"CREATE INDEX idx_name ON t (name)"}""", hdrs)
    status shouldBe 200
    val result = jsonObj(jsonArr(parseJson(body)).head)
    result("command") shouldBe Json.Str("create index")
    result("index") shouldBe Json.Str("idx_name")
    result should not contain key("name")
  }

  "DROP INDEX response shape" in withServer { port =>
    val (_, sb) = post(port, "/session", "")
    val sessionId = jsonObj(parseJson(sb))("sessionId").asInstanceOf[Json.Str].value
    val hdrs = Map("X-Session-Id" -> sessionId)

    post(port, "/sql", """{"sql":"CREATE TABLE t (id INT, name TEXT)"}""", hdrs)
    post(port, "/sql", """{"sql":"CREATE INDEX idx_name ON t (name)"}""", hdrs)
    val (status, body) = post(port, "/sql", """{"sql":"DROP INDEX idx_name"}""", hdrs)
    status shouldBe 200
    val result = jsonObj(jsonArr(parseJson(body)).head)
    result("command") shouldBe Json.Str("drop index")
    result("index") shouldBe Json.Str("idx_name")
    result should not contain key("name")
  }

  "INSERT with array row mode includes rows as arrays" in withServer { port =>
    post(port, "/sql", """{"sql":"CREATE TABLE t (id SERIAL, name TEXT)"}""")

    val (status, body) = post(port, "/sql", """{"sql":"INSERT INTO t (name) VALUES ('Bob')","rowMode":"array"}""")
    status shouldBe 200
    val result = jsonObj(jsonArr(parseJson(body)).head)
    val rows = jsonArr(result("rows"))
    rows.length shouldBe 1
    val row = jsonArr(rows.head)
    row(0) shouldBe a[Json.Num]
  }

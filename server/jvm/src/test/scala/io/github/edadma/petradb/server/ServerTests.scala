package io.github.edadma.petradb.server

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.github.edadma.microserve.EventLoop
import zio.json.*
import zio.json.ast.Json

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

class ServerTests extends AnyFreeSpec with Matchers:
  private val client = HttpClient.newHttpClient()

  private def withServer(test: Int => Unit): Unit =
    val loop = new EventLoop
    val server = new PetraServer(loop, port = 0)
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

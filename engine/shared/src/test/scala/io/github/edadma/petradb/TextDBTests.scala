package io.github.edadma.petradb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterEach
import io.github.edadma.cross_platform.{createTempFile, deleteFile, exists, readFile}
import scala.compiletime.uninitialized

class TextDBTests extends AnyFreeSpec with Matchers with BeforeAndAfterEach:
  protected var tmpFile: String = uninitialized

  override def beforeEach(): Unit =
    tmpFile = createTempFile("textdb_test_", ".ptxt")
    deleteFile(tmpFile)

  override def afterEach(): Unit =
    try deleteFile(tmpFile)
    catch case _: Exception => ()

  "TextDB" - {

    "nonexistent file creates empty DB" in {
      val db = TextDB.open(tmpFile)
      given Session = db.connect()
      db.tableNames.toSeq shouldBe empty
      exists(tmpFile) shouldBe false
    }

    "create table persists to file" in {
      locally {
        val db = TextDB.open(tmpFile)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, name TEXT)")
      }
      exists(tmpFile) shouldBe true
      val content = readFile(tmpFile)
      content should include("t")
      content should include("id:integer")
      content should include("name:text")
    }

    "insert rows persist and reload" in {
      locally {
        val db = TextDB.open(tmpFile)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, name TEXT)")
        executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice')")
        executeSQL("INSERT INTO t (id, name) VALUES (2, 'Bob')")
      }

      locally {
        val db = TextDB.open(tmpFile)
        given Session = db.connect()
        val res = executeSQL("SELECT id, name FROM t ORDER BY id").collect { case QueryResult(t) => t }.head
        res.data.length shouldBe 2
        res.data(0).data(0) shouldBe NumberValue(1)
        res.data(0).data(1) shouldBe TextValue("Alice")
        res.data(1).data(0) shouldBe NumberValue(2)
        res.data(1).data(1) shouldBe TextValue("Bob")
      }
    }

    "update round-trips correctly" in {
      locally {
        val db = TextDB.open(tmpFile)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, name TEXT)")
        executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice')")
        executeSQL("INSERT INTO t (id, name) VALUES (2, 'Bob')")
        executeSQL("UPDATE t SET name = 'Charlie' WHERE id = 1")
      }

      locally {
        val db = TextDB.open(tmpFile)
        given Session = db.connect()
        val res = executeSQL("SELECT name FROM t WHERE id = 1").collect { case QueryResult(t) => t }.head
        res.data(0).data(0) shouldBe TextValue("Charlie")
      }
    }

    "delete round-trips correctly" in {
      locally {
        val db = TextDB.open(tmpFile)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, name TEXT)")
        executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice')")
        executeSQL("INSERT INTO t (id, name) VALUES (2, 'Bob')")
        executeSQL("DELETE FROM t WHERE id = 1")
      }

      locally {
        val db = TextDB.open(tmpFile)
        given Session = db.connect()
        val res = executeSQL("SELECT id FROM t ORDER BY id").collect { case QueryResult(t) => t }.head
        res.data.length shouldBe 1
        res.data(0).data(0) shouldBe NumberValue(2)
      }
    }

    "multiple tables in one file" in {
      locally {
        val db = TextDB.open(tmpFile)
        given Session = db.connect()
        executeSQL("CREATE TABLE a (x INTEGER)")
        executeSQL("CREATE TABLE b (y TEXT)")
        executeSQL("INSERT INTO a (x) VALUES (42)")
        executeSQL("INSERT INTO b (y) VALUES ('hello')")
      }

      locally {
        val db = TextDB.open(tmpFile)
        given Session = db.connect()
        val ra = executeSQL("SELECT x FROM a").collect { case QueryResult(t) => t }.head
        ra.data(0).data(0) shouldBe NumberValue(42)
        val rb = executeSQL("SELECT y FROM b").collect { case QueryResult(t) => t }.head
        rb.data(0).data(0) shouldBe TextValue("hello")
      }
    }

    "NULL values round-trip" in {
      locally {
        val db = TextDB.open(tmpFile)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, name TEXT)")
        executeSQL("INSERT INTO t (id, name) VALUES (1, NULL)")
      }

      locally {
        val db = TextDB.open(tmpFile)
        given Session = db.connect()
        val res = executeSQL("SELECT name FROM t WHERE id = 1").collect { case QueryResult(t) => t }.head
        res.data(0).data(0).isNull shouldBe true
      }
    }

    "enum types round-trip" in {
      locally {
        val db = TextDB.open(tmpFile)
        given Session = db.connect()
        executeSQL("CREATE TYPE mood AS ENUM ('happy', 'sad', 'neutral')")
        executeSQL("CREATE TABLE t (id INTEGER, feeling mood)")
        executeSQL("INSERT INTO t (id, feeling) VALUES (1, 'happy')")
        executeSQL("INSERT INTO t (id, feeling) VALUES (2, 'sad')")
      }

      locally {
        val db = TextDB.open(tmpFile)
        given Session = db.connect()
        val res = executeSQL("SELECT id, feeling FROM t ORDER BY id").collect { case QueryResult(t) => t }.head
        res.data.length shouldBe 2
        res.data(0).data(1).string shouldBe "happy"
        res.data(1).data(1).string shouldBe "sad"
      }
    }

    "boolean values round-trip" in {
      locally {
        val db = TextDB.open(tmpFile)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, flag BOOLEAN)")
        executeSQL("INSERT INTO t (id, flag) VALUES (1, TRUE)")
        executeSQL("INSERT INTO t (id, flag) VALUES (2, FALSE)")
      }

      locally {
        val db = TextDB.open(tmpFile)
        given Session = db.connect()
        val res = executeSQL("SELECT flag FROM t ORDER BY id").collect { case QueryResult(t) => t }.head
        res.data(0).data(0) shouldBe BooleanValue(true)
        res.data(1).data(0) shouldBe BooleanValue(false)
      }
    }

    "date values round-trip" in {
      locally {
        val db = TextDB.open(tmpFile)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, d DATE)")
        executeSQL("INSERT INTO t (id, d) VALUES (1, '2024-06-15')")
      }

      locally {
        val db = TextDB.open(tmpFile)
        given Session = db.connect()
        val res = executeSQL("SELECT d FROM t").collect { case QueryResult(t) => t }.head
        res.data(0).data(0).string shouldBe "2024-06-15"
      }
    }

    "written file has no ANSI codes" in {
      val db = TextDB.open(tmpFile)
      given Session = db.connect()
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT)")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'hello')")

      val content = readFile(tmpFile)
      content should not include "\u001b["
    }

    "drop table removes it from file" in {
      locally {
        val db = TextDB.open(tmpFile)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER)")
        executeSQL("INSERT INTO t (id) VALUES (1)")
        executeSQL("DROP TABLE t")
      }

      locally {
        val db = TextDB.open(tmpFile)
        given Session = db.connect()
        db.tableNames.toSeq shouldBe empty
      }
    }
  }

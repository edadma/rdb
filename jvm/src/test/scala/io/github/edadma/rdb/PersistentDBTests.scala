package io.github.edadma.rdb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterEach

import java.nio.file.{Files, Path}
import scala.compiletime.uninitialized

class PersistentDBTests extends AnyFreeSpec with Matchers with BeforeAndAfterEach:

  private var tmpFile: Path = uninitialized

  override def beforeEach(): Unit =
    tmpFile = Files.createTempFile("rdb_test_", ".db")
    Files.delete(tmpFile) // FilePageStore.create needs a non-existent path

  override def afterEach(): Unit =
    try Files.deleteIfExists(tmpFile)
    catch case _: Exception => ()

  private val pageSize = 4096

  "Basic persistence" - {
    "create table, insert, close, reopen, query" in {
      locally {
        val db = PersistentDB.create(tmpFile.toString, pageSize)
        given DB = db
        executeSQL("CREATE TABLE users (id SERIAL, name TEXT, PRIMARY KEY (id));")
        executeSQL("INSERT INTO users (name) VALUES ('Alice');")
        executeSQL("INSERT INTO users (name) VALUES ('Bob');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile.toString)
        given DB = db
        val results = executeSQL("SELECT id, name FROM users ORDER BY id;")
        val table = results.collect { case QueryResult(t) => t }.head

        table.data.length shouldBe 2
        table.data(0).data(1) shouldBe TextValue("Alice")
        table.data(1).data(1) shouldBe TextValue("Bob")
        db.close()
      }
    }

    "empty table persists" in {
      locally {
        val db = PersistentDB.create(tmpFile.toString, pageSize)
        given DB = db
        executeSQL("CREATE TABLE empty_table (id INTEGER, name TEXT);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile.toString)
        given DB = db
        val results = executeSQL("SELECT * FROM empty_table;")
        val table = results.collect { case QueryResult(t) => t }.head

        table.data.length shouldBe 0
        db.close()
      }
    }
  }

  "Auto-increment persistence" - {
    "serial counter persists across reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile.toString, pageSize)
        given DB = db
        executeSQL("CREATE TABLE items (id SERIAL, name TEXT, PRIMARY KEY (id));")
        executeSQL("INSERT INTO items (name) VALUES ('first');")
        executeSQL("INSERT INTO items (name) VALUES ('second');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile.toString)
        given DB = db
        executeSQL("INSERT INTO items (name) VALUES ('third');")
        val results = executeSQL("SELECT id, name FROM items ORDER BY id;")
        val table = results.collect { case QueryResult(t) => t }.head

        table.data.length shouldBe 3
        table.data(0).data(0) shouldBe NumberValue(1)
        table.data(1).data(0) shouldBe NumberValue(2)
        table.data(2).data(0) shouldBe NumberValue(3)
        db.close()
      }
    }
  }

  "Data type persistence" - {
    "integer types roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile.toString, pageSize)
        given DB = db
        executeSQL("CREATE TABLE nums (a SMALLINT, b INTEGER, c BIGINT);")
        executeSQL("INSERT INTO nums (a, b, c) VALUES (42, 100000, 999999999);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile.toString)
        given DB = db
        val results = executeSQL("SELECT a, b, c FROM nums;")
        val table = results.collect { case QueryResult(t) => t }.head

        table.data.length shouldBe 1
        table.data(0).data(0).string shouldBe "42"
        table.data(0).data(1).string shouldBe "100000"
        table.data(0).data(2).string shouldBe "999999999"
        db.close()
      }
    }

    "boolean roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile.toString, pageSize)
        given DB = db
        executeSQL("CREATE TABLE flags (a BOOLEAN, b BOOLEAN);")
        executeSQL("INSERT INTO flags (a, b) VALUES (true, false);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile.toString)
        given DB = db
        val results = executeSQL("SELECT a, b FROM flags;")
        val table = results.collect { case QueryResult(t) => t }.head

        table.data(0).data(0) shouldBe BooleanValue(true)
        table.data(0).data(1) shouldBe BooleanValue(false)
        db.close()
      }
    }

    "date and time roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile.toString, pageSize)
        given DB = db
        executeSQL("CREATE TABLE events (d DATE, t TIME, ts TIMESTAMP);")
        executeSQL("INSERT INTO events (d, t, ts) VALUES ('2024-01-15', '14:30:00', '2024-01-15 14:30:00');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile.toString)
        given DB = db
        val results = executeSQL("SELECT d, t, ts FROM events;")
        val table = results.collect { case QueryResult(t) => t }.head

        table.data(0).data(0).string shouldBe "2024-01-15"
        table.data(0).data(1).string shouldBe "14:30"
        table.data(0).data(2).string shouldBe "2024-01-15T14:30"
        db.close()
      }
    }

    "null values roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile.toString, pageSize)
        given DB = db
        executeSQL("CREATE TABLE nullable (id INTEGER, name TEXT);")
        executeSQL("INSERT INTO nullable (id) VALUES (1);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile.toString)
        given DB = db
        val results = executeSQL("SELECT id, name FROM nullable;")
        val table = results.collect { case QueryResult(t) => t }.head

        table.data(0).data(0).string shouldBe "1"
        table.data(0).data(1).isNull shouldBe true
        db.close()
      }
    }
  }

  "Large values" - {
    "large TEXT uses chain storage" in {
      val longText = "x" * 500 // well above 64-byte inline threshold

      locally {
        val db = PersistentDB.create(tmpFile.toString, pageSize)
        given DB = db
        executeSQL("CREATE TABLE docs (id INTEGER, content TEXT);")
        db.getTable("docs").get.insert(Map("id" -> NumberValue(1), "content" -> TextValue(longText)), None)
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile.toString)
        given DB = db
        val results = executeSQL("SELECT id, content FROM docs;")
        val table = results.collect { case QueryResult(t) => t }.head

        table.data(0).data(1) shouldBe TextValue(longText)
        db.close()
      }
    }
  }

  "DML persistence" - {
    "UPDATE survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile.toString, pageSize)
        given DB = db
        executeSQL("CREATE TABLE users (id INTEGER, name TEXT);")
        executeSQL("INSERT INTO users (id, name) VALUES (1, 'Alice');")
        executeSQL("INSERT INTO users (id, name) VALUES (2, 'Bob');")
        executeSQL("UPDATE users SET name = 'Alicia' WHERE id = 1;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile.toString)
        given DB = db
        val results = executeSQL("SELECT id, name FROM users ORDER BY id;")
        val table = results.collect { case QueryResult(t) => t }.head

        table.data.length shouldBe 2
        table.data(0).data(1) shouldBe TextValue("Alicia")
        table.data(1).data(1) shouldBe TextValue("Bob")
        db.close()
      }
    }

    "DELETE survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile.toString, pageSize)
        given DB = db
        executeSQL("CREATE TABLE users (id INTEGER, name TEXT);")
        executeSQL("INSERT INTO users (id, name) VALUES (1, 'Alice');")
        executeSQL("INSERT INTO users (id, name) VALUES (2, 'Bob');")
        executeSQL("INSERT INTO users (id, name) VALUES (3, 'Charlie');")
        executeSQL("DELETE FROM users WHERE id = 2;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile.toString)
        given DB = db
        val results = executeSQL("SELECT id, name FROM users ORDER BY id;")
        val table = results.collect { case QueryResult(t) => t }.head

        table.data.length shouldBe 2
        table.data(0).data(1) shouldBe TextValue("Alice")
        table.data(1).data(1) shouldBe TextValue("Charlie")
        db.close()
      }
    }
  }

  "DDL persistence" - {
    "ALTER TABLE ADD COLUMN survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile.toString, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER);")
        executeSQL("INSERT INTO t (id) VALUES (1);")
        executeSQL("ALTER TABLE t ADD COLUMN name TEXT DEFAULT 'unknown';")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile.toString)
        given DB = db
        val results = executeSQL("SELECT id, name FROM t;")
        val table = results.collect { case QueryResult(t) => t }.head

        table.data.length shouldBe 1
        table.data(0).data(0).string shouldBe "1"
        table.data(0).data(1) shouldBe TextValue("unknown")
        db.close()
      }
    }

    "ALTER TABLE DROP COLUMN survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile.toString, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER, name TEXT, email TEXT);")
        executeSQL("INSERT INTO t (id, name, email) VALUES (1, 'Alice', 'alice@test.com');")
        executeSQL("ALTER TABLE t DROP COLUMN email;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile.toString)
        given DB = db
        val results = executeSQL("SELECT id, name FROM t;")
        val table = results.collect { case QueryResult(t) => t }.head

        table.data.length shouldBe 1
        table.data(0).data(0).string shouldBe "1"
        table.data(0).data(1) shouldBe TextValue("Alice")
        db.close()
      }
    }
  }

  "DROP TABLE" - {
    "table gone after reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile.toString, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t1 (id INTEGER);")
        executeSQL("CREATE TABLE t2 (id INTEGER);")
        executeSQL("INSERT INTO t1 (id) VALUES (1);")
        executeSQL("INSERT INTO t2 (id) VALUES (2);")
        executeSQL("DROP TABLE t1;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile.toString)
        given DB = db
        (db hasTable "t1") shouldBe false
        (db hasTable "t2") shouldBe true

        val results = executeSQL("SELECT id FROM t2;")
        val table = results.collect { case QueryResult(t) => t }.head
        table.data(0).data(0).string shouldBe "2"
        db.close()
      }
    }
  }

  "Multiple tables" - {
    "multiple tables persist independently" in {
      locally {
        val db = PersistentDB.create(tmpFile.toString, pageSize)
        given DB = db
        executeSQL("CREATE TABLE users (id SERIAL, name TEXT, PRIMARY KEY (id));")
        executeSQL("CREATE TABLE products (id SERIAL, title TEXT, price INTEGER, PRIMARY KEY (id));")
        executeSQL("INSERT INTO users (name) VALUES ('Alice');")
        executeSQL("INSERT INTO products (title, price) VALUES ('Widget', 100);")
        executeSQL("INSERT INTO products (title, price) VALUES ('Gadget', 200);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile.toString)
        given DB = db
        val users = executeSQL("SELECT name FROM users;").collect { case QueryResult(t) => t }.head
        val products = executeSQL("SELECT title, price FROM products ORDER BY price;").collect { case QueryResult(t) => t }.head

        users.data.length shouldBe 1
        users.data(0).data(0) shouldBe TextValue("Alice")

        products.data.length shouldBe 2
        products.data(0).data(0) shouldBe TextValue("Widget")
        products.data(1).data(0) shouldBe TextValue("Gadget")
        db.close()
      }
    }
  }

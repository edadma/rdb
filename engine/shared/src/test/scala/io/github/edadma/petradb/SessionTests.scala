package io.github.edadma.petradb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SessionTests extends AnyFreeSpec with Matchers:

  "Session basics" - {
    "session.db returns the underlying DB" in {
      val db = new MemoryDB
      val session = db.connect()
      session.db shouldBe db
    }

    "fresh session is not in a transaction" in {
      val session = new MemoryDB().connect()
      session.inTransaction shouldBe false
      session.isTransactionAborted shouldBe false
    }

    "fresh session has empty preparedStatements" in {
      val session = new MemoryDB().connect()
      session.preparedStatements shouldBe empty
    }
  }

  "Shared state" - {
    "two sessions share the same tables" in {
      val db = new MemoryDB
      val s1 = db.connect()
      val s2 = db.connect()

      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")(using s1)
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")(using s1)

      val table = executeSQL("SELECT * FROM t;")(using s2).collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("Alice")
    }
  }

  "Independent transaction state" - {
    "two sessions have independent transaction state" in {
      val db = new MemoryDB
      val s1 = db.connect()
      val s2 = db.connect()

      s1.beginTransaction()
      s1.inTransaction shouldBe true
      s2.inTransaction shouldBe false
    }
  }

  "Interleaved transactions on same DB" - {
    "rollback in session 1 does not discard session 2's committed insert" in {
      val db = new MemoryDB
      val s1 = db.connect()
      val s2 = db.connect()

      executeSQL("CREATE TABLE t (id INTEGER, v TEXT);")(using s1)

      // s1 starts a transaction and inserts a row
      executeSQL("BEGIN;")(using s1)
      executeSQL("INSERT INTO t (id, v) VALUES (1, 'from s1');")(using s1)

      // s2 starts its own transaction and inserts a different row
      executeSQL("BEGIN;")(using s2)
      executeSQL("INSERT INTO t (id, v) VALUES (2, 'from s2');")(using s2)
      executeSQL("COMMIT;")(using s2)

      // s1 rolls back — should undo only its own insert
      executeSQL("ROLLBACK;")(using s1)

      // The row inserted by s2 must survive
      val rows = executeSQL("SELECT * FROM t ORDER BY id;")(using s1)
        .collect { case QueryResult(t) => t }.head.data
      rows.length shouldBe 1
      rows(0).data(0) shouldBe NumberValue(2)
      rows(0).data(1) shouldBe TextValue("from s2")
    }
  }

  "Auto-increment rollback semantics" - {
    "auto-increment not reset by rollback (PostgreSQL semantics)" in {
      val db = new MemoryDB
      val s1 = db.connect()
      val s2 = db.connect()

      executeSQL("CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT);")(using s1)
      executeSQL("INSERT INTO t (name) VALUES ('base');")(using s1)

      executeSQL("BEGIN;")(using s1)
      executeSQL("INSERT INTO t (name) VALUES ('from_s1');")(using s1)
      executeSQL("INSERT INTO t (name) VALUES ('from_s2');")(using s2)
      executeSQL("ROLLBACK;")(using s1)

      // This should not conflict with s2's row
      executeSQL("INSERT INTO t (name) VALUES ('post');")(using s1)

      val table = executeSQL("SELECT * FROM t ORDER BY id;")(using s1)
        .collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 3
    }
  }

  "Per-session prepared statements" - {
    "prepared statements are per-session" in {
      val db = new MemoryDB
      val s1 = db.connect()
      val s2 = db.connect()

      given Session = s1
      executeSQL("CREATE TABLE t (id INTEGER);")

      val ps = s1.prepare("SELECT * FROM t WHERE id = $1")
      s1.preparedStatements.contains(ps.name) shouldBe true
      s2.preparedStatements.contains(ps.name) shouldBe false
    }
  }

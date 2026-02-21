package io.github.edadma.rdb

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

package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import java.time.LocalDate
import scala.concurrent.ExecutionContext

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

  // The engine's `execute` completes synchronously (Future.successful), so results extract via
  // `.value.get.get` without blocking — keeping these assertions cross-platform (JS/Native have no
  // Await). Binding correctness across DML positions is also covered by ParameterizedQueryTests at
  // the internal executeSQL level; here we drive the public trait method with Seq[Value] directly.
  "Parameterized execute (Seq[Value]) via the Session trait" - {
    given ExecutionContext = ExecutionContext.parasitic

    def run(s: Session, sql: String, params: Value*): Seq[Result] =
      s.execute(sql, params).value.get.get

    def query(s: Session, sql: String, params: Value*): TableValue =
      run(s, sql, params*).collect { case QueryResult(t) => t }.last

    def fresh(): Session =
      val s = new MemoryDB().connect()
      run(s, "CREATE TABLE t (id INTEGER, name TEXT, active BOOLEAN)")
      run(
        s,
        "INSERT INTO t (id, name, active) VALUES (1, 'alice', true), (2, 'bob', false), (3, 'charlie', true)",
      )
      s

    "binds $1 in a SELECT WHERE clause" in {
      val t = query(fresh(), "SELECT name FROM t WHERE id = $1", NumberValue(2))
      t.data.map(_.data(0).string) shouldBe IndexedSeq("bob")
    }

    "binds params in INSERT VALUES" in {
      val s = fresh()
      run(s, "INSERT INTO t (id, name, active) VALUES ($1, $2, $3)", NumberValue(4), TextValue("dave"), BooleanValue(true))
      query(s, "SELECT name FROM t WHERE id = $1", NumberValue(4)).data.map(_.data(0).string) shouldBe IndexedSeq("dave")
    }

    "binds params in UPDATE SET and WHERE" in {
      val s = fresh()
      run(s, "UPDATE t SET name = $1 WHERE id = $2", TextValue("alicia"), NumberValue(1))
      query(s, "SELECT name FROM t WHERE id = $1", NumberValue(1)).data.map(_.data(0).string) shouldBe IndexedSeq("alicia")
    }

    "binds a param in DELETE WHERE" in {
      val s = fresh()
      run(s, "DELETE FROM t WHERE id = $1", NumberValue(2))
      query(s, "SELECT id FROM t ORDER BY id").data.map(_.data(0).intValue) shouldBe IndexedSeq(1, 3)
    }

    "binds a param and returns rows via RETURNING" in {
      val s   = fresh()
      val ins = run(s, "INSERT INTO t (id, name, active) VALUES ($1, $2, $3) RETURNING name",
        NumberValue(5), TextValue("eve"), BooleanValue(false)).last.asInstanceOf[InsertResult]
      ins.obj("name") shouldBe TextValue("eve")
    }

    "binds params in INSERT ... ON CONFLICT DO UPDATE" in {
      val s = new MemoryDB().connect()
      run(s, "CREATE TABLE u (id INTEGER PRIMARY KEY, name TEXT)")
      run(s, "INSERT INTO u (id, name) VALUES (1, 'alice')")
      run(s, "INSERT INTO u (id, name) VALUES ($1, $2) ON CONFLICT (id) DO UPDATE SET name = $2",
        NumberValue(1), TextValue("alice2"))
      query(s, "SELECT name FROM u WHERE id = 1").data(0).data(0) shouldBe TextValue("alice2")
    }

    "binds a NULL parameter" in {
      val s = fresh()
      run(s, "INSERT INTO t (id, name, active) VALUES ($1, $2, $3)", NumberValue(6), NullValue(), BooleanValue(true))
      query(s, "SELECT name FROM t WHERE id = $1", NumberValue(6)).data(0).data(0).isNull shouldBe true
    }

    "preserves the exact type of a bound Value (no parser re-inference)" in {
      val s = new MemoryDB().connect()
      run(s, "CREATE TABLE d (id INTEGER, at DATE)")
      run(s, "INSERT INTO d (id, at) VALUES ($1, $2)", NumberValue(1), DateValue(LocalDate.parse("2024-01-01")))
      val t = query(s, "SELECT at FROM d WHERE at = $1", DateValue(LocalDate.parse("2024-01-01")))
      t.data.length shouldBe 1
      t.data(0).data(0) shouldBe DateValue(LocalDate.parse("2024-01-01"))
    }

    "reuses the same placeholder positionally ($1 twice)" in {
      val t = query(fresh(), "SELECT name FROM t WHERE id = $1 OR id = $1", NumberValue(2))
      t.data.map(_.data(0).string) shouldBe IndexedSeq("bob")
    }

    "throws when a placeholder index is out of range" in {
      val s = fresh()
      an[ExecutionException] should be thrownBy s.execute("SELECT name FROM t WHERE id = $2", Seq(NumberValue(1)))
    }
  }

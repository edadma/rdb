package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ParameterizedQueryTests extends AnyFreeSpec with Matchers {

  private def withSession(fn: Session ?=> Unit): Unit =
    given session: Session = new MemoryDB().connect()
    executeSQL(
      """CREATE TABLE t (id INT, name TEXT, active BOOLEAN, score FLOAT);
        |INSERT INTO t VALUES (1, 'alice', true, 9.5);
        |INSERT INTO t VALUES (2, 'bob', false, 7.2);
        |INSERT INTO t VALUES (3, 'charlie', true, 8.8);
        |INSERT INTO t VALUES (4, 'dave', false, 6.1);
        |""".stripMargin
    )
    fn

  private def queryParam(sql: String, params: Any*)(using Session): TableValue =
    executeSQL(sql, params.toIndexedSeq).collect { case QueryResult(t) => t }.last

  // ── Basic parameter binding ───────────────────────────────────────

  "basic parameter binding" - {
    "single integer parameter" in withSession {
      val t = queryParam("SELECT name FROM t WHERE id = $1", 2)
      t.data(0).data(0).string shouldBe "bob"
    }

    "single string parameter" in withSession {
      val t = queryParam("SELECT id FROM t WHERE name = $1", "charlie")
      t.data(0).data(0).intValue shouldBe 3
    }

    "single boolean parameter" in withSession {
      val t = queryParam("SELECT name FROM t WHERE active = $1 ORDER BY id", true)
      t.data.map(_.data(0).string) shouldBe IndexedSeq("alice", "charlie")
    }

    "single float parameter" in withSession {
      val t = queryParam("SELECT name FROM t WHERE score > $1 ORDER BY id", 8.0)
      t.data.map(_.data(0).string) shouldBe IndexedSeq("alice", "charlie")
    }

    "null parameter" in withSession {
      val t = queryParam("SELECT $1::text IS NULL AS result", null)
      t.data(0).data(0).asInstanceOf[BooleanValue].b shouldBe true
    }
  }

  // ── Multiple parameters ───────────────────────────────────────────

  "multiple parameters" - {
    "two parameters" in withSession {
      val t = queryParam("SELECT name FROM t WHERE id >= $1 AND id <= $2 ORDER BY id", 2, 3)
      t.data.map(_.data(0).string) shouldBe IndexedSeq("bob", "charlie")
    }

    "same parameter used twice" in withSession {
      val t = queryParam("SELECT name FROM t WHERE id = $1 OR id = $1", 1)
      t.data.map(_.data(0).string) shouldBe IndexedSeq("alice")
    }

    "parameters in different order" in withSession {
      val t = queryParam("SELECT name FROM t WHERE name = $2 OR id = $1 ORDER BY id", 3, "alice")
      t.data.map(_.data(0).string) shouldBe IndexedSeq("alice", "charlie")
    }

    "three parameters of different types" in withSession {
      val t = queryParam("SELECT name FROM t WHERE id > $1 AND active = $2 AND score < $3", 1, true, 9.0)
      t.data.map(_.data(0).string) shouldBe IndexedSeq("charlie")
    }
  }

  // ── Array parameters ──────────────────────────────────────────────

  "array parameters" - {
    "ANY with Seq parameter" in withSession {
      val t = queryParam("SELECT name FROM t WHERE id = ANY($1) ORDER BY id", Seq(1, 3))
      t.data.map(_.data(0).string) shouldBe IndexedSeq("alice", "charlie")
    }

    "ANY with empty Seq returns no rows" in withSession {
      val t = queryParam("SELECT name FROM t WHERE id = ANY($1)", Seq.empty[Int])
      t.data shouldBe empty
    }

    "ANY with single-element Seq" in withSession {
      val t = queryParam("SELECT name FROM t WHERE id = ANY($1)", Seq(2))
      t.data.map(_.data(0).string) shouldBe IndexedSeq("bob")
    }

    "ANY with string Seq" in withSession {
      val t = queryParam("SELECT name FROM t WHERE name = ANY($1) ORDER BY name", Seq("alice", "dave"))
      t.data.map(_.data(0).string) shouldBe IndexedSeq("alice", "dave")
    }
  }

  // ── Parameter in INSERT ───────────────────────────────────────────

  "parameters in INSERT" - {
    "insert with parameters" in withSession {
      executeSQL("INSERT INTO t VALUES ($1, $2, $3, $4)", IndexedSeq(5, "eve", true, 9.9))
      val t = queryParam("SELECT name FROM t WHERE id = $1", 5)
      t.data(0).data(0).string shouldBe "eve"
    }
  }

  // ── Parameter in UPDATE ───────────────────────────────────────────

  "parameters in UPDATE" - {
    "update with parameters" in withSession {
      executeSQL("UPDATE t SET name = $1 WHERE id = $2", IndexedSeq("alicia", 1))
      val t = queryParam("SELECT name FROM t WHERE id = $1", 1)
      t.data(0).data(0).string shouldBe "alicia"
    }
  }

  // ── Parameter in DELETE ───────────────────────────────────────────

  "parameters in DELETE" - {
    "delete with parameters" in withSession {
      executeSQL("DELETE FROM t WHERE id = $1", IndexedSeq(4))
      val t = queryParam("SELECT count(*) FROM t")
      t.data(0).data(0).intValue shouldBe 3
    }
  }

  // ── Edge cases ────────────────────────────────────────────────────

  "edge cases" - {
    "long parameter" in withSession {
      executeSQL(
        """CREATE TABLE big (id BIGINT);
          |INSERT INTO big VALUES ($1);
          |""".stripMargin,
        IndexedSeq(9999999999L)
      )
      val t = queryParam("SELECT id FROM big")
      t.data(0).data(0).longValue shouldBe 9999999999L
    }

    "parameter with LIKE" in withSession {
      val t = queryParam("SELECT name FROM t WHERE name LIKE $1 ORDER BY name", "a%")
      t.data.map(_.data(0).string) shouldBe IndexedSeq("alice")
    }

    "parameter in LIMIT" in withSession {
      val t = queryParam("SELECT name FROM t ORDER BY id LIMIT $1", 2)
      t.data should have length 2
    }

    "parameter in OFFSET" in withSession {
      val t = queryParam("SELECT name FROM t ORDER BY id LIMIT 2 OFFSET $1", 1)
      t.data(0).data(0).string shouldBe "bob"
    }

    "out-of-range parameter index throws" in withSession {
      assertThrows[ExecutionException] {
        queryParam("SELECT * FROM t WHERE id = $2", 1)
      }
    }

    "zero params with no placeholders" in withSession {
      val t = queryParam("SELECT count(*) FROM t")
      t.data(0).data(0).intValue shouldBe 4
    }
  }

  // ── anyToValue conversion ─────────────────────────────────────────

  "anyToValue conversion" - {
    "Int" in { anyToValue(42) shouldBe a[NumberValue] }
    "Long" in { anyToValue(42L) shouldBe a[NumberValue] }
    "Double" in { anyToValue(3.14) shouldBe a[NumberValue] }
    "Float" in { anyToValue(3.14f) shouldBe a[NumberValue] }
    "Short" in { anyToValue(42.toShort) shouldBe a[NumberValue] }
    "Byte" in { anyToValue(42.toByte) shouldBe a[NumberValue] }
    "BigDecimal" in { anyToValue(BigDecimal("123.45")) shouldBe a[NumberValue] }
    "String" in { anyToValue("hello") shouldBe TextValue("hello") }
    "Boolean true" in { anyToValue(true) shouldBe BooleanValue(true) }
    "Boolean false" in { anyToValue(false) shouldBe BooleanValue(false) }
    "null" in { anyToValue(null) shouldBe a[NullValue] }
    "Seq[Int]" in {
      val v = anyToValue(Seq(1, 2, 3))
      v shouldBe a[ArrayValue]
      v.asInstanceOf[ArrayValue].data.map(_.intValue) shouldBe IndexedSeq(1, 2, 3)
    }
    "Array[String]" in {
      val v = anyToValue(Array("a", "b"))
      v shouldBe a[ArrayValue]
      v.asInstanceOf[ArrayValue].data.map(_.string) shouldBe IndexedSeq("a", "b")
    }
    "empty Seq" in {
      val v = anyToValue(Seq.empty[Int])
      v shouldBe a[ArrayValue]
      v.asInstanceOf[ArrayValue].data shouldBe empty
    }
    "LocalDate" in { anyToValue(java.time.LocalDate.of(2026, 1, 15)) shouldBe DateValue(java.time.LocalDate.of(2026, 1, 15)) }
    "LocalTime" in { anyToValue(java.time.LocalTime.of(10, 30)) shouldBe TimeValue(java.time.LocalTime.of(10, 30)) }
    "LocalDateTime" in {
      val dt = java.time.LocalDateTime.of(2026, 1, 15, 10, 30)
      anyToValue(dt) shouldBe TimestampValue(dt)
    }
    "Duration" in {
      val dur = java.time.Duration.ofHours(2)
      anyToValue(dur) shouldBe IntervalValue(dur)
    }
    "unsupported type throws" in {
      assertThrows[ExecutionException] {
        anyToValue(new Object)
      }
    }
    "Value passes through" in {
      val v = TextValue("hello")
      anyToValue(v) should be theSameInstanceAs v
    }
    "List[Int] (Iterable path)" in {
      val v = anyToValue(List(10, 20, 30))
      v shouldBe a[ArrayValue]
      v.asInstanceOf[ArrayValue].data.map(_.intValue) shouldBe IndexedSeq(10, 20, 30)
    }
    "Set[String] (Iterable path)" in {
      val v = anyToValue(Set("a"))
      v shouldBe a[ArrayValue]
      v.asInstanceOf[ArrayValue].data.map(_.string) shouldBe IndexedSeq("a")
    }
    "Vector[Boolean] (Iterable path)" in {
      val v = anyToValue(Vector(true, false))
      v shouldBe a[ArrayValue]
      v.asInstanceOf[ArrayValue].data.map(_.asInstanceOf[BooleanValue].b) shouldBe IndexedSeq(true, false)
    }
    "nested Seq[Seq[Int]]" in {
      val v = anyToValue(Seq(Seq(1, 2), Seq(3, 4)))
      v shouldBe a[ArrayValue]
      val outer = v.asInstanceOf[ArrayValue].data
      outer should have length 2
      outer(0).asInstanceOf[ArrayValue].data.map(_.intValue) shouldBe IndexedSeq(1, 2)
      outer(1).asInstanceOf[ArrayValue].data.map(_.intValue) shouldBe IndexedSeq(3, 4)
    }
  }

  // ── Iterable parameter in queries ─────────────────────────────────

  "Iterable parameters in queries" - {
    "ANY with List parameter" in withSession {
      val t = queryParam("SELECT name FROM t WHERE id = ANY($1) ORDER BY id", List(1, 3))
      t.data.map(_.data(0).string) shouldBe IndexedSeq("alice", "charlie")
    }

    "ANY with Vector parameter" in withSession {
      val t = queryParam("SELECT name FROM t WHERE name = ANY($1) ORDER BY name", Vector("bob", "dave"))
      t.data.map(_.data(0).string) shouldBe IndexedSeq("bob", "dave")
    }

    "ANY with empty List returns no rows" in withSession {
      val t = queryParam("SELECT * FROM t WHERE id = ANY($1)", List.empty[Int])
      t.data shouldBe empty
    }
  }
}

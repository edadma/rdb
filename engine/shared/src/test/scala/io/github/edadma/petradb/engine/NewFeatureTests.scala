package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import io.github.edadma.dal.{IntType => DIntType, DoubleType => DDoubleType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class NewFeatureTests extends AnyFreeSpec with Matchers with Testing {

  // ── 1. ^ Power Operator ─────────────────────────────────────────

  "power operator (^)" - {
    "computes 2^3 = 8" in {
      val table = query("SELECT 2 ^ 3")
      table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue shouldBe 8.0
    }

    "respects precedence: 2*3^2 = 18" in {
      val table = query("SELECT 2 * 3 ^ 2")
      table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue shouldBe 18.0
    }

    "fractional exponent: 9^0.5 = 3.0" in {
      val table = query("SELECT 9 ^ 0.5")
      table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue shouldBe 3.0
    }
  }

  // ── 2. setseed + random ─────────────────────────────────────────

  "setseed" - {
    "same seed produces same random sequence" in {
      val table = query(
        """
          |SELECT setseed(0.42);
          |SELECT random() AS r1;
          |""".trim.stripMargin
      )
      val r1 = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue

      val table2 = query(
        """
          |SELECT setseed(0.42);
          |SELECT random() AS r1;
          |""".trim.stripMargin
      )
      val r2 = table2.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue

      r1 shouldBe r2
    }
  }

  // ── 3. scale ────────────────────────────────────────────────────

  "scale" - {
    "integer returns 0" in {
      val table = query("SELECT scale(42)")
      table.data(0).data(0).asInstanceOf[NumberValue].value.intValue shouldBe 0
    }
  }

  // ── 4. make_timestamptz ─────────────────────────────────────────

  "make_timestamptz" - {
    "without timezone defaults to UTC" in {
      val table = query("SELECT make_timestamptz(2024, 6, 15, 12, 30, 0)")
      val v = table.data(0).data(0)
      v shouldBe a[TimestampTZValue]
      v.string should include("2024-06-15")
      v.string should include("12:30")
    }

    "with timezone string" in {
      val table = query("SELECT make_timestamptz(2024, 6, 15, 12, 30, 0, '+05:00')")
      val v = table.data(0).data(0)
      v shouldBe a[TimestampTZValue]
      v.string should include("+05:00")
    }
  }

  // ── 5. timetz Type ──────────────────────────────────────────────

  "timetz type" - {
    "create table and insert/select" in {
      val table = query(
        """
          |CREATE TABLE tz_test (id INT, t TIMETZ);
          |INSERT INTO tz_test (id, t) VALUES (1, '10:30:00+02:00');
          |SELECT t FROM tz_test;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0)
      v shouldBe a[TimeTZValue]
      v.string should include("10:30")
    }

    "TIME WITH TIME ZONE syntax" in {
      val table = query(
        """
          |CREATE TABLE tz_test2 (id INT, t TIME WITH TIME ZONE);
          |INSERT INTO tz_test2 (id, t) VALUES (1, '14:00:00+00:00');
          |SELECT t FROM tz_test2;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0)
      v shouldBe a[TimeTZValue]
    }

    "cast string to timetz" in {
      val table = query("SELECT '08:15:00+03:00'::TIMETZ")
      val v = table.data(0).data(0)
      v shouldBe a[TimeTZValue]
    }
  }

  // ── 6. UPDATE RETURNING ─────────────────────────────────────────

  "UPDATE RETURNING" - {
    val setup =
      """
        |CREATE TABLE ret_test (id INT PRIMARY KEY, name TEXT, val INT);
        |INSERT INTO ret_test (id, name, val) VALUES (1, 'a', 10), (2, 'b', 20), (3, 'c', 30);
        |""".trim.stripMargin

    "returns updated rows with RETURNING *" in {
      val table = query(
        s"""
          |$setup
          |UPDATE ret_test SET val = 99 WHERE id = 1 RETURNING *;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      val row = table.data(0).data
      row(0).asInstanceOf[NumberValue].value.intValue shouldBe 1
      row(2).asInstanceOf[NumberValue].value.intValue shouldBe 99
    }

    "returns specific columns" in {
      val table = query(
        s"""
          |$setup
          |UPDATE ret_test SET val = 50 WHERE name = 'b' RETURNING name, val;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("b")
      table.data(0).data(1).asInstanceOf[NumberValue].value.intValue shouldBe 50
    }

    "returns multiple rows" in {
      val table = query(
        s"""
          |$setup
          |UPDATE ret_test SET val = val + 1 RETURNING id;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 3
    }
  }

  // ── 7. DELETE RETURNING ─────────────────────────────────────────

  "DELETE RETURNING" - {
    val setup =
      """
        |CREATE TABLE del_ret_test (id INT PRIMARY KEY, name TEXT);
        |INSERT INTO del_ret_test (id, name) VALUES (1, 'x'), (2, 'y'), (3, 'z');
        |""".trim.stripMargin

    "returns deleted row" in {
      val table = query(
        s"""
          |$setup
          |DELETE FROM del_ret_test WHERE id = 2 RETURNING *;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0).asInstanceOf[NumberValue].value.intValue shouldBe 2
      table.data(0).data(1) shouldBe TextValue("y")
    }

    "returns all deleted rows" in {
      val table = query(
        s"""
          |$setup
          |DELETE FROM del_ret_test RETURNING name;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 3
    }
  }

  // ── 8. format function ──────────────────────────────────────────

  "format function" - {
    "basic %s substitution" in {
      val table = query("SELECT format('Hello %s', 'World')")
      table.data(0).data(0) shouldBe TextValue("Hello World")
    }

    "%I identifier quoting" in {
      val table = query("SELECT format('SELECT * FROM %I', 'my table')")
      table.data(0).data(0) shouldBe TextValue("""SELECT * FROM "my table"""")
    }

    "%L literal quoting" in {
      val table = query("SELECT format('WHERE name = %L', 'O''Brien')")
      table.data(0).data(0) shouldBe TextValue("WHERE name = 'O''Brien'")
    }

    "%% escape" in {
      val table = query("SELECT format('100%%')")
      table.data(0).data(0) shouldBe TextValue("100%")
    }

    "multiple args" in {
      val table = query("SELECT format('%s has %s items', 'cart', 3)")
      table.data(0).data(0) shouldBe TextValue("cart has 3 items")
    }
  }

  // ── 9. EXPLAIN ──────────────────────────────────────────────────

  "EXPLAIN" - {
    "returns non-empty plan text" in {
      val res = results(
        """
          |CREATE TABLE exp_test (id INT, name TEXT);
          |EXPLAIN SELECT * FROM exp_test WHERE id = 1;
          |""".trim.stripMargin
      )
      val explain = res.collect { case e: ExplainResult => e }.head
      explain.plan should not be empty
      explain.plan should include("Scan")
    }
  }

  // ── 10. ON CONFLICT DO NOTHING ──────────────────────────────────

  "ON CONFLICT DO NOTHING" - {
    val setup =
      """
        |CREATE TABLE upsert_test (id INT PRIMARY KEY, name TEXT);
        |INSERT INTO upsert_test (id, name) VALUES (1, 'first');
        |""".trim.stripMargin

    "skips duplicate" in {
      val table = query(
        s"""
          |$setup
          |INSERT INTO upsert_test (id, name) VALUES (1, 'duplicate') ON CONFLICT DO NOTHING;
          |SELECT name FROM upsert_test WHERE id = 1;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("first")
    }

    "inserts non-conflicting rows" in {
      val table = query(
        s"""
          |$setup
          |INSERT INTO upsert_test (id, name) VALUES (2, 'second') ON CONFLICT DO NOTHING;
          |SELECT name FROM upsert_test WHERE id = 2;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("second")
    }

    "multi-row with some conflicts" in {
      val table = query(
        s"""
          |$setup
          |INSERT INTO upsert_test (id, name) VALUES (1, 'dup'), (2, 'new1'), (3, 'new2') ON CONFLICT DO NOTHING;
          |SELECT id, name FROM upsert_test ORDER BY id;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 3
      table.data(0).data(1) shouldBe TextValue("first") // unchanged
      table.data(1).data(1) shouldBe TextValue("new1")
      table.data(2).data(1) shouldBe TextValue("new2")
    }
  }

  // ── 11. generate_series ─────────────────────────────────────────

  "generate_series" - {
    "basic range" in {
      val table = query("SELECT * FROM generate_series(1, 5)")
      table.data.length shouldBe 5
      table.data.map(_.data(0).asInstanceOf[NumberValue].value.intValue) shouldBe Seq(1, 2, 3, 4, 5)
    }

    "with step" in {
      val table = query("SELECT * FROM generate_series(0, 10, 3)")
      table.data.map(_.data(0).asInstanceOf[NumberValue].value.intValue) shouldBe Seq(0, 3, 6, 9)
    }

    "negative step" in {
      val table = query("SELECT * FROM generate_series(5, 1, -1)")
      table.data.map(_.data(0).asInstanceOf[NumberValue].value.intValue) shouldBe Seq(5, 4, 3, 2, 1)
    }

    "with alias" in {
      val table = query("SELECT n FROM generate_series(1, 3) AS g(n)")
      table.data.length shouldBe 3
    }

    "empty range" in {
      val table = query("SELECT * FROM generate_series(5, 1)")
      table.data.length shouldBe 0
    }

    "in CROSS JOIN" in {
      val table = query(
        """
          |CREATE TABLE gs_test (id INT);
          |INSERT INTO gs_test (id) VALUES (10), (20);
          |SELECT gs_test.id, g.n FROM gs_test CROSS JOIN generate_series(1, 2) AS g(n);
          |""".trim.stripMargin
      )
      table.data.length shouldBe 4
    }
  }
}

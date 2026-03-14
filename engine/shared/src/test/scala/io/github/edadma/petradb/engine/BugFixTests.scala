package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BugFixTests extends AnyFreeSpec with Matchers with Testing {

  // ── Bug 1: current_date not recognized as a built-in variable ──────

  "current_date" - {
    "returns a date value without parentheses" in {
      val table = query("SELECT current_date")
      table.data should have length 1
      table.data(0).data(0) shouldBe a[DateValue]
    }

    "works with parentheses as function call" in {
      val table = query("SELECT current_date()")
      table.data(0).data(0) shouldBe a[DateValue]
    }

    "usable in expressions" in {
      // current_date - INTERVAL '1 day' should not throw
      val table = query("SELECT current_date - INTERVAL '1 day'")
      table.data should have length 1
    }
  }

  "current_time" - {
    "returns a time value without parentheses" in {
      val table = query("SELECT current_time")
      table.data should have length 1
      table.data(0).data(0) shouldBe a[TimeValue]
    }
  }

  // ── Bug 2: DISTINCT ON not supported ───────────────────────────────

  "DISTINCT ON" - {
    "basic DISTINCT ON with ORDER BY" in {
      val table = query(
        """CREATE TABLE t (category TEXT, value INT, name TEXT);
          |INSERT INTO t VALUES ('a', 1, 'x'), ('a', 2, 'y'), ('b', 3, 'z'), ('b', 4, 'w');
          |SELECT DISTINCT ON (category) category, value, name FROM t ORDER BY category, value;
          |""".stripMargin
      )
      table.data should have length 2
      table.data(0).data(0).string shouldBe "a"
      table.data(0).data(1).intValue shouldBe 1
      table.data(1).data(0).string shouldBe "b"
      table.data(1).data(1).intValue shouldBe 3
    }

    "DISTINCT ON with multiple columns" in {
      val table = query(
        """CREATE TABLE t (a TEXT, b TEXT, c INT);
          |INSERT INTO t VALUES ('x', 'p', 1), ('x', 'p', 2), ('x', 'q', 3), ('y', 'p', 4);
          |SELECT DISTINCT ON (a, b) a, b, c FROM t ORDER BY a, b, c;
          |""".stripMargin
      )
      table.data should have length 3
    }

    "DISTINCT ON without ORDER BY" in {
      val table = query(
        """CREATE TABLE t (grp TEXT, val INT);
          |INSERT INTO t VALUES ('a', 1), ('a', 2), ('b', 3);
          |SELECT DISTINCT ON (grp) grp, val FROM t;
          |""".stripMargin
      )
      table.data should have length 2
    }

    "regular DISTINCT still works" in {
      val table = query(
        """CREATE TABLE t (x INT);
          |INSERT INTO t VALUES (1), (1), (2), (2), (3);
          |SELECT DISTINCT x FROM t ORDER BY x;
          |""".stripMargin
      )
      table.data should have length 3
    }
  }

  // ── Bug 3: left()/right() functions — parse error ──────────────────

  "left() and right() functions" - {
    "left() extracts leftmost characters" in {
      val table = query("SELECT left('hello', 3)")
      table.data(0).data(0).string shouldBe "hel"
    }

    "right() extracts rightmost characters" in {
      val table = query("SELECT right('hello', 3)")
      table.data(0).data(0).string shouldBe "llo"
    }

    "left() with zero length" in {
      val table = query("SELECT left('hello', 0)")
      table.data(0).data(0).string shouldBe ""
    }

    "right() with length exceeding string" in {
      val table = query("SELECT right('hi', 10)")
      table.data(0).data(0).string shouldBe "hi"
    }

    "left/right as column names" in {
      val table = query(
        """CREATE TABLE t ("left" INT, "right" INT);
          |INSERT INTO t VALUES (1, 2);
          |SELECT "left", "right" FROM t;
          |""".stripMargin
      )
      table.data(0).data(0).intValue shouldBe 1
      table.data(0).data(1).intValue shouldBe 2
    }

    "LEFT JOIN still works" in {
      val table = query(
        """CREATE TABLE a (id INT);
          |CREATE TABLE b (id INT, a_id INT);
          |INSERT INTO a VALUES (1), (2);
          |INSERT INTO b VALUES (10, 1);
          |SELECT a.id, b.id FROM a LEFT JOIN b ON a.id = b.a_id ORDER BY a.id;
          |""".stripMargin
      )
      table.data should have length 2
      table.data(0).data(0).intValue shouldBe 1
      table.data(0).data(1).intValue shouldBe 10
      table.data(1).data(0).intValue shouldBe 2
      table.data(1).data(1).isNull shouldBe true
    }

    "RIGHT JOIN still works" in {
      val table = query(
        """CREATE TABLE a (id INT);
          |CREATE TABLE b (id INT, a_id INT);
          |INSERT INTO a VALUES (1);
          |INSERT INTO b VALUES (10, 1), (20, 2);
          |SELECT a.id, b.id FROM a RIGHT JOIN b ON a.id = b.a_id ORDER BY b.id;
          |""".stripMargin
      )
      table.data should have length 2
    }
  }

  // ── Bug 4a: INTERVAL literal syntax not supported ──────────────────

  "INTERVAL literal syntax" - {
    "basic INTERVAL literal" in {
      val table = query("SELECT INTERVAL '1 day'")
      table.data(0).data(0) shouldBe IntervalValue(java.time.Duration.ofDays(1))
    }

    "INTERVAL with multiple units" in {
      val table = query("SELECT INTERVAL '2 hours 30 minutes'")
      table.data(0).data(0) shouldBe IntervalValue(java.time.Duration.ofHours(2).plusMinutes(30))
    }

    "date + INTERVAL" in {
      val today = java.time.LocalDate.now(java.time.ZoneOffset.UTC)
      val table = query("SELECT current_date + INTERVAL '1 day'")
      table.data(0).data(0) shouldBe TimestampValue(today.plusDays(1).atStartOfDay)
    }

    "date - INTERVAL" in {
      val today = java.time.LocalDate.now(java.time.ZoneOffset.UTC)
      val table = query("SELECT current_date - INTERVAL '1 day'")
      table.data(0).data(0) shouldBe TimestampValue(today.minusDays(1).atStartOfDay)
    }

    "INTERVAL arithmetic" in {
      val table = query("SELECT INTERVAL '1 day' + INTERVAL '2 hours'")
      table.data(0).data(0) shouldBe IntervalValue(java.time.Duration.ofDays(1).plusHours(2))
    }

    "INTERVAL with ::interval cast still works" in {
      val table = query("SELECT '3 days'::interval")
      table.data(0).data(0) shouldBe IntervalValue(java.time.Duration.ofDays(3))
    }
  }

  // ── Bug 5: JSONB #>/#>> after ::jsonb cast ─────────────────────────

  "JSONB path operators after cast" - {
    "#> navigates nested path" in {
      val table = query("""SELECT '{"a": {"b": {"c": 42}}}'::jsonb #> ARRAY['a', 'b', 'c']""")
      table.data(0).data(0).intValue shouldBe 42
    }

    "#>> returns text" in {
      val table = query("""SELECT '{"a": {"b": "hello"}}'::jsonb #>> ARRAY['a', 'b']""")
      table.data(0).data(0).string shouldBe "hello"
    }

    "-> after cast" in {
      val table = query("""SELECT '{"a": 1}'::jsonb -> 'a'""")
      table.data(0).data(0).intValue shouldBe 1
    }

    "->> after cast" in {
      val table = query("""SELECT '{"a": "hello"}'::jsonb ->> 'a'""")
      table.data(0).data(0).string shouldBe "hello"
    }

    "chained cast and access" in {
      val table = query("""SELECT ('{"x": [1,2,3]}'::jsonb -> 'x') ->> 1""")
      table.data(0).data(0).string shouldBe "2"
    }
  }
}

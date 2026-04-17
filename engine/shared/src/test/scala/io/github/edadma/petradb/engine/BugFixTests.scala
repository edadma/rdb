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

  // ── Bug 6: 'type' should not be a reserved word ────────────────────

  "type as column name" - {
    "CREATE TABLE with unquoted type column" in {
      val res = results("CREATE TABLE steps (id UUID PRIMARY KEY, type TEXT NOT NULL)")
      res.head shouldBe a[CreateTableResult]
    }

    "INSERT and SELECT with type column" in {
      val table = query(
        """CREATE TABLE steps (id UUID PRIMARY KEY, type TEXT NOT NULL);
          |INSERT INTO steps (type) VALUES ('pickup');
          |INSERT INTO steps (type) VALUES ('dropoff');
          |SELECT type FROM steps ORDER BY type;
          |""".stripMargin
      )
      table.data should have length 2
      table.data(0).data(0).string shouldBe "dropoff"
      table.data(1).data(0).string shouldBe "pickup"
    }

    "CREATE TYPE still works" in {
      val res = results("CREATE TYPE color AS ENUM ('red', 'green', 'blue')")
      res.head shouldBe a[CreateTypeResult]
    }
  }

  // ── Bug 7: Self-referential FK in CREATE TABLE ─────────────────────

  "self-referential foreign key" - {
    "column-level REFERENCES to own table" in {
      val res = results(
        "CREATE TABLE trips (id UUID PRIMARY KEY, state TEXT NOT NULL, return_trip_for_id UUID REFERENCES trips(id))"
      )
      res.head shouldBe a[CreateTableResult]
    }

    "self-referential FK insert works" in {
      val table = query(
        """CREATE TABLE categories (id SERIAL PRIMARY KEY, name TEXT NOT NULL, parent_id INT REFERENCES categories(id));
          |INSERT INTO categories (name) VALUES ('Root');
          |INSERT INTO categories (name, parent_id) VALUES ('Child', 1);
          |SELECT name, parent_id FROM categories ORDER BY id;
          |""".stripMargin
      )
      table.data should have length 2
      table.data(0).data(0).string shouldBe "Root"
      table.data(1).data(0).string shouldBe "Child"
      table.data(1).data(1).intValue shouldBe 1
    }

    "self-referential FK rejects invalid column" in {
      an[UndefinedReferenceException] should be thrownBy {
        results("CREATE TABLE nodes (id SERIAL PRIMARY KEY, parent_id INT REFERENCES nodes(nonexistent))")
      }
    }

    "non-self FK still validated" in {
      an[UndefinedReferenceException] should be thrownBy {
        results("CREATE TABLE child (id INT, parent_id INT REFERENCES no_such_table(id))")
      }
    }
  }

  // ── Bug 8: Text-to-timestamp coercion in comparisons ───────────────

  "text-to-timestamp coercion" - {
    "text param compared to TIMESTAMP column" in {
      given session: Session = setupSession(
        """CREATE TABLE events (id SERIAL, name TEXT, created_at TIMESTAMP);
          |INSERT INTO events (name, created_at) VALUES ('a', '2024-06-15T10:00:00');
          |INSERT INTO events (name, created_at) VALUES ('b', '2024-07-15T10:00:00');
          |""".stripMargin
      )
      val res = executeSQL("SELECT name FROM events WHERE created_at BETWEEN $1 AND $2 ORDER BY name",
        IndexedSeq("2024-06-01T00:00:00", "2024-06-30T23:59:59"))
      val table = res.collect { case QueryResult(t) => t }.head
      table.data should have length 1
      table.data(0).data(0).string shouldBe "a"
    }

    "text param compared to DATE column" in {
      given session: Session = setupSession(
        """CREATE TABLE logs (id SERIAL, d DATE);
          |INSERT INTO logs (d) VALUES ('2024-01-15');
          |INSERT INTO logs (d) VALUES ('2024-02-15');
          |""".stripMargin
      )
      val res = executeSQL("SELECT d FROM logs WHERE d > $1 ORDER BY d", IndexedSeq("2024-01-31"))
      val table = res.collect { case QueryResult(t) => t }.head
      table.data should have length 1
    }

    "ISO-8601 with timezone coerced to timestamp" in {
      given session: Session = setupSession(
        """CREATE TABLE events (id SERIAL, ts TIMESTAMP);
          |INSERT INTO events (ts) VALUES ('2024-06-15T10:00:00');
          |""".stripMargin
      )
      val res = executeSQL("SELECT * FROM events WHERE ts >= $1", IndexedSeq("2024-06-01T00:00:00Z"))
      val table = res.collect { case QueryResult(t) => t }.head
      table.data should have length 1
    }
  }

  // ── Bug 9: Subquery expression in WHERE clause ─────────────────────

  "subquery expression in WHERE" - {
    "scalar subquery compared to value" in {
      val table = query(
        """CREATE TABLE vehicles (id SERIAL PRIMARY KEY, make TEXT);
          |CREATE TABLE drivers (id SERIAL PRIMARY KEY, vehicle_id INT REFERENCES vehicles(id), name TEXT);
          |INSERT INTO vehicles (make) VALUES ('Toyota');
          |INSERT INTO vehicles (make) VALUES ('Honda');
          |INSERT INTO drivers (vehicle_id, name) VALUES (1, 'Alice');
          |SELECT make FROM vehicles WHERE (SELECT count(*) FROM drivers WHERE drivers.vehicle_id = vehicles.id) = 0 ORDER BY make;
          |""".stripMargin
      )
      table.data should have length 1
      table.data(0).data(0).string shouldBe "Honda"
    }

    "scalar subquery with greater-than" in {
      val table = query(
        """CREATE TABLE departments (id SERIAL PRIMARY KEY, name TEXT);
          |CREATE TABLE employees (id SERIAL PRIMARY KEY, dept_id INT, name TEXT);
          |INSERT INTO departments (name) VALUES ('Engineering');
          |INSERT INTO departments (name) VALUES ('Sales');
          |INSERT INTO employees (dept_id, name) VALUES (1, 'Alice');
          |INSERT INTO employees (dept_id, name) VALUES (1, 'Bob');
          |INSERT INTO employees (dept_id, name) VALUES (2, 'Charlie');
          |SELECT name FROM departments WHERE (SELECT count(*) FROM employees WHERE employees.dept_id = departments.id) > 1;
          |""".stripMargin
      )
      table.data should have length 1
      table.data(0).data(0).string shouldBe "Engineering"
    }
  }
}

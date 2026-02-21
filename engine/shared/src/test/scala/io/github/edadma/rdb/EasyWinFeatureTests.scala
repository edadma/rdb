package io.github.edadma.rdb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class EasyWinFeatureTests extends AnyFreeSpec with Matchers with Testing {

  // ── Math functions ──────────────────────────────────────────────────

  "cbrt" - {
    "returns cube root" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT cbrt(27) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 3.0 +- 0.0001
    }

    "returns cube root of negative number" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT cbrt(-8) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe -2.0 +- 0.0001
    }
  }

  "div" - {
    "performs integer division" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT div(7, 2) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 3.0
    }

    "truncates toward zero" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT div(17, 5) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 3.0
    }
  }

  "factorial" - {
    "computes factorial of 5" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT factorial(5) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 120.0
    }

    "computes factorial of 0" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT factorial(0) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 1.0
    }

    "computes factorial of 1" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT factorial(1) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 1.0
    }
  }

  "gcd" - {
    "computes greatest common divisor" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT gcd(12, 8) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 4.0
    }

    "gcd with zero" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT gcd(7, 0) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 7.0
    }
  }

  "lcm" - {
    "computes least common multiple" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT lcm(4, 6) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 12.0
    }

    "lcm with zero" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT lcm(5, 0) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 0.0
    }
  }

  "sinh" - {
    "computes hyperbolic sine of 0" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT sinh(0) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(0.0)
    }

    "computes hyperbolic sine of 1" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT sinh(1) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe math.sinh(1.0) +- 0.0001
    }
  }

  "cosh" - {
    "computes hyperbolic cosine of 0" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT cosh(0) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(1.0)
    }
  }

  "tanh" - {
    "computes hyperbolic tangent of 0" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT tanh(0) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(0.0)
    }
  }

  "asinh" - {
    "computes inverse hyperbolic sine" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT asinh(0) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 0.0 +- 0.0001
    }

    "round-trips with sinh" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT asinh(sinh(1.5)) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 1.5 +- 0.0001
    }
  }

  "acosh" - {
    "computes inverse hyperbolic cosine of 1" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT acosh(1) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 0.0 +- 0.0001
    }

    "round-trips with cosh" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT acosh(cosh(2.0)) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 2.0 +- 0.0001
    }
  }

  "atanh" - {
    "computes inverse hyperbolic tangent of 0" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT atanh(0) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 0.0 +- 0.0001
    }

    "round-trips with tanh" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT atanh(tanh(0.5)) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 0.5 +- 0.0001
    }
  }

  // ── Array functions ─────────────────────────────────────────────────

  "array_cat" - {
    "concatenates two arrays" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT array_length(array_cat(ARRAY[1, 2], ARRAY[3, 4])) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(4)
    }
  }

  "array_lower" - {
    "always returns 1" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT array_lower(ARRAY[10, 20, 30], 1) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(1)
    }
  }

  "array_upper" - {
    "returns array length" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT array_upper(ARRAY[10, 20, 30], 1) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(3)
    }
  }

  "array_ndims" - {
    "returns 1 for flat arrays" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT array_ndims(ARRAY[1, 2, 3]) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(1)
    }
  }

  "array_replace" - {
    "replaces matching elements" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT array_to_string(array_replace(ARRAY['a', 'b', 'a', 'c'], 'a', 'x'), ',') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("x,b,x,c")
    }

    "no match leaves array unchanged" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT array_length(array_replace(ARRAY[1, 2, 3], 9, 0)) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(3)
    }
  }

  "cardinality" - {
    "returns array length" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT cardinality(ARRAY[10, 20, 30, 40]) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(4)
    }

    "returns 0 for empty array" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT cardinality(ARRAY[]) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(0)
    }
  }

  // ── String functions ────────────────────────────────────────────────

  "translate" - {
    "replaces characters" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT translate('hello', 'helo', 'HELO') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("HELLO")
    }

    "deletes characters with shorter to string" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT translate('hello world', 'lo', 'L') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("heLL wrLd")
    }

    "PostgreSQL-style vowel removal" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT translate('abcdef', 'ace', 'XY') FROM t;
          |""".trim.stripMargin
      )
      // a->X, c->Y, e->deleted (no mapping)
      table.data(0).data(0) shouldBe TextValue("XbYdf")
    }
  }

  "btrim" - {
    "trims whitespace with no second arg" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT btrim('  hello  ') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("hello")
    }

    "trims specific characters" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT btrim('xxhelloxx', 'x') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("hello")
    }

    "trims multiple characters from set" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT btrim('xyxhelloyx', 'xy') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("hello")
    }

    "returns empty string when all characters trimmed" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT btrim('aaa', 'a') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("")
    }
  }

  // ── Date/time functions ─────────────────────────────────────────────

  "make_timestamp" - {
    "creates timestamp from parts" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT make_timestamp(2024, 6, 15, 14, 30, 0) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe TimestampType
      table.data(0).data(0).string shouldBe "2024-06-15T14:30"
    }

    "creates timestamp with seconds" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT make_timestamp(2024, 1, 1, 0, 0, 0) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).string shouldBe "2024-01-01T00:00"
    }
  }

  "make_interval" - {
    "creates interval from days" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT make_interval(5) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe IntervalType
      table.data(0).data(0).string shouldBe "5 days"
    }

    "creates interval from days and hours" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT make_interval(1, 12) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).string shouldBe "1 day 12 hours"
    }

    "creates interval from days, hours, mins, secs" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT make_interval(0, 2, 30, 0) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).string shouldBe "2 hours 30 minutes"
    }
  }

  "to_number" - {
    "parses numeric string" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT to_number('12345.67', '99999.99') FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 12345.67 +- 0.001
    }
  }

  "isfinite" - {
    "returns true for date" in {
      val table = query(
        """
          |CREATE TABLE t (val DATE);
          |INSERT INTO t (val) VALUES ('2024-06-15');
          |SELECT isfinite(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "returns true for timestamp" in {
      val table = query(
        """
          |CREATE TABLE t (val TIMESTAMP);
          |INSERT INTO t (val) VALUES ('2024-06-15 12:00:00');
          |SELECT isfinite(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
    }
  }

  // ── every aggregate ─────────────────────────────────────────────────

  "every" - {
    "returns true when all true" in {
      val table = query(
        """
          |CREATE TABLE t (val BOOLEAN);
          |INSERT INTO t (val) VALUES (TRUE), (TRUE), (TRUE);
          |SELECT every(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "returns false when any false" in {
      val table = query(
        """
          |CREATE TABLE t (val BOOLEAN);
          |INSERT INTO t (val) VALUES (TRUE), (FALSE), (TRUE);
          |SELECT every(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(false)
    }

    "with GROUP BY" in {
      val table = query(
        """
          |CREATE TABLE t (grp TEXT, val BOOLEAN);
          |INSERT INTO t (grp, val) VALUES ('a', TRUE), ('a', TRUE), ('b', TRUE), ('b', FALSE);
          |SELECT grp, every(val) FROM t GROUP BY grp ORDER BY grp;
          |""".trim.stripMargin
      )
      table.data(0).data(1) shouldBe BooleanValue(true)
      table.data(1).data(1) shouldBe BooleanValue(false)
    }
  }

  // ── % modulo operator ──────────────────────────────────────────────

  "modulo operator (%)" - {
    "computes remainder" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT 10 % 3 FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 1.0
    }

    "works with column values" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b INT);
          |INSERT INTO t (a, b) VALUES (17, 5), (10, 4), (9, 3);
          |SELECT a % b FROM t;
          |""".trim.stripMargin
      )
      val results = table.data.map(_.data(0).asInstanceOf[NumberValue].value.doubleValue)
      results shouldBe Seq(2.0, 2.0, 0.0)
    }

    "respects operator precedence with addition" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT 2 + 10 % 3 FROM t;
          |""".trim.stripMargin
      )
      // % has same precedence as * and /, higher than +
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 3.0
    }
  }

  // ── Bitwise operators ─────────────────────────────────────────────

  "bitwise AND (&)" - {
    "computes bitwise AND" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT 6 & 3 FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).asInstanceOf[NumberValue].value.longValue shouldBe 2L
    }
  }

  "bitwise OR (|)" - {
    "computes bitwise OR" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT 6 | 3 FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).asInstanceOf[NumberValue].value.longValue shouldBe 7L
    }
  }

  "bitwise XOR (#)" - {
    "computes bitwise XOR" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT 6 # 3 FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).asInstanceOf[NumberValue].value.longValue shouldBe 5L
    }
  }

  "bitwise NOT (~)" - {
    "computes bitwise NOT" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT ~1 FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).asInstanceOf[NumberValue].value.longValue shouldBe -2L
    }
  }

  "left shift (<<)" - {
    "shifts bits left" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT 1 << 4 FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).asInstanceOf[NumberValue].value.longValue shouldBe 16L
    }
  }

  "right shift (>>)" - {
    "shifts bits right" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT 16 >> 2 FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).asInstanceOf[NumberValue].value.longValue shouldBe 4L
    }
  }

  "bitwise precedence" - {
    "arithmetic binds tighter than bitwise" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT 2 + 3 & 7 FROM t;
          |""".trim.stripMargin
      )
      // (2 + 3) & 7 = 5 & 7 = 5
      table.data(0).data(0).asInstanceOf[NumberValue].value.longValue shouldBe 5L
    }
  }

  "bitwise with column values" - {
    "works with table columns" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b INT);
          |INSERT INTO t (a, b) VALUES (12, 10), (7, 3);
          |SELECT a & b FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).asInstanceOf[NumberValue].value.longValue shouldBe 8L
      table.data(1).data(0).asInstanceOf[NumberValue].value.longValue shouldBe 3L
    }
  }

  // ── VARCHAR type ────────────────────────────────────────────────────

  "VARCHAR type" - {
    "creates table with VARCHAR(n) column" in {
      val table = query(
        """
          |CREATE TABLE t (name VARCHAR(5));
          |INSERT INTO t (name) VALUES ('hello');
          |SELECT name FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("hello")
    }

    "truncates values exceeding length" in {
      val table = query(
        """
          |CREATE TABLE t (name VARCHAR(3));
          |INSERT INTO t (name) VALUES ('hello');
          |SELECT name FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("hel")
    }

    "does not pad short values (unlike CHAR)" in {
      val table = query(
        """
          |CREATE TABLE t (name VARCHAR(10));
          |INSERT INTO t (name) VALUES ('hi');
          |SELECT length(name) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(2)
    }

    "bare VARCHAR without length acts as TEXT" in {
      val table = query(
        """
          |CREATE TABLE t (name VARCHAR);
          |INSERT INTO t (name) VALUES ('this can be any length');
          |SELECT name FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("this can be any length")
    }
  }

  // ── CHECK constraints ───────────────────────────────────────────────

  "CHECK constraint" - {
    "valid insert succeeds" in {
      val table = query(
        """
          |CREATE TABLE products (name TEXT, price INT, CHECK (price > 0));
          |INSERT INTO products (name, price) VALUES ('widget', 10);
          |SELECT name, price FROM products;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("widget")
      table.data(0).data(1) shouldBe NumberValue(10)
    }

    "invalid insert fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE products (name TEXT, price INT, CHECK (price > 0));
            |INSERT INTO products (name, price) VALUES ('widget', -5);
            |""".trim.stripMargin
        )
      }
    }

    "named constraint error includes name" in {
      val ex = the[Exception] thrownBy {
        results(
          """
            |CREATE TABLE products (name TEXT, price INT, CONSTRAINT positive_price CHECK (price > 0));
            |INSERT INTO products (name, price) VALUES ('widget', 0);
            |""".trim.stripMargin
        )
      }
      ex.getMessage should include("positive_price")
    }

    "multi-column check" in {
      val table = query(
        """
          |CREATE TABLE events (start_val INT, end_val INT, CHECK (start_val < end_val));
          |INSERT INTO events (start_val, end_val) VALUES (1, 10);
          |SELECT start_val, end_val FROM events;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(0).data(1) shouldBe NumberValue(10)
    }

    "multi-column check rejects invalid" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE events (start_val INT, end_val INT, CHECK (start_val < end_val));
            |INSERT INTO events (start_val, end_val) VALUES (10, 5);
            |""".trim.stripMargin
        )
      }
    }

    "column-level check" in {
      val table = query(
        """
          |CREATE TABLE t (val INT CHECK (val >= 0));
          |INSERT INTO t (val) VALUES (5);
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(5)
    }

    "column-level check rejects invalid" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (val INT CHECK (val >= 0));
            |INSERT INTO t (val) VALUES (-1);
            |""".trim.stripMargin
        )
      }
    }

    "update violating check fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE products (name TEXT, price INT, CHECK (price > 0));
            |INSERT INTO products (name, price) VALUES ('widget', 10);
            |UPDATE products SET price = -1;
            |""".trim.stripMargin
        )
      }
    }

    "update respecting check succeeds" in {
      val table = query(
        """
          |CREATE TABLE products (name TEXT, price INT, CHECK (price > 0));
          |INSERT INTO products (name, price) VALUES ('widget', 10);
          |UPDATE products SET price = 20;
          |SELECT price FROM products;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(20)
    }

    "multiple checks on same table" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b INT, CHECK (a > 0), CHECK (b > 0));
          |INSERT INTO t (a, b) VALUES (1, 2);
          |SELECT a, b FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(0).data(1) shouldBe NumberValue(2)
    }

    "multiple checks - second violated" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (a INT, b INT, CHECK (a > 0), CHECK (b > 0));
            |INSERT INTO t (a, b) VALUES (1, -1);
            |""".trim.stripMargin
        )
      }
    }

    "check with IN expression" in {
      val table = query(
        """
          |CREATE TABLE t (status TEXT, CHECK (status IN ('active', 'inactive')));
          |INSERT INTO t (status) VALUES ('active');
          |SELECT status FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("active")
    }

    "check with IN expression rejects invalid" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (status TEXT, CHECK (status IN ('active', 'inactive')));
            |INSERT INTO t (status) VALUES ('deleted');
            |""".trim.stripMargin
        )
      }
    }
  }
}

package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ArrayDateFunctionTests extends AnyFreeSpec with Matchers with Testing {

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
}

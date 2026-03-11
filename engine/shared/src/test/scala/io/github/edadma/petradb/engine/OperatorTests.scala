package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class OperatorTests extends AnyFreeSpec with Matchers with Testing {

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
      val v = table.data(0).data(0).doubleValue
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
      val results = table.data.map(_.data(0).doubleValue)
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
      val v = table.data(0).data(0).doubleValue
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
      table.data(0).data(0).longValue shouldBe 2L
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
      table.data(0).data(0).longValue shouldBe 7L
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
      table.data(0).data(0).longValue shouldBe 5L
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
      table.data(0).data(0).longValue shouldBe -2L
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
      table.data(0).data(0).longValue shouldBe 16L
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
      table.data(0).data(0).longValue shouldBe 4L
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
      table.data(0).data(0).longValue shouldBe 5L
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
      table.data(0).data(0).longValue shouldBe 8L
      table.data(1).data(0).longValue shouldBe 3L
    }
  }
}

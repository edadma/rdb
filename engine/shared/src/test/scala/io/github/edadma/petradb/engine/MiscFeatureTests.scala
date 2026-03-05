package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MiscFeatureTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE t (id INT, name TEXT, val INT);
      |INSERT INTO t (id, name, val) VALUES (1, 'Thomas', 10), (2, 'Hello World', 25), (3, 'ABC', 50);
      |""".trim.stripMargin

  // ── overlay ────────────────────────────────────────────────────────

  "overlay" - {
    "replaces substring with FOR count" in {
      val table = query(s"$setup SELECT overlay(name PLACING 'XX' FROM 4 FOR 3) FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe TextValue("ThoXX")
    }

    "replaces substring without FOR (defaults to replacement length)" in {
      val table = query(s"$setup SELECT overlay(name PLACING 'YY' FROM 1) FROM t WHERE id = 3;")
      table.data(0).data(0) shouldBe TextValue("YYC")
    }

    "inserts when FOR 0" in {
      val table = query(s"$setup SELECT overlay(name PLACING '--' FROM 4 FOR 0) FROM t WHERE id = 3;")
      table.data(0).data(0) shouldBe TextValue("ABC--")
    }
  }

  // ── width_bucket ───────────────────────────────────────────────────

  "width_bucket" - {
    "value in middle bucket" in {
      val table = query(s"$setup SELECT width_bucket(val, 0, 100, 4) FROM t WHERE id = 2;")
      table.data(0).data(0) shouldBe NumberValue(2)
    }

    "value below range returns 0" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT width_bucket(-5, 0, 100, 10) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(0)
    }

    "value at or above max returns count+1" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT width_bucket(100, 0, 100, 10) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(11)
    }
  }

  // ── get_byte / set_byte ────────────────────────────────────────────

  "get_byte / set_byte" - {
    "get_byte extracts byte" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, data BYTEA);
          |INSERT INTO t (id, data) VALUES (1, E'\\x01020304');
          |SELECT get_byte(data, 2) FROM t WHERE id = 1;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(3)
    }

    "set_byte replaces byte" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, data BYTEA);
          |INSERT INTO t (id, data) VALUES (1, E'\\x01020304');
          |SELECT set_byte(data, 1, 255) FROM t WHERE id = 1;
          |""".trim.stripMargin
      )
      val result = table.data(0).data(0).asInstanceOf[ByteaValue]
      (result.data(1) & 0xff) shouldBe 255
    }
  }

  // ── IS DISTINCT FROM / IS NOT DISTINCT FROM ────────────────────────

  "IS DISTINCT FROM" - {
    "null vs null is not distinct" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, a INT, b INT);
          |INSERT INTO t (id, a, b) VALUES (1, null, null);
          |SELECT id FROM t WHERE a IS NOT DISTINCT FROM b;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
    }

    "null vs value is distinct" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, a INT, b INT);
          |INSERT INTO t (id, a, b) VALUES (1, null, 5);
          |SELECT id FROM t WHERE a IS DISTINCT FROM b;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
    }

    "equal values are not distinct" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, a INT, b INT);
          |INSERT INTO t (id, a, b) VALUES (1, 5, 5);
          |SELECT id FROM t WHERE a IS NOT DISTINCT FROM b;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
    }

    "different values are distinct" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, a INT, b INT);
          |INSERT INTO t (id, a, b) VALUES (1, 5, 10);
          |SELECT id FROM t WHERE a IS DISTINCT FROM b;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
    }

    "filters correctly across rows" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, a INT, b INT);
          |INSERT INTO t (id, a, b) VALUES (1, null, null), (2, null, 5), (3, 5, 5), (4, 5, 10);
          |SELECT id FROM t WHERE a IS DISTINCT FROM b;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 2
      table.data.map(_.data(0).asInstanceOf[NumberValue].value.intValue) shouldBe Seq(2, 4)
    }
  }

  // ── BETWEEN SYMMETRIC ─────────────────────────────────────────────

  "BETWEEN SYMMETRIC" - {
    "matches when bounds are in order" in {
      val table = query(s"$setup SELECT id FROM t WHERE val BETWEEN SYMMETRIC 10 AND 30;")
      table.data.map(_.data(0).asInstanceOf[NumberValue].value.intValue) shouldBe Seq(1, 2)
    }

    "matches when bounds are reversed" in {
      val table = query(s"$setup SELECT id FROM t WHERE val BETWEEN SYMMETRIC 30 AND 10;")
      table.data.map(_.data(0).asInstanceOf[NumberValue].value.intValue) shouldBe Seq(1, 2)
    }

    "NOT BETWEEN SYMMETRIC" in {
      val table = query(s"$setup SELECT id FROM t WHERE val NOT BETWEEN SYMMETRIC 30 AND 10;")
      table.data.map(_.data(0).asInstanceOf[NumberValue].value.intValue) shouldBe Seq(3)
    }
  }
}

package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TimestampComparisonTests extends AnyFreeSpec with Matchers with Testing {

  // A `timestamp` (without time zone) is comparable to a `timestamptz`, like PostgreSQL. With no
  // session zone tracked, the naive operand is read as UTC and the two are compared as instants. The
  // fixtures below pin that: the timestamp '2024-06-01T12:00:00' is the instant 12:00Z.
  //
  // tzEqual names 12:00Z (equal), tzEarlier 10:00Z, tzLater 16:00Z.

  private val ts      = "'2024-06-01T12:00:00'::timestamp"
  private val tzEqual = "make_timestamptz(2024, 6, 1, 12, 0, 0, '+00:00')"
  private val tzEarly = "make_timestamptz(2024, 6, 1, 10, 0, 0, '+00:00')"
  private val tzLate  = "make_timestamptz(2024, 6, 1, 16, 0, 0, '+00:00')"

  private def bool(expr: String): Value = query(s"SELECT $expr").data(0).data(0)

  "timestamp vs timestamptz — every operator, both orders" - {
    "=" in {
      bool(s"$ts = $tzEqual") shouldBe BooleanValue(true)
      bool(s"$tzEqual = $ts") shouldBe BooleanValue(true)
      bool(s"$ts = $tzLate") shouldBe BooleanValue(false)
    }
    "<>" in {
      bool(s"$ts <> $tzLate") shouldBe BooleanValue(true)
      bool(s"$tzLate <> $ts") shouldBe BooleanValue(true)
      bool(s"$ts <> $tzEqual") shouldBe BooleanValue(false)
    }
    "<" in {
      bool(s"$ts < $tzLate") shouldBe BooleanValue(true)
      bool(s"$tzEarly < $ts") shouldBe BooleanValue(true)
      bool(s"$ts < $tzEqual") shouldBe BooleanValue(false)
    }
    "<=" in {
      bool(s"$ts <= $tzEqual") shouldBe BooleanValue(true)
      bool(s"$tzEqual <= $ts") shouldBe BooleanValue(true)
      bool(s"$tzLate <= $ts") shouldBe BooleanValue(false)
    }
    ">" in {
      bool(s"$ts > $tzEarly") shouldBe BooleanValue(true)
      bool(s"$tzLate > $ts") shouldBe BooleanValue(true)
      bool(s"$ts > $tzEqual") shouldBe BooleanValue(false)
    }
    ">=" in {
      bool(s"$ts >= $tzEqual") shouldBe BooleanValue(true)
      bool(s"$tzEqual >= $ts") shouldBe BooleanValue(true)
      bool(s"$ts >= $tzLate") shouldBe BooleanValue(false)
    }
  }

  // Proves the comparison is by instant, honoring the offset — not a naive field-by-field compare.
  // 14:00+02:00 is the same instant as the timestamp's 12:00Z, so they are equal.
  "comparison honors the timestamptz offset (instant equality)" in {
    bool(s"$ts = make_timestamptz(2024, 6, 1, 14, 0, 0, '+02:00')") shouldBe BooleanValue(true)
    bool(s"$ts = make_timestamptz(2024, 6, 1, 12, 0, 0, '+02:00')") shouldBe BooleanValue(false)
  }

  "BETWEEN spans timestamptz bounds" in {
    bool(s"$ts BETWEEN $tzEarly AND $tzLate") shouldBe BooleanValue(true)
    bool(s"$tzEqual BETWEEN $ts AND $tzLate") shouldBe BooleanValue(true)
    bool(s"$ts BETWEEN $tzLate AND $tzEarly") shouldBe BooleanValue(false)
  }

  // The original report: a timestamp column compared to CURRENT_TIMESTAMP (a timestamptz) in WHERE
  // and in the projection must compare, not throw. (The `query` helper opens a fresh database per
  // call, so the table and the queries against it live in one multi-statement script.)
  "a timestamp column compares to CURRENT_TIMESTAMP in WHERE" in {
    val table = query(
      """
        |CREATE TABLE t (id int, at timestamp);
        |INSERT INTO t VALUES (1, '2024-01-01T00:00:00'), (2, '2999-01-01T00:00:00');
        |SELECT id FROM t WHERE at <= CURRENT_TIMESTAMP ORDER BY id;
        |""".trim.stripMargin
    )
    table.data.map(_.data(0)) shouldBe Vector(NumberValue(1))
  }

  "a timestamp column compares to CURRENT_TIMESTAMP in the projection" in {
    val table = query(
      """
        |CREATE TABLE t (id int, at timestamp);
        |INSERT INTO t VALUES (1, '2024-01-01T00:00:00'), (2, '2999-01-01T00:00:00');
        |SELECT at <= CURRENT_TIMESTAMP AS past FROM t ORDER BY id;
        |""".trim.stripMargin
    )
    table.data.map(_.data(0)) shouldBe Vector(BooleanValue(true), BooleanValue(false))
  }
}

package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.time.OffsetDateTime

class CastTypeAliasTests extends AnyFreeSpec with Matchers with Testing {

  // PostgreSQL accepts `timestamptz` / `timetz` as aliases for the `... WITH TIME ZONE` spellings,
  // both as cast targets and as column types. Living in the shared test build, these run on every
  // platform (JVM, JS, Native).

  "timestamptz" - {
    "is a cast target via ::" in {
      val v = query("SELECT '2024-01-15T10:30:00+05:00'::timestamptz").data(0).data(0)
      v shouldBe a[TimestampTZValue]
      val TimestampTZValue(t) = v: @unchecked
      t shouldBe OffsetDateTime.parse("2024-01-15T10:30:00+05:00")
    }

    "is a cast target via CAST(... AS timestamptz)" in {
      query("SELECT CAST('2024-01-15T10:30:00+05:00' AS timestamptz)").data(0).data(0).vtyp shouldBe TimestampTZType
    }

    "is usable as a column type" in {
      val table = query(
        """
          |CREATE TABLE t (ts timestamptz);
          |INSERT INTO t (ts) VALUES ('2024-01-15T10:30:00+05:00');
          |SELECT ts FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe TimestampTZType
      table.meta.columns(0).typ shouldBe TimestampTZType
    }

    "is equivalent to TIMESTAMP WITH TIME ZONE" in {
      val viaAlias = query("SELECT '2024-01-15T10:30:00+05:00'::timestamptz").data(0).data(0)
      val viaSpell = query("SELECT '2024-01-15T10:30:00+05:00'::timestamp with time zone").data(0).data(0)
      viaAlias shouldBe viaSpell
    }
  }

  "timetz" - {
    "is a cast target via ::" in {
      query("SELECT '10:30:00+05:00'::timetz").data(0).data(0) shouldBe a[TimeTZValue]
    }

    "is usable as a column type" in {
      val table = query(
        """
          |CREATE TABLE t (tm timetz);
          |INSERT INTO t (tm) VALUES ('10:30:00+05:00');
          |SELECT tm FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe TimeTZType
      table.meta.columns(0).typ shouldBe TimeTZType
    }
  }
}

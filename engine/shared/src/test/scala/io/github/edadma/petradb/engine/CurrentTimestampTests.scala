package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Instant, LocalDate, OffsetDateTime, ZoneId}

class CurrentTimestampTests extends AnyFreeSpec with Matchers with Testing {

  // CURRENT_TIMESTAMP / now() / clock_timestamp() are `timestamp with time zone` in PostgreSQL:
  // they capture a real instant, not a zone-naive wall clock. These tests pin both the type and the
  // instant — comparing the returned value against the test's own wall clock — so that a regression
  // to a zone-naive UTC value (the shape that previously caused timezone shifting across the
  // JS/JVM/Native value bridges) fails loudly.

  private val toleranceMillis = 2L

  private def instantOf(v: Value): Instant =
    v match
      case TimestampTZValue(t) => t.toInstant
      case TimestampValue(t)   => t.atZone(ZoneId.systemDefault).toInstant
      case other               => fail(s"expected a timestamp value, got $other")

  // Brackets the query with wall-clock reads. The value is computed during execution, so it must
  // land within [before, after]; the tolerance only absorbs clock granularity between reads.
  private def assertNearNow(sql: String): Unit =
    val before = Instant.now()
    val actual = instantOf(query(sql).data(0).data(0))
    val after  = Instant.now()

    withClue(s"$sql returned $actual, expected within ${toleranceMillis}ms of [$before, $after]: ") {
      actual.isBefore(before.minusMillis(toleranceMillis)) shouldBe false
      actual.isAfter(after.plusMillis(toleranceMillis)) shouldBe false
    }

  "CURRENT_TIMESTAMP" - {
    "is a timestamptz value" in {
      query("SELECT CURRENT_TIMESTAMP").data(0).data(0) shouldBe a[TimestampTZValue]
    }

    "matches the wall clock" in {
      assertNearNow("SELECT CURRENT_TIMESTAMP")
    }
  }

  "now()" - {
    "is a timestamptz value" in {
      query("SELECT now()").data(0).data(0) shouldBe a[TimestampTZValue]
    }

    "matches the wall clock" in {
      assertNearNow("SELECT now()")
    }
  }

  "clock_timestamp()" - {
    "is a timestamptz value matching the wall clock" in {
      query("SELECT clock_timestamp()").data(0).data(0) shouldBe a[TimestampTZValue]
      assertNearNow("SELECT clock_timestamp()")
    }
  }

  "transaction_timestamp() and statement_timestamp()" - {
    "both match the wall clock" in {
      assertNearNow("SELECT transaction_timestamp()")
      assertNearNow("SELECT statement_timestamp()")
    }
  }

  "LOCALTIMESTAMP" - {
    "is a zone-naive timestamp matching the wall clock" in {
      query("SELECT LOCALTIMESTAMP").data(0).data(0) shouldBe a[TimestampValue]
      assertNearNow("SELECT LOCALTIMESTAMP")
    }
  }

  "CURRENT_TIME and LOCALTIME" - {
    "CURRENT_TIME is timetz, LOCALTIME is zone-naive time" in {
      query("SELECT CURRENT_TIME").data(0).data(0) shouldBe a[TimeTZValue]
      query("SELECT LOCALTIME").data(0).data(0) shouldBe a[TimeValue]
    }
  }

  "CURRENT_DATE" - {
    "is the current date in the session zone" in {
      val before = LocalDate.now()
      val v      = query("SELECT CURRENT_DATE").data(0).data(0)
      val after  = LocalDate.now()
      v shouldBe a[DateValue]
      val DateValue(d) = v: @unchecked
      // Bracketed to tolerate a midnight rollover between the two reads.
      withClue(s"CURRENT_DATE=$d expected in [$before, $after]: ") {
        (d == before || d == after) shouldBe true
      }
    }
  }

  // The column metadata type (what JDBC ResultSetMetaData and the JS `fields[].dataType` report)
  // must follow the value: CURRENT_TIMESTAMP is timestamptz, not timestamp.
  "result metadata reports PostgreSQL types" in {
    query("SELECT CURRENT_TIMESTAMP").meta.columns(0).typ shouldBe TimestampTZType
    query("SELECT LOCALTIMESTAMP").meta.columns(0).typ shouldBe TimestampType
    query("SELECT CURRENT_TIME").meta.columns(0).typ shouldBe TimeTZType
    query("SELECT LOCALTIME").meta.columns(0).typ shouldBe TimeType
    query("SELECT CURRENT_DATE").meta.columns(0).typ shouldBe DateType
  }

  // The now() family is timestamptz, so the temporal functions that consume it must accept
  // TimestampTZValue. A fixed-offset literal pins the behaviour deterministically: '+05:00' names
  // the instant 2024-01-15T05:30:00Z.
  "temporal functions over timestamptz" - {
    val tz  = "make_timestamptz(2024, 1, 15, 10, 30, 0, '+05:00')"
    val odt = OffsetDateTime.parse("2024-01-15T10:30:00+05:00")

    "date_part extracts offset-local components" in {
      query(s"SELECT date_part('year', $tz)").data(0).data(0) shouldBe NumberValue(2024)
      query(s"SELECT date_part('hour', $tz)").data(0).data(0) shouldBe NumberValue(10)
    }

    "date_part 'timezone' returns the offset in seconds" in {
      query(s"SELECT date_part('timezone', $tz)").data(0).data(0) shouldBe NumberValue(5 * 3600)
    }

    "date_part 'epoch' returns the instant's epoch seconds" in {
      query(s"SELECT date_part('epoch', $tz)").data(0).data(0) shouldBe NumberValue(odt.toEpochSecond.toDouble)
    }

    "EXTRACT routes through date_part for timestamptz" in {
      query(s"SELECT EXTRACT(month FROM $tz)").data(0).data(0) shouldBe NumberValue(1)
    }

    "date_trunc keeps the offset and yields a timestamptz" in {
      val v = query(s"SELECT date_trunc('day', $tz)").data(0).data(0)
      v shouldBe a[TimestampTZValue]
      val TimestampTZValue(t) = v: @unchecked
      t shouldBe OffsetDateTime.parse("2024-01-15T00:00:00+05:00")
    }

    "to_char formats a timestamptz" in {
      query(s"SELECT to_char($tz, 'YYYY-MM-DD')").data(0).data(0) shouldBe TextValue("2024-01-15")
    }

    "age over two timestamptz values is the instant difference" in {
      val later   = "make_timestamptz(2024, 1, 15, 12, 30, 0, '+05:00')"
      val v       = query(s"SELECT age($later, $tz)").data(0).data(0)
      v shouldBe a[IntervalValue]
      val IntervalValue(d) = v: @unchecked
      d.toHours shouldBe 2L
    }

    "single-argument age accepts a timestamptz" in {
      query(s"SELECT age($tz)").data(0).data(0) shouldBe a[IntervalValue]
    }
  }
}

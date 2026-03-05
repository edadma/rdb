package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class OverlapsTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE events (
      |  name TEXT,
      |  start_ts TIMESTAMP,
      |  end_ts TIMESTAMP
      |);
      |INSERT INTO events (name, start_ts, end_ts) VALUES ('A', '2024-01-01 00:00:00', '2024-01-10 00:00:00');
      |INSERT INTO events (name, start_ts, end_ts) VALUES ('B', '2024-01-20 00:00:00', '2024-01-25 00:00:00');
      |INSERT INTO events (name, start_ts, end_ts) VALUES ('C', '2024-01-08 00:00:00', '2024-01-12 00:00:00');
      |""".trim.stripMargin

  "OVERLAPS" - {
    "overlapping ranges match" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM events
          |  WHERE (start_ts, end_ts) OVERLAPS ('2024-01-05 00:00:00'::TIMESTAMP, '2024-01-09 00:00:00'::TIMESTAMP)
          |  ORDER BY name;
          |""".trim.stripMargin,
      )

      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      names shouldBe Vector("A", "C")
    }

    "non-overlapping ranges excluded" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM events
          |  WHERE (start_ts, end_ts) OVERLAPS ('2024-02-01 00:00:00'::TIMESTAMP, '2024-02-10 00:00:00'::TIMESTAMP)
          |  ORDER BY name;
          |""".trim.stripMargin,
      )

      table.data.length shouldBe 0
    }

    "adjacent (touching) ranges do not overlap (strict <)" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM events
          |  WHERE (start_ts, end_ts) OVERLAPS ('2024-01-10 00:00:00'::TIMESTAMP, '2024-01-15 00:00:00'::TIMESTAMP)
          |  ORDER BY name;
          |""".trim.stripMargin,
      )

      // A ends at 01-10 and query starts at 01-10: s1 < e2 (01-01 < 01-15 yes) AND s2 < e1 (01-10 < 01-10 no)
      // C ends at 01-12: s1 < e2 (01-08 < 01-15 yes) AND s2 < e1 (01-10 < 01-12 yes)
      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      names shouldBe Vector("C")
    }

    "date column overlap" in {
      val table = query(
        """
          |CREATE TABLE bookings (
          |  id INT,
          |  check_in DATE,
          |  check_out DATE
          |);
          |INSERT INTO bookings (id, check_in, check_out) VALUES (1, '2024-03-01', '2024-03-05');
          |INSERT INTO bookings (id, check_in, check_out) VALUES (2, '2024-03-10', '2024-03-15');
          |INSERT INTO bookings (id, check_in, check_out) VALUES (3, '2024-03-04', '2024-03-08');
          |SELECT id FROM bookings
          |  WHERE (check_in, check_out) OVERLAPS ('2024-03-03'::DATE, '2024-03-06'::DATE)
          |  ORDER BY id;
          |""".trim.stripMargin,
      )

      val ids = table.data.map(_.data(0).asInstanceOf[NumberValue].value)
      ids shouldBe Vector(1, 3)
    }

    "all rows overlap wide range" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM events
          |  WHERE (start_ts, end_ts) OVERLAPS ('2020-01-01 00:00:00'::TIMESTAMP, '2030-01-01 00:00:00'::TIMESTAMP)
          |  ORDER BY name;
          |""".trim.stripMargin,
      )

      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      names shouldBe Vector("A", "B", "C")
    }

    "no rows for range before all events" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM events
          |  WHERE (start_ts, end_ts) OVERLAPS ('2023-01-01 00:00:00'::TIMESTAMP, '2023-06-01 00:00:00'::TIMESTAMP);
          |""".trim.stripMargin,
      )

      table.data.length shouldBe 0
    }
  }
}

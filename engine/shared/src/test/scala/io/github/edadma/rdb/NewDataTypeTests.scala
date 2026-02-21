package io.github.edadma.rdb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class NewDataTypeTests extends AnyFreeSpec with Matchers with Testing {

  // Group 1: Parser Aliases

  "DECIMAL" - {
    "DECIMAL(p,s) maps to NUMERIC" in {
      val table = query(
        """
          |CREATE TABLE t (val DECIMAL(10,2));
          |INSERT INTO t (val) VALUES (123.45), (0.99);
          |SELECT val FROM t ORDER BY val;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 2
    }
  }

  "FLOAT" - {
    "FLOAT maps to DOUBLE" in {
      val table = query(
        """
          |CREATE TABLE t (val FLOAT);
          |INSERT INTO t (val) VALUES (3.14), (2.71);
          |SELECT val FROM t ORDER BY val;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(2.71)
      table.data(1).data(0) shouldBe NumberValue(3.14)
    }
  }

  "REAL" - {
    "REAL maps to DOUBLE" in {
      val table = query(
        """
          |CREATE TABLE t (val REAL);
          |INSERT INTO t (val) VALUES (1.5);
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(1.5)
    }
  }

  "JSONB" - {
    "JSONB maps to JSON" in {
      val table = query(
        """
          |CREATE TABLE t (data JSONB);
          |INSERT INTO t (data) VALUES ('{"key": "value"}');
          |SELECT data FROM t;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0).isNull shouldBe false
    }
  }

  // Group 2: SMALLINT / SMALLSERIAL

  "SMALLINT" - {
    "stores small integer values" in {
      val table = query(
        """
          |CREATE TABLE t (val SMALLINT);
          |INSERT INTO t (val) VALUES (100), (-200), (0);
          |SELECT val FROM t ORDER BY val;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe NumberValue(-200)
      table.data(1).data(0) shouldBe NumberValue(0)
      table.data(2).data(0) shouldBe NumberValue(100)
    }

    "rejects out-of-range values" in {
      Console.withErr(java.io.OutputStream.nullOutputStream()) {
        an[Exception] should be thrownBy {
          query(
            """
              |CREATE TABLE t (val SMALLINT);
              |INSERT INTO t (val) VALUES (40000);
              |SELECT val FROM t;
              |""".trim.stripMargin
          )
        }
      }
    }
  }

  "SMALLSERIAL" - {
    "auto-increments" in {
      val table = query(
        """
          |CREATE TABLE t (id SMALLSERIAL, name TEXT);
          |INSERT INTO t (name) VALUES ('a'), ('b'), ('c');
          |SELECT id FROM t ORDER BY id;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(1).data(0) shouldBe NumberValue(2)
      table.data(2).data(0) shouldBe NumberValue(3)
    }
  }

  // Group 3: CHAR(n)

  "CHAR" - {
    "pads short strings with spaces" in {
      val table = query(
        """
          |CREATE TABLE t (val CHAR(5));
          |INSERT INTO t (val) VALUES ('ab');
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("ab   ")
    }

    "truncates long strings" in {
      val table = query(
        """
          |CREATE TABLE t (val CHAR(3));
          |INSERT INTO t (val) VALUES ('hello');
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("hel")
    }

    "exact length stays unchanged" in {
      val table = query(
        """
          |CREATE TABLE t (val CHAR(4));
          |INSERT INTO t (val) VALUES ('abcd');
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("abcd")
    }
  }

  // Group 4a: DATE

  "DATE" - {
    "stores and retrieves date values" in {
      val table = query(
        """
          |CREATE TABLE t (val DATE);
          |INSERT INTO t (val) VALUES ('2024-01-15'), ('2024-06-20');
          |SELECT val FROM t ORDER BY val;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 2
      table.data(0).data(0).string shouldBe "2024-01-15"
      table.data(1).data(0).string shouldBe "2024-06-20"
    }

    "date_part extracts parts from DATE" in {
      val table = query(
        """
          |CREATE TABLE t (val DATE);
          |INSERT INTO t (val) VALUES ('2024-03-15');
          |SELECT date_part('year', val), date_part('month', val), date_part('day', val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(2024)
      table.data(0).data(1) shouldBe NumberValue(3)
      table.data(0).data(2) shouldBe NumberValue(15)
    }

    "make_date creates a DATE" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT make_date(2024, 6, 15) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).string shouldBe "2024-06-15"
    }

    "current_date returns a DATE" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT current_date() FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe DateType
    }
  }

  // Group 4b: TIME

  "TIME" - {
    "stores and retrieves time values" in {
      val table = query(
        """
          |CREATE TABLE t (val TIME);
          |INSERT INTO t (val) VALUES ('10:30:00'), ('14:45:30');
          |SELECT val FROM t ORDER BY val;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 2
      table.data(0).data(0).string shouldBe "10:30"
      table.data(1).data(0).string shouldBe "14:45:30"
    }

    "date_part extracts parts from TIME" in {
      val table = query(
        """
          |CREATE TABLE t (val TIME);
          |INSERT INTO t (val) VALUES ('14:30:45');
          |SELECT date_part('hour', val), date_part('minute', val), date_part('second', val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(14)
      table.data(0).data(1) shouldBe NumberValue(30)
      table.data(0).data(2) shouldBe NumberValue(45)
    }

    "make_time creates a TIME" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT make_time(10, 30, 0) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).string shouldBe "10:30"
    }
  }

  // Group 4c: INTERVAL

  "INTERVAL" - {
    "stores interval from ISO 8601 format" in {
      val table = query(
        """
          |CREATE TABLE t (val INTERVAL);
          |INSERT INTO t (val) VALUES ('PT1H30M');
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).string shouldBe "1 hour 30 minutes"
    }

    "stores interval from simple format" in {
      val table = query(
        """
          |CREATE TABLE t (val INTERVAL);
          |INSERT INTO t (val) VALUES ('2 days 3 hours');
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).string shouldBe "2 days 3 hours"
    }
  }

  // Group 4d: Date/Time Arithmetic

  "Date/Time Arithmetic" - {
    "date + integer adds days" in {
      val table = query(
        """
          |CREATE TABLE t (val DATE);
          |INSERT INTO t (val) VALUES ('2024-01-01');
          |SELECT val + 10 FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).string shouldBe "2024-01-11"
    }

    "date - integer subtracts days" in {
      val table = query(
        """
          |CREATE TABLE t (val DATE);
          |INSERT INTO t (val) VALUES ('2024-01-15');
          |SELECT val - 5 FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).string shouldBe "2024-01-10"
    }

    "date - date gives days between" in {
      val table = query(
        """
          |CREATE TABLE t (d1 DATE, d2 DATE);
          |INSERT INTO t (d1, d2) VALUES ('2024-01-15', '2024-01-10');
          |SELECT d1 - d2 FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(5)
    }

    "timestamp + interval" in {
      val table = query(
        """
          |CREATE TABLE t (val TIMESTAMP);
          |INSERT INTO t (val) VALUES ('2024-01-01 10:00:00');
          |SELECT val + '2 hours'::INTERVAL FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).string shouldBe "2024-01-01T12:00"
    }

    "timestamp - interval" in {
      val table = query(
        """
          |CREATE TABLE t (val TIMESTAMP);
          |INSERT INTO t (val) VALUES ('2024-01-01 10:00:00');
          |SELECT val - 'PT30M'::INTERVAL FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).string shouldBe "2024-01-01T09:30"
    }

    "interval + interval" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT '1 hour'::INTERVAL + '30 minutes'::INTERVAL FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).string shouldBe "1 hour 30 minutes"
    }

    "interval * number" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT '1 hour'::INTERVAL * 3 FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).string shouldBe "3 hours"
    }

    "numeric arithmetic still works" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b INT);
          |INSERT INTO t (a, b) VALUES (10, 3);
          |SELECT a + b, a - b, a * b FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(13)
      table.data(0).data(1) shouldBe NumberValue(7)
      table.data(0).data(2) shouldBe NumberValue(30)
    }
  }

  // Group 4e: TIMESTAMP WITH TIME ZONE

  "TIMESTAMP WITH TIME ZONE" - {
    "stores and retrieves timestamptz values" in {
      val table = query(
        """
          |CREATE TABLE t (val TIMESTAMP WITH TIME ZONE);
          |INSERT INTO t (val) VALUES ('2024-01-15T10:30:00+05:00');
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0).vtyp shouldBe TimestampTZType
    }

    "cross-converts from TIMESTAMP" in {
      val table = query(
        """
          |CREATE TABLE t (val TIMESTAMP);
          |INSERT INTO t (val) VALUES ('2024-01-15 10:30:00');
          |SELECT val::TIMESTAMP WITH TIME ZONE FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe TimestampTZType
    }
  }

  // Group 5: BYTEA

  "BYTEA" - {
    "stores bytea from text" in {
      val table = query(
        """
          |CREATE TABLE t (val BYTEA);
          |INSERT INTO t (val) VALUES ('hello');
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0).vtyp shouldBe ByteaType
    }

    "octet_length returns byte count" in {
      val table = query(
        """
          |CREATE TABLE t (val BYTEA);
          |INSERT INTO t (val) VALUES ('hello');
          |SELECT octet_length(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(5)
    }

    "encode/decode roundtrip with hex" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT encode(decode('cafe', 'hex'), 'hex') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("cafe")
    }

    "decode creates bytea from hex string" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT octet_length(decode('deadbeef', 'hex')) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(4)
    }
  }

  // Group 6: Typed Arrays

  "Typed Arrays" - {
    "INT[] column type" in {
      val table = query(
        """
          |CREATE TABLE t (vals INT[]);
          |INSERT INTO t (vals) VALUES (ARRAY[1, 2, 3]);
          |SELECT vals FROM t;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe a[ArrayValue]
    }

    "TEXT[] column type" in {
      val table = query(
        """
          |CREATE TABLE t (tags TEXT[]);
          |INSERT INTO t (tags) VALUES (ARRAY['a', 'b', 'c']);
          |SELECT tags FROM t;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
    }

    "array_length works with typed arrays" in {
      val table = query(
        """
          |CREATE TABLE t (vals INT[]);
          |INSERT INTO t (vals) VALUES (ARRAY[10, 20, 30]);
          |SELECT array_length(vals) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(3)
    }

    "ARRAY constructor in expression" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT ARRAY[1, 2, 3] FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe a[ArrayValue]
      table.data(0).data(0).asInstanceOf[ArrayValue].data.length shouldBe 3
    }
  }

  // Cast operator (::) tests for new types

  "Cast operator (::)" - {
    "text to DATE" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT '2024-03-15'::DATE FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe DateType
      table.data(0).data(0).string shouldBe "2024-03-15"
    }

    "text to TIME" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT '14:30:00'::TIME FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe TimeType
      table.data(0).data(0).string shouldBe "14:30"
    }

    "text to INTERVAL" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT '2 hours 30 minutes'::INTERVAL FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe IntervalType
      table.data(0).data(0).string shouldBe "2 hours 30 minutes"
    }

    "text to TIMESTAMP WITH TIME ZONE" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT '2024-01-15T10:30:00+05:00'::TIMESTAMP WITH TIME ZONE FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe TimestampTZType
    }

    "text to SMALLINT" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT '100'::SMALLINT FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(100)
    }

    "text to BYTEA" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT 'hello'::BYTEA FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe ByteaType
    }

    "timestamp to DATE" in {
      val table = query(
        """
          |CREATE TABLE t (val TIMESTAMP);
          |INSERT INTO t (val) VALUES ('2024-06-15 10:30:00');
          |SELECT val::DATE FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe DateType
      table.data(0).data(0).string shouldBe "2024-06-15"
    }

    "timestamp to TIME" in {
      val table = query(
        """
          |CREATE TABLE t (val TIMESTAMP);
          |INSERT INTO t (val) VALUES ('2024-06-15 14:30:45');
          |SELECT val::TIME FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe TimeType
      table.data(0).data(0).string shouldBe "14:30:45"
    }

    "date to TIMESTAMP" in {
      val table = query(
        """
          |CREATE TABLE t (val DATE);
          |INSERT INTO t (val) VALUES ('2024-06-15');
          |SELECT val::TIMESTAMP FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe TimestampType
      table.data(0).data(0).string shouldBe "2024-06-15T00:00"
    }

    "date to TIMESTAMP WITH TIME ZONE" in {
      val table = query(
        """
          |CREATE TABLE t (val DATE);
          |INSERT INTO t (val) VALUES ('2024-06-15');
          |SELECT val::TIMESTAMP WITH TIME ZONE FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe TimestampTZType
    }

    "timestamptz to DATE" in {
      val table = query(
        """
          |CREATE TABLE t (val TIMESTAMP WITH TIME ZONE);
          |INSERT INTO t (val) VALUES ('2024-06-15T10:30:00+02:00');
          |SELECT val::DATE FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe DateType
      table.data(0).data(0).string shouldBe "2024-06-15"
    }

    "timestamptz to TIME" in {
      val table = query(
        """
          |CREATE TABLE t (val TIMESTAMP WITH TIME ZONE);
          |INSERT INTO t (val) VALUES ('2024-06-15T14:30:00+02:00');
          |SELECT val::TIME FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe TimeType
      table.data(0).data(0).string shouldBe "14:30"
    }

    "timestamptz to TIMESTAMP" in {
      val table = query(
        """
          |CREATE TABLE t (val TIMESTAMP WITH TIME ZONE);
          |INSERT INTO t (val) VALUES ('2024-06-15T14:30:00+02:00');
          |SELECT val::TIMESTAMP FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe TimestampType
    }

    "text to BOOLEAN" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT 'true'::BOOLEAN, 'false'::BOOLEAN FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
      table.data(0).data(1) shouldBe BooleanValue(false)
    }

    "integer to BOOLEAN" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT 1::BOOLEAN, 0::BOOLEAN FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
      table.data(0).data(1) shouldBe BooleanValue(false)
    }

    "boolean to INTEGER" in {
      val table = query(
        """
          |CREATE TABLE t (val BOOLEAN);
          |INSERT INTO t (val) VALUES (TRUE), (FALSE);
          |SELECT val::INTEGER FROM t ORDER BY val::INTEGER;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(0)
      table.data(1).data(0) shouldBe NumberValue(1)
    }

    "text to FLOAT" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT '3.14'::FLOAT FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(3.14)
    }

    "text to DECIMAL" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT 42::DECIMAL(10,2) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe NumberType
    }

    "integer to CHAR" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (42);
          |SELECT id::CHAR(5) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("42   ")
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
}

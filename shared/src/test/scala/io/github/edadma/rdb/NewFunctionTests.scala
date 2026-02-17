package io.github.edadma.rdb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class NewFunctionTests extends AnyFreeSpec with Matchers with Testing {

  "greatest" - {
    "returns largest of integers" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT greatest(3, 7, 1, 5) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(7)
    }

    "returns largest of text" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT greatest('apple', 'cherry', 'banana') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("cherry")
    }

    "ignores NULLs" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b INT);
          |INSERT INTO t (a, b) VALUES (5, NULL);
          |SELECT greatest(a, b) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(5)
    }
  }

  "least" - {
    "returns smallest of integers" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT least(3, 7, 1, 5) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(1)
    }
  }

  "concat_ws" - {
    "concatenates with separator" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT concat_ws(', ', 'a', 'b', 'c') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("a, b, c")
    }

    "skips NULL values" in {
      val table = query(
        """
          |CREATE TABLE t (a TEXT, b TEXT, c TEXT);
          |INSERT INTO t (a, b, c) VALUES ('x', NULL, 'z');
          |SELECT concat_ws('-', a, b, c) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("x-z")
    }
  }

  "initcap" - {
    "capitalizes first letter of each word" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT initcap('hello world foo') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("Hello World Foo")
    }
  }

  "char_length" - {
    "returns character length" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT char_length('hello') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(5)
    }
  }

  "ascii and chr" - {
    "ascii returns code point" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT ascii('A') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(65)
    }

    "chr returns character" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT chr(65) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("A")
    }
  }

  "regexp_replace" - {
    "replaces first match" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT regexp_replace('hello world hello', 'hello', 'hi') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("hi world hello")
    }

    "replaces all with g flag" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT regexp_replace('hello world hello', 'hello', 'hi', 'g') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("hi world hi")
    }
  }

  "regexp_match" - {
    "returns captured groups" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT regexp_match('abc 123 def', '([0-9]+)') FROM t;
          |""".trim.stripMargin
      )
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.length shouldBe 1
      arr.data(0) shouldBe TextValue("123")
    }

    "returns NULL on no match" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT regexp_match('abc', '([0-9]+)') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }
  }

  "date_trunc" - {
    "truncates to year" in {
      val table = query(
        """
          |CREATE TABLE t (val TIMESTAMP);
          |INSERT INTO t (val) VALUES ('2024-06-15 14:30:45');
          |SELECT date_trunc('year', val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).string shouldBe "2024-01-01T00:00"
    }

    "truncates to month" in {
      val table = query(
        """
          |CREATE TABLE t (val TIMESTAMP);
          |INSERT INTO t (val) VALUES ('2024-06-15 14:30:45');
          |SELECT date_trunc('month', val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).string shouldBe "2024-06-01T00:00"
    }

    "truncates to day" in {
      val table = query(
        """
          |CREATE TABLE t (val TIMESTAMP);
          |INSERT INTO t (val) VALUES ('2024-06-15 14:30:45');
          |SELECT date_trunc('day', val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).string shouldBe "2024-06-15T00:00"
    }

    "truncates to hour" in {
      val table = query(
        """
          |CREATE TABLE t (val TIMESTAMP);
          |INSERT INTO t (val) VALUES ('2024-06-15 14:30:45');
          |SELECT date_trunc('hour', val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).string shouldBe "2024-06-15T14:00"
    }

    "truncates DATE to month" in {
      val table = query(
        """
          |CREATE TABLE t (val DATE);
          |INSERT INTO t (val) VALUES ('2024-06-15');
          |SELECT date_trunc('month', val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).string shouldBe "2024-06-01"
    }
  }

  "age" - {
    "interval between two timestamps" in {
      val table = query(
        """
          |CREATE TABLE t (t1 TIMESTAMP, t2 TIMESTAMP);
          |INSERT INTO t (t1, t2) VALUES ('2024-06-15 12:00:00', '2024-06-15 10:00:00');
          |SELECT age(t1, t2) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe IntervalType
      table.data(0).data(0).string shouldBe "2 hours"
    }

    "interval between two dates" in {
      val table = query(
        """
          |CREATE TABLE t (d1 DATE, d2 DATE);
          |INSERT INTO t (d1, d2) VALUES ('2024-06-15', '2024-06-10');
          |SELECT age(d1, d2) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).string shouldBe "5 days"
    }
  }

  "to_char" - {
    "formats date" in {
      val table = query(
        """
          |CREATE TABLE t (val DATE);
          |INSERT INTO t (val) VALUES ('2024-06-15');
          |SELECT to_char(val, 'YYYY-MM-DD') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("2024-06-15")
    }
  }

  "to_date" - {
    "parses date with format" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT to_date('2024-06-15', 'YYYY-MM-DD') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).vtyp shouldBe DateType
      table.data(0).data(0).string shouldBe "2024-06-15"
    }
  }

  "EXTRACT" - {
    "extracts year from timestamp" in {
      val table = query(
        """
          |CREATE TABLE t (val TIMESTAMP);
          |INSERT INTO t (val) VALUES ('2024-06-15 14:30:00');
          |SELECT EXTRACT(year FROM val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(2024)
    }

    "extracts month from date" in {
      val table = query(
        """
          |CREATE TABLE t (val DATE);
          |INSERT INTO t (val) VALUES ('2024-06-15');
          |SELECT EXTRACT(month FROM val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(6)
    }

    "extracts hour from time" in {
      val table = query(
        """
          |CREATE TABLE t (val TIME);
          |INSERT INTO t (val) VALUES ('14:30:45');
          |SELECT EXTRACT(hour FROM val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(14)
    }
  }

  "math functions" - {
    "pi" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT pi() FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(math.Pi)
    }

    "log with base" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT log(2, 8) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 3.0 +- 0.0001
    }

    "degrees" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT degrees(pi()) FROM t;
          |""".trim.stripMargin
      )
      val v = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      v shouldBe 180.0 +- 0.0001
    }

    "sin" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT sin(0) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(0.0)
    }
  }

  "string_to_array / array_to_string" - {
    "string_to_array splits" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT string_to_array('a,b,c', ',') FROM t;
          |""".trim.stripMargin
      )
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.length shouldBe 3
      arr.data(0) shouldBe TextValue("a")
      arr.data(2) shouldBe TextValue("c")
    }

    "array_to_string joins" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT array_to_string(ARRAY['x', 'y', 'z'], '-') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("x-y-z")
    }
  }

  "array functions" - {
    "array_remove" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT array_length(array_remove(ARRAY[1, 2, 3, 2], 2)) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(2)
    }

    "array_position" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT array_position(ARRAY['a', 'b', 'c'], 'b') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(2)
    }

    "array_distinct" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT array_length(array_distinct(ARRAY[1, 2, 2, 3, 3, 3])) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(3)
    }
  }

  // New aggregate functions

  "string_agg" - {
    "concatenates with separator" in {
      val table = query(
        """
          |CREATE TABLE t (name TEXT);
          |INSERT INTO t (name) VALUES ('Alice'), ('Bob'), ('Carol');
          |SELECT string_agg(name, ', ') FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("Alice, Bob, Carol")
    }

    "with GROUP BY" in {
      val table = query(
        """
          |CREATE TABLE t (dept TEXT, name TEXT);
          |INSERT INTO t (dept, name) VALUES ('eng', 'Alice'), ('eng', 'Bob'), ('sales', 'Carol');
          |SELECT dept, string_agg(name, ', ') FROM t GROUP BY dept ORDER BY dept;
          |""".trim.stripMargin
      )
      table.data(0).data(1) shouldBe TextValue("Alice, Bob")
      table.data(1).data(1) shouldBe TextValue("Carol")
    }
  }

  "array_agg" - {
    "collects values into array" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (1), (2), (3);
          |SELECT array_agg(val) FROM t;
          |""".trim.stripMargin
      )
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.length shouldBe 3
    }

    "with GROUP BY" in {
      val table = query(
        """
          |CREATE TABLE t (dept TEXT, name TEXT);
          |INSERT INTO t (dept, name) VALUES ('eng', 'Alice'), ('eng', 'Bob'), ('sales', 'Carol');
          |SELECT dept, array_length(array_agg(name)) FROM t GROUP BY dept ORDER BY dept;
          |""".trim.stripMargin
      )
      table.data(0).data(1) shouldBe NumberValue(2)
      table.data(1).data(1) shouldBe NumberValue(1)
    }
  }

  "bool_and" - {
    "returns true when all true" in {
      val table = query(
        """
          |CREATE TABLE t (val BOOLEAN);
          |INSERT INTO t (val) VALUES (TRUE), (TRUE), (TRUE);
          |SELECT bool_and(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "returns false when any false" in {
      val table = query(
        """
          |CREATE TABLE t (val BOOLEAN);
          |INSERT INTO t (val) VALUES (TRUE), (FALSE), (TRUE);
          |SELECT bool_and(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(false)
    }
  }

  "bool_or" - {
    "returns true when any true" in {
      val table = query(
        """
          |CREATE TABLE t (val BOOLEAN);
          |INSERT INTO t (val) VALUES (FALSE), (TRUE), (FALSE);
          |SELECT bool_or(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "returns false when all false" in {
      val table = query(
        """
          |CREATE TABLE t (val BOOLEAN);
          |INSERT INTO t (val) VALUES (FALSE), (FALSE), (FALSE);
          |SELECT bool_or(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(false)
    }
  }
}

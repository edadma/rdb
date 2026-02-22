package io.github.edadma.petradb

import io.github.edadma.dal.{IntType => DIntType, DoubleType => DDoubleType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DataTypeTests extends AnyFreeSpec with Matchers with Testing {

  "INTEGER" - {
    "stores and retrieves integer values" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (42), (-7), (0);
          |SELECT val FROM t ORDER BY val;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe NumberValue(-7)
      table.data(1).data(0) shouldBe NumberValue(0)
      table.data(2).data(0) shouldBe NumberValue(42)
    }

    "INTEGER keyword is equivalent to INT" in {
      val table = query(
        """
          |CREATE TABLE t (val INTEGER);
          |INSERT INTO t (val) VALUES (99);
          |SELECT val FROM t;
          |""".trim.stripMargin
      )

      table.data(0).data(0) shouldBe NumberValue(99)
    }
  }

  "BIGINT" - {
    "stores large integer values" in {
      val table = query(
        """
          |CREATE TABLE t (val BIGINT);
          |INSERT INTO t (val) VALUES (1000000), (0), (-500);
          |SELECT val FROM t ORDER BY val;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe NumberValue(-500)
      table.data(2).data(0) shouldBe NumberValue(1000000)
    }
  }

  "DOUBLE" - {
    "stores decimal values" in {
      val table = query(
        """
          |CREATE TABLE t (val DOUBLE);
          |INSERT INTO t (val) VALUES (3.14), (-2.5), (0.0);
          |SELECT val FROM t ORDER BY val;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe NumberValue(-2.5)
      table.data(2).data(0) shouldBe NumberValue(3.14)
    }

    "DOUBLE PRECISION keyword is equivalent" in {
      val table = query(
        """
          |CREATE TABLE t (val DOUBLE PRECISION);
          |INSERT INTO t (val) VALUES (1.5);
          |SELECT val FROM t;
          |""".trim.stripMargin
      )

      table.data(0).data(0) shouldBe NumberValue(1.5)
    }
  }

  "NUMERIC" - {
    "stores decimal values with precision" in {
      val table = query(
        """
          |CREATE TABLE t (val NUMERIC(10,2));
          |INSERT INTO t (val) VALUES (123.45), (0.99), (1000.00);
          |SELECT val FROM t ORDER BY val;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
    }
  }

  "TEXT" - {
    "stores and retrieves text values" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT);
          |INSERT INTO t (val) VALUES ('hello'), (''), ('world');
          |SELECT val FROM t ORDER BY val;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe TextValue("")
      table.data(1).data(0) shouldBe TextValue("hello")
      table.data(2).data(0) shouldBe TextValue("world")
    }

    "stores strings with backslash escapes" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT);
          |INSERT INTO t (val) VALUES ('hello\nworld');
          |SELECT val FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
    }
  }

  "BOOLEAN" - {
    "stores and retrieves boolean values" in {
      val table = query(
        """
          |CREATE TABLE t (val BOOLEAN);
          |INSERT INTO t (val) VALUES (TRUE), (FALSE);
          |SELECT val FROM t ORDER BY val;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
    }

    "can filter by boolean column" in {
      val table = query(
        """
          |CREATE TABLE t (name TEXT, active BOOLEAN);
          |INSERT INTO t (name, active) VALUES ('Alice', TRUE), ('Bob', FALSE), ('Carol', TRUE);
          |SELECT name FROM t WHERE active = TRUE ORDER BY name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(1).data(0) shouldBe TextValue("Carol")
    }
  }

  "TIMESTAMP" - {
    "stores and retrieves timestamp values" in {
      val table = query(
        """
          |CREATE TABLE t (val TIMESTAMP);
          |INSERT INTO t (val) VALUES ('2024-01-15 10:30:00'), ('2024-06-20 14:00:00');
          |SELECT val FROM t ORDER BY val;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
    }

    "TIMESTAMP WITHOUT TIME ZONE is equivalent" in {
      val table = query(
        """
          |CREATE TABLE t (val TIMESTAMP WITHOUT TIME ZONE);
          |INSERT INTO t (val) VALUES ('2024-01-01 00:00:00');
          |SELECT val FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
    }
  }

  "UUID" - {
    "generates and stores UUID values" in {
      val table = query(
        """
          |CREATE TABLE t (id UUID DEFAULT gen_random_uuid(), name TEXT);
          |INSERT INTO t (name) VALUES ('test');
          |SELECT id FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0).isNull shouldBe false
    }
  }

  "JSON" - {
    "stores JSON object" in {
      val table = query(
        """
          |CREATE TABLE t (data JSON);
          |INSERT INTO t (data) VALUES ('{"key": "value", "num": 42}');
          |SELECT data FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0).isNull shouldBe false
    }

    "stores JSON array" in {
      val table = query(
        """
          |CREATE TABLE t (data JSON);
          |INSERT INTO t (data) VALUES ('[1, 2, 3]');
          |SELECT data FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
    }
  }

  "SERIAL" - {
    "auto-increments from 1" in {
      val table = query(
        """
          |CREATE TABLE t (id SERIAL, name TEXT);
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

  "BIGSERIAL" - {
    "auto-increments" in {
      val table = query(
        """
          |CREATE TABLE t (id BIGSERIAL, name TEXT);
          |INSERT INTO t (name) VALUES ('a'), ('b');
          |SELECT id FROM t ORDER BY id;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(1).data(0) shouldBe NumberValue(2)
    }
  }

  "NULL handling" - {
    "NULL values in columns" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, val TEXT);
          |INSERT INTO t (id, val) VALUES (1, 'a'), (2, NULL), (3, 'c');
          |SELECT val FROM t WHERE val IS NULL;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0).isNull shouldBe true
    }

    "NULL comparison with IS NULL" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b INT);
          |INSERT INTO t (a, b) VALUES (1, NULL), (2, 3), (3, NULL);
          |SELECT a FROM t WHERE b IS NULL ORDER BY a;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(1).data(0) shouldBe NumberValue(3)
    }
  }

  "ENUM type" - {
    "stores and queries enum values" in {
      val table = query(
        """
          |CREATE TYPE color AS ENUM ('red', 'green', 'blue');
          |CREATE TABLE t (name TEXT, c color);
          |INSERT INTO t (name, c) VALUES ('stop', 'red'), ('go', 'green');
          |SELECT name, c FROM t ORDER BY name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("go")
      table.data(1).data(0) shouldBe TextValue("stop")
    }

    "enum comparison respects declaration order" in {
      val table = query(
        """
          |CREATE TYPE priority AS ENUM ('low', 'medium', 'high');
          |CREATE TABLE t (name TEXT, p priority);
          |INSERT INTO t (name, p) VALUES ('a', 'high'), ('b', 'low'), ('c', 'medium');
          |SELECT name FROM t ORDER BY p;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe TextValue("b")
      table.data(1).data(0) shouldBe TextValue("c")
      table.data(2).data(0) shouldBe TextValue("a")
    }
  }
}

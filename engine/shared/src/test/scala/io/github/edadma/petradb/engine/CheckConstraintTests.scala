package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CheckConstraintTests extends AnyFreeSpec with Matchers with Testing {

  "CHECK constraint" - {
    "valid insert succeeds" in {
      val table = query(
        """
          |CREATE TABLE products (name TEXT, price INT, CHECK (price > 0));
          |INSERT INTO products (name, price) VALUES ('widget', 10);
          |SELECT name, price FROM products;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("widget")
      table.data(0).data(1) shouldBe NumberValue(10)
    }

    "invalid insert fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE products (name TEXT, price INT, CHECK (price > 0));
            |INSERT INTO products (name, price) VALUES ('widget', -5);
            |""".trim.stripMargin
        )
      }
    }

    "named constraint error includes name" in {
      val ex = the[Exception] thrownBy {
        results(
          """
            |CREATE TABLE products (name TEXT, price INT, CONSTRAINT positive_price CHECK (price > 0));
            |INSERT INTO products (name, price) VALUES ('widget', 0);
            |""".trim.stripMargin
        )
      }
      ex.getMessage should include("positive_price")
    }

    "multi-column check" in {
      val table = query(
        """
          |CREATE TABLE events (start_val INT, end_val INT, CHECK (start_val < end_val));
          |INSERT INTO events (start_val, end_val) VALUES (1, 10);
          |SELECT start_val, end_val FROM events;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(0).data(1) shouldBe NumberValue(10)
    }

    "multi-column check rejects invalid" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE events (start_val INT, end_val INT, CHECK (start_val < end_val));
            |INSERT INTO events (start_val, end_val) VALUES (10, 5);
            |""".trim.stripMargin
        )
      }
    }

    "column-level check" in {
      val table = query(
        """
          |CREATE TABLE t (val INT CHECK (val >= 0));
          |INSERT INTO t (val) VALUES (5);
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(5)
    }

    "column-level check rejects invalid" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (val INT CHECK (val >= 0));
            |INSERT INTO t (val) VALUES (-1);
            |""".trim.stripMargin
        )
      }
    }

    "update violating check fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE products (name TEXT, price INT, CHECK (price > 0));
            |INSERT INTO products (name, price) VALUES ('widget', 10);
            |UPDATE products SET price = -1;
            |""".trim.stripMargin
        )
      }
    }

    "update respecting check succeeds" in {
      val table = query(
        """
          |CREATE TABLE products (name TEXT, price INT, CHECK (price > 0));
          |INSERT INTO products (name, price) VALUES ('widget', 10);
          |UPDATE products SET price = 20;
          |SELECT price FROM products;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(20)
    }

    "multiple checks on same table" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b INT, CHECK (a > 0), CHECK (b > 0));
          |INSERT INTO t (a, b) VALUES (1, 2);
          |SELECT a, b FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(0).data(1) shouldBe NumberValue(2)
    }

    "multiple checks - second violated" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (a INT, b INT, CHECK (a > 0), CHECK (b > 0));
            |INSERT INTO t (a, b) VALUES (1, -1);
            |""".trim.stripMargin
        )
      }
    }

    "check with IN expression" in {
      val table = query(
        """
          |CREATE TABLE t (status TEXT, CHECK (status IN ('active', 'inactive')));
          |INSERT INTO t (status) VALUES ('active');
          |SELECT status FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("active")
    }

    "check with IN expression rejects invalid" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (status TEXT, CHECK (status IN ('active', 'inactive')));
            |INSERT INTO t (status) VALUES ('deleted');
            |""".trim.stripMargin
        )
      }
    }
  }
}

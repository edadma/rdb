package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class GeneratedColumnTests extends AnyFreeSpec with Matchers:

  private def test(sql: String): String =
    given Session = new MemoryDB().connect()
    try {
      executeSQL(sql).toString
    } catch {
      case e: RuntimeException => e.getMessage
    }

  private def query(sql: String): TableValue =
    given Session = new MemoryDB().connect()
    executeSQL(sql).collect { case QueryResult(t) => t }.last

  "basic GENERATED ALWAYS AS" - {
    "computed column from arithmetic expression" in {
      val t = query(
        """
          |CREATE TABLE orders (
          |  id SERIAL PRIMARY KEY,
          |  price NUMERIC NOT NULL,
          |  quantity INTEGER NOT NULL,
          |  total NUMERIC GENERATED ALWAYS AS (price * quantity) STORED
          |);
          |INSERT INTO orders (price, quantity) VALUES (10, 3);
          |INSERT INTO orders (price, quantity) VALUES (25, 2);
          |SELECT price, quantity, total FROM orders;
          |""".trim.stripMargin
      )

      t.data.length shouldBe 2
      t.data(0).data(2).intValue shouldBe 30
      t.data(1).data(2).intValue shouldBe 50
    }

    "computed column from string concatenation" in {
      val t = query(
        """
          |CREATE TABLE users (
          |  id SERIAL PRIMARY KEY,
          |  first_name TEXT NOT NULL,
          |  last_name TEXT NOT NULL,
          |  full_name TEXT GENERATED ALWAYS AS (first_name || ' ' || last_name) STORED
          |);
          |INSERT INTO users (first_name, last_name) VALUES ('John', 'Doe');
          |INSERT INTO users (first_name, last_name) VALUES ('Jane', 'Smith');
          |SELECT full_name FROM users;
          |""".trim.stripMargin
      )

      t.data.length shouldBe 2
      t.data(0).data(0).string shouldBe "John Doe"
      t.data(1).data(0).string shouldBe "Jane Smith"
    }

    "generated column included in SELECT *" in {
      val t = query(
        """
          |CREATE TABLE items (
          |  id SERIAL PRIMARY KEY,
          |  price NUMERIC NOT NULL,
          |  tax_rate NUMERIC NOT NULL DEFAULT 0.1,
          |  total NUMERIC GENERATED ALWAYS AS (price * (1 + tax_rate)) STORED
          |);
          |INSERT INTO items (price) VALUES (100);
          |SELECT * FROM items;
          |""".trim.stripMargin
      )

      t.data.length shouldBe 1
      // total = 100 * (1 + 0.1) = 110
      t.data(0).data(3).string shouldBe "110.0"
    }
  }

  "UPDATE recomputes generated columns" - {
    "generated column updates when source columns change" in {
      val t = query(
        """
          |CREATE TABLE orders (
          |  id SERIAL PRIMARY KEY,
          |  price NUMERIC NOT NULL,
          |  quantity INTEGER NOT NULL,
          |  total NUMERIC GENERATED ALWAYS AS (price * quantity) STORED
          |);
          |INSERT INTO orders (price, quantity) VALUES (10, 3);
          |UPDATE orders SET quantity = 5 WHERE id = 1;
          |SELECT total FROM orders;
          |""".trim.stripMargin
      )

      t.data.length shouldBe 1
      t.data(0).data(0).string shouldBe "50"
    }

    "generated column updates when multiple source columns change" in {
      val t = query(
        """
          |CREATE TABLE orders (
          |  id SERIAL PRIMARY KEY,
          |  price NUMERIC NOT NULL,
          |  quantity INTEGER NOT NULL,
          |  total NUMERIC GENERATED ALWAYS AS (price * quantity) STORED
          |);
          |INSERT INTO orders (price, quantity) VALUES (10, 3);
          |UPDATE orders SET price = 20, quantity = 4 WHERE id = 1;
          |SELECT total FROM orders;
          |""".trim.stripMargin
      )

      t.data.length shouldBe 1
      t.data(0).data(0).string shouldBe "80"
    }
  }

  "INSERT validation" - {
    "rejects explicit value for generated column" in {
      val result = test(
        """
          |CREATE TABLE orders (
          |  id SERIAL PRIMARY KEY,
          |  price NUMERIC NOT NULL,
          |  quantity INTEGER NOT NULL,
          |  total NUMERIC GENERATED ALWAYS AS (price * quantity) STORED
          |);
          |INSERT INTO orders (price, quantity, total) VALUES (10, 3, 99);
          |""".trim.stripMargin
      )

      result should include("cannot insert a value into generated column")
    }
  }

  "UPDATE validation" - {
    "rejects explicit update to generated column" in {
      val result = test(
        """
          |CREATE TABLE orders (
          |  id SERIAL PRIMARY KEY,
          |  price NUMERIC NOT NULL,
          |  quantity INTEGER NOT NULL,
          |  total NUMERIC GENERATED ALWAYS AS (price * quantity) STORED
          |);
          |INSERT INTO orders (price, quantity) VALUES (10, 3);
          |UPDATE orders SET total = 99 WHERE id = 1;
          |""".trim.stripMargin
      )

      result should include("can only be updated to DEFAULT")
    }
  }

  "parser validation" - {
    "rejects both DEFAULT and GENERATED on same column" in {
      val result = test(
        """
          |CREATE TABLE bad (
          |  id SERIAL PRIMARY KEY,
          |  x INTEGER DEFAULT 0 GENERATED ALWAYS AS (1 + 1) STORED
          |);
          |""".trim.stripMargin
      )

      result should include("generated column cannot have a DEFAULT")
    }
  }

  "RETURNING with generated columns" - {
    "INSERT RETURNING includes generated value" in {
      val result = test(
        """
          |CREATE TABLE orders (
          |  id SERIAL PRIMARY KEY,
          |  price NUMERIC NOT NULL,
          |  quantity INTEGER NOT NULL,
          |  total NUMERIC GENERATED ALWAYS AS (price * quantity) STORED
          |);
          |INSERT INTO orders (price, quantity) VALUES (15, 4) RETURNING total;
          |""".trim.stripMargin
      )

      result should include("60")
    }
  }

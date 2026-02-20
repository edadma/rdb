package io.github.edadma.rdb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LateralJoinTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE users (
      | id INT,
      | name TEXT
      |);
      |CREATE TABLE orders (
      | id INT,
      | user_id INT,
      | amount INT
      |);
      |INSERT INTO users (id, name) VALUES (1, 'Alice'), (2, 'Bob'), (3, 'Carol');
      |INSERT INTO orders (id, user_id, amount) VALUES
      |  (1, 1, 100),
      |  (2, 1, 200),
      |  (3, 2, 150);
      |""".trim.stripMargin

  "comma LATERAL" - {
    "basic lateral subquery" in {
      val table = query(
        s"""
          |$setup
          |SELECT u.name, o.amount
          |FROM users u, LATERAL (SELECT amount FROM orders WHERE user_id = u.id) AS o
          |ORDER BY u.name, o.amount;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1) shouldBe NumberValue(100)
      table.data(1).data(0) shouldBe TextValue("Alice")
      table.data(1).data(1) shouldBe NumberValue(200)
      table.data(2).data(0) shouldBe TextValue("Bob")
      table.data(2).data(1) shouldBe NumberValue(150)
    }

    "lateral with no matching rows produces no output" in {
      val table = query(
        s"""
          |$setup
          |SELECT u.name, o.amount
          |FROM users u, LATERAL (SELECT amount FROM orders WHERE user_id = u.id) AS o
          |WHERE u.name = 'Carol';
          |""".trim.stripMargin
      )

      table.data.length shouldBe 0
    }
  }

  "CROSS JOIN LATERAL" - {
    "equivalent to comma lateral" in {
      val table = query(
        s"""
          |$setup
          |SELECT u.name, o.amount
          |FROM users u
          |CROSS JOIN LATERAL (SELECT amount FROM orders WHERE user_id = u.id) AS o
          |ORDER BY u.name, o.amount;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1) shouldBe NumberValue(100)
      table.data(1).data(0) shouldBe TextValue("Alice")
      table.data(1).data(1) shouldBe NumberValue(200)
      table.data(2).data(0) shouldBe TextValue("Bob")
      table.data(2).data(1) shouldBe NumberValue(150)
    }
  }

  "LEFT JOIN LATERAL" - {
    "returns all left rows with NULLs for non-matching" in {
      val table = query(
        s"""
          |$setup
          |SELECT u.name, o.amount
          |FROM users u
          |LEFT JOIN LATERAL (SELECT amount FROM orders WHERE user_id = u.id) AS o ON true
          |ORDER BY u.name, o.amount;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 4
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1) shouldBe NumberValue(100)
      table.data(1).data(0) shouldBe TextValue("Alice")
      table.data(1).data(1) shouldBe NumberValue(200)
      table.data(2).data(0) shouldBe TextValue("Bob")
      table.data(2).data(1) shouldBe NumberValue(150)
      table.data(3).data(0) shouldBe TextValue("Carol")
      table.data(3).data(1) shouldBe a[NullValue]
    }
  }

  "JOIN LATERAL" - {
    "inner join semantics with condition" in {
      val table = query(
        s"""
          |$setup
          |SELECT u.name, o.amount
          |FROM users u
          |JOIN LATERAL (SELECT amount FROM orders WHERE user_id = u.id) AS o ON o.amount > 100
          |ORDER BY u.name, o.amount;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1) shouldBe NumberValue(200)
      table.data(1).data(0) shouldBe TextValue("Bob")
      table.data(1).data(1) shouldBe NumberValue(150)
    }
  }

  "LATERAL with aggregation" - {
    "aggregate in lateral subquery" in {
      val table = query(
        s"""
          |$setup
          |SELECT u.name, o.total
          |FROM users u, LATERAL (SELECT SUM(amount) AS total FROM orders WHERE user_id = u.id) AS o
          |ORDER BY u.name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1) shouldBe NumberValue(300)
      table.data(1).data(0) shouldBe TextValue("Bob")
      table.data(1).data(1) shouldBe NumberValue(150)
      table.data(2).data(0) shouldBe TextValue("Carol")
      table.data(2).data(1) shouldBe NumberValue(0)
    }
  }

  "LATERAL with aliased table" - {
    "references aliased table" in {
      val table = query(
        s"""
          |$setup
          |SELECT u.name, o.amount
          |FROM users AS u, LATERAL (SELECT amount FROM orders WHERE user_id = u.id ORDER BY amount DESC LIMIT 1) AS o
          |ORDER BY u.name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1) shouldBe NumberValue(200)
      table.data(1).data(0) shouldBe TextValue("Bob")
      table.data(1).data(1) shouldBe NumberValue(150)
    }
  }

  "non-LATERAL subqueries still work" - {
    "regression: plain subquery in FROM" in {
      val table = query(
        s"""
          |$setup
          |SELECT sub.total FROM (SELECT SUM(amount) AS total FROM orders) AS sub;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(450)
    }
  }
}

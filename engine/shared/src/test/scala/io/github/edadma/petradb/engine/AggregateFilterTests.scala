package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AggregateFilterTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE orders (
      |  id SERIAL,
      |  customer TEXT NOT NULL,
      |  amount INT NOT NULL,
      |  status TEXT NOT NULL,
      |  PRIMARY KEY (id)
      |);
      |INSERT INTO orders (customer, amount, status) VALUES
      |  ('Alice', 100, 'paid'),
      |  ('Alice', 200, 'pending'),
      |  ('Alice', 50, 'paid'),
      |  ('Bob', 300, 'paid'),
      |  ('Bob', 150, 'cancelled'),
      |  ('Charlie', 80, 'pending'),
      |  ('Charlie', 120, 'paid');
      |""".trim.stripMargin

  "FILTER clause on aggregates" - {

    "COUNT with FILTER" in {
      val table = query(
        s"""
          |$setup
          |SELECT COUNT(*) FILTER (WHERE status = 'paid') FROM orders;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data.head.data.head.intValue shouldBe 4
    }

    "COUNT with FILTER vs total COUNT" in {
      val table = query(
        s"""
          |$setup
          |SELECT COUNT(*) AS total, COUNT(*) FILTER (WHERE status = 'paid') AS paid_count FROM orders;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data.head.data(0).intValue shouldBe 7
      table.data.head.data(1).intValue shouldBe 4
    }

    "SUM with FILTER" in {
      val table = query(
        s"""
          |$setup
          |SELECT SUM(amount) FILTER (WHERE status = 'paid') FROM orders;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      // paid amounts: 100 + 50 + 300 + 120 = 570
      table.data.head.data.head.intValue shouldBe 570
    }

    "AVG with FILTER" in {
      val table = query(
        s"""
          |$setup
          |SELECT AVG(amount) FILTER (WHERE status = 'pending') FROM orders;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      // pending amounts: 200 + 80 = 280, avg = 140
      table.data.head.data.head.doubleValue shouldBe 140.0
    }

    "MIN with FILTER" in {
      val table = query(
        s"""
          |$setup
          |SELECT MIN(amount) FILTER (WHERE status = 'paid') FROM orders;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data.head.data.head.intValue shouldBe 50
    }

    "MAX with FILTER" in {
      val table = query(
        s"""
          |$setup
          |SELECT MAX(amount) FILTER (WHERE status = 'paid') FROM orders;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data.head.data.head.intValue shouldBe 300
    }

    "FILTER with GROUP BY" in {
      val table = query(
        s"""
          |$setup
          |SELECT customer,
          |       COUNT(*) AS total,
          |       COUNT(*) FILTER (WHERE status = 'paid') AS paid
          |FROM orders
          |GROUP BY customer
          |ORDER BY customer;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      // Alice: total=3, paid=2
      table.data(0).data(0).string shouldBe "Alice"
      table.data(0).data(1).intValue shouldBe 3
      table.data(0).data(2).intValue shouldBe 2
      // Bob: total=2, paid=1
      table.data(1).data(0).string shouldBe "Bob"
      table.data(1).data(1).intValue shouldBe 2
      table.data(1).data(2).intValue shouldBe 1
      // Charlie: total=2, paid=1
      table.data(2).data(0).string shouldBe "Charlie"
      table.data(2).data(1).intValue shouldBe 2
      table.data(2).data(2).intValue shouldBe 1
    }

    "FILTER with no matching rows returns NULL for SUM" in {
      val table = query(
        s"""
          |$setup
          |SELECT SUM(amount) FILTER (WHERE status = 'refunded') FROM orders;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data.head.data.head.isNull shouldBe true
    }

    "FILTER with no matching rows returns 0 for COUNT" in {
      val table = query(
        s"""
          |$setup
          |SELECT COUNT(*) FILTER (WHERE status = 'refunded') FROM orders;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data.head.data.head.intValue shouldBe 0
    }

    "multiple different FILTER clauses" in {
      val table = query(
        s"""
          |$setup
          |SELECT
          |  SUM(amount) FILTER (WHERE status = 'paid') AS paid_total,
          |  SUM(amount) FILTER (WHERE status = 'pending') AS pending_total,
          |  SUM(amount) FILTER (WHERE status = 'cancelled') AS cancelled_total
          |FROM orders;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data.head.data(0).intValue shouldBe 570     // paid: 100+50+300+120
      table.data.head.data(1).intValue shouldBe 280     // pending: 200+80
      table.data.head.data(2).intValue shouldBe 150     // cancelled: 150
    }

    "FILTER with complex WHERE condition" in {
      val table = query(
        s"""
          |$setup
          |SELECT COUNT(*) FILTER (WHERE status = 'paid' AND amount > 100) FROM orders;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      // paid AND amount > 100: 300, 120 → count = 2
      table.data.head.data.head.intValue shouldBe 2
    }

    "FILTER with HAVING" in {
      val table = query(
        s"""
          |$setup
          |SELECT customer, COUNT(*) FILTER (WHERE status = 'paid') AS paid
          |FROM orders
          |GROUP BY customer
          |HAVING COUNT(*) FILTER (WHERE status = 'paid') >= 2
          |ORDER BY customer;
          |""".trim.stripMargin
      )

      // Only Alice has 2 paid orders
      table.data.length shouldBe 1
      table.data.head.data(0).string shouldBe "Alice"
      table.data.head.data(1).intValue shouldBe 2
    }
  }

  "FILTER clause — negative tests" - {

    "FILTER on scalar function should fail" in {
      an[Exception] should be thrownBy {
        query(
          s"""
            |$setup
            |SELECT upper('hello') FILTER (WHERE true) FROM orders;
            |""".trim.stripMargin
        )
      }
    }

    "FILTER on non-existent function should fail" in {
      an[Exception] should be thrownBy {
        query(
          s"""
            |$setup
            |SELECT bogus(1) FILTER (WHERE true) FROM orders;
            |""".trim.stripMargin
        )
      }
    }
  }
}

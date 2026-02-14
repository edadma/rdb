package io.github.edadma.rdb

import io.github.edadma.dal.{IntType => DIntType, DoubleType => DDoubleType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AggregateEdgeCaseTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE emp (
      | id SERIAL,
      | name TEXT,
      | department TEXT,
      | salary INT,
      | PRIMARY KEY (id)
      |);
      |INSERT INTO emp (name, department, salary) VALUES
      |  ('Alice', 'Engineering', 75000),
      |  ('Bob', 'Sales', 65000),
      |  ('Charlie', 'Engineering', 80000),
      |  ('Diana', 'Marketing', 70000),
      |  ('Eve', 'Sales', 60000),
      |  ('Frank', NULL, 55000);
      |""".trim.stripMargin

  "ORDER BY aggregate expression" - {
    "ORDER BY COUNT(*) DESC" in {
      val table = query(
        s"""
          |$setup
          |SELECT department, COUNT(*) FROM emp GROUP BY department ORDER BY COUNT(*) DESC;
          |""".trim.stripMargin
      )

      val counts = table.data.map(_.data(1).asInstanceOf[NumberValue].value.intValue)
      // Engineering=2, Sales=2, Marketing=1, NULL=1 — sorted descending
      table.data.length shouldBe 4
      counts shouldBe counts.sorted.reverse
    }

    "ORDER BY SUM(salary)" in {
      val table = query(
        s"""
          |$setup
          |SELECT department, SUM(salary) FROM emp GROUP BY department ORDER BY SUM(salary) DESC;
          |""".trim.stripMargin
      )

      val sums = table.data.map(_.data(1).asInstanceOf[NumberValue].value.intValue)
      // Should be in descending order
      sums shouldBe sums.sorted.reverse
    }
  }

  "HAVING without GROUP BY" - {
    "filters ungrouped aggregate" in {
      val table = query(
        s"""
          |$setup
          |SELECT COUNT(*) FROM emp HAVING COUNT(*) > 3;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data.head.data(0) shouldBe NumberValue(DIntType, 6)
    }

    "returns no rows when condition fails" in {
      val table = query(
        s"""
          |$setup
          |SELECT COUNT(*) FROM emp HAVING COUNT(*) > 100;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 0
    }
  }

  "GROUP BY with NULLs" - {
    "groups NULL values together" in {
      val table = query(
        s"""
          |$setup
          |SELECT department, COUNT(*) FROM emp GROUP BY department ORDER BY department;
          |""".trim.stripMargin
      )

      // Should have 4 groups: NULL, Engineering, Marketing, Sales
      table.data.length shouldBe 4
    }
  }

  "HAVING with compound conditions" - {
    "AND combines multiple aggregate conditions" in {
      val table = query(
        s"""
          |$setup
          |SELECT department, COUNT(*), SUM(salary) FROM emp
          |  GROUP BY department
          |  HAVING COUNT(*) > 1 AND SUM(salary) > 100000;
          |""".trim.stripMargin
      )

      val rows = table.data.map(_.data)
      // Engineering: count=2, sum=155000 — matches both
      // Sales: count=2, sum=125000 — matches both
      rows.length shouldBe 2
    }

    "OR with aggregate conditions" in {
      val table = query(
        s"""
          |$setup
          |SELECT department, COUNT(*), SUM(salary) FROM emp
          |  GROUP BY department
          |  HAVING COUNT(*) > 1 OR SUM(salary) > 70000;
          |""".trim.stripMargin
      )

      // Engineering: count=2 (matches), Sales: count=2 (matches), Marketing: sum=70000 (not > 70000)
      // NULL dept: sum=55000 (no match)
      // Should be Engineering and Sales
      table.data.length should be >= 2
    }
  }

  "ORDER BY alias" - {
    "sorts by column alias" in {
      val table = query(
        s"""
          |$setup
          |SELECT department, COUNT(*) as cnt FROM emp GROUP BY department ORDER BY cnt DESC;
          |""".trim.stripMargin
      )

      val counts = table.data.map(_.data(1).asInstanceOf[NumberValue].value.intValue)
      counts shouldBe counts.sorted.reverse
    }
  }

  "Aggregate + GROUP BY + ORDER BY + LIMIT" - {
    "top-N query" in {
      val table = query(
        s"""
          |$setup
          |SELECT department, SUM(salary) as total FROM emp
          |  GROUP BY department
          |  ORDER BY total DESC
          |  LIMIT 2;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      // First should be Engineering (155000), second Sales (125000)
      val totals = table.data.map(_.data(1).asInstanceOf[NumberValue].value.intValue)
      totals(0) should be > totals(1)
    }
  }
}

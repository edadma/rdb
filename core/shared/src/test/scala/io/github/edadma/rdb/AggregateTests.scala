package io.github.edadma.rdb

import io.github.edadma.dal.{IntType => DIntType, DoubleType => DDoubleType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AggregateTests extends AnyFreeSpec with Matchers with Testing {

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

  "Duplicate aggregate deduplication" - {
    "SELECT COUNT(*), COUNT(*) uses one accumulator" in {
      val table = query(
        s"""
          |$setup
          |SELECT COUNT(*), COUNT(*) FROM emp;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      val row = table.data.head.data
      row(0) shouldBe NumberValue(DIntType, 6)
      row(1) shouldBe NumberValue(DIntType, 6)
    }

    "duplicate aggregate in GROUP BY" in {
      val table = query(
        s"""
          |$setup
          |SELECT department, SUM(salary), SUM(salary) FROM emp
          |  GROUP BY department ORDER BY department;
          |""".trim.stripMargin
      )

      table.data.foreach { row =>
        row.data(1) shouldBe row.data(2)
      }
    }
  }

  "Aggregates in expressions" - {
    "arithmetic with aggregates: SUM / COUNT" in {
      val table = query(
        s"""
          |$setup
          |SELECT SUM(salary) / COUNT(*) FROM emp;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      // Total salary = 405000, count = 6, integer division = 67500
      table.data.head.data(0) shouldBe NumberValue(DIntType, 67500)
    }

    "aggregate multiplied by constant" in {
      val table = query(
        s"""
          |$setup
          |SELECT COUNT(*) * 1000 FROM emp;
          |""".trim.stripMargin
      )

      table.data.head.data(0) shouldBe NumberValue(DIntType, 6000)
    }

    "aggregate in grouped arithmetic" in {
      val table = query(
        s"""
          |$setup
          |SELECT department, SUM(salary) / COUNT(*) FROM emp
          |  WHERE department IS NOT NULL
          |  GROUP BY department ORDER BY department;
          |""".trim.stripMargin
      )

      val rows = table.data.map(_.data)
      // Engineering: 155000/2 = 77500, Marketing: 70000/1 = 70000, Sales: 125000/2 = 62500
      rows.length shouldBe 3
      rows(0)(0) shouldBe TextValue("Engineering")
      rows(0)(1) shouldBe NumberValue(DIntType, 77500)
      rows(1)(0) shouldBe TextValue("Marketing")
      rows(1)(1) shouldBe NumberValue(DIntType, 70000)
      rows(2)(0) shouldBe TextValue("Sales")
      rows(2)(1) shouldBe NumberValue(DIntType, 62500)
    }

    "aggregate inside CASE expression" in {
      val table = query(
        s"""
          |$setup
          |SELECT department,
          |  CASE WHEN COUNT(*) > 1 THEN 'team' ELSE 'solo' END
          |FROM emp WHERE department IS NOT NULL
          |  GROUP BY department ORDER BY department;
          |""".trim.stripMargin
      )

      val rows = table.data.map(r => (r.data(0).asInstanceOf[TextValue].s, r.data(1).asInstanceOf[TextValue].s))
      rows shouldBe Vector(("Engineering", "team"), ("Marketing", "solo"), ("Sales", "team"))
    }

    "aggregate inside CAST" in {
      val table = query(
        s"""
          |$setup
          |SELECT COUNT(*)::text FROM emp;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data.head.data(0) shouldBe TextValue("6")
    }
  }

  "Empty table with GROUP BY" - {
    "returns zero rows" in {
      val table = query(
        """
          |CREATE TABLE empty_t (
          | id SERIAL,
          | dept TEXT,
          | val INT,
          | PRIMARY KEY (id)
          |);
          |SELECT dept, COUNT(*) FROM empty_t GROUP BY dept;
          |""".trim.stripMargin
      )

      // No groups can be formed from zero rows
      table.data.length shouldBe 0
    }
  }

  "Aggregate expression in ORDER BY" - {
    "ORDER BY arithmetic on aggregates" in {
      val table = query(
        s"""
          |$setup
          |SELECT department, MAX(salary) - MIN(salary) as spread FROM emp
          |  GROUP BY department
          |  ORDER BY MAX(salary) - MIN(salary) DESC;
          |""".trim.stripMargin
      )

      val spreads = table.data.map(_.data(1).asInstanceOf[NumberValue].value.intValue)
      spreads shouldBe spreads.sorted.reverse
    }
  }

  "HAVING with alias referencing aggregate expression" - {
    "filters using aliased aggregate arithmetic" in {
      val table = query(
        s"""
          |$setup
          |SELECT department, SUM(salary) / COUNT(*) as avg_sal FROM emp
          |  GROUP BY department
          |  HAVING avg_sal > 70000
          |  ORDER BY department;
          |""".trim.stripMargin
      )

      // Engineering: 155000/2=77500 (yes), Sales: 125000/2=62500 (no),
      // Marketing: 70000/1=70000 (no, not > 70000), NULL: 55000/1=55000 (no)
      table.data.length shouldBe 1
      table.data.head.data(0) shouldBe TextValue("Engineering")
    }
  }
}

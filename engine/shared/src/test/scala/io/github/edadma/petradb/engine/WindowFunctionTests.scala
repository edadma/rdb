package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class WindowFunctionTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE emp (
      |  id SERIAL,
      |  name TEXT NOT NULL,
      |  department TEXT NOT NULL,
      |  salary INT NOT NULL,
      |  PRIMARY KEY (id)
      |);
      |INSERT INTO emp (name, department, salary) VALUES
      |  ('Alice', 'Engineering', 90000),
      |  ('Bob', 'Engineering', 80000),
      |  ('Charlie', 'Engineering', 80000),
      |  ('Diana', 'Sales', 70000),
      |  ('Eve', 'Sales', 60000),
      |  ('Frank', 'Marketing', 75000);
      |""".trim.stripMargin

  "ROW_NUMBER" - {

    "basic ROW_NUMBER with ORDER BY" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary, ROW_NUMBER() OVER (ORDER BY salary DESC) AS rn
          |FROM emp
          |ORDER BY salary DESC, name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
      // Sorted by salary DESC: Alice(90000)=1, Bob/Charlie(80000)=2/3, Frank(75000)=4, Diana(70000)=5, Eve(60000)=6
      val rns = table.data.map(_.data(2).intValue)
      rns shouldBe Vector(1, 2, 3, 4, 5, 6)
      table.data.head.data(0).string shouldBe "Alice"
    }

    "ROW_NUMBER with PARTITION BY" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, department, ROW_NUMBER() OVER (PARTITION BY department ORDER BY salary DESC) AS rn
          |FROM emp
          |ORDER BY department, salary DESC;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6

      // Engineering partition: Alice(1), Bob/Charlie(2,3)
      val eng = table.data.filter(_.data(1).string == "Engineering")
      eng.length shouldBe 3
      eng.map(_.data(2).intValue) shouldBe Vector(1, 2, 3)

      // Marketing partition: Frank(1)
      val mkt = table.data.filter(_.data(1).string == "Marketing")
      mkt.length shouldBe 1
      mkt.head.data(2).intValue shouldBe 1

      // Sales partition: Diana(1), Eve(2)
      val sales = table.data.filter(_.data(1).string == "Sales")
      sales.length shouldBe 2
      sales.map(_.data(2).intValue) shouldBe Vector(1, 2)
    }

    "ROW_NUMBER with no PARTITION BY and no ORDER BY" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, ROW_NUMBER() OVER () AS rn
          |FROM emp;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
      val rns = table.data.map(_.data(1).intValue).sorted
      rns shouldBe Vector(1, 2, 3, 4, 5, 6)
    }
  }

  "RANK" - {

    "RANK with ties" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary, RANK() OVER (ORDER BY salary DESC) AS rnk
          |FROM emp
          |ORDER BY salary DESC, name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
      val ranks = table.data.map(_.data(2).intValue)
      // 90000→1, 80000→2, 80000→2, 75000→4, 70000→5, 60000→6
      ranks shouldBe Vector(1, 2, 2, 4, 5, 6)
    }

    "RANK with PARTITION BY" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, department, salary, RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS rnk
          |FROM emp
          |ORDER BY department, salary DESC;
          |""".trim.stripMargin
      )

      // Engineering: Alice(1), Bob(2), Charlie(2) — Bob and Charlie tie at 80000
      val eng = table.data.filter(_.data(1).string == "Engineering")
      eng.map(_.data(3).intValue) shouldBe Vector(1, 2, 2)
    }
  }

  "DENSE_RANK" - {

    "DENSE_RANK with ties — no gaps" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary, DENSE_RANK() OVER (ORDER BY salary DESC) AS drnk
          |FROM emp
          |ORDER BY salary DESC, name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
      val dranks = table.data.map(_.data(2).intValue)
      // 90000→1, 80000→2, 80000→2, 75000→3, 70000→4, 60000→5
      dranks shouldBe Vector(1, 2, 2, 3, 4, 5)
    }

    "DENSE_RANK with PARTITION BY" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, department, salary, DENSE_RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS drnk
          |FROM emp
          |ORDER BY department, salary DESC;
          |""".trim.stripMargin
      )

      // Engineering: Alice(1), Bob(2), Charlie(2)
      val eng = table.data.filter(_.data(1).string == "Engineering")
      eng.map(_.data(3).intValue) shouldBe Vector(1, 2, 2)

      // Sales: Diana(1), Eve(2)
      val sales = table.data.filter(_.data(1).string == "Sales")
      sales.map(_.data(3).intValue) shouldBe Vector(1, 2)
    }
  }

  "aggregate window functions" - {

    "SUM OVER (PARTITION BY)" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, department, salary, SUM(salary) OVER (PARTITION BY department) AS dept_total
          |FROM emp
          |ORDER BY department, name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6

      // Engineering total: 90000 + 80000 + 80000 = 250000
      val eng = table.data.filter(_.data(1).string == "Engineering")
      eng.foreach(_.data(3).intValue shouldBe 250000)

      // Sales total: 70000 + 60000 = 130000
      val sales = table.data.filter(_.data(1).string == "Sales")
      sales.foreach(_.data(3).intValue shouldBe 130000)

      // Marketing total: 75000
      val mkt = table.data.filter(_.data(1).string == "Marketing")
      mkt.foreach(_.data(3).intValue shouldBe 75000)
    }

    "COUNT OVER ()" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, COUNT(*) OVER () AS total_count
          |FROM emp;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
      table.data.foreach(_.data(1).intValue shouldBe 6)
    }

    "AVG OVER (PARTITION BY)" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, department, AVG(salary) OVER (PARTITION BY department) AS dept_avg
          |FROM emp
          |ORDER BY department, name;
          |""".trim.stripMargin
      )

      // Engineering avg: (90000 + 80000 + 80000) / 3 ≈ 83333.33
      val eng = table.data.filter(_.data(1).string == "Engineering")
      eng.head.data(2).doubleValue shouldBe (250000.0 / 3) +- 0.01
    }

    "aggregate window with FILTER" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, department, salary,
          |       COUNT(*) FILTER (WHERE salary >= 80000) OVER (PARTITION BY department) AS high_earners
          |FROM emp
          |ORDER BY department, name;
          |""".trim.stripMargin
      )

      // Engineering: Alice(90k) + Bob(80k) + Charlie(80k) → 3 high earners
      val eng = table.data.filter(_.data(1).string == "Engineering")
      eng.foreach(_.data(3).intValue shouldBe 3)

      // Sales: neither >= 80000 → 0
      val sales = table.data.filter(_.data(1).string == "Sales")
      sales.foreach(_.data(3).intValue shouldBe 0)
    }
  }

  "multiple window functions" - {

    "multiple different window functions in same query" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary,
          |       ROW_NUMBER() OVER (ORDER BY salary DESC) AS rn,
          |       RANK() OVER (ORDER BY salary DESC) AS rnk,
          |       DENSE_RANK() OVER (ORDER BY salary DESC) AS drnk
          |FROM emp
          |ORDER BY salary DESC, name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6

      // First row: Alice, 90000
      table.data.head.data(0).string shouldBe "Alice"
      table.data.head.data(2).intValue shouldBe 1 // row_number
      table.data.head.data(3).intValue shouldBe 1 // rank
      table.data.head.data(4).intValue shouldBe 1 // dense_rank

      // Check tied rows (salary = 80000, rows 2-3)
      val tied = table.data.filter(_.data(1).intValue == 80000)
      tied.length shouldBe 2
      tied.map(_.data(3).intValue).toSet shouldBe Set(2) // both rank 2
      tied.map(_.data(4).intValue).toSet shouldBe Set(2) // both dense_rank 2
    }

    "window function mixed with regular columns and WHERE" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary, ROW_NUMBER() OVER (ORDER BY salary DESC) AS rn
          |FROM emp
          |WHERE department = 'Engineering'
          |ORDER BY salary DESC;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      // Only Engineering rows, numbered within filtered set
      val rns = table.data.map(_.data(2).intValue)
      rns shouldBe Vector(1, 2, 3)
    }
  }

  "edge cases" - {

    "window function on empty result set" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, ROW_NUMBER() OVER (ORDER BY salary) AS rn
          |FROM emp
          |WHERE salary > 999999;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 0
    }

    "window function on single row" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, ROW_NUMBER() OVER (ORDER BY salary) AS rn,
          |       RANK() OVER (ORDER BY salary) AS rnk
          |FROM emp
          |WHERE name = 'Alice';
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data.head.data(1).intValue shouldBe 1
      table.data.head.data(2).intValue shouldBe 1
    }

    "ORDER BY ascending in window spec" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary, ROW_NUMBER() OVER (ORDER BY salary ASC) AS rn
          |FROM emp
          |ORDER BY salary ASC;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
      // Eve(60000) should be rn=1
      table.data.head.data(0).string shouldBe "Eve"
      table.data.head.data(2).intValue shouldBe 1
    }

    "multi-column ORDER BY in window spec" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, department, salary,
          |       ROW_NUMBER() OVER (ORDER BY department, salary DESC) AS rn
          |FROM emp
          |ORDER BY department, salary DESC;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
      val rns = table.data.map(_.data(3).intValue)
      rns shouldBe Vector(1, 2, 3, 4, 5, 6)
      // First should be Engineering + highest salary
      table.data.head.data(1).string shouldBe "Engineering"
    }

    "multi-column PARTITION BY" in {
      val setupExtra =
        """
          |CREATE TABLE sales (
          |  id SERIAL,
          |  region TEXT NOT NULL,
          |  category TEXT NOT NULL,
          |  amount INT NOT NULL,
          |  PRIMARY KEY (id)
          |);
          |INSERT INTO sales (region, category, amount) VALUES
          |  ('East', 'A', 100),
          |  ('East', 'A', 200),
          |  ('East', 'B', 300),
          |  ('West', 'A', 400);
          |""".trim.stripMargin

      val table = query(
        s"""
          |$setupExtra
          |SELECT region, category, amount,
          |       SUM(amount) OVER (PARTITION BY region, category) AS subtotal
          |FROM sales
          |ORDER BY region, category, amount;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 4
      // East/A: 100+200=300
      table.data(0).data(3).intValue shouldBe 300
      table.data(1).data(3).intValue shouldBe 300
      // East/B: 300
      table.data(2).data(3).intValue shouldBe 300
      // West/A: 400
      table.data(3).data(3).intValue shouldBe 400
    }
  }

  "negative tests" - {

    "ROW_NUMBER with arguments should fail" in {
      an[Exception] should be thrownBy {
        query(
          s"""
            |$setup
            |SELECT ROW_NUMBER(salary) OVER (ORDER BY salary) FROM emp;
            |""".trim.stripMargin
        )
      }
    }

    "RANK with arguments should fail" in {
      an[Exception] should be thrownBy {
        query(
          s"""
            |$setup
            |SELECT RANK(salary) OVER (ORDER BY salary) FROM emp;
            |""".trim.stripMargin
        )
      }
    }

    "scalar function with OVER should fail" in {
      an[Exception] should be thrownBy {
        query(
          s"""
            |$setup
            |SELECT upper(name) OVER (ORDER BY salary) FROM emp;
            |""".trim.stripMargin
        )
      }
    }

    "unknown function with OVER should fail" in {
      an[Exception] should be thrownBy {
        query(
          s"""
            |$setup
            |SELECT bogus() OVER (ORDER BY salary) FROM emp;
            |""".trim.stripMargin
        )
      }
    }
  }
}

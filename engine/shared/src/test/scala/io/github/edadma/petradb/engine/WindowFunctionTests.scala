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

  "LAG" - {

    "basic LAG with default offset" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary, LAG(salary) OVER (ORDER BY salary) AS prev_salary
          |FROM emp
          |ORDER BY salary;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
      // First row has no previous → NULL
      table.data.head.data(2).isNull shouldBe true
      // Second row: previous salary
      table.data(1).data(2).intValue shouldBe table.data(0).data(1).intValue
    }

    "LAG with explicit offset" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary, LAG(salary, 2) OVER (ORDER BY salary) AS prev2_salary
          |FROM emp
          |ORDER BY salary;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
      // First two rows have no value 2 back → NULL
      table.data(0).data(2).isNull shouldBe true
      table.data(1).data(2).isNull shouldBe true
      // Third row: salary from first row
      table.data(2).data(2).intValue shouldBe table.data(0).data(1).intValue
    }

    "LAG with default value" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary, LAG(salary, 1, 0) OVER (ORDER BY salary) AS prev_salary
          |FROM emp
          |ORDER BY salary;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
      // First row: default value 0
      table.data.head.data(2).intValue shouldBe 0
      // Second row onwards: previous salary
      table.data(1).data(2).intValue shouldBe table.data(0).data(1).intValue
    }

    "LAG with PARTITION BY" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, department, salary,
          |       LAG(salary) OVER (PARTITION BY department ORDER BY salary) AS prev_salary
          |FROM emp
          |ORDER BY department, salary;
          |""".trim.stripMargin
      )

      // Engineering partition: 80000, 80000, 90000
      val eng = table.data.filter(_.data(1).string == "Engineering")
      eng(0).data(3).isNull shouldBe true     // first in partition
      eng(1).data(3).intValue shouldBe 80000  // previous
      eng(2).data(3).intValue shouldBe 80000  // previous
    }
  }

  "LEAD" - {

    "basic LEAD with default offset" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary, LEAD(salary) OVER (ORDER BY salary) AS next_salary
          |FROM emp
          |ORDER BY salary;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
      // Last row has no next → NULL
      table.data.last.data(2).isNull shouldBe true
      // First row: next salary
      table.data(0).data(2).intValue shouldBe table.data(1).data(1).intValue
    }

    "LEAD with explicit offset" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary, LEAD(salary, 2) OVER (ORDER BY salary) AS next2_salary
          |FROM emp
          |ORDER BY salary;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
      // Last two rows have no value 2 ahead → NULL
      table.data(4).data(2).isNull shouldBe true
      table.data(5).data(2).isNull shouldBe true
      // First row: salary from third row
      table.data(0).data(2).intValue shouldBe table.data(2).data(1).intValue
    }

    "LEAD with default value" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary, LEAD(salary, 1, -1) OVER (ORDER BY salary) AS next_salary
          |FROM emp
          |ORDER BY salary;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
      // Last row: default value -1
      table.data.last.data(2).intValue shouldBe -1
      // First row: next salary
      table.data(0).data(2).intValue shouldBe table.data(1).data(1).intValue
    }

    "LEAD with PARTITION BY" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, department, salary,
          |       LEAD(salary) OVER (PARTITION BY department ORDER BY salary) AS next_salary
          |FROM emp
          |ORDER BY department, salary;
          |""".trim.stripMargin
      )

      // Engineering partition: 80000, 80000, 90000
      val eng = table.data.filter(_.data(1).string == "Engineering")
      eng(0).data(3).intValue shouldBe 80000  // next
      eng(1).data(3).intValue shouldBe 90000  // next
      eng(2).data(3).isNull shouldBe true     // last in partition
    }
  }

  "NTILE" - {

    "NTILE divides into equal groups" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary, NTILE(3) OVER (ORDER BY salary) AS tile
          |FROM emp
          |ORDER BY salary;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
      val tiles = table.data.map(_.data(2).intValue)
      // 6 rows / 3 tiles = 2 per tile
      tiles shouldBe Vector(1, 1, 2, 2, 3, 3)
    }

    "NTILE with uneven distribution" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary, NTILE(4) OVER (ORDER BY salary) AS tile
          |FROM emp
          |ORDER BY salary;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
      val tiles = table.data.map(_.data(2).intValue)
      // 6 rows / 4 tiles: first 2 tiles get 2 rows, last 2 get 1 row
      tiles shouldBe Vector(1, 1, 2, 2, 3, 4)
    }

    "NTILE with more buckets than rows" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary, NTILE(10) OVER (ORDER BY salary) AS tile
          |FROM emp
          |ORDER BY salary;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
      val tiles = table.data.map(_.data(2).intValue)
      // Each row gets its own tile: 1, 2, 3, 4, 5, 6
      tiles shouldBe Vector(1, 2, 3, 4, 5, 6)
    }

    "NTILE with PARTITION BY" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, department, salary,
          |       NTILE(2) OVER (PARTITION BY department ORDER BY salary) AS tile
          |FROM emp
          |ORDER BY department, salary;
          |""".trim.stripMargin
      )

      // Engineering (3 rows): tile 1 gets 2 rows, tile 2 gets 1 row
      val eng = table.data.filter(_.data(1).string == "Engineering")
      eng.map(_.data(3).intValue) shouldBe Vector(1, 1, 2)

      // Sales (2 rows): 1 per tile
      val sales = table.data.filter(_.data(1).string == "Sales")
      sales.map(_.data(3).intValue) shouldBe Vector(1, 2)

      // Marketing (1 row): just tile 1
      val mkt = table.data.filter(_.data(1).string == "Marketing")
      mkt.map(_.data(3).intValue) shouldBe Vector(1)
    }

    "NTILE(1) puts all rows in one tile" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, NTILE(1) OVER (ORDER BY salary) AS tile
          |FROM emp;
          |""".trim.stripMargin
      )

      table.data.foreach(_.data(1).intValue shouldBe 1)
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

    "LAG with no arguments should fail" in {
      an[Exception] should be thrownBy {
        query(
          s"""
            |$setup
            |SELECT LAG() OVER (ORDER BY salary) FROM emp;
            |""".trim.stripMargin
        )
      }
    }

    "LAG with too many arguments should fail" in {
      an[Exception] should be thrownBy {
        query(
          s"""
            |$setup
            |SELECT LAG(salary, 1, 0, 'extra') OVER (ORDER BY salary) FROM emp;
            |""".trim.stripMargin
        )
      }
    }

    "NTILE with no arguments should fail" in {
      an[Exception] should be thrownBy {
        query(
          s"""
            |$setup
            |SELECT NTILE() OVER (ORDER BY salary) FROM emp;
            |""".trim.stripMargin
        )
      }
    }

    "NTILE with too many arguments should fail" in {
      an[Exception] should be thrownBy {
        query(
          s"""
            |$setup
            |SELECT NTILE(3, 4) OVER (ORDER BY salary) FROM emp;
            |""".trim.stripMargin
        )
      }
    }
  }

  "frame specifications" - {

    "running SUM with ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary,
          |  SUM(salary) OVER (ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
          |FROM emp ORDER BY salary;
          |""".trim.stripMargin
      )
      // salary order: 60000, 70000, 75000, 80000, 80000, 90000
      // running sum:  60000, 130000, 205000, 285000, 365000, 455000
      val sums = table.data.map(_.data(2).intValue)
      sums shouldBe Vector(60000, 130000, 205000, 285000, 365000, 455000)
    }

    "running COUNT with ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary,
          |  COUNT(*) OVER (ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
          |FROM emp ORDER BY salary;
          |""".trim.stripMargin
      )
      val counts = table.data.map(_.data(2).intValue)
      counts shouldBe Vector(1, 2, 3, 4, 5, 6)
    }

    "sliding window SUM with ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary,
          |  SUM(salary) OVER (ORDER BY salary ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING)
          |FROM emp ORDER BY salary;
          |""".trim.stripMargin
      )
      // salary order: 60000, 70000, 75000, 80000, 80000, 90000
      val sums = table.data.map(_.data(2).intValue)
      sums shouldBe Vector(130000, 205000, 225000, 235000, 250000, 170000)
    }

    "ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary,
          |  SUM(salary) OVER (ORDER BY salary ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING)
          |FROM emp ORDER BY salary;
          |""".trim.stripMargin
      )
      // salary order: 60000, 70000, 75000, 80000, 80000, 90000 (total = 455000)
      val sums = table.data.map(_.data(2).intValue)
      sums shouldBe Vector(455000, 395000, 325000, 250000, 170000, 90000)
    }

    "ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING is same as whole partition" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary,
          |  SUM(salary) OVER (ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING)
          |FROM emp ORDER BY salary;
          |""".trim.stripMargin
      )
      val sums = table.data.map(_.data(2).intValue)
      sums shouldBe Vector.fill(6)(455000)
    }

    "frame with PARTITION BY" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, department, salary,
          |  SUM(salary) OVER (PARTITION BY department ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
          |FROM emp ORDER BY department, salary;
          |""".trim.stripMargin
      )
      // Engineering (80000, 80000, 90000): running sums 80000, 160000, 250000
      // Marketing (75000): 75000
      // Sales (60000, 70000): 60000, 130000
      val sums = table.data.map(_.data(3).intValue)
      sums shouldBe Vector(80000, 160000, 250000, 75000, 60000, 130000)
    }

    "AVG with frame" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary,
          |  AVG(salary) OVER (ORDER BY salary ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING)
          |FROM emp ORDER BY salary;
          |""".trim.stripMargin
      )
      // salary order: 60000, 70000, 75000, 80000, 80000, 90000
      val avgs = table.data.map(_.data(2).doubleValue)
      avgs(0) shouldBe 65000.0 +- 1.0
      avgs(2) shouldBe 75000.0 +- 1.0
      avgs(5) shouldBe 85000.0 +- 1.0
    }

    "ROWS BETWEEN 2 PRECEDING AND CURRENT ROW" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary,
          |  SUM(salary) OVER (ORDER BY salary ROWS BETWEEN 2 PRECEDING AND CURRENT ROW)
          |FROM emp ORDER BY salary;
          |""".trim.stripMargin
      )
      val sums = table.data.map(_.data(2).intValue)
      sums shouldBe Vector(60000, 130000, 205000, 225000, 235000, 250000)
    }

    "frame with FILTER" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary,
          |  SUM(salary) FILTER (WHERE salary > 65000) OVER (ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
          |FROM emp ORDER BY salary;
          |""".trim.stripMargin
      )
      // salary order: 60000, 70000, 75000, 80000, 80000, 90000
      // filtered running sum (skip 60000):
      //   row 1: 70000
      //   row 2: 70000+75000 = 145000
      //   row 5: 70000+75000+80000+80000+90000 = 395000
      table.data(1).data(2).intValue shouldBe 70000
      table.data(2).data(2).intValue shouldBe 145000
      table.data(5).data(2).intValue shouldBe 395000
    }

    "no frame uses entire partition" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, salary,
          |  SUM(salary) OVER (ORDER BY salary)
          |FROM emp ORDER BY salary;
          |""".trim.stripMargin
      )
      // Without frame, all rows get total of partition (whole table)
      val sums = table.data.map(_.data(2).intValue)
      sums shouldBe Vector.fill(6)(455000)
    }
  }
}

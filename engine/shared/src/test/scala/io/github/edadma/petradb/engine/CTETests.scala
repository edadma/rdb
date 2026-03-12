package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CTETests extends AnyFreeSpec with Matchers with Testing:

  val setup: String =
    """
      |CREATE TABLE employees (
      |  id INT,
      |  name TEXT,
      |  dept TEXT,
      |  salary INT
      |);
      |INSERT INTO employees (id, name, dept, salary) VALUES
      |  (1, 'Alice', 'Engineering', 90000),
      |  (2, 'Bob', 'Engineering', 80000),
      |  (3, 'Carol', 'Marketing', 70000),
      |  (4, 'Dave', 'Marketing', 60000),
      |  (5, 'Eve', 'Sales', 50000);
      |CREATE TABLE orders (
      |  id INT,
      |  employee_id INT,
      |  amount INT
      |);
      |INSERT INTO orders (id, employee_id, amount) VALUES
      |  (1, 1, 500),
      |  (2, 1, 300),
      |  (3, 2, 200),
      |  (4, 3, 400);
      |""".trim.stripMargin

  "basic CTE" - {

    "simple CTE" in {
      val t = query(s"$setup; WITH eng AS (SELECT name FROM employees WHERE dept = 'Engineering') SELECT name FROM eng ORDER BY name")
      t.data.map(_.data(0).asInstanceOf[TextValue].s) shouldBe Seq("Alice", "Bob")
    }

    "CTE with column aliases" in {
      val t = query(s"$setup; WITH t(n, s) AS (SELECT name, salary FROM employees WHERE dept = 'Sales') SELECT n, s FROM t")
      t.data.map(_.data(0).asInstanceOf[TextValue].s) shouldBe Seq("Eve")
      t.data.map(_.data(1).intValue) shouldBe Seq(50000)
    }

    "CTE referenced multiple times" in {
      val t = query(
        s"""$setup;
           |WITH dept_stats AS (
           |  SELECT dept, COUNT(*) AS cnt FROM employees GROUP BY dept
           |)
           |SELECT a.dept, b.dept
           |FROM dept_stats a CROSS JOIN dept_stats b
           |WHERE a.dept < b.dept
           |ORDER BY a.dept, b.dept""".stripMargin)
      // Engineering < Marketing, Engineering < Sales, Marketing < Sales = 3 rows
      t.data.length shouldBe 3
    }

    "CTE with aggregation" in {
      val t = query(
        s"""$setup;
           |WITH dept_avg AS (
           |  SELECT dept, AVG(salary) AS avg_sal FROM employees GROUP BY dept
           |)
           |SELECT dept, avg_sal FROM dept_avg ORDER BY dept""".stripMargin)
      t.data.length shouldBe 3
      t.data.map(_.data(0).asInstanceOf[TextValue].s) shouldBe Seq("Engineering", "Marketing", "Sales")
    }
  }

  "multiple CTEs" - {

    "two independent CTEs" in {
      val t = query(
        s"""$setup;
           |WITH
           |  eng AS (SELECT name FROM employees WHERE dept = 'Engineering'),
           |  mkt AS (SELECT name FROM employees WHERE dept = 'Marketing')
           |SELECT name FROM eng
           |UNION ALL
           |SELECT name FROM mkt
           |ORDER BY name""".stripMargin)
      t.data.map(_.data(0).asInstanceOf[TextValue].s) shouldBe Seq("Alice", "Bob", "Carol", "Dave")
    }

    "CTE referencing earlier CTE" in {
      val t = query(
        s"""$setup;
           |WITH
           |  high_earners AS (SELECT id, name, salary FROM employees WHERE salary > 60000),
           |  top AS (SELECT name, salary FROM high_earners WHERE salary > 80000)
           |SELECT name FROM top""".stripMargin)
      t.data.map(_.data(0).asInstanceOf[TextValue].s) shouldBe Seq("Alice")
    }
  }

  "CTE with joins" - {

    "CTE joined with regular table" in {
      val t = query(
        s"""$setup;
           |WITH eng AS (SELECT id, name FROM employees WHERE dept = 'Engineering')
           |SELECT e.name, o.amount
           |FROM eng e INNER JOIN orders o ON e.id = o.employee_id
           |ORDER BY e.name, o.amount""".stripMargin)
      t.data.length shouldBe 3
      t.data.map(_.data(0).asInstanceOf[TextValue].s) shouldBe Seq("Alice", "Alice", "Bob")
    }

    "two CTEs joined together" in {
      val t = query(
        s"""$setup;
           |WITH
           |  emp AS (SELECT id, name FROM employees),
           |  ord AS (SELECT employee_id, SUM(amount) AS total FROM orders GROUP BY employee_id)
           |SELECT emp.name, ord.total
           |FROM emp INNER JOIN ord ON emp.id = ord.employee_id
           |ORDER BY emp.name""".stripMargin)
      t.data.length shouldBe 3
    }
  }

  "CTE in subquery" - {

    "CTE with subquery in WHERE" in {
      val t = query(
        s"""$setup;
           |WITH high AS (SELECT id FROM employees WHERE salary > 70000)
           |SELECT name FROM employees WHERE id IN (SELECT id FROM high) ORDER BY name""".stripMargin)
      t.data.map(_.data(0).asInstanceOf[TextValue].s) shouldBe Seq("Alice", "Bob")
    }
  }

  "CTE name shadowing" - {

    "CTE shadows table name" in {
      val t = query(
        s"""$setup;
           |WITH employees AS (SELECT 1 AS id, 'Shadow' AS name)
           |SELECT name FROM employees""".stripMargin)
      t.data.map(_.data(0).asInstanceOf[TextValue].s) shouldBe Seq("Shadow")
    }
  }

  "CTE case insensitivity" - {

    "CTE name is case-insensitive" in {
      val t = query(
        s"""$setup;
           |WITH MyData AS (SELECT name FROM employees WHERE dept = 'Sales')
           |SELECT name FROM mydata""".stripMargin)
      t.data.map(_.data(0).asInstanceOf[TextValue].s) shouldBe Seq("Eve")
    }
  }

  "CTE with ORDER BY and LIMIT" - {

    "CTE body with ORDER BY and LIMIT" in {
      val t = query(
        s"""$setup;
           |WITH top3 AS (SELECT name, salary FROM employees ORDER BY salary DESC LIMIT 3)
           |SELECT name FROM top3 ORDER BY name""".stripMargin)
      t.data.map(_.data(0).asInstanceOf[TextValue].s) shouldBe Seq("Alice", "Bob", "Carol")
    }
  }

  "error cases" - {

    "undefined CTE reference" in {
      an[Exception] should be thrownBy {
        query(s"$setup; WITH a AS (SELECT 1) SELECT * FROM b")
      }
    }

    "later CTE cannot reference itself (not recursive)" in {
      an[Exception] should be thrownBy {
        query(s"$setup; WITH a AS (SELECT * FROM a) SELECT * FROM a")
      }
    }
  }

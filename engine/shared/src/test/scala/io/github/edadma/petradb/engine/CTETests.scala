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

  "recursive CTE" - {

    "generate series 1..10" in {
      val t = query(
        """WITH RECURSIVE nums(n) AS (
          |  SELECT 1
          |  UNION ALL
          |  SELECT n + 1 FROM nums WHERE n < 10
          |)
          |SELECT n FROM nums ORDER BY n""".stripMargin)
      t.data.map(_.data(0).intValue) shouldBe (1 to 10)
    }

    "factorial" in {
      val t = query(
        """WITH RECURSIVE factorial(n, f) AS (
          |  SELECT 1, 1
          |  UNION ALL
          |  SELECT n + 1, f * (n + 1) FROM factorial WHERE n < 5
          |)
          |SELECT n, f FROM factorial ORDER BY n""".stripMargin)
      t.data.map(_.data(0).intValue) shouldBe Seq(1, 2, 3, 4, 5)
      t.data.map(_.data(1).intValue) shouldBe Seq(1, 2, 6, 24, 120)
    }

    "fibonacci sequence" in {
      val t = query(
        """WITH RECURSIVE fib(n, a, b) AS (
          |  SELECT 1, 0, 1
          |  UNION ALL
          |  SELECT n + 1, b, a + b FROM fib WHERE n < 8
          |)
          |SELECT n, a FROM fib ORDER BY n""".stripMargin)
      t.data.map(_.data(1).intValue) shouldBe Seq(0, 1, 1, 2, 3, 5, 8, 13)
    }

    "tree traversal" in {
      val t = query(
        s"""$setup;
           |CREATE TABLE tree (id INT, parent_id INT, name TEXT);
           |INSERT INTO tree (id, parent_id, name) VALUES
           |  (1, NULL, 'root'),
           |  (2, 1, 'child1'),
           |  (3, 1, 'child2'),
           |  (4, 2, 'grandchild1'),
           |  (5, 3, 'grandchild2');
           |WITH RECURSIVE descendants(id, name, depth) AS (
           |  SELECT id, name, 0 FROM tree WHERE parent_id IS NULL
           |  UNION ALL
           |  SELECT t.id, t.name, d.depth + 1
           |  FROM tree t INNER JOIN descendants d ON t.parent_id = d.id
           |)
           |SELECT name, depth FROM descendants ORDER BY depth, name""".stripMargin)
      t.data.map(_.data(0).asInstanceOf[TextValue].s) shouldBe Seq("root", "child1", "child2", "grandchild1", "grandchild2")
      t.data.map(_.data(1).intValue) shouldBe Seq(0, 1, 1, 2, 2)
    }

    "UNION (deduplicated) recursive CTE" in {
      val t = query(
        """WITH RECURSIVE nums(n) AS (
          |  SELECT 1
          |  UNION
          |  SELECT n + 1 FROM nums WHERE n < 6
          |)
          |SELECT n FROM nums ORDER BY n""".stripMargin)
      t.data.map(_.data(0).intValue) shouldBe (1 to 6)
    }

    "recursive CTE with non-recursive CTE" in {
      val t = query(
        s"""$setup;
           |WITH RECURSIVE
           |  eng AS (SELECT id, name FROM employees WHERE dept = 'Engineering'),
           |  nums(n) AS (
           |    SELECT 1
           |    UNION ALL
           |    SELECT n + 1 FROM nums WHERE n < 3
           |  )
           |SELECT e.name, nums.n
           |FROM eng e CROSS JOIN nums
           |ORDER BY e.name, nums.n""".stripMargin)
      t.data.length shouldBe 6
    }

    "recursive CTE terminates on empty result" in {
      val t = query(
        """WITH RECURSIVE countdown(n) AS (
          |  SELECT 5
          |  UNION ALL
          |  SELECT n - 1 FROM countdown WHERE n > 1
          |)
          |SELECT n FROM countdown ORDER BY n""".stripMargin)
      t.data.map(_.data(0).intValue) shouldBe Seq(1, 2, 3, 4, 5)
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

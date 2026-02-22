package io.github.edadma.petradb

import io.github.edadma.dal.{IntType => DIntType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class JoinTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE departments (
      | id INT,
      | name TEXT
      |);
      |CREATE TABLE employees (
      | id INT,
      | name TEXT,
      | dept_id INT
      |);
      |INSERT INTO departments (id, name) VALUES (1, 'Engineering'), (2, 'Marketing'), (3, 'Sales');
      |INSERT INTO employees (id, name, dept_id) VALUES
      |  (1, 'Alice', 1),
      |  (2, 'Bob', 1),
      |  (3, 'Carol', 2),
      |  (4, 'Dave', 4);
      |""".trim.stripMargin

  "INNER JOIN" - {
    "returns only matching rows" in {
      val table = query(
        s"""
          |$setup
          |SELECT e.name, d.name FROM employees e INNER JOIN departments d ON e.dept_id = d.id ORDER BY e.name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1) shouldBe TextValue("Engineering")
      table.data(1).data(0) shouldBe TextValue("Bob")
      table.data(1).data(1) shouldBe TextValue("Engineering")
      table.data(2).data(0) shouldBe TextValue("Carol")
      table.data(2).data(1) shouldBe TextValue("Marketing")
    }

    "bare JOIN defaults to INNER" in {
      val table = query(
        s"""
          |$setup
          |SELECT e.name, d.name FROM employees e JOIN departments d ON e.dept_id = d.id ORDER BY e.name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
    }
  }

  "LEFT JOIN" - {
    "returns all left rows with NULLs for non-matching right" in {
      val table = query(
        s"""
          |$setup
          |SELECT e.name, d.name FROM employees e LEFT JOIN departments d ON e.dept_id = d.id ORDER BY e.name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 4
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1) shouldBe TextValue("Engineering")
      table.data(1).data(0) shouldBe TextValue("Bob")
      table.data(1).data(1) shouldBe TextValue("Engineering")
      table.data(2).data(0) shouldBe TextValue("Carol")
      table.data(2).data(1) shouldBe TextValue("Marketing")
      table.data(3).data(0) shouldBe TextValue("Dave")
      table.data(3).data(1).isNull shouldBe true
    }

    "LEFT OUTER JOIN is equivalent" in {
      val table = query(
        s"""
          |$setup
          |SELECT e.name, d.name FROM employees e LEFT OUTER JOIN departments d ON e.dept_id = d.id ORDER BY e.name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 4
      table.data(3).data(0) shouldBe TextValue("Dave")
      table.data(3).data(1).isNull shouldBe true
    }
  }

  "RIGHT JOIN" - {
    "returns all right rows with NULLs for non-matching left" in {
      val table = query(
        s"""
          |$setup
          |SELECT e.name, d.name FROM employees e RIGHT JOIN departments d ON e.dept_id = d.id ORDER BY d.name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 4
      table.data(0).data(1) shouldBe TextValue("Engineering")
      table.data(1).data(1) shouldBe TextValue("Engineering")
      table.data(2).data(1) shouldBe TextValue("Marketing")
      table.data(3).data(0).isNull shouldBe true
      table.data(3).data(1) shouldBe TextValue("Sales")
    }

    "RIGHT OUTER JOIN is equivalent" in {
      val table = query(
        s"""
          |$setup
          |SELECT e.name, d.name FROM employees e RIGHT OUTER JOIN departments d ON e.dept_id = d.id ORDER BY d.name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 4
      table.data(3).data(0).isNull shouldBe true
      table.data(3).data(1) shouldBe TextValue("Sales")
    }
  }

  "FULL JOIN" - {
    "returns all rows from both sides" in {
      val table = query(
        s"""
          |$setup
          |SELECT e.name, d.name FROM employees e FULL JOIN departments d ON e.dept_id = d.id ORDER BY e.name NULLS LAST;
          |""".trim.stripMargin
      )

      // Alice+Engineering, Bob+Engineering, Carol+Marketing, Dave+NULL, NULL+Sales = 5 rows
      table.data.length shouldBe 5
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1) shouldBe TextValue("Engineering")
      table.data(3).data(0) shouldBe TextValue("Dave")
      table.data(3).data(1).isNull shouldBe true
      table.data(4).data(0).isNull shouldBe true
      table.data(4).data(1) shouldBe TextValue("Sales")
    }

    "FULL OUTER JOIN is equivalent" in {
      val table = query(
        s"""
          |$setup
          |SELECT e.name, d.name FROM employees e FULL OUTER JOIN departments d ON e.dept_id = d.id ORDER BY e.name NULLS LAST;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 5
    }

    "with no matches on either side" in {
      val table = query(
        """
          |CREATE TABLE t1 (id INT, val TEXT);
          |CREATE TABLE t2 (id INT, val TEXT);
          |INSERT INTO t1 (id, val) VALUES (1, 'a'), (2, 'b');
          |INSERT INTO t2 (id, val) VALUES (3, 'c'), (4, 'd');
          |SELECT t1.val, t2.val FROM t1 FULL JOIN t2 ON t1.id = t2.id ORDER BY t1.val NULLS LAST;
          |""".trim.stripMargin
      )

      // No matches: 2 left-only + 2 right-only = 4
      table.data.length shouldBe 4
      table.data(0).data(0) shouldBe TextValue("a")
      table.data(0).data(1).isNull shouldBe true
      table.data(1).data(0) shouldBe TextValue("b")
      table.data(1).data(1).isNull shouldBe true
      table.data(2).data(0).isNull shouldBe true
      table.data(2).data(1) shouldBe TextValue("c")
      table.data(3).data(0).isNull shouldBe true
      table.data(3).data(1) shouldBe TextValue("d")
    }
  }

  "CROSS JOIN" - {
    "produces cartesian product" in {
      val table = query(
        """
          |CREATE TABLE colors (name TEXT);
          |CREATE TABLE sizes (name TEXT);
          |INSERT INTO colors (name) VALUES ('Red'), ('Blue');
          |INSERT INTO sizes (name) VALUES ('S'), ('M'), ('L');
          |SELECT colors.name, sizes.name FROM colors CROSS JOIN sizes ORDER BY colors.name, sizes.name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
      table.data(0).data(0) shouldBe TextValue("Blue")
      table.data(0).data(1) shouldBe TextValue("L")
      table.data(5).data(0) shouldBe TextValue("Red")
      table.data(5).data(1) shouldBe TextValue("S")
    }

    "implicit cross join (FROM a, b) is equivalent" in {
      val table = query(
        """
          |CREATE TABLE colors (name TEXT);
          |CREATE TABLE sizes (name TEXT);
          |INSERT INTO colors (name) VALUES ('Red'), ('Blue');
          |INSERT INTO sizes (name) VALUES ('S'), ('M'), ('L');
          |SELECT colors.name, sizes.name FROM colors, sizes ORDER BY colors.name, sizes.name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
    }
  }

  "Multi-table JOIN" - {
    "chained INNER JOINs" in {
      val table = query(
        """
          |CREATE TABLE a (id INT, val TEXT);
          |CREATE TABLE b (id INT, a_id INT, val TEXT);
          |CREATE TABLE c (id INT, b_id INT, val TEXT);
          |INSERT INTO a (id, val) VALUES (1, 'x');
          |INSERT INTO b (id, a_id, val) VALUES (10, 1, 'y');
          |INSERT INTO c (id, b_id, val) VALUES (100, 10, 'z');
          |SELECT a.val, b.val, c.val FROM a JOIN b ON b.a_id = a.id JOIN c ON c.b_id = b.id;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("x")
      table.data(0).data(1) shouldBe TextValue("y")
      table.data(0).data(2) shouldBe TextValue("z")
    }

    "mixed join types" in {
      val table = query(
        s"""
          |$setup
          |CREATE TABLE projects (id INT, dept_id INT, name TEXT);
          |INSERT INTO projects (id, dept_id, name) VALUES (1, 1, 'Alpha'), (2, 5, 'Beta');
          |SELECT d.name, p.name FROM departments d LEFT JOIN projects p ON p.dept_id = d.id ORDER BY d.name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe TextValue("Engineering")
      table.data(0).data(1) shouldBe TextValue("Alpha")
      table.data(1).data(0) shouldBe TextValue("Marketing")
      table.data(1).data(1).isNull shouldBe true
      table.data(2).data(0) shouldBe TextValue("Sales")
      table.data(2).data(1).isNull shouldBe true
    }
  }

  "Self JOIN" - {
    "join table to itself with aliases" in {
      val table = query(
        """
          |CREATE TABLE employees (id INT, name TEXT, manager_id INT);
          |INSERT INTO employees (id, name, manager_id) VALUES
          |  (1, 'Alice', NULL),
          |  (2, 'Bob', 1),
          |  (3, 'Carol', 1);
          |SELECT e.name, m.name FROM employees e LEFT JOIN employees m ON e.manager_id = m.id ORDER BY e.name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1).isNull shouldBe true
      table.data(1).data(0) shouldBe TextValue("Bob")
      table.data(1).data(1) shouldBe TextValue("Alice")
      table.data(2).data(0) shouldBe TextValue("Carol")
      table.data(2).data(1) shouldBe TextValue("Alice")
    }
  }

  "JOIN with empty tables" - {
    "INNER JOIN with empty right table" in {
      val table = query(
        """
          |CREATE TABLE t1 (id INT, val TEXT);
          |CREATE TABLE t2 (id INT, t1_id INT, val TEXT);
          |INSERT INTO t1 (id, val) VALUES (1, 'a');
          |SELECT t1.val, t2.val FROM t1 INNER JOIN t2 ON t1.id = t2.t1_id;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 0
    }

    "LEFT JOIN with empty right table" in {
      val table = query(
        """
          |CREATE TABLE t1 (id INT, val TEXT);
          |CREATE TABLE t2 (id INT, t1_id INT, val TEXT);
          |INSERT INTO t1 (id, val) VALUES (1, 'a');
          |SELECT t1.val, t2.val FROM t1 LEFT JOIN t2 ON t1.id = t2.t1_id;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("a")
      table.data(0).data(1).isNull shouldBe true
    }

    "RIGHT JOIN with empty left table" in {
      val table = query(
        """
          |CREATE TABLE t1 (id INT, val TEXT);
          |CREATE TABLE t2 (id INT, val TEXT);
          |INSERT INTO t2 (id, val) VALUES (1, 'a');
          |SELECT t1.val, t2.val FROM t1 RIGHT JOIN t2 ON t1.id = t2.id;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0).isNull shouldBe true
      table.data(0).data(1) shouldBe TextValue("a")
    }

    "FULL JOIN with both tables empty" in {
      val table = query(
        """
          |CREATE TABLE t1 (id INT, val TEXT);
          |CREATE TABLE t2 (id INT, val TEXT);
          |SELECT t1.val, t2.val FROM t1 FULL JOIN t2 ON t1.id = t2.id;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 0
    }
  }
}

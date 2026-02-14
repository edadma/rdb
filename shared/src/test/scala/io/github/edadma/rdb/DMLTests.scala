package io.github.edadma.rdb

import io.github.edadma.dal.{IntType => DIntType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DMLTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE employees (
      | id SERIAL,
      | name TEXT,
      | salary INT,
      | department TEXT,
      | PRIMARY KEY (id)
      |);
      |INSERT INTO employees (name, salary, department) VALUES
      |  ('Alice', 75000, 'Engineering'),
      |  ('Bob', 65000, 'Sales'),
      |  ('Charlie', 80000, 'Engineering'),
      |  ('Diana', 70000, 'Marketing');
      |""".trim.stripMargin

  "UPDATE" - {
    "updates matching rows with WHERE" in {
      val table = query(
        s"""
          |$setup
          |UPDATE employees SET salary = 90000 WHERE name = 'Alice';
          |SELECT name, salary FROM employees WHERE name = 'Alice';
          |""".trim.stripMargin
      )

      val row = table.data.head.data
      row(0) shouldBe TextValue("Alice")
      row(1) shouldBe NumberValue(DIntType, 90000)
    }

    "updates multiple columns" in {
      val table = query(
        s"""
          |$setup
          |UPDATE employees SET salary = 100000, department = 'Management' WHERE name = 'Bob';
          |SELECT name, salary, department FROM employees WHERE name = 'Bob';
          |""".trim.stripMargin
      )

      val row = table.data.head.data
      row(0) shouldBe TextValue("Bob")
      row(1) shouldBe NumberValue(DIntType, 100000)
      row(2) shouldBe TextValue("Management")
    }

    "updates all rows without WHERE" in {
      val table = query(
        s"""
          |$setup
          |UPDATE employees SET department = 'General';
          |SELECT department FROM employees GROUP BY department;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data.head.data(0) shouldBe TextValue("General")
    }

    "returns correct update count" in {
      val res = results(
        s"""
          |$setup
          |UPDATE employees SET salary = 99000 WHERE department = 'Engineering';
          |""".trim.stripMargin
      )

      res.last shouldBe UpdateResult(2)
    }

    "returns zero count when no rows match" in {
      val res = results(
        s"""
          |$setup
          |UPDATE employees SET salary = 99000 WHERE department = 'Nonexistent';
          |""".trim.stripMargin
      )

      res.last shouldBe UpdateResult(0)
    }
  }

  "DELETE" - {
    "deletes matching rows with WHERE" in {
      val table = query(
        s"""
          |$setup
          |DELETE FROM employees WHERE name = 'Alice';
          |SELECT name FROM employees ORDER BY name;
          |""".trim.stripMargin
      )

      val names = table.data.map(_.data(0))
      names shouldBe Vector(TextValue("Bob"), TextValue("Charlie"), TextValue("Diana"))
    }

    "deletes all rows without WHERE" in {
      val table = query(
        s"""
          |$setup
          |DELETE FROM employees;
          |SELECT * FROM employees;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 0
    }

    "returns correct delete count" in {
      val res = results(
        s"""
          |$setup
          |DELETE FROM employees WHERE department = 'Engineering';
          |""".trim.stripMargin
      )

      res.last shouldBe DeleteResult(2)
    }

    "returns zero count when no rows match" in {
      val res = results(
        s"""
          |$setup
          |DELETE FROM employees WHERE department = 'Nonexistent';
          |""".trim.stripMargin
      )

      res.last shouldBe DeleteResult(0)
    }
  }
}

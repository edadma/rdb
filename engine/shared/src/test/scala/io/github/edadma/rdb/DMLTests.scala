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

  "INSERT without column list" - {
    "inserts with all columns specified positionally" in {
      val table = query(
        """
          |CREATE TABLE t (name TEXT, age INT);
          |INSERT INTO t VALUES ('Alice', 30), ('Bob', 25);
          |SELECT name, age FROM t ORDER BY name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1) shouldBe NumberValue(DIntType, 30)
      table.data(1).data(0) shouldBe TextValue("Bob")
      table.data(1).data(1) shouldBe NumberValue(DIntType, 25)
    }

    "inserts with serial column included" in {
      val table = query(
        """
          |CREATE TABLE t (id SERIAL, name TEXT);
          |INSERT INTO t VALUES (1, 'Alice');
          |SELECT id, name FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
      table.data(0).data(1) shouldBe TextValue("Alice")
    }

    "rejects wrong number of values" in {
      Console.withErr(java.io.OutputStream.nullOutputStream()) {
        an[Exception] should be thrownBy {
          query(
            """
              |CREATE TABLE t (name TEXT, age INT);
              |INSERT INTO t VALUES ('Alice');
              |SELECT * FROM t;
              |""".trim.stripMargin
          )
        }
      }
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

  "string escaping" - {
    "doubled single quotes in standard strings" in {
      val table = query(
        """
          |CREATE TABLE t (name TEXT);
          |INSERT INTO t (name) VALUES ('O''Brien');
          |SELECT name FROM t;
          |""".trim.stripMargin
      )

      table.data.head.data(0) shouldBe TextValue("O'Brien")
    }

    "multiple doubled quotes" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT);
          |INSERT INTO t (val) VALUES ('it''s a ''test''');
          |SELECT val FROM t;
          |""".trim.stripMargin
      )

      table.data.head.data(0) shouldBe TextValue("it's a 'test'")
    }

    "E-string backslash escapes still work" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT);
          |INSERT INTO t (val) VALUES (E'line1\nline2');
          |SELECT val FROM t;
          |""".trim.stripMargin
      )

      table.data.head.data(0) shouldBe TextValue("line1\nline2")
    }

    "empty string" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT);
          |INSERT INTO t (val) VALUES ('');
          |SELECT val FROM t;
          |""".trim.stripMargin
      )

      table.data.head.data(0) shouldBe TextValue("")
    }

    "string with only a quote" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT);
          |INSERT INTO t (val) VALUES ('''');
          |SELECT val FROM t;
          |""".trim.stripMargin
      )

      table.data.head.data(0) shouldBe TextValue("'")
    }
  }

  "<> operator" - {
    "filters with <> same as !=" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM employees WHERE department <> 'Engineering' ORDER BY name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("Bob")
      table.data(1).data(0) shouldBe TextValue("Diana")
    }

    "<> with integer values" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM employees WHERE salary <> 75000 ORDER BY name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      table.data.map(_.data(0)) should not contain TextValue("Alice")
    }
  }

  "Identifier case folding" - {
    "unquoted table name is case-insensitive" in {
      val table = query(
        """
          |CREATE TABLE Users (Name TEXT);
          |INSERT INTO users (name) VALUES ('Alice');
          |SELECT name FROM USERS;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("Alice")
    }

    "unquoted column name is case-insensitive" in {
      val table = query(
        """
          |CREATE TABLE t (MyCol INT);
          |INSERT INTO t (mycol) VALUES (42);
          |SELECT MYCOL FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(DIntType, 42)
    }

    "mixed-case keywords and identifiers" in {
      val table = query(
        """
          |Create Table Stuff (Id SERIAL, Label TEXT, Primary Key (Id));
          |Insert Into stuff (label) Values ('hello');
          |Select label From STUFF Where id = 1;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("hello")
    }

    "double-quoted identifier preserves case" in {
      val table = query(
        """
          |CREATE TABLE t ("MixedCase" TEXT);
          |INSERT INTO t ("MixedCase") VALUES ('yes');
          |SELECT "MixedCase" FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("yes")
    }

    "quoted uppercase column not accessible via unquoted name" in {
      suppressStderr {
        an[Exception] should be thrownBy query(
          """
            |CREATE TABLE t ("UPPER" TEXT);
            |INSERT INTO t ("UPPER") VALUES ('val');
            |SELECT UPPER FROM t;
            |""".trim.stripMargin
        )
      }
    }

    "quoted table name preserves case" in {
      val table = query(
        """
          |CREATE TABLE "MyTable" (x INT);
          |INSERT INTO "MyTable" (x) VALUES (1);
          |SELECT x FROM "MyTable";
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
    }
  }
}

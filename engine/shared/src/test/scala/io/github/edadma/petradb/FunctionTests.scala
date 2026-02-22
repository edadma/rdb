package io.github.edadma.petradb

import io.github.edadma.dal.{IntType => DIntType, DoubleType => DDoubleType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class FunctionTests extends AnyFreeSpec with Matchers with Testing {

  "Aggregate functions" - {
    "min, max, avg compute correct values" in {
      val table = query(
        """
          |CREATE TABLE numbers (
          | id SERIAL,
          | value INT,
          | PRIMARY KEY (id)
          |);
          |INSERT INTO numbers (value) VALUES (10), (20), (30), (5);
          |SELECT MIN(value), MAX(value), AVG(value) FROM numbers;
          |""".trim.stripMargin
      )

      val row = table.data.head.data
      row(0) shouldBe NumberValue(DIntType, 5)
      row(1) shouldBe NumberValue(DIntType, 30)
      row(2) shouldBe NumberValue(DDoubleType, 16.25)
    }

    "count with nulls correctly excludes nulls from COUNT(column)" in {
      val result = test(
        """
          |CREATE TABLE test_nulls (
          | id SERIAL,
          | value INT,
          | PRIMARY KEY (id)
          |);
          |INSERT INTO test_nulls (value) VALUES (10), (NULL), (20), (NULL), (30);
          |SELECT COUNT(value), COUNT(*) FROM test_nulls;
          |""".trim.stripMargin
      )
      
      // COUNT(value) should be 3 (excludes NULLs), COUNT(*) should be 5 (includes NULLs)
      result should include("NumberValue(typ = IntType, value = 3)") // COUNT(value) - excludes NULLs
      result should include("NumberValue(typ = IntType, value = 5)") // COUNT(*) - includes NULLs
    }

    "sum computes correct total" in {
      val table = query(
        """
          |CREATE TABLE items (
          | id SERIAL,
          | price INT,
          | PRIMARY KEY (id)
          |);
          |INSERT INTO items (price) VALUES (100), (200), (300);
          |SELECT SUM(price) FROM items;
          |""".trim.stripMargin
      )

      table.data.head.data(0) shouldBe NumberValue(DIntType, 600)
    }

    "aggregates with GROUP BY" in {
      val table = query(
        """
          |CREATE TABLE emp (
          | id SERIAL,
          | department TEXT,
          | salary INT,
          | PRIMARY KEY (id)
          |);
          |INSERT INTO emp (department, salary) VALUES
          |  ('Engineering', 75000), ('Sales', 65000),
          |  ('Engineering', 80000), ('Marketing', 70000);
          |SELECT department, COUNT(*), SUM(salary) FROM emp GROUP BY department ORDER BY department;
          |""".trim.stripMargin
      )

      val rows = table.data.map(_.data)
      rows.length shouldBe 3

      // Engineering: count=2, sum=155000
      rows(0)(0) shouldBe TextValue("Engineering")
      rows(0)(1) shouldBe NumberValue(DIntType, 2)
      rows(0)(2) shouldBe NumberValue(DIntType, 155000)

      // Marketing: count=1, sum=70000
      rows(1)(0) shouldBe TextValue("Marketing")
      rows(1)(1) shouldBe NumberValue(DIntType, 1)
      rows(1)(2) shouldBe NumberValue(DIntType, 70000)

      // Sales: count=1, sum=65000
      rows(2)(0) shouldBe TextValue("Sales")
      rows(2)(1) shouldBe NumberValue(DIntType, 1)
      rows(2)(2) shouldBe NumberValue(DIntType, 65000)
    }

    "ungrouped aggregates over whole table" in {
      val table = query(
        """
          |CREATE TABLE staff (
          | id SERIAL,
          | salary INT,
          | PRIMARY KEY (id)
          |);
          |INSERT INTO staff (salary) VALUES (50000), (60000), (70000), (80000);
          |SELECT COUNT(*), MIN(salary), MAX(salary), SUM(salary), AVG(salary) FROM staff;
          |""".trim.stripMargin
      )

      val row = table.data.head.data
      row(0) shouldBe NumberValue(DIntType, 4)
      row(1) shouldBe NumberValue(DIntType, 50000)
      row(2) shouldBe NumberValue(DIntType, 80000)
      row(3) shouldBe NumberValue(DIntType, 260000)
      row(4) shouldBe NumberValue(DDoubleType, 65000.0)
    }

    "aggregates handle empty results" in {
      val table = query(
        """
          |CREATE TABLE empty_test (
          | id SERIAL,
          | value INT,
          | PRIMARY KEY (id)
          |);
          |SELECT MIN(value), MAX(value), AVG(value), COUNT(value) FROM empty_test;
          |""".trim.stripMargin
      )

      // Standard SQL: aggregates on empty table return one row
      // COUNT→0, MIN/MAX/AVG→NULL
      table.data.length shouldBe 1
      val row = table.data.head.data
      row(0).isNull shouldBe true  // MIN
      row(1).isNull shouldBe true  // MAX
      row(2).isNull shouldBe true  // AVG
      row(3) shouldBe NumberValue(DIntType, 0)  // COUNT
    }
  }

  "String functions" - {
    "coalesce returns first non-null value" in {
      val result = test(
        """
          |CREATE TABLE test_text (
          | id SERIAL,
          | name TEXT,
          | PRIMARY KEY (id)
          |);
          |INSERT INTO test_text (name) VALUES ('Alice'), (NULL), ('Bob');
          |SELECT COALESCE(name, 'Unknown') FROM test_text;
          |""".trim.stripMargin
      )
      
      // Should replace NULL with 'Unknown'
      result should include("TextValue(\"Alice\")")
      result should include("TextValue(\"Unknown\")")  // NULL was replaced
      result should include("TextValue(\"Bob\")")
    }

    "nullif returns null when values match" in {
      val result = test(
        """
          |SELECT NULLIF('hello', 'hello'), NULLIF('hello', 'world');
          |""".trim.stripMargin
      )
      
      // First should be NULL (values match), second should be 'hello' (values don't match)
      result should include("NullValue()")
      result should include("TextValue(\"hello\")")
    }

    "split_part extracts correct field" in {
      val result = test(
        """
          |SELECT SPLIT_PART('a,b,c,d', ',', 1), SPLIT_PART('a,b,c,d', ',', 3), SPLIT_PART('a,b,c,d', ',', 99);
          |""".trim.stripMargin
      )
      
      result should include("TextValue(\"a\")")  // Field 1
      result should include("TextValue(\"c\")")  // Field 3
      result should include("TextValue(\"\")")   // Field 99 (out of range)
    }

    "reverse works correctly" in {
      val result = test(
        """
          |SELECT REVERSE('hello'), REVERSE('12345');
          |""".trim.stripMargin
      )
      
      result should include("TextValue(\"olleh\")")
      result should include("TextValue(\"54321\")")
    }
  }

  "Date/time functions" - {
    "now and current_date return timestamps" in {
      val result = test(
        """
          |SELECT NOW(), CURRENT_DATE();
          |""".trim.stripMargin
      )
      
      // Both should return TimestampValue instances
      result should include("TimestampValue")
    }

    "date_part extracts correct components" in {
      val result = test(
        """
          |SELECT DATE_PART('year', NOW()), DATE_PART('month', NOW()), DATE_PART('day', NOW());
          |""".trim.stripMargin
      )
      
      val currentYear = java.time.Year.now().getValue.toString
      result should include("NumberValue")
      result should include(currentYear)
    }

    "is null and is not null syntax work correctly" in {
      val result = test(
        """
          |CREATE TABLE test_is_null (
          | id SERIAL,
          | name TEXT,
          | PRIMARY KEY (id)
          |);
          |INSERT INTO test_is_null (name) VALUES ('Alice'), (NULL), ('Bob');
          |SELECT name FROM test_is_null WHERE name IS NOT NULL ORDER BY name;
          |""".trim.stripMargin
      )
      
      // Should return only Alice and Bob, not the NULL row
      result should include("TextValue(\"Alice\")")
      result should include("TextValue(\"Bob\")")
      result should not include "NullValue()" // NULL row excluded by WHERE clause
    }
  }

  "Cast operator (::)" - {
    "cast string to double precision" in {
      val result = test(
        """
          |SELECT '123.45'::double precision, '0'::integer;
          |""".trim.stripMargin
      )

      result should include("NumberValue(typ = DoubleType, value = 123.45)")
      result should include("NumberValue(typ = IntType, value = 0)")
    }

    "cast values to text" in {
      val result = test(
        """
          |CREATE TABLE test_convert (
          | id SERIAL,
          | num INT,
          | PRIMARY KEY (id)
          |);
          |INSERT INTO test_convert (num) VALUES (456), (789);
          |SELECT num::text, id::text FROM test_convert;
          |""".trim.stripMargin
      )

      result should include("TextValue(\"456\")")
      result should include("TextValue(\"789\")")
      result should include("TextValue(\"1\")")
      result should include("TextValue(\"2\")")
    }
  }

  "CAST(expr AS type) syntax" - {
    "cast string to integer" in {
      val table = query("SELECT CAST('42' AS integer);")

      table.data.head.data(0) shouldBe NumberValue(DIntType, 42)
    }

    "cast integer to text" in {
      val table = query("SELECT CAST(123 AS text);")

      table.data.head.data(0) shouldBe TextValue("123")
    }

    "cast string to double precision" in {
      val table = query("SELECT CAST('3.14' AS double precision);")

      table.data.head.data(0) shouldBe NumberValue(DDoubleType, 3.14)
    }

    "cast in WHERE clause" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT);
          |INSERT INTO t (val) VALUES ('10'), ('20'), ('30');
          |SELECT val FROM t WHERE CAST(val AS integer) > 15 ORDER BY val;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("20")
      table.data(1).data(0) shouldBe TextValue("30")
    }

    "cast column reference" in {
      val table = query(
        """
          |CREATE TABLE t (price INT);
          |INSERT INTO t (price) VALUES (100), (250);
          |SELECT CAST(price AS text) FROM t ORDER BY price;
          |""".trim.stripMargin
      )

      table.data(0).data(0) shouldBe TextValue("100")
      table.data(1).data(0) shouldBe TextValue("250")
    }

    "cast nested in expression" in {
      val table = query("SELECT CAST('10' AS integer) + 5;")

      table.data.head.data(0) shouldBe NumberValue(DIntType, 15)
    }

    "cast NULL" in {
      val table = query("SELECT CAST(NULL AS integer);")

      table.data.head.data(0).isNull shouldBe true
    }

    "CAST and :: produce same result" in {
      val table = query("SELECT CAST('99' AS integer), '99'::integer;")

      table.data.head.data(0) shouldBe table.data.head.data(1)
    }
  }

  "HAVING clause" - {
    "filters groups based on aggregate conditions" in {
      val result = test(
        """
          |CREATE TABLE sales (
          | id SERIAL,
          | department TEXT,
          | amount INT,
          | PRIMARY KEY (id)
          |);
          |INSERT INTO sales (department, amount) VALUES 
          |  ('Sales', 1000), ('Sales', 2000), ('Marketing', 500), ('Engineering', 3000);
          |SELECT department, SUM(amount) as total FROM sales GROUP BY department HAVING total > 1500;
          |""".trim.stripMargin
      )
      
      // Should return Sales (3000) and Engineering (3000), not Marketing (500)
      result should include("TextValue(\"Sales\")")
      result should include("TextValue(\"Engineering\")")
      result should not include "TextValue(\"Marketing\")" // Marketing sum = 500 < 1500
    }

    "works with COUNT aggregate in HAVING" in {
      val result = test(
        """
          |CREATE TABLE orders (
          | id SERIAL,
          | customer TEXT,
          | amount INT,
          | PRIMARY KEY (id)
          |);
          |INSERT INTO orders (customer, amount) VALUES 
          |  ('Alice', 100), ('Alice', 200), ('Bob', 300), ('Charlie', 400), ('Charlie', 500);
          |SELECT customer, COUNT(*) as order_count FROM orders GROUP BY customer HAVING order_count > 1;
          |""".trim.stripMargin
      )
      
      // Should return Alice (2 orders) and Charlie (2 orders), not Bob (1 order)  
      result should include("TextValue(\"Alice\")")
      result should include("TextValue(\"Charlie\")")
      result should not include "TextValue(\"Bob\")" // Bob has only 1 order
    }
  }
}
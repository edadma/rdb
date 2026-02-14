package io.github.edadma.rdb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class FunctionTests extends AnyFreeSpec with Matchers with Testing {

  "Aggregate functions" - {
    "min, max, avg compute correct values" in {
      val result = test(
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

      // Check that the results contain the expected calculated values
      result should include("NumberValue(typ = IntType, value = 5)")    // MIN should be 5
      result should include("NumberValue(typ = IntType, value = 30)")   // MAX should be 30
      result should include("NumberValue(typ = DoubleType, value = 16.25)") // AVG: (10+20+30+5)/4
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

    "aggregates handle empty results" in {
      val result = test(
        """
          |CREATE TABLE empty_test (
          | id SERIAL,
          | value INT,
          | PRIMARY KEY (id)
          |);
          |SELECT MIN(value), MAX(value), AVG(value), COUNT(value) FROM empty_test;
          |""".trim.stripMargin
      )
      
      // For empty table, the query returns no rows (data = ArraySeq()), 
      // but metadata shows the expected column structure
      result should include("data = ArraySeq()") // No rows returned for empty table
      result should include("ColumnMetadata") // But metadata structure is preserved
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
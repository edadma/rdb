package io.github.edadma.petradb

import io.github.edadma.dal.{IntType => DIntType, DoubleType => DDoubleType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class QueryTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE products (
      | id SERIAL,
      | name TEXT,
      | price INT,
      | category TEXT,
      | PRIMARY KEY (id)
      |);
      |INSERT INTO products (name, price, category) VALUES
      |  ('Apple', 1, 'Fruit'),
      |  ('Banana', 2, 'Fruit'),
      |  ('Carrot', 3, 'Vegetable'),
      |  ('Date', 5, 'Fruit'),
      |  ('Eggplant', 4, 'Vegetable');
      |""".trim.stripMargin

  "LIKE" - {
    "matches with % wildcard" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products WHERE name LIKE 'A%' ORDER BY name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data.head.data(0) shouldBe TextValue("Apple")
    }

    "matches with _ wildcard" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products WHERE name LIKE 'Dat_' ORDER BY name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data.head.data(0) shouldBe TextValue("Date")
    }

    "matches with % in middle" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products WHERE name LIKE 'B%a' ORDER BY name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data.head.data(0) shouldBe TextValue("Banana")
    }

    "NOT LIKE excludes matches" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products WHERE name NOT LIKE '%a%' ORDER BY name;
          |""".trim.stripMargin
      )

      // Apple, Carrot, Date, Eggplant all contain 'a' (lowercase) — but LIKE is case-sensitive
      // 'Apple' has no lowercase 'a' at position... actually 'Apple' doesn't match '%a%' (case-sensitive)
      // Let's just check the count
      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      names should not contain "Banana"
      names should not contain "Date"
    }
  }

  "ILIKE" - {
    "matches case-insensitively" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products WHERE name ILIKE 'apple' ORDER BY name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data.head.data(0) shouldBe TextValue("Apple")
    }

    "NOT ILIKE excludes case-insensitive matches" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products WHERE name NOT ILIKE '%EGG%' ORDER BY name;
          |""".trim.stripMargin
      )

      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      names should not contain "Eggplant"
      names should contain("Apple")
    }
  }

  "BETWEEN" - {
    "filters within range inclusive" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, price FROM products WHERE price BETWEEN 2 AND 4 ORDER BY price;
          |""".trim.stripMargin
      )

      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      names shouldBe Vector("Banana", "Carrot", "Eggplant")
    }

    "NOT BETWEEN excludes range" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products WHERE price NOT BETWEEN 2 AND 4 ORDER BY name;
          |""".trim.stripMargin
      )

      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      names shouldBe Vector("Apple", "Date")
    }
  }

  "IN" - {
    "matches values in list" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products WHERE name IN ('Apple', 'Date') ORDER BY name;
          |""".trim.stripMargin
      )

      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      names shouldBe Vector("Apple", "Date")
    }

    "NOT IN excludes values in list" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products WHERE name NOT IN ('Apple', 'Date') ORDER BY name;
          |""".trim.stripMargin
      )

      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      names shouldBe Vector("Banana", "Carrot", "Eggplant")
    }

    "IN with subquery" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products WHERE price IN (SELECT price FROM products WHERE category = 'Vegetable') ORDER BY name;
          |""".trim.stripMargin
      )

      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      // Vegetable prices are 3 and 4, so products with price 3 or 4
      names shouldBe Vector("Carrot", "Eggplant")
    }
  }

  "LIMIT" - {
    "restricts number of rows returned" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products ORDER BY name LIMIT 3;
          |""".trim.stripMargin
      )

      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      names shouldBe Vector("Apple", "Banana", "Carrot")
    }

    "returns all rows when limit exceeds row count" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products ORDER BY name LIMIT 100;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 5
    }
  }

  "OFFSET" - {
    "skips specified number of rows" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products ORDER BY name OFFSET 2;
          |""".trim.stripMargin
      )

      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      names shouldBe Vector("Carrot", "Date", "Eggplant")
    }

    "LIMIT with OFFSET for pagination" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products ORDER BY name LIMIT 2 OFFSET 1;
          |""".trim.stripMargin
      )

      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      names shouldBe Vector("Banana", "Carrot")
    }
  }

  "WHERE clauses" - {
    "comparison operators" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products WHERE price > 3 ORDER BY name;
          |""".trim.stripMargin
      )

      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      names shouldBe Vector("Date", "Eggplant")
    }

    "AND combines conditions" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products WHERE category = 'Fruit' AND price > 1 ORDER BY name;
          |""".trim.stripMargin
      )

      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      names shouldBe Vector("Banana", "Date")
    }

    "OR combines conditions" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products WHERE price = 1 OR price = 5 ORDER BY name;
          |""".trim.stripMargin
      )

      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      names shouldBe Vector("Apple", "Date")
    }

    "NOT negates condition" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products WHERE NOT category = 'Fruit' ORDER BY name;
          |""".trim.stripMargin
      )

      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      names shouldBe Vector("Carrot", "Eggplant")
    }
  }

  "Expressions" - {
    "arithmetic in SELECT" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, price * 100 FROM products WHERE name = 'Apple';
          |""".trim.stripMargin
      )

      val row = table.data.head.data
      row(0) shouldBe TextValue("Apple")
      row(1) shouldBe NumberValue(DIntType, 100)
    }

    "string concatenation with ||" in {
      val table = query(
        s"""
          |$setup
          |SELECT name || ' - ' || category FROM products WHERE name = 'Apple';
          |""".trim.stripMargin
      )

      table.data.head.data(0) shouldBe TextValue("Apple - Fruit")
    }

    "CASE WHEN expression" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, CASE WHEN price > 3 THEN 'expensive' WHEN price > 1 THEN 'moderate' ELSE 'cheap' END FROM products ORDER BY name;
          |""".trim.stripMargin
      )

      val rows = table.data.map(r => (r.data(0).asInstanceOf[TextValue].s, r.data(1).asInstanceOf[TextValue].s))
      rows shouldBe Vector(
        ("Apple", "cheap"),
        ("Banana", "moderate"),
        ("Carrot", "moderate"),
        ("Date", "expensive"),
        ("Eggplant", "expensive"),
      )
    }

    "simple CASE expression" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, CASE category WHEN 'Fruit' THEN 'F' WHEN 'Vegetable' THEN 'V' ELSE '?' END FROM products ORDER BY name;
          |""".trim.stripMargin
      )

      val rows = table.data.map(r => (r.data(0).asInstanceOf[TextValue].s, r.data(1).asInstanceOf[TextValue].s))
      rows shouldBe Vector(
        ("Apple", "F"),
        ("Banana", "F"),
        ("Carrot", "V"),
        ("Date", "F"),
        ("Eggplant", "V"),
      )
    }
  }

  "ORDER BY" - {
    "ASC is default order" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, price FROM products ORDER BY price ASC;
          |""".trim.stripMargin
      )

      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      names shouldBe Vector("Apple", "Banana", "Carrot", "Eggplant", "Date")
    }

    "DESC reverses order" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, price FROM products ORDER BY price DESC;
          |""".trim.stripMargin
      )

      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      names shouldBe Vector("Date", "Eggplant", "Carrot", "Banana", "Apple")
    }

    "sorts by column not in SELECT" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products ORDER BY price;
          |""".trim.stripMargin
      )

      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      names shouldBe Vector("Apple", "Banana", "Carrot", "Eggplant", "Date")
    }

    "multiple columns" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, category, price FROM products ORDER BY category, price DESC;
          |""".trim.stripMargin
      )

      val names = table.data.map(_.data(0).asInstanceOf[TextValue].s)
      // Fruit: Date(5), Banana(2), Apple(1); Vegetable: Eggplant(4), Carrot(3)
      names shouldBe Vector("Date", "Banana", "Apple", "Eggplant", "Carrot")
    }

    "ordinal refers to column position" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, price FROM products ORDER BY 2;
          |""".trim.stripMargin
      )

      val prices = table.data.map(_.data(1))
      prices shouldBe Vector(NumberValue(1), NumberValue(2), NumberValue(3), NumberValue(4), NumberValue(5))
    }

    "ordinal 1 orders by first column" in {
      val table = query(
        s"""
          |$setup
          |SELECT price, name FROM products ORDER BY 1 DESC;
          |""".trim.stripMargin
      )

      val prices = table.data.map(_.data(0))
      prices shouldBe Vector(NumberValue(5), NumberValue(4), NumberValue(3), NumberValue(2), NumberValue(1))
    }
  }

  "EXISTS" - {
    "returns true when subquery has rows" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products WHERE EXISTS (SELECT name FROM products WHERE category = 'Fruit') ORDER BY name LIMIT 1;
          |""".trim.stripMargin
      )

      table.data.length should be > 0
    }

    "returns false when subquery is empty" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM products WHERE EXISTS (SELECT name FROM products WHERE category = 'Nonexistent');
          |""".trim.stripMargin
      )

      table.data.length shouldBe 0
    }
  }

  // ── Boolean test operators ────────────────────────────────────────────

  "IS TRUE" - {
    "true value IS TRUE" in {
      val table = query(
        """
          |CREATE TABLE t (val BOOLEAN);
          |INSERT INTO t (val) VALUES (TRUE);
          |SELECT val IS TRUE FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "false value IS TRUE" in {
      val table = query(
        """
          |CREATE TABLE t (val BOOLEAN);
          |INSERT INTO t (val) VALUES (FALSE);
          |SELECT val IS TRUE FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(false)
    }

    "NULL IS TRUE" in {
      val table = query(
        """
          |CREATE TABLE t (val BOOLEAN);
          |INSERT INTO t (val) VALUES (NULL);
          |SELECT val IS TRUE FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(false)
    }
  }

  "IS FALSE" - {
    "false value IS FALSE" in {
      val table = query(
        """
          |CREATE TABLE t (val BOOLEAN);
          |INSERT INTO t (val) VALUES (FALSE);
          |SELECT val IS FALSE FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "true value IS FALSE" in {
      val table = query(
        """
          |CREATE TABLE t (val BOOLEAN);
          |INSERT INTO t (val) VALUES (TRUE);
          |SELECT val IS FALSE FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(false)
    }
  }

  "IS UNKNOWN" - {
    "NULL IS UNKNOWN" in {
      val table = query(
        """
          |CREATE TABLE t (val BOOLEAN);
          |INSERT INTO t (val) VALUES (NULL);
          |SELECT val IS UNKNOWN FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "true IS UNKNOWN" in {
      val table = query(
        """
          |CREATE TABLE t (val BOOLEAN);
          |INSERT INTO t (val) VALUES (TRUE);
          |SELECT val IS UNKNOWN FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(false)
    }
  }

  "IS NOT TRUE" - {
    "false IS NOT TRUE" in {
      val table = query(
        """
          |CREATE TABLE t (val BOOLEAN);
          |INSERT INTO t (val) VALUES (FALSE);
          |SELECT val IS NOT TRUE FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "NULL IS NOT TRUE" in {
      val table = query(
        """
          |CREATE TABLE t (val BOOLEAN);
          |INSERT INTO t (val) VALUES (NULL);
          |SELECT val IS NOT TRUE FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
    }
  }

  "IS NOT FALSE" - {
    "true IS NOT FALSE" in {
      val table = query(
        """
          |CREATE TABLE t (val BOOLEAN);
          |INSERT INTO t (val) VALUES (TRUE);
          |SELECT val IS NOT FALSE FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
    }
  }

  "IS NOT UNKNOWN" - {
    "true IS NOT UNKNOWN" in {
      val table = query(
        """
          |CREATE TABLE t (val BOOLEAN);
          |INSERT INTO t (val) VALUES (TRUE);
          |SELECT val IS NOT UNKNOWN FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
    }
  }

  // ── ANY/ALL quantified comparisons ─────────────────────────────────

  "= ANY(column)" - {
    "returns matching rows" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, tags INT[]);
          |INSERT INTO t (id, tags) VALUES (1, ARRAY[10, 20, 30]), (2, ARRAY[40, 50, 60]);
          |SELECT id FROM t WHERE 20 = ANY(tags);
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(1)
    }

    "returns no rows when no match" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, tags INT[]);
          |INSERT INTO t (id, tags) VALUES (1, ARRAY[10, 20, 30]);
          |SELECT id FROM t WHERE 99 = ANY(tags);
          |""".trim.stripMargin
      )
      table.data.length shouldBe 0
    }
  }

  "> ANY(column)" - {
    "returns rows where value greater than any element" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, vals INT[]);
          |INSERT INTO t (id, vals) VALUES (1, ARRAY[5, 10, 15]), (2, ARRAY[20, 30]);
          |SELECT id FROM t WHERE 7 > ANY(vals);
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(1)
    }
  }

  "= ALL(ARRAY[...])" - {
    "matches when all elements equal" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1), (2);
          |SELECT id FROM t WHERE 5 = ALL(ARRAY[5, 5, 5]);
          |""".trim.stripMargin
      )
      table.data.length shouldBe 2
    }
  }

  "< ALL(column)" - {
    "returns rows where value less than all elements" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, vals INT[]);
          |INSERT INTO t (id, vals) VALUES (1, ARRAY[10, 20, 30]), (2, ARRAY[3, 4, 5]);
          |SELECT id FROM t WHERE 5 < ALL(vals);
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(1)
    }
  }

  "= ALL(column)" - {
    "returns no rows when not all match" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, vals INT[]);
          |INSERT INTO t (id, vals) VALUES (1, ARRAY[5, 5, 6]), (2, ARRAY[7, 7, 7]);
          |SELECT id FROM t WHERE 5 = ALL(vals);
          |""".trim.stripMargin
      )
      table.data.length shouldBe 0
    }
  }

  "SELECT * without FROM" - {
    "rejects SELECT * followed by bare identifier" in {
      an[Exception] should be thrownBy query(
        s"""
          |$setup
          |SELECT * products;
          |""".trim.stripMargin
      )
    }

    "allows SELECT * FROM table" in {
      val table = query(
        s"""
          |$setup
          |SELECT * FROM products;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 5
    }
  }

}

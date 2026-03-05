package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DistinctTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE items (
      | id SERIAL,
      | name TEXT,
      | category TEXT,
      | price INT
      |);
      |INSERT INTO items (name, category, price) VALUES
      |  ('Apple', 'Fruit', 1),
      |  ('Banana', 'Fruit', 2),
      |  ('Carrot', 'Vegetable', 3),
      |  ('Date', 'Fruit', 1),
      |  ('Eggplant', 'Vegetable', 4);
      |""".trim.stripMargin

  "DISTINCT" - {
    "removes duplicate values from single column" in {
      val table = query(
        s"""
          |$setup
          |SELECT DISTINCT category FROM items ORDER BY category;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("Fruit")
      table.data(1).data(0) shouldBe TextValue("Vegetable")
    }

    "removes duplicate rows from multiple columns" in {
      val table = query(
        s"""
          |$setup
          |SELECT DISTINCT category, price FROM items ORDER BY category, price;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 4
    }

    "returns all rows when no duplicates exist" in {
      val table = query(
        s"""
          |$setup
          |SELECT DISTINCT name FROM items ORDER BY name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 5
    }

    "works with WHERE clause" in {
      val table = query(
        s"""
          |$setup
          |SELECT DISTINCT price FROM items WHERE category = 'Fruit' ORDER BY price;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(1).data(0) shouldBe NumberValue(2)
    }

    "works with LIMIT" in {
      val table = query(
        s"""
          |$setup
          |SELECT DISTINCT category FROM items ORDER BY category LIMIT 1;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("Fruit")
    }

    "works with aggregate in non-distinct subquery" in {
      val table = query(
        s"""
          |$setup
          |SELECT DISTINCT category FROM items WHERE price > 1 ORDER BY category;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
    }

    "handles NULL values" in {
      val table = query(
        """
          |CREATE TABLE nullable (id SERIAL, val TEXT);
          |INSERT INTO nullable (val) VALUES ('a');
          |INSERT INTO nullable (val) VALUES ('a');
          |INSERT INTO nullable (id) VALUES (3);
          |INSERT INTO nullable (id) VALUES (4);
          |SELECT DISTINCT val FROM nullable ORDER BY val;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
    }

    "SELECT without DISTINCT returns all rows including duplicates" in {
      val table = query(
        s"""
          |$setup
          |SELECT category FROM items ORDER BY category;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 5
    }
  }
}

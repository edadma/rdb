package io.github.edadma.petradb

import io.github.edadma.dal.{IntType => DIntType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class UpdateFromTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE targets (id INT, name TEXT, value INT);
      |INSERT INTO targets (id, name, value) VALUES (1, 'a', 10), (2, 'b', 20), (3, 'c', 30);
      |""".trim.stripMargin

  "UPDATE ... FROM with another table" - {
    "basic update from source table" in {
      val table = query(
        s"""
          |$setup
          |CREATE TABLE source (id INT, new_value INT);
          |INSERT INTO source (id, new_value) VALUES (1, 100), (3, 300);
          |UPDATE targets SET value = source.new_value FROM source WHERE targets.id = source.id;
          |SELECT id, name, value FROM targets ORDER BY id;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      table.data(0).data(2) shouldBe NumberValue(DIntType, 100)
      table.data(1).data(2) shouldBe NumberValue(DIntType, 20) // unchanged
      table.data(2).data(2) shouldBe NumberValue(DIntType, 300)
    }

    "returns correct update count" in {
      val res = results(
        s"""
          |$setup
          |CREATE TABLE source (id INT, new_value INT);
          |INSERT INTO source (id, new_value) VALUES (1, 100), (3, 300);
          |UPDATE targets SET value = source.new_value FROM source WHERE targets.id = source.id;
          |""".trim.stripMargin
      )

      res.last shouldBe UpdateResult(2)
    }
  }

  "UPDATE ... FROM with VALUES" - {
    "update from VALUES with column aliases" in {
      val table = query(
        s"""
          |$setup
          |UPDATE targets SET value = d.new_value
          |  FROM (VALUES (1, 100), (2, 200)) AS d (id, new_value)
          |  WHERE targets.id = d.id;
          |SELECT id, name, value FROM targets ORDER BY id;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      table.data(0).data(2) shouldBe NumberValue(DIntType, 100)
      table.data(1).data(2) shouldBe NumberValue(DIntType, 200)
      table.data(2).data(2) shouldBe NumberValue(DIntType, 30) // unchanged
    }
  }

  "UPDATE ... FROM multiple SET columns" - {
    "update name and value from FROM source" in {
      val table = query(
        s"""
          |$setup
          |UPDATE targets SET name = d.new_name, value = d.new_value
          |  FROM (VALUES (2, 'updated', 999)) AS d (id, new_name, new_value)
          |  WHERE targets.id = d.id;
          |SELECT id, name, value FROM targets ORDER BY id;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      table.data(1).data(1) shouldBe TextValue("updated")
      table.data(1).data(2) shouldBe NumberValue(DIntType, 999)
    }
  }

  "UPDATE ... FROM non-matching rows untouched" - {
    "rows without match in FROM are not modified" in {
      val table = query(
        s"""
          |$setup
          |UPDATE targets SET value = d.new_value
          |  FROM (VALUES (99, 999)) AS d (id, new_value)
          |  WHERE targets.id = d.id;
          |SELECT id, value FROM targets ORDER BY id;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      table.data(0).data(1) shouldBe NumberValue(DIntType, 10)
      table.data(1).data(1) shouldBe NumberValue(DIntType, 20)
      table.data(2).data(1) shouldBe NumberValue(DIntType, 30)
    }

    "returns zero update count when no match" in {
      val res = results(
        s"""
          |$setup
          |UPDATE targets SET value = d.new_value
          |  FROM (VALUES (99, 999)) AS d (id, new_value)
          |  WHERE targets.id = d.id;
          |""".trim.stripMargin
      )

      res.last shouldBe UpdateResult(0)
    }
  }

  "UPDATE ... FROM with FK constraints" - {
    "FK enforcement on UPDATE ... FROM" in {
      val res = results(
        """
          |CREATE TABLE parents (id INT, PRIMARY KEY (id));
          |INSERT INTO parents (id) VALUES (1), (2), (3);
          |CREATE TABLE children (id INT, parent_id INT REFERENCES parents (id));
          |INSERT INTO children (id, parent_id) VALUES (10, 1), (20, 2);
          |UPDATE children SET parent_id = d.new_parent
          |  FROM (VALUES (10, 3)) AS d (id, new_parent)
          |  WHERE children.id = d.id;
          |SELECT id, parent_id FROM children ORDER BY id;
          |""".trim.stripMargin
      )

      val table = res.collect { case QueryResult(t) => t }.last
      table.data(0).data(1) shouldBe NumberValue(DIntType, 3)
    }

    "FK violation on UPDATE ... FROM throws error" in {
      an[Exception] should be thrownBy results(
        """
          |CREATE TABLE parents (id INT, PRIMARY KEY (id));
          |INSERT INTO parents (id) VALUES (1), (2);
          |CREATE TABLE children (id INT, parent_id INT REFERENCES parents (id));
          |INSERT INTO children (id, parent_id) VALUES (10, 1);
          |UPDATE children SET parent_id = d.new_parent
          |  FROM (VALUES (10, 999)) AS d (id, new_parent)
          |  WHERE children.id = d.id;
          |""".trim.stripMargin
      )
    }
  }
}

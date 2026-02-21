package io.github.edadma.rdb

import io.github.edadma.dal.{IntType => DIntType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class InsertSelectTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE source (id INT, name TEXT, value INT);
      |INSERT INTO source (id, name, value) VALUES (1, 'a', 10), (2, 'b', 20), (3, 'c', 30);
      |CREATE TABLE target (id INT, name TEXT, value INT);
      |""".trim.stripMargin

  "INSERT INTO ... SELECT" - {
    "insert all rows from another table" in {
      val table = query(
        s"""
          |$setup
          |INSERT INTO target SELECT * FROM source;
          |SELECT id, name, value FROM target ORDER BY id;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
      table.data(0).data(1) shouldBe TextValue("a")
      table.data(0).data(2) shouldBe NumberValue(DIntType, 10)
      table.data(2).data(0) shouldBe NumberValue(DIntType, 3)
    }

    "insert with WHERE filter" in {
      val table = query(
        s"""
          |$setup
          |INSERT INTO target SELECT * FROM source WHERE value > 15;
          |SELECT id, name FROM target ORDER BY id;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(1) shouldBe TextValue("b")
      table.data(1).data(1) shouldBe TextValue("c")
    }

    "insert with column list" in {
      val table = query(
        s"""
          |$setup
          |INSERT INTO target (id, name) SELECT id, name FROM source WHERE id = 1;
          |SELECT id, name, value FROM target;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
      table.data(0).data(1) shouldBe TextValue("a")
    }

    "insert with expressions in SELECT" in {
      val table = query(
        s"""
          |$setup
          |INSERT INTO target SELECT id, name, value * 2 FROM source WHERE id <= 2;
          |SELECT id, value FROM target ORDER BY id;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(1) shouldBe NumberValue(DIntType, 20)
      table.data(1).data(1) shouldBe NumberValue(DIntType, 40)
    }

    "insert from VALUES subquery" in {
      val table = query(
        s"""
          |$setup
          |INSERT INTO target SELECT * FROM (VALUES (10, 'x', 100), (20, 'y', 200)) AS t (id, name, value);
          |SELECT id, name, value FROM target ORDER BY id;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(10)
      table.data(1).data(0) shouldBe NumberValue(20)
    }

    "column count mismatch throws error" in {
      an[Exception] should be thrownBy query(
        s"""
          |$setup
          |INSERT INTO target SELECT id, name FROM source;
          |""".trim.stripMargin
      )
    }

    "insert zero rows from empty result" in {
      val table = query(
        s"""
          |$setup
          |INSERT INTO target SELECT * FROM source WHERE value > 999;
          |SELECT * FROM target;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 0
    }

    "FK constraints enforced" in {
      an[Exception] should be thrownBy results(
        """
          |CREATE TABLE parents (id INT, PRIMARY KEY (id));
          |INSERT INTO parents (id) VALUES (1), (2);
          |CREATE TABLE children (id INT, parent_id INT REFERENCES parents (id));
          |CREATE TABLE bad_data (id INT, parent_id INT);
          |INSERT INTO bad_data (id, parent_id) VALUES (10, 999);
          |INSERT INTO children SELECT * FROM bad_data;
          |""".trim.stripMargin
      )
    }
  }
}

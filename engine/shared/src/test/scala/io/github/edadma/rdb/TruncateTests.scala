package io.github.edadma.rdb

import io.github.edadma.dal.{IntType => DIntType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TruncateTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE t (id SERIAL, name TEXT, PRIMARY KEY (id));
      |INSERT INTO t (name) VALUES ('a'), ('b'), ('c');
      |""".trim.stripMargin

  "TRUNCATE TABLE" - {
    "empties table" in {
      val table = query(
        s"""
          |$setup
          |TRUNCATE TABLE t;
          |SELECT * FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 0
    }

    "TRUNCATE without TABLE keyword" in {
      val table = query(
        s"""
          |$setup
          |TRUNCATE t;
          |SELECT * FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 0
    }

    "returns TruncateResult" in {
      val res = results(
        s"""
          |$setup
          |TRUNCATE TABLE t;
          |""".trim.stripMargin
      )

      res.last shouldBe TruncateResult("t")
    }

    "resets serial sequences" in {
      val table = query(
        s"""
          |$setup
          |TRUNCATE TABLE t;
          |INSERT INTO t (name) VALUES ('new');
          |SELECT id, name FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
      table.data(0).data(1) shouldBe TextValue("new")
    }

    "table is usable after truncate" in {
      val table = query(
        s"""
          |$setup
          |TRUNCATE TABLE t;
          |INSERT INTO t (name) VALUES ('x'), ('y');
          |SELECT name FROM t ORDER BY name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("x")
      table.data(1).data(0) shouldBe TextValue("y")
    }

    "truncate on empty table is no-op" in {
      val table = query(
        """
          |CREATE TABLE empty_t (id INT);
          |TRUNCATE TABLE empty_t;
          |SELECT * FROM empty_t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 0
    }

    "unknown table fails" in {
      an[Exception] should be thrownBy results("TRUNCATE TABLE nonexistent;")
    }

    "FK constraint prevents truncate when referenced" in {
      an[Exception] should be thrownBy results(
        """
          |CREATE TABLE parents (id INT, PRIMARY KEY (id));
          |INSERT INTO parents (id) VALUES (1), (2);
          |CREATE TABLE children (id INT, parent_id INT REFERENCES parents (id));
          |INSERT INTO children (id, parent_id) VALUES (10, 1);
          |TRUNCATE TABLE parents;
          |""".trim.stripMargin
      )
    }

    "truncate succeeds when no child rows reference it" in {
      val res = results(
        """
          |CREATE TABLE parents (id INT, PRIMARY KEY (id));
          |INSERT INTO parents (id) VALUES (1), (2);
          |CREATE TABLE children (id INT, parent_id INT REFERENCES parents (id));
          |TRUNCATE TABLE parents;
          |""".trim.stripMargin
      )

      res.last shouldBe TruncateResult("parents")
    }
  }
}

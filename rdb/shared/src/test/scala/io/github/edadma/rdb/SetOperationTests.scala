package io.github.edadma.rdb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SetOperationTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE t1 (id INT, name TEXT);
      |INSERT INTO t1 (id, name) VALUES (1, 'Alice'), (2, 'Bob'), (3, 'Charlie');
      |CREATE TABLE t2 (id INT, name TEXT);
      |INSERT INTO t2 (id, name) VALUES (2, 'Bob'), (3, 'Charlie'), (4, 'Dave');
      |""".trim.stripMargin

  "UNION" - {
    "removes duplicates" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM t1 UNION SELECT name FROM t2;
          |""".trim.stripMargin
      )

      table.data.map(_.data(0)) should contain theSameElementsAs Seq(
        TextValue("Alice"),
        TextValue("Bob"),
        TextValue("Charlie"),
        TextValue("Dave"),
      )
    }

    "combines results from two tables" in {
      val table = query(
        s"""
          |$setup
          |SELECT id FROM t1 UNION SELECT id FROM t2;
          |""".trim.stripMargin
      )

      table.data.map(_.data(0)) should contain theSameElementsAs Seq(
        NumberValue(1),
        NumberValue(2),
        NumberValue(3),
        NumberValue(4),
      )
    }
  }

  "UNION ALL" - {
    "keeps duplicates" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM t1 UNION ALL SELECT name FROM t2;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
      table.data.map(_.data(0)) should contain theSameElementsAs Seq(
        TextValue("Alice"),
        TextValue("Bob"),
        TextValue("Charlie"),
        TextValue("Bob"),
        TextValue("Charlie"),
        TextValue("Dave"),
      )
    }
  }

  "INTERSECT" - {
    "returns only common rows" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM t1 INTERSECT SELECT name FROM t2;
          |""".trim.stripMargin
      )

      table.data.map(_.data(0)) should contain theSameElementsAs Seq(
        TextValue("Bob"),
        TextValue("Charlie"),
      )
    }
  }

  "EXCEPT" - {
    "returns rows in first but not second" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM t1 EXCEPT SELECT name FROM t2;
          |""".trim.stripMargin
      )

      table.data.map(_.data(0)) should contain theSameElementsAs Seq(
        TextValue("Alice"),
      )
    }

    "returns rows in second but not first" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM t2 EXCEPT SELECT name FROM t1;
          |""".trim.stripMargin
      )

      table.data.map(_.data(0)) should contain theSameElementsAs Seq(
        TextValue("Dave"),
      )
    }
  }

  "compound ORDER BY / LIMIT / OFFSET" - {
    "ORDER BY applies to union result" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM t1 UNION SELECT name FROM t2 ORDER BY name;
          |""".trim.stripMargin
      )

      table.data.map(_.data(0)) shouldBe Vector(
        TextValue("Alice"),
        TextValue("Bob"),
        TextValue("Charlie"),
        TextValue("Dave"),
      )
    }

    "LIMIT applies to union result" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM t1 UNION SELECT name FROM t2 ORDER BY name LIMIT 2;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data.map(_.data(0)) shouldBe Vector(
        TextValue("Alice"),
        TextValue("Bob"),
      )
    }

    "OFFSET applies to union result" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM t1 UNION SELECT name FROM t2 ORDER BY name OFFSET 2;
          |""".trim.stripMargin
      )

      table.data.map(_.data(0)) shouldBe Vector(
        TextValue("Charlie"),
        TextValue("Dave"),
      )
    }

    "LIMIT and OFFSET together" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM t1 UNION SELECT name FROM t2 ORDER BY name LIMIT 1 OFFSET 1;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("Bob")
    }
  }

  "chained operations" - {
    "three-way UNION" in {
      val table = query(
        """
          |CREATE TABLE a (v INT);
          |INSERT INTO a (v) VALUES (1), (2);
          |CREATE TABLE b (v INT);
          |INSERT INTO b (v) VALUES (3), (4);
          |CREATE TABLE c (v INT);
          |INSERT INTO c (v) VALUES (5), (6);
          |SELECT v FROM a UNION SELECT v FROM b UNION SELECT v FROM c;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 6
    }
  }

  "empty table" - {
    "UNION with empty right side" in {
      val table = query(
        """
          |CREATE TABLE x (v INT);
          |INSERT INTO x (v) VALUES (1), (2);
          |CREATE TABLE y (v INT);
          |SELECT v FROM x UNION SELECT v FROM y;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
    }

    "INTERSECT with empty right side" in {
      val table = query(
        """
          |CREATE TABLE x (v INT);
          |INSERT INTO x (v) VALUES (1), (2);
          |CREATE TABLE y (v INT);
          |SELECT v FROM x INTERSECT SELECT v FROM y;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 0
    }

    "EXCEPT with empty right side returns all from left" in {
      val table = query(
        """
          |CREATE TABLE x (v INT);
          |INSERT INTO x (v) VALUES (1), (2);
          |CREATE TABLE y (v INT);
          |SELECT v FROM x EXCEPT SELECT v FROM y;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
    }
  }

  "INTERSECT precedence over UNION" - {
    "INTERSECT binds tighter than UNION" in {
      // A UNION B INTERSECT C  should be  A UNION (B INTERSECT C)
      val table = query(
        """
          |CREATE TABLE a (v INT);
          |INSERT INTO a (v) VALUES (1), (2);
          |CREATE TABLE b (v INT);
          |INSERT INTO b (v) VALUES (2), (3);
          |CREATE TABLE c (v INT);
          |INSERT INTO c (v) VALUES (3), (4);
          |SELECT v FROM a UNION SELECT v FROM b INTERSECT SELECT v FROM c;
          |""".trim.stripMargin
      )

      // B INTERSECT C = {3}, then A UNION {3} = {1, 2, 3}
      table.data.map(_.data(0)) should contain theSameElementsAs Seq(
        NumberValue(1),
        NumberValue(2),
        NumberValue(3),
      )
    }
  }
}

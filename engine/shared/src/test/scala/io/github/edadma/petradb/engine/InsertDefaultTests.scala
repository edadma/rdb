package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import io.github.edadma.dal.{IntType => DIntType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class InsertDefaultTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE items (
      |  id SERIAL PRIMARY KEY,
      |  name TEXT NOT NULL,
      |  quantity INT DEFAULT 0,
      |  active BOOLEAN DEFAULT true
      |)
      |""".trim.stripMargin

  "INSERT with DEFAULT keyword" - {
    "DEFAULT for serial column" in {
      val table = query(
        s"""
          |$setup;
          |INSERT INTO items (id, name, quantity) VALUES (DEFAULT, 'Widget', 5);
          |SELECT id, name, quantity FROM items;
          |""".trim.stripMargin
      )

      table.data should have length 1
      val row = table.data.head.data
      row(0) shouldBe NumberValue(DIntType, 1)
      row(1) shouldBe TextValue("Widget")
      row(2) shouldBe NumberValue(DIntType, 5)
    }

    "DEFAULT for column with default value" in {
      val table = query(
        s"""
          |$setup;
          |INSERT INTO items (id, name, quantity, active) VALUES (DEFAULT, 'Gadget', DEFAULT, DEFAULT);
          |SELECT name, quantity, active FROM items;
          |""".trim.stripMargin
      )

      table.data should have length 1
      val row = table.data.head.data
      row(0) shouldBe TextValue("Gadget")
      row(1) shouldBe NumberValue(DIntType, 0)
      row(2) shouldBe BooleanValue(true)
    }

    "DEFAULT in multi-row insert" in {
      val table = query(
        s"""
          |$setup;
          |INSERT INTO items (id, name, quantity) VALUES
          |  (DEFAULT, 'Alpha', 10),
          |  (DEFAULT, 'Beta', 20);
          |SELECT id, name, quantity FROM items ORDER BY id;
          |""".trim.stripMargin
      )

      table.data should have length 2
      val row0 = table.data(0).data
      row0(0) shouldBe NumberValue(DIntType, 1)
      row0(1) shouldBe TextValue("Alpha")
      val row1 = table.data(1).data
      row1(0) shouldBe NumberValue(DIntType, 2)
      row1(1) shouldBe TextValue("Beta")
    }

    "DEFAULT with RETURNING" in {
      val res = results(
        s"""
          |$setup;
          |INSERT INTO items (id, name, quantity, active) VALUES (DEFAULT, 'Doohickey', DEFAULT, DEFAULT) RETURNING *;
          |""".trim.stripMargin
      )

      val insert = res.collect { case r: InsertResult => r }.last
      insert.table.data should have length 1
      val row = insert.table.data.head.data
      row(0) shouldBe NumberValue(DIntType, 1)
      row(1) shouldBe TextValue("Doohickey")
      row(2) shouldBe NumberValue(DIntType, 0)
      row(3) shouldBe BooleanValue(true)
    }

    "case-insensitive DEFAULT" in {
      val table = query(
        s"""
          |$setup;
          |INSERT INTO items (id, name) VALUES (default, 'lower');
          |INSERT INTO items (id, name) VALUES (DEFAULT, 'upper');
          |SELECT id, name FROM items ORDER BY id;
          |""".trim.stripMargin
      )

      table.data should have length 2
      table.data(0).data(1) shouldBe TextValue("lower")
      table.data(1).data(1) shouldBe TextValue("upper")
    }
  }
}

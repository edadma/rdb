package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ParameterCoercionTests extends AnyFreeSpec with Matchers with Testing {

  "comparing INT column to text parameter" - {
    "equality with string number" in {
      val table = query(
        """
          |CREATE TABLE t (id SERIAL, name TEXT);
          |INSERT INTO t (name) VALUES ('Alice'), ('Bob'), ('Carol');
          |PREPARE q AS SELECT name FROM t WHERE id = $1;
          |EXECUTE q ('2');
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("Bob")
    }

    "equality with string number in subquery context" in {
      val table = query(
        """
          |CREATE TABLE t (id SERIAL, val TEXT);
          |INSERT INTO t (val) VALUES ('x'), ('y');
          |PREPARE q AS SELECT val FROM t WHERE id = $1;
          |EXECUTE q ('1');
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("x")
    }
  }

  "comparing TEXT column to numeric parameter" - {
    "equality" in {
      val table = query(
        """
          |CREATE TABLE t (code TEXT, label TEXT);
          |INSERT INTO t (code, label) VALUES ('100', 'alpha'), ('200', 'beta');
          |PREPARE q AS SELECT label FROM t WHERE code = $1;
          |EXECUTE q (100);
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("alpha")
    }
  }

  "comparing BIGINT column to text parameter" - {
    "equality" in {
      val table = query(
        """
          |CREATE TABLE t (id BIGINT, name TEXT);
          |INSERT INTO t (id, name) VALUES (1000000, 'big');
          |PREPARE q AS SELECT name FROM t WHERE id = $1;
          |EXECUTE q ('1000000');
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("big")
    }
  }

  "comparison operators with coercion" - {
    "less than" in {
      val table = query(
        """
          |CREATE TABLE t (id SERIAL, name TEXT);
          |INSERT INTO t (name) VALUES ('a'), ('b'), ('c');
          |PREPARE q AS SELECT name FROM t WHERE id < $1 ORDER BY id;
          |EXECUTE q ('3');
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("a")
      table.data(1).data(0) shouldBe TextValue("b")
    }

    "greater than or equal" in {
      val table = query(
        """
          |CREATE TABLE t (id SERIAL, name TEXT);
          |INSERT INTO t (name) VALUES ('a'), ('b'), ('c');
          |PREPARE q AS SELECT name FROM t WHERE id >= $1 ORDER BY id;
          |EXECUTE q ('2');
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("b")
      table.data(1).data(0) shouldBe TextValue("c")
    }
  }
}

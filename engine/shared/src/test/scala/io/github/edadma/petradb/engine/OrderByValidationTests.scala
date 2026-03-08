package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import io.github.edadma.dal.{IntType => DIntType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class OrderByValidationTests extends AnyFreeSpec with Matchers with Testing {

  // ── Nonexistent column detection (row-count independent) ───────────

  "nonexistent column detection" - {
    "fails with 0 rows" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |SELECT a FROM t ORDER BY b;
            |""".trim.stripMargin
        )
      }
    }

    "fails with 1 row" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |INSERT INTO t (a) VALUES (1);
            |SELECT a FROM t ORDER BY b;
            |""".trim.stripMargin
        )
      }
    }

    "fails with multiple rows" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |INSERT INTO t (a) VALUES (1), (2), (3);
            |SELECT a FROM t ORDER BY b;
            |""".trim.stripMargin
        )
      }
    }

    "fails for nonexistent column in expression" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |SELECT a FROM t ORDER BY b + 1;
            |""".trim.stripMargin
        )
      }
    }

    "fails for nonexistent column in complex expression" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |SELECT a FROM t ORDER BY a + b;
            |""".trim.stripMargin
        )
      }
    }

    "fails for nonexistent qualified column" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |SELECT a FROM t ORDER BY t.b;
            |""".trim.stripMargin
        )
      }
    }
  }

  // ── Valid ORDER BY usage ───────────────────────────────────────────

  "valid ORDER BY" - {
    "by existing column" in {
      val table = query(
        """
          |CREATE TABLE t (a INT);
          |INSERT INTO t (a) VALUES (3), (1), (2);
          |SELECT a FROM t ORDER BY a;
          |""".trim.stripMargin
      )
      table.data.map(_.data(0)) shouldBe Vector(
        NumberValue(DIntType, 1),
        NumberValue(DIntType, 2),
        NumberValue(DIntType, 3),
      )
    }

    "by ordinal" in {
      val table = query(
        """
          |CREATE TABLE t (a INT);
          |INSERT INTO t (a) VALUES (3), (1), (2);
          |SELECT a FROM t ORDER BY 1;
          |""".trim.stripMargin
      )
      table.data.map(_.data(0)) shouldBe Vector(
        NumberValue(DIntType, 1),
        NumberValue(DIntType, 2),
        NumberValue(DIntType, 3),
      )
    }

    "by expression on valid column" in {
      val table = query(
        """
          |CREATE TABLE t (a INT);
          |INSERT INTO t (a) VALUES (3), (1), (2);
          |SELECT a FROM t ORDER BY a * -1;
          |""".trim.stripMargin
      )
      table.data.map(_.data(0)) shouldBe Vector(
        NumberValue(DIntType, 3),
        NumberValue(DIntType, 2),
        NumberValue(DIntType, 1),
      )
    }

    "by valid qualified column" in {
      val table = query(
        """
          |CREATE TABLE t (a INT);
          |INSERT INTO t (a) VALUES (3), (1), (2);
          |SELECT a FROM t ORDER BY t.a;
          |""".trim.stripMargin
      )
      table.data.map(_.data(0)) shouldBe Vector(
        NumberValue(DIntType, 1),
        NumberValue(DIntType, 2),
        NumberValue(DIntType, 3),
      )
    }

    "DESC ordering" in {
      val table = query(
        """
          |CREATE TABLE t (a INT);
          |INSERT INTO t (a) VALUES (1), (3), (2);
          |SELECT a FROM t ORDER BY a DESC;
          |""".trim.stripMargin
      )
      table.data.map(_.data(0)) shouldBe Vector(
        NumberValue(DIntType, 3),
        NumberValue(DIntType, 2),
        NumberValue(DIntType, 1),
      )
    }

    "multiple columns" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b INT);
          |INSERT INTO t (a, b) VALUES (1, 2), (1, 1), (2, 1);
          |SELECT a, b FROM t ORDER BY a, b;
          |""".trim.stripMargin
      )
      table.data(0).data shouldBe Vector(NumberValue(DIntType, 1), NumberValue(DIntType, 1))
      table.data(1).data shouldBe Vector(NumberValue(DIntType, 1), NumberValue(DIntType, 2))
      table.data(2).data shouldBe Vector(NumberValue(DIntType, 2), NumberValue(DIntType, 1))
    }

    "on empty table succeeds" in {
      val table = query(
        """
          |CREATE TABLE t (a INT);
          |SELECT a FROM t ORDER BY a;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 0
    }
  }
}

package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import io.github.edadma.dal.{IntType => DIntType, DoubleType => DDoubleType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class NullEdgeCaseTests extends AnyFreeSpec with Matchers with Testing {

  // ── NULL comparisons ──────────────────────────────────────────────

  "NULL comparisons" - {
    "NULL = NULL is not true (returns no rows)" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (NULL);
          |SELECT val FROM t WHERE val = NULL;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 0
    }

    "NULL != NULL is not true (returns no rows)" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (NULL);
          |SELECT val FROM t WHERE val != NULL;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 0
    }

    "NULL > 0 is not true" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (NULL);
          |SELECT val FROM t WHERE val > 0;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 0
    }

    "IS NULL finds NULL rows" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (NULL), (1), (NULL);
          |SELECT COUNT(*) FROM t WHERE val IS NULL;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 2)
    }

    "IS NOT NULL excludes NULL rows" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (NULL), (1), (NULL);
          |SELECT COUNT(*) FROM t WHERE val IS NOT NULL;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
    }
  }

  // ── NULL in aggregates ────────────────────────────────────────────

  "NULL in aggregates" - {
    "COUNT(*) includes NULL rows" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (1), (NULL), (3);
          |SELECT COUNT(*) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 3)
    }

    "COUNT(column) excludes NULLs" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (1), (NULL), (3);
          |SELECT COUNT(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 2)
    }

    "SUM ignores NULLs" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (10), (NULL), (20);
          |SELECT SUM(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 30)
    }

    "AVG ignores NULLs" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (10), (NULL), (30);
          |SELECT AVG(val) FROM t;
          |""".trim.stripMargin
      )
      // AVG of 10,30 = 20 (not 10+30/3=13.3)
      val avg = table.data(0).data(0).asInstanceOf[NumberValue].value.doubleValue
      avg shouldBe 20.0
    }

    "MIN ignores NULLs" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (10), (NULL), (5);
          |SELECT MIN(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 5)
    }

    "MAX ignores NULLs" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (10), (NULL), (20);
          |SELECT MAX(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 20)
    }

    "SUM of all NULLs returns NULL" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (NULL), (NULL);
          |SELECT SUM(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }

    "AVG of all NULLs returns NULL" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (NULL), (NULL);
          |SELECT AVG(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }

    "MIN of all NULLs returns NULL" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (NULL), (NULL);
          |SELECT MIN(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }

    "MAX of all NULLs returns NULL" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (NULL), (NULL);
          |SELECT MAX(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }

    "COUNT(*) on empty table returns 0" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |SELECT COUNT(*) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 0)
    }

    "SUM on empty table returns NULL" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |SELECT SUM(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }
  }

  // ── NULL in expressions ───────────────────────────────────────────

  "NULL in expressions" - {
    "NULL + number is NULL" in {
      val table = query("SELECT NULL + 1;")
      table.data(0).data(0).isNull shouldBe true
    }

    "NULL || text is NULL" in {
      val table = query("SELECT NULL || 'hello';")
      table.data(0).data(0).isNull shouldBe true
    }

    "COALESCE returns first non-null" in {
      val table = query("SELECT COALESCE(NULL, NULL, 3, 4);")
      table.data(0).data(0) shouldBe NumberValue(DIntType, 3)
    }

    "COALESCE of all NULLs returns NULL" in {
      val table = query("SELECT COALESCE(NULL, NULL);")
      table.data(0).data(0).isNull shouldBe true
    }

    "NULLIF returns NULL when values are equal" in {
      val table = query("SELECT NULLIF(1, 1);")
      table.data(0).data(0).isNull shouldBe true
    }

    "NULLIF returns first value when values differ" in {
      val table = query("SELECT NULLIF(1, 2);")
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
    }
  }

  // ── NULL in DISTINCT ──────────────────────────────────────────────

  "NULL in DISTINCT" - {
    "DISTINCT treats multiple NULLs as one" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (NULL), (1), (NULL), (1);
          |SELECT DISTINCT val FROM t ORDER BY val NULLS LAST;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 2
    }
  }

  // ── NULL in ORDER BY ──────────────────────────────────────────────

  "NULL in ORDER BY" - {
    "NULLS FIRST puts NULLs at start" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (3), (NULL), (1);
          |SELECT val FROM t ORDER BY val NULLS FIRST;
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
      table.data(1).data(0) shouldBe NumberValue(DIntType, 1)
      table.data(2).data(0) shouldBe NumberValue(DIntType, 3)
    }

    "NULLS LAST puts NULLs at end" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (3), (NULL), (1);
          |SELECT val FROM t ORDER BY val NULLS LAST;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
      table.data(1).data(0) shouldBe NumberValue(DIntType, 3)
      table.data(2).data(0).isNull shouldBe true
    }
  }

  // ── NULL in CASE ──────────────────────────────────────────────────

  "NULL in CASE" - {
    "CASE WHEN with NULL condition" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (NULL);
          |SELECT CASE WHEN val = 1 THEN 'one' ELSE 'other' END FROM t;
          |""".trim.stripMargin
      )
      // NULL = 1 is unknown, falls to ELSE
      table.data(0).data(0) shouldBe TextValue("other")
    }

    "CASE WHEN IS NULL" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (NULL);
          |SELECT CASE WHEN val IS NULL THEN 'null' ELSE 'not null' END FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("null")
    }
  }

  // ── NULL in IN ────────────────────────────────────────────────────

  "NULL in IN" - {
    "NULL IN (1, 2, NULL) returns no rows (unknown)" ignore { // BUG: needs three-valued logic for IN
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (NULL);
          |SELECT val FROM t WHERE val IN (1, 2, NULL);
          |""".trim.stripMargin
      )
      table.data.length shouldBe 0
    }

    "value NOT IN list containing NULL returns no rows" ignore { // BUG: needs three-valued logic for NOT IN
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (3);
          |SELECT val FROM t WHERE val NOT IN (1, 2, NULL);
          |""".trim.stripMargin
      )
      // 3 NOT IN (1,2,NULL) → NOT (FALSE OR FALSE OR NULL) → NOT NULL → unknown → no rows
      table.data.length shouldBe 0
    }
  }

  // ── Empty string vs NULL ──────────────────────────────────────────

  "empty string vs NULL" - {
    "empty string is not NULL" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT);
          |INSERT INTO t (val) VALUES ('');
          |SELECT val IS NULL FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(false)
    }

    "empty string has length 0" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT);
          |INSERT INTO t (val) VALUES ('');
          |SELECT LENGTH(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 0)
    }

    "empty string equals empty string" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT);
          |INSERT INTO t (val) VALUES ('');
          |SELECT val FROM t WHERE val = '';
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
    }
  }
}

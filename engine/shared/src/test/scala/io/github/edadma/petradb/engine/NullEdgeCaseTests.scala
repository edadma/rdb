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
      val avg = table.data(0).data(0).doubleValue
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

  // ── NULL in AND/OR ─────────────────────────────────────────────────

  "NULL in AND" - {
    "TRUE AND NULL is NULL" in {
      val table = query("SELECT TRUE AND NULL;")
      table.data(0).data(0).isNull shouldBe true
    }

    "NULL AND TRUE is NULL" in {
      val table = query("SELECT NULL AND TRUE;")
      table.data(0).data(0).isNull shouldBe true
    }

    "FALSE AND NULL is FALSE" in {
      val table = query("SELECT FALSE AND NULL;")
      table.data(0).data(0) shouldBe BooleanValue(false)
    }

    "NULL AND FALSE is FALSE" in {
      val table = query("SELECT NULL AND FALSE;")
      table.data(0).data(0) shouldBe BooleanValue(false)
    }

    "NULL AND NULL is NULL" in {
      val table = query("SELECT NULL AND NULL;")
      table.data(0).data(0).isNull shouldBe true
    }
  }

  "NULL in OR" - {
    "TRUE OR NULL is TRUE" in {
      val table = query("SELECT TRUE OR NULL;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "NULL OR TRUE is TRUE" in {
      val table = query("SELECT NULL OR TRUE;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "FALSE OR NULL is NULL" in {
      val table = query("SELECT FALSE OR NULL;")
      table.data(0).data(0).isNull shouldBe true
    }

    "NULL OR FALSE is NULL" in {
      val table = query("SELECT NULL OR FALSE;")
      table.data(0).data(0).isNull shouldBe true
    }

    "NULL OR NULL is NULL" in {
      val table = query("SELECT NULL OR NULL;")
      table.data(0).data(0).isNull shouldBe true
    }
  }

  "NULL AND/OR in WHERE" - {
    "TRUE AND NULL excludes row" in {
      val table = query(
        """
          |CREATE TABLE t (a BOOLEAN, b BOOLEAN);
          |INSERT INTO t (a, b) VALUES (TRUE, NULL), (FALSE, NULL), (NULL, TRUE);
          |SELECT COUNT(*) FROM t WHERE a AND b;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 0)
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

    "ASC defaults to NULLS LAST (SQL standard)" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (3), (NULL), (1);
          |SELECT val FROM t ORDER BY val ASC;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
      table.data(1).data(0) shouldBe NumberValue(DIntType, 3)
      table.data(2).data(0).isNull shouldBe true
    }

    "DESC defaults to NULLS FIRST (SQL standard)" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (3), (NULL), (1);
          |SELECT val FROM t ORDER BY val DESC;
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
      table.data(1).data(0) shouldBe NumberValue(DIntType, 3)
      table.data(2).data(0) shouldBe NumberValue(DIntType, 1)
    }

    "implicit ORDER BY (no ASC/DESC) defaults to NULLS LAST" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (3), (NULL), (1);
          |SELECT val FROM t ORDER BY val;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
      table.data(1).data(0) shouldBe NumberValue(DIntType, 3)
      table.data(2).data(0).isNull shouldBe true
    }

    "DESC NULLS LAST overrides default" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (3), (NULL), (1);
          |SELECT val FROM t ORDER BY val DESC NULLS LAST;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 3)
      table.data(1).data(0) shouldBe NumberValue(DIntType, 1)
      table.data(2).data(0).isNull shouldBe true
    }

    "ASC NULLS FIRST overrides default" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (3), (NULL), (1);
          |SELECT val FROM t ORDER BY val ASC NULLS FIRST;
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
      table.data(1).data(0) shouldBe NumberValue(DIntType, 1)
      table.data(2).data(0) shouldBe NumberValue(DIntType, 3)
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
    "NULL IN (1, 2, NULL) returns no rows (unknown)" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (NULL);
          |SELECT val FROM t WHERE val IN (1, 2, NULL);
          |""".trim.stripMargin
      )
      table.data.length shouldBe 0
    }

    "value NOT IN list containing NULL returns no rows" in {
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

  // ── NULL stored value integrity across types ─────────────────────
  // Verifies that NULL inserted into typed columns is stored and
  // retrieved as NullValue, not silently converted (e.g. TextValue("NULL")).

  "NULL round-trip through typed columns" - {
    "INSERT NULL into TEXT column yields NullValue" in {
      val t = query("CREATE TABLE nrt1 (v TEXT); INSERT INTO nrt1 VALUES (NULL); SELECT v FROM nrt1;")
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "INSERT NULL into VARCHAR column yields NullValue" in {
      val t = query("CREATE TABLE nrt2 (v VARCHAR(50)); INSERT INTO nrt2 VALUES (NULL); SELECT v FROM nrt2;")
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "INSERT NULL into DATE column yields NullValue" in {
      val t = query("CREATE TABLE nrt3 (v DATE); INSERT INTO nrt3 VALUES (NULL); SELECT v FROM nrt3;")
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "INSERT NULL into TIMESTAMP column yields NullValue" in {
      val t = query("CREATE TABLE nrt4 (v TIMESTAMP); INSERT INTO nrt4 VALUES (NULL); SELECT v FROM nrt4;")
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "INSERT NULL into BOOLEAN column yields NullValue" in {
      val t = query("CREATE TABLE nrt5 (v BOOLEAN); INSERT INTO nrt5 VALUES (NULL); SELECT v FROM nrt5;")
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "INSERT NULL into UUID column yields NullValue" in {
      val t = query("CREATE TABLE nrt6 (v UUID); INSERT INTO nrt6 VALUES (NULL); SELECT v FROM nrt6;")
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "INSERT NULL into JSON column yields NullValue" in {
      val t = query("CREATE TABLE nrt7 (v JSON); INSERT INTO nrt7 VALUES (NULL); SELECT v FROM nrt7;")
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "INSERT NULL into NUMERIC column yields NullValue" in {
      val t = query("CREATE TABLE nrt8 (v NUMERIC(10,2)); INSERT INTO nrt8 VALUES (NULL); SELECT v FROM nrt8;")
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "INSERT NULL into ENUM column yields NullValue" in {
      val t = query(
        """CREATE TYPE mood AS ENUM ('happy', 'sad');
          |CREATE TABLE nrt9 (v mood);
          |INSERT INTO nrt9 VALUES (NULL);
          |SELECT v FROM nrt9;""".stripMargin
      )
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "UPDATE to NULL on TEXT column yields NullValue" in {
      val t = query(
        """CREATE TABLE nrt10 (id INT, v TEXT);
          |INSERT INTO nrt10 VALUES (1, 'hello');
          |UPDATE nrt10 SET v = NULL WHERE id = 1;
          |SELECT v FROM nrt10;""".stripMargin
      )
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "UPDATE to NULL on DATE column yields NullValue" in {
      val t = query(
        """CREATE TABLE nrt11 (id INT, v DATE);
          |INSERT INTO nrt11 VALUES (1, '2026-01-01');
          |UPDATE nrt11 SET v = NULL WHERE id = 1;
          |SELECT v FROM nrt11;""".stripMargin
      )
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "UPDATE to NULL on ENUM column yields NullValue" in {
      val t = query(
        """CREATE TYPE priority AS ENUM ('low', 'high');
          |CREATE TABLE nrt12 (id INT, v priority);
          |INSERT INTO nrt12 VALUES (1, 'low');
          |UPDATE nrt12 SET v = NULL WHERE id = 1;
          |SELECT v FROM nrt12;""".stripMargin
      )
      t.data(0).data(0) shouldBe a[NullValue]
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

package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import io.github.edadma.dal.{IntType => DIntType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ExpressionParsingTests extends AnyFreeSpec with Matchers with Testing {

  // ── AND/OR/NOT in SELECT ──────────────────────────────────────────

  "AND/OR/NOT in SELECT expressions" - {
    "SELECT TRUE AND TRUE" in {
      val table = query("SELECT TRUE AND TRUE;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "SELECT TRUE AND FALSE" in {
      val table = query("SELECT TRUE AND FALSE;")
      table.data(0).data(0) shouldBe BooleanValue(false)
    }

    "SELECT FALSE OR TRUE" in {
      val table = query("SELECT FALSE OR TRUE;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "SELECT NOT TRUE" in {
      val table = query("SELECT NOT TRUE;")
      table.data(0).data(0) shouldBe BooleanValue(false)
    }

    "SELECT NOT FALSE" in {
      val table = query("SELECT NOT FALSE;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "nested NOT NOT TRUE" in {
      val table = query("SELECT NOT NOT TRUE;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }
  }

  // ── Precedence: AND binds tighter than OR ─────────────────────────

  "operator precedence" - {
    "OR has lower precedence than AND" in {
      // TRUE OR FALSE AND FALSE → TRUE OR (FALSE AND FALSE) → TRUE OR FALSE → TRUE
      val table = query("SELECT TRUE OR FALSE AND FALSE;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "NOT has higher precedence than AND" in {
      // NOT FALSE AND NOT FALSE → (NOT FALSE) AND (NOT FALSE) → TRUE AND TRUE → TRUE
      val table = query("SELECT NOT FALSE AND NOT FALSE;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "comparison has higher precedence than AND" in {
      // 1 = 1 AND 2 = 2 → (1 = 1) AND (2 = 2) → TRUE AND TRUE → TRUE
      val table = query("SELECT 1 = 1 AND 2 = 2;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "comparison has higher precedence than OR" in {
      // 1 = 2 OR 3 = 3 → (1 = 2) OR (3 = 3) → FALSE OR TRUE → TRUE
      val table = query("SELECT 1 = 2 OR 3 = 3;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "arithmetic inside comparison" in {
      // 2 + 3 = 5 → (2 + 3) = 5 → TRUE
      val table = query("SELECT 2 + 3 = 5;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "parentheses override precedence" in {
      // (TRUE OR FALSE) AND FALSE → TRUE AND FALSE → FALSE
      val table = query("SELECT (TRUE OR FALSE) AND FALSE;")
      table.data(0).data(0) shouldBe BooleanValue(false)
    }
  }

  // ── Boolean expressions with aliases in SELECT ────────────────────

  "boolean expressions with aliases" - {
    "AND expression with alias" in {
      val table = query(
        """
          |CREATE TABLE t (a BOOLEAN, b BOOLEAN);
          |INSERT INTO t (a, b) VALUES (TRUE, FALSE);
          |SELECT a AND b AS result FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(false)
    }

    "comparison with alias" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (5);
          |SELECT val > 3 AS bigger FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "IS NULL with alias" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (NULL);
          |SELECT val IS NULL AS is_null FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
    }
  }

  // ── Comparisons in all contexts ───────────────────────────────────

  "comparisons in SELECT" - {
    "equals" in {
      val table = query("SELECT 1 = 1;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "not equals" in {
      val table = query("SELECT 1 != 2;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "less than" in {
      val table = query("SELECT 1 < 2;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "greater than" in {
      val table = query("SELECT 2 > 1;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "less than or equal" in {
      val table = query("SELECT 1 <= 1;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "greater than or equal" in {
      val table = query("SELECT 1 >= 1;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }
  }

  // ── LIKE in SELECT ────────────────────────────────────────────────

  "LIKE in SELECT" - {
    "LIKE returns boolean" in {
      val table = query("SELECT 'hello' LIKE 'hel%';")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "NOT LIKE returns boolean" in {
      val table = query("SELECT 'hello' NOT LIKE 'xyz%';")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "ILIKE returns boolean" in {
      val table = query("SELECT 'Hello' ILIKE 'hello';")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }
  }

  // ── IN in SELECT ──────────────────────────────────────────────────

  "IN in SELECT" - {
    "IN returns boolean" in {
      val table = query("SELECT 1 IN (1, 2, 3);")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "NOT IN returns boolean" in {
      val table = query("SELECT 4 NOT IN (1, 2, 3);")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }
  }

  // ── IS NULL / IS TRUE in SELECT ───────────────────────────────────

  "IS operators in SELECT" - {
    "IS NULL" in {
      val table = query("SELECT NULL IS NULL;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "IS NOT NULL" in {
      val table = query("SELECT 1 IS NOT NULL;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "IS TRUE" in {
      val table = query("SELECT TRUE IS TRUE;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "IS FALSE" in {
      val table = query("SELECT FALSE IS FALSE;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "IS NOT TRUE" in {
      val table = query("SELECT FALSE IS NOT TRUE;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "IS UNKNOWN" in {
      val table = query("SELECT NULL IS UNKNOWN;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }
  }

  // ── BETWEEN in all contexts ───────────────────────────────────────

  "BETWEEN" - {
    "BETWEEN in SELECT" in {
      val table = query("SELECT 5 BETWEEN 1 AND 10;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "NOT BETWEEN in SELECT" in {
      val table = query("SELECT 15 NOT BETWEEN 1 AND 10;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "BETWEEN in WHERE still works" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (1), (5), (10), (15);
          |SELECT COUNT(*) FROM t WHERE val BETWEEN 3 AND 10;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 2)
    }

    "BETWEEN combined with AND" in {
      // val BETWEEN 1 AND 10 AND val > 3 → (val BETWEEN 1 AND 10) AND (val > 3)
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (1), (5), (10), (15);
          |SELECT COUNT(*) FROM t WHERE val BETWEEN 1 AND 10 AND val > 3;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 2)
    }
  }

  // ── IS DISTINCT FROM ──────────────────────────────────────────────

  "IS DISTINCT FROM" - {
    "non-null values" in {
      val table = query("SELECT 1 IS DISTINCT FROM 2;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "null vs non-null" in {
      val table = query("SELECT NULL IS DISTINCT FROM 1;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "null vs null" in {
      val table = query("SELECT NULL IS NOT DISTINCT FROM NULL;")
      table.data(0).data(0) shouldBe BooleanValue(true)
    }
  }

  // ── EXISTS in SELECT ──────────────────────────────────────────────

  "EXISTS in SELECT" - {
    "EXISTS returns boolean" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (1);
          |SELECT EXISTS (SELECT 1 FROM t);
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
    }

    "NOT EXISTS returns boolean" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |SELECT NOT EXISTS (SELECT 1 FROM t);
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
    }
  }

  // ── Boolean in CASE WHEN ──────────────────────────────────────────

  "boolean in CASE" - {
    "AND in CASE WHEN condition" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b INT);
          |INSERT INTO t (a, b) VALUES (1, 2);
          |SELECT CASE WHEN a = 1 AND b = 2 THEN 'both' ELSE 'no' END FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("both")
    }

    "OR in CASE WHEN condition" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b INT);
          |INSERT INTO t (a, b) VALUES (1, 2);
          |SELECT CASE WHEN a = 99 OR b = 2 THEN 'yes' ELSE 'no' END FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("yes")
    }

    "boolean expression as CASE result" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (5);
          |SELECT CASE WHEN val > 0 THEN val > 3 ELSE FALSE END FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
    }
  }

  // ── Boolean in WHERE with complex expressions ─────────────────────

  "complex WHERE expressions" - {
    "AND with comparisons" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b INT);
          |INSERT INTO t (a, b) VALUES (1, 2), (3, 4), (5, 6);
          |SELECT COUNT(*) FROM t WHERE a > 1 AND b < 6;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
    }

    "OR with comparisons" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b INT);
          |INSERT INTO t (a, b) VALUES (1, 2), (3, 4), (5, 6);
          |SELECT COUNT(*) FROM t WHERE a = 1 OR b = 6;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 2)
    }

    "NOT with comparison" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (1), (2), (3);
          |SELECT COUNT(*) FROM t WHERE NOT val = 2;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 2)
    }

    "AND with IN" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b TEXT);
          |INSERT INTO t (a, b) VALUES (1, 'x'), (2, 'y'), (3, 'x');
          |SELECT COUNT(*) FROM t WHERE a IN (1, 3) AND b = 'x';
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 2)
    }

    "AND with LIKE" in {
      val table = query(
        """
          |CREATE TABLE t (name TEXT, active BOOLEAN);
          |INSERT INTO t (name, active) VALUES ('Alice', TRUE), ('Bob', FALSE), ('Amy', TRUE);
          |SELECT COUNT(*) FROM t WHERE name LIKE 'A%' AND active;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 2)
    }

    "AND with BETWEEN" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b TEXT);
          |INSERT INTO t (a, b) VALUES (1, 'x'), (5, 'y'), (10, 'x');
          |SELECT COUNT(*) FROM t WHERE a BETWEEN 2 AND 8 AND b = 'y';
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
    }

    "AND with IS NULL" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b INT);
          |INSERT INTO t (a, b) VALUES (1, NULL), (2, 3), (NULL, NULL);
          |SELECT COUNT(*) FROM t WHERE a IS NOT NULL AND b IS NULL;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
    }

    "OR with IS NULL" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b INT);
          |INSERT INTO t (a, b) VALUES (1, 2), (NULL, 3), (4, NULL);
          |SELECT COUNT(*) FROM t WHERE a IS NULL OR b IS NULL;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 2)
    }

    "NOT with IN" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (1), (2), (3), (4), (5);
          |SELECT COUNT(*) FROM t WHERE NOT val IN (2, 4);
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 3)
    }
  }

  // ── JOIN ON with boolean expressions ──────────────────────────────

  "JOIN ON with boolean expressions" - {
    "AND in JOIN ON" in {
      val table = query(
        """
          |CREATE TABLE t1 (a INT, b INT);
          |CREATE TABLE t2 (x INT, y INT);
          |INSERT INTO t1 (a, b) VALUES (1, 10), (2, 20);
          |INSERT INTO t2 (x, y) VALUES (1, 10), (1, 99);
          |SELECT t1.a FROM t1 JOIN t2 ON t1.a = t2.x AND t1.b = t2.y;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
    }
  }

  // ── CHECK constraint with boolean expressions ─────────────────────

  "CHECK with boolean expressions" - {
    "AND in CHECK" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b INT, CHECK (a > 0 AND b > 0));
          |INSERT INTO t (a, b) VALUES (1, 2);
          |SELECT a, b FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
    }

    "OR in CHECK" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT, CHECK (val = 'a' OR val = 'b'));
          |INSERT INTO t (val) VALUES ('a');
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("a")
    }

    "AND in CHECK rejects invalid" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (a INT, b INT, CHECK (a > 0 AND b > 0));
            |INSERT INTO t (a, b) VALUES (1, -1);
            |""".trim.stripMargin
        )
      }
    }
  }

  // ── Boolean in function arguments ─────────────────────────────────

  "boolean in function arguments" - {
    "COALESCE with boolean expressions" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (5);
          |SELECT COALESCE(val > 10, FALSE) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(false)
    }
  }

  // ── Multiple boolean expressions in SELECT list ───────────────────

  "multiple boolean expressions in SELECT" - {
    "two boolean columns" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b INT);
          |INSERT INTO t (a, b) VALUES (1, 2);
          |SELECT a = 1, b > 1 FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe BooleanValue(true)
      table.data(0).data(1) shouldBe BooleanValue(true)
    }

    "mixed boolean and value expressions" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (5);
          |SELECT val, val > 3, val * 2 FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 5)
      table.data(0).data(1) shouldBe BooleanValue(true)
      table.data(0).data(2) shouldBe NumberValue(DIntType, 10)
    }
  }

  // ── HAVING with boolean expressions ───────────────────────────────

  "HAVING with boolean expressions" - {
    "AND in HAVING" in {
      val table = query(
        """
          |CREATE TABLE t (grp TEXT, val INT);
          |INSERT INTO t (grp, val) VALUES ('a', 10), ('a', 20), ('b', 5);
          |SELECT grp, COUNT(*), SUM(val) FROM t GROUP BY grp HAVING COUNT(*) > 1 AND SUM(val) > 15;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("a")
    }
  }

  // ── UPDATE SET with boolean expressions ───────────────────────────

  "UPDATE with boolean expressions" - {
    "UPDATE WHERE with AND" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b TEXT);
          |INSERT INTO t (a, b) VALUES (1, 'x'), (2, 'y'), (3, 'x');
          |UPDATE t SET b = 'z' WHERE a > 1 AND b = 'x';
          |SELECT a, b FROM t ORDER BY a;
          |""".trim.stripMargin
      )
      table.data(0).data(1) shouldBe TextValue("x")
      table.data(1).data(1) shouldBe TextValue("y")
      table.data(2).data(1) shouldBe TextValue("z")
    }
  }

  // ── DELETE with boolean expressions ───────────────────────────────

  "DELETE with boolean expressions" - {
    "DELETE WHERE with OR" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (1), (2), (3), (4), (5);
          |DELETE FROM t WHERE val = 1 OR val = 5;
          |SELECT COUNT(*) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 3)
    }
  }
}

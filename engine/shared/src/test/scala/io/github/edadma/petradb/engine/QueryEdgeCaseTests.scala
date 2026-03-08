package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import io.github.edadma.dal.{IntType => DIntType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class QueryEdgeCaseTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE t (id INT, name TEXT, val INT);
      |INSERT INTO t (id, name, val) VALUES
      |  (1, 'Alice', 10),
      |  (2, 'Bob', 20),
      |  (3, 'Carol', 30);
      |""".trim.stripMargin

  // ── SELECT from empty table ───────────────────────────────────────

  "empty table queries" - {
    "SELECT * from empty table returns no rows" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b TEXT);
          |SELECT * FROM t;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 0
    }

    "WHERE on empty table returns no rows" in {
      val table = query(
        """
          |CREATE TABLE t (a INT);
          |SELECT a FROM t WHERE a = 1;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 0
    }

    "ORDER BY on empty table returns no rows" in {
      val table = query(
        """
          |CREATE TABLE t (a INT);
          |SELECT a FROM t ORDER BY a;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 0
    }

    "LIMIT on empty table returns no rows" in {
      val table = query(
        """
          |CREATE TABLE t (a INT);
          |SELECT a FROM t LIMIT 10;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 0
    }
  }

  // ── LIMIT / OFFSET edge cases ─────────────────────────────────────

  "LIMIT / OFFSET edge cases" - {
    "LIMIT 0 returns no rows" in {
      val table = query(
        s"""
          |$setup
          |SELECT * FROM t LIMIT 0;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 0
    }

    "LIMIT larger than result set returns all rows" in {
      val table = query(
        s"""
          |$setup
          |SELECT * FROM t LIMIT 100;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 3
    }

    "OFFSET past end returns no rows" in {
      val table = query(
        s"""
          |$setup
          |SELECT * FROM t ORDER BY id OFFSET 100;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 0
    }

    "OFFSET 0 returns all rows" in {
      val table = query(
        s"""
          |$setup
          |SELECT * FROM t ORDER BY id OFFSET 0;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 3
    }

    "LIMIT 1 OFFSET 1 returns second row" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM t ORDER BY id LIMIT 1 OFFSET 1;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("Bob")
    }
  }

  // ── WHERE with no matches ─────────────────────────────────────────

  "WHERE with no matches" - {
    "returns empty result" in {
      val table = query(
        s"""
          |$setup
          |SELECT * FROM t WHERE name = 'Nobody';
          |""".trim.stripMargin
      )
      table.data.length shouldBe 0
    }

    "with aggregate returns correct count" in {
      val table = query(
        s"""
          |$setup
          |SELECT COUNT(*) FROM t WHERE name = 'Nobody';
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 0)
    }
  }

  // ── Single-row vs multi-row behavior ──────────────────────────────

  "single row behavior" - {
    "aggregate on single row" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (42);
          |SELECT MIN(val), MAX(val), AVG(val), SUM(val), COUNT(*) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 42)
      table.data(0).data(1) shouldBe NumberValue(DIntType, 42)
      table.data(0).data(4) shouldBe NumberValue(DIntType, 1)
    }

    "GROUP BY on single row" in {
      val table = query(
        """
          |CREATE TABLE t (grp TEXT, val INT);
          |INSERT INTO t (grp, val) VALUES ('a', 10);
          |SELECT grp, SUM(val) FROM t GROUP BY grp;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("a")
      table.data(0).data(1) shouldBe NumberValue(DIntType, 10)
    }
  }

  // ── BETWEEN edge cases ────────────────────────────────────────────

  "BETWEEN edge cases" - {
    "BETWEEN includes endpoints" in {
      val table = query(
        s"""
          |$setup
          |SELECT COUNT(*) FROM t WHERE val BETWEEN 10 AND 30;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 3)
    }

    "BETWEEN with same value on both sides matches exact value" in {
      val table = query(
        s"""
          |$setup
          |SELECT COUNT(*) FROM t WHERE val BETWEEN 20 AND 20;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
    }

    "BETWEEN with reversed range matches nothing" in {
      val table = query(
        s"""
          |$setup
          |SELECT COUNT(*) FROM t WHERE val BETWEEN 30 AND 10;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 0)
    }

    "NOT BETWEEN excludes endpoints" in {
      val table = query(
        s"""
          |$setup
          |SELECT COUNT(*) FROM t WHERE val NOT BETWEEN 10 AND 30;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 0)
    }
  }

  // ── LIKE edge cases ───────────────────────────────────────────────

  "LIKE edge cases" - {
    "LIKE with no wildcards is exact match" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM t WHERE name LIKE 'Alice';
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
    }

    "LIKE '%' matches everything" in {
      val table = query(
        s"""
          |$setup
          |SELECT COUNT(*) FROM t WHERE name LIKE '%';
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 3)
    }

    "LIKE with empty string matches only empty strings" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT);
          |INSERT INTO t (val) VALUES (''), ('a'), ('abc');
          |SELECT COUNT(*) FROM t WHERE val LIKE '';
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
    }

    "LIKE '_' matches single character" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT);
          |INSERT INTO t (val) VALUES ('a'), ('ab'), ('abc'), ('');
          |SELECT COUNT(*) FROM t WHERE val LIKE '_';
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
    }

    "LIKE with NULL value returns no rows" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT);
          |INSERT INTO t (val) VALUES (NULL);
          |SELECT COUNT(*) FROM t WHERE val LIKE '%';
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 0)
    }
  }

  // ── IN edge cases ─────────────────────────────────────────────────

  "IN edge cases" - {
    "IN with single value" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM t WHERE name IN ('Alice');
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("Alice")
    }

    "NOT IN with all values returns nothing" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM t WHERE name NOT IN ('Alice', 'Bob', 'Carol');
          |""".trim.stripMargin
      )
      table.data.length shouldBe 0
    }

    "IN with empty subquery returns nothing" in {
      val table = query(
        s"""
          |$setup
          |CREATE TABLE empty_t (name TEXT);
          |SELECT name FROM t WHERE name IN (SELECT name FROM empty_t);
          |""".trim.stripMargin
      )
      table.data.length shouldBe 0
    }
  }

  // ── EXISTS edge cases ─────────────────────────────────────────────

  "EXISTS edge cases" - {
    "EXISTS with empty subquery is false" in {
      val table = query(
        s"""
          |$setup
          |CREATE TABLE empty_t (x INT);
          |SELECT COUNT(*) FROM t WHERE EXISTS (SELECT 1 FROM empty_t);
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 0)
    }

    "NOT EXISTS with empty subquery is true" in {
      val table = query(
        s"""
          |$setup
          |CREATE TABLE empty_t (x INT);
          |SELECT COUNT(*) FROM t WHERE NOT EXISTS (SELECT 1 FROM empty_t);
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 3)
    }

    "EXISTS with non-empty subquery is true" in {
      val table = query(
        s"""
          |$setup
          |SELECT COUNT(*) FROM t WHERE EXISTS (SELECT 1 FROM t);
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 3)
    }
  }

  // ── Set operations edge cases ─────────────────────────────────────

  "set operation edge cases" - {
    "UNION removes duplicates" in {
      val table = query(
        """
          |CREATE TABLE t1 (val INT);
          |CREATE TABLE t2 (val INT);
          |INSERT INTO t1 (val) VALUES (1), (2);
          |INSERT INTO t2 (val) VALUES (2), (3);
          |SELECT val FROM t1 UNION SELECT val FROM t2 ORDER BY val;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 3
    }

    "UNION ALL preserves duplicates" in {
      val table = query(
        """
          |CREATE TABLE t1 (val INT);
          |CREATE TABLE t2 (val INT);
          |INSERT INTO t1 (val) VALUES (1), (2);
          |INSERT INTO t2 (val) VALUES (2), (3);
          |SELECT val FROM t1 UNION ALL SELECT val FROM t2 ORDER BY val;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 4
    }

    "INTERSECT returns common rows" in {
      val table = query(
        """
          |CREATE TABLE t1 (val INT);
          |CREATE TABLE t2 (val INT);
          |INSERT INTO t1 (val) VALUES (1), (2), (3);
          |INSERT INTO t2 (val) VALUES (2), (3), (4);
          |SELECT val FROM t1 INTERSECT SELECT val FROM t2 ORDER BY val;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(DIntType, 2)
      table.data(1).data(0) shouldBe NumberValue(DIntType, 3)
    }

    "EXCEPT returns rows only in first" in {
      val table = query(
        """
          |CREATE TABLE t1 (val INT);
          |CREATE TABLE t2 (val INT);
          |INSERT INTO t1 (val) VALUES (1), (2), (3);
          |INSERT INTO t2 (val) VALUES (2), (3), (4);
          |SELECT val FROM t1 EXCEPT SELECT val FROM t2;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
    }

    "UNION with empty first table" in {
      val table = query(
        """
          |CREATE TABLE t1 (val INT);
          |CREATE TABLE t2 (val INT);
          |INSERT INTO t2 (val) VALUES (1);
          |SELECT val FROM t1 UNION SELECT val FROM t2;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
    }

    "UNION with empty second table" in {
      val table = query(
        """
          |CREATE TABLE t1 (val INT);
          |CREATE TABLE t2 (val INT);
          |INSERT INTO t1 (val) VALUES (1);
          |SELECT val FROM t1 UNION SELECT val FROM t2;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
    }
  }

  // ── Aggregate without GROUP BY on empty table ─────────────────────

  "aggregate without GROUP BY on empty table" - {
    "SUM returns NULL" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |SELECT SUM(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }

    "COUNT returns 0" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |SELECT COUNT(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 0)
    }

    "MIN returns NULL" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |SELECT MIN(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }

    "MAX returns NULL" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |SELECT MAX(val) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }
  }

  // ── ORDER BY with expressions ─────────────────────────────────────

  "ORDER BY edge cases" - {
    "ORDER BY column not in SELECT list" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM t ORDER BY val DESC;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("Carol")
      table.data(1).data(0) shouldBe TextValue("Bob")
      table.data(2).data(0) shouldBe TextValue("Alice")
    }

    "ORDER BY column number" in {
      val table = query(
        s"""
          |$setup
          |SELECT name, val FROM t ORDER BY 2 DESC;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("Carol")
    }

    "ORDER BY multiple columns" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b INT);
          |INSERT INTO t (a, b) VALUES (1, 2), (1, 1), (2, 1);
          |SELECT a, b FROM t ORDER BY a ASC, b DESC;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
      table.data(0).data(1) shouldBe NumberValue(DIntType, 2)
      table.data(1).data(0) shouldBe NumberValue(DIntType, 1)
      table.data(1).data(1) shouldBe NumberValue(DIntType, 1)
      table.data(2).data(0) shouldBe NumberValue(DIntType, 2)
      table.data(2).data(1) shouldBe NumberValue(DIntType, 1)
    }
  }
}

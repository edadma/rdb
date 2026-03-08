package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import io.github.edadma.dal.{IntType => DIntType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class EagerColumnValidationTests extends AnyFreeSpec with Matchers with Testing {

  // ── WHERE clause validation ────────────────────────────────────────

  "WHERE nonexistent column" - {
    "fails on empty table" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |SELECT a FROM t WHERE b = 1;
            |""".trim.stripMargin
        )
      }
    }

    "fails with nonexistent column in expression" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |SELECT a FROM t WHERE b + 1 > 0;
            |""".trim.stripMargin
        )
      }
    }

    "fails with nonexistent column in AND" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |SELECT a FROM t WHERE a = 1 AND b = 2;
            |""".trim.stripMargin
        )
      }
    }

    "fails with nonexistent column in OR" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |SELECT a FROM t WHERE a = 1 OR b = 2;
            |""".trim.stripMargin
        )
      }
    }

    "fails with nonexistent column in CASE" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |SELECT a FROM t WHERE CASE WHEN b = 1 THEN TRUE ELSE FALSE END;
            |""".trim.stripMargin
        )
      }
    }

    "fails with nonexistent column in BETWEEN" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |SELECT a FROM t WHERE b BETWEEN 1 AND 10;
            |""".trim.stripMargin
        )
      }
    }
  }

  // ── GROUP BY validation ────────────────────────────────────────────

  "GROUP BY nonexistent column" - {
    "fails on empty table" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |SELECT a FROM t GROUP BY b;
            |""".trim.stripMargin
        )
      }
    }

    "fails with nonexistent column in expression" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |SELECT a FROM t GROUP BY b + 1;
            |""".trim.stripMargin
        )
      }
    }
  }

  // ── HAVING validation ──────────────────────────────────────────────

  "HAVING nonexistent column" - {
    "fails on empty table" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |SELECT a, COUNT(*) FROM t GROUP BY a HAVING b > 1;
            |""".trim.stripMargin
        )
      }
    }

    "fails with nonexistent column in expression" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |SELECT a, COUNT(*) FROM t GROUP BY a HAVING b + 1 > 2;
            |""".trim.stripMargin
        )
      }
    }
  }

  // ── ORDER BY validation (expression depth) ─────────────────────────

  "ORDER BY nonexistent column in nested expression" - {
    "fails with nonexistent column in CASE" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |SELECT a FROM t ORDER BY CASE WHEN b = 1 THEN 0 ELSE 1 END;
            |""".trim.stripMargin
        )
      }
    }

    "fails with nonexistent column in BETWEEN" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |SELECT a FROM t ORDER BY CASE WHEN a BETWEEN b AND 10 THEN 0 ELSE 1 END;
            |""".trim.stripMargin
        )
      }
    }
  }
}

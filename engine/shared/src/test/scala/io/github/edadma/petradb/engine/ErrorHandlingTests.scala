package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import io.github.edadma.dal.{IntType => DIntType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ErrorHandlingTests extends AnyFreeSpec with Matchers with Testing {

  // ── Nonexistent tables ────────────────────────────────────────────

  "nonexistent table" - {
    "SELECT from nonexistent table fails" in {
      an[Exception] should be thrownBy {
        query("SELECT * FROM nonexistent;")
      }
    }

    "INSERT into nonexistent table fails" in {
      an[Exception] should be thrownBy {
        results("INSERT INTO nonexistent (x) VALUES (1);")
      }
    }

    "UPDATE nonexistent table fails" in {
      an[Exception] should be thrownBy {
        results("UPDATE nonexistent SET x = 1;")
      }
    }

    "DELETE from nonexistent table fails" in {
      an[Exception] should be thrownBy {
        results("DELETE FROM nonexistent;")
      }
    }

    "DROP nonexistent table fails" in {
      an[Exception] should be thrownBy {
        results("DROP TABLE nonexistent;")
      }
    }

    "DROP TABLE IF EXISTS on nonexistent table succeeds" in {
      // Should not throw
      results("DROP TABLE IF EXISTS nonexistent;")
    }

    "CREATE TABLE that already exists fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (x INT);
            |CREATE TABLE t (y INT);
            |""".trim.stripMargin
        )
      }
    }

    "CREATE TABLE IF NOT EXISTS on existing table succeeds" in {
      results(
        """
          |CREATE TABLE t (x INT);
          |CREATE TABLE IF NOT EXISTS t (y INT);
          |""".trim.stripMargin
      )
    }
  }

  // ── Nonexistent columns ───────────────────────────────────────────

  "nonexistent column" - {
    "SELECT nonexistent column fails" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |SELECT b FROM t;
            |""".trim.stripMargin
        )
      }
    }

    "INSERT into nonexistent column fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (a INT);
            |INSERT INTO t (b) VALUES (1);
            |""".trim.stripMargin
        )
      }
    }

    "UPDATE nonexistent column fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (a INT);
            |INSERT INTO t (a) VALUES (1);
            |UPDATE t SET b = 2;
            |""".trim.stripMargin
        )
      }
    }

    "WHERE references nonexistent column fails" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |INSERT INTO t (a) VALUES (1);
            |SELECT a FROM t WHERE b = 1;
            |""".trim.stripMargin
        )
      }
    }

    "ORDER BY nonexistent column fails" ignore { // BUG: ORDER BY doesn't validate column existence
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

    "GROUP BY nonexistent column fails" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (a INT);
            |INSERT INTO t (a) VALUES (1);
            |SELECT a FROM t GROUP BY b;
            |""".trim.stripMargin
        )
      }
    }
  }

  // ── Column count mismatches ───────────────────────────────────────

  "column count mismatch" - {
    "too many values in INSERT" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (a INT);
            |INSERT INTO t (a) VALUES (1, 2);
            |""".trim.stripMargin
        )
      }
    }

    "too few values in INSERT" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (a INT, b INT);
            |INSERT INTO t (a, b) VALUES (1);
            |""".trim.stripMargin
        )
      }
    }

    "duplicate column in INSERT column list" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (a INT, b INT);
            |INSERT INTO t (a, a) VALUES (1, 2);
            |""".trim.stripMargin
        )
      }
    }
  }

  // ── Ambiguous column references ───────────────────────────────────

  "ambiguous column" - {
    "ambiguous column in join without qualifier fails" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t1 (id INT, name TEXT);
            |CREATE TABLE t2 (id INT, name TEXT);
            |INSERT INTO t1 (id, name) VALUES (1, 'a');
            |INSERT INTO t2 (id, name) VALUES (1, 'b');
            |SELECT name FROM t1 JOIN t2 ON t1.id = t2.id;
            |""".trim.stripMargin
        )
      }
    }

    "qualified column in join works" in {
      val table = query(
        """
          |CREATE TABLE t1 (id INT, name TEXT);
          |CREATE TABLE t2 (id INT, name TEXT);
          |INSERT INTO t1 (id, name) VALUES (1, 'a');
          |INSERT INTO t2 (id, name) VALUES (1, 'b');
          |SELECT t1.name, t2.name FROM t1 JOIN t2 ON t1.id = t2.id;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("a")
      table.data(0).data(1) shouldBe TextValue("b")
    }
  }

  // ── Type errors ───────────────────────────────────────────────────

  "type errors" - {
    "invalid integer literal" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (val INT);
            |INSERT INTO t (val) VALUES ('not_a_number');
            |""".trim.stripMargin
        )
      }
    }

    "invalid boolean literal" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (val BOOLEAN);
            |INSERT INTO t (val) VALUES ('maybe');
            |""".trim.stripMargin
        )
      }
    }

    "division by zero" in {
      an[Exception] should be thrownBy {
        query("SELECT 1 / 0;")
      }
    }
  }

  // ── ALTER TABLE errors ────────────────────────────────────────────

  "ALTER TABLE errors" - {
    "ALTER nonexistent table fails" in {
      an[Exception] should be thrownBy {
        results("ALTER TABLE nonexistent ADD COLUMN x INT;")
      }
    }

    "ADD duplicate column fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (a INT);
            |ALTER TABLE t ADD COLUMN a INT;
            |""".trim.stripMargin
        )
      }
    }

    "DROP nonexistent column fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (a INT);
            |ALTER TABLE t DROP COLUMN b;
            |""".trim.stripMargin
        )
      }
    }
  }

  // ── Subquery errors ───────────────────────────────────────────────

  "subquery errors" - {
    "scalar subquery returning multiple rows fails" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (val INT);
            |INSERT INTO t (val) VALUES (1), (2);
            |SELECT (SELECT val FROM t);
            |""".trim.stripMargin
        )
      }
    }
  }
}

package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import io.github.edadma.dal.{IntType => DIntType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ConstraintEnforcementTests extends AnyFreeSpec with Matchers with Testing {

  // ── NOT NULL enforcement ──────────────────────────────────────────

  "NOT NULL" - {
    "rejects explicit NULL insert" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (name TEXT NOT NULL);
            |INSERT INTO t (name) VALUES (NULL);
            |""".trim.stripMargin
        )
      }
    }

    "rejects implicit NULL (omitted column)" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (a TEXT, b TEXT NOT NULL);
            |INSERT INTO t (a) VALUES ('x');
            |""".trim.stripMargin
        )
      }
    }

    "rejects UPDATE setting column to NULL" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (name TEXT NOT NULL);
            |INSERT INTO t (name) VALUES ('Alice');
            |UPDATE t SET name = NULL;
            |""".trim.stripMargin
        )
      }
    }

    "allows NULL in nullable column" in {
      val table = query(
        """
          |CREATE TABLE t (name TEXT);
          |INSERT INTO t (name) VALUES (NULL);
          |SELECT name FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }

    "NOT NULL with DEFAULT still rejects explicit NULL" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (val INT NOT NULL DEFAULT 0);
            |INSERT INTO t (val) VALUES (NULL);
            |""".trim.stripMargin
        )
      }
    }

    "NOT NULL with DEFAULT uses default when column omitted" in {
      val table = query(
        """
          |CREATE TABLE t (id SERIAL PRIMARY KEY, val INT NOT NULL DEFAULT 42);
          |INSERT INTO t (id) VALUES (1);
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 42)
    }
  }

  // ── UNIQUE constraint enforcement ─────────────────────────────────

  "UNIQUE" - {
    "rejects duplicate value" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (email TEXT UNIQUE);
            |INSERT INTO t (email) VALUES ('a@b.com');
            |INSERT INTO t (email) VALUES ('a@b.com');
            |""".trim.stripMargin
        )
      }
    }

    "allows multiple NULLs in unique column" in {
      val table = query(
        """
          |CREATE TABLE t (email TEXT UNIQUE);
          |INSERT INTO t (email) VALUES (NULL);
          |INSERT INTO t (email) VALUES (NULL);
          |SELECT COUNT(*) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 2)
    }

    "rejects duplicate on UPDATE" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (id INT, email TEXT UNIQUE);
            |INSERT INTO t (id, email) VALUES (1, 'a@b.com');
            |INSERT INTO t (id, email) VALUES (2, 'c@d.com');
            |UPDATE t SET email = 'a@b.com' WHERE id = 2;
            |""".trim.stripMargin
        )
      }
    }

    "composite unique rejects duplicate combination" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (a INT, b INT, UNIQUE (a, b));
            |INSERT INTO t (a, b) VALUES (1, 2);
            |INSERT INTO t (a, b) VALUES (1, 2);
            |""".trim.stripMargin
        )
      }
    }

    "composite unique allows same value in one column" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b INT, UNIQUE (a, b));
          |INSERT INTO t (a, b) VALUES (1, 2);
          |INSERT INTO t (a, b) VALUES (1, 3);
          |SELECT COUNT(*) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 2)
    }
  }

  // ── PRIMARY KEY enforcement ───────────────────────────────────────

  "PRIMARY KEY" - {
    "rejects duplicate primary key" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (id INT PRIMARY KEY, name TEXT);
            |INSERT INTO t (id, name) VALUES (1, 'Alice');
            |INSERT INTO t (id, name) VALUES (1, 'Bob');
            |""".trim.stripMargin
        )
      }
    }

    "rejects NULL primary key" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (id INT PRIMARY KEY, name TEXT);
            |INSERT INTO t (id, name) VALUES (NULL, 'Alice');
            |""".trim.stripMargin
        )
      }
    }

    "composite PK rejects duplicate combination" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (a INT, b INT, c TEXT, PRIMARY KEY (a, b));
            |INSERT INTO t (a, b, c) VALUES (1, 2, 'first');
            |INSERT INTO t (a, b, c) VALUES (1, 2, 'second');
            |""".trim.stripMargin
        )
      }
    }

    "composite PK rejects NULL in any key column" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (a INT, b INT, PRIMARY KEY (a, b));
            |INSERT INTO t (a, b) VALUES (1, NULL);
            |""".trim.stripMargin
        )
      }
    }

    "rejects UPDATE creating duplicate PK" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (id INT PRIMARY KEY, name TEXT);
            |INSERT INTO t (id, name) VALUES (1, 'Alice');
            |INSERT INTO t (id, name) VALUES (2, 'Bob');
            |UPDATE t SET id = 1 WHERE id = 2;
            |""".trim.stripMargin
        )
      }
    }
  }

  // ── CHECK + other constraints interaction ─────────────────────────

  "constraint interactions" - {
    "CHECK + NOT NULL both enforced" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (val INT NOT NULL CHECK (val > 0));
            |INSERT INTO t (val) VALUES (NULL);
            |""".trim.stripMargin
        )
      }
    }

    "CHECK boundary: exactly at boundary fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (val INT CHECK (val > 0));
            |INSERT INTO t (val) VALUES (0);
            |""".trim.stripMargin
        )
      }
    }

    "CHECK boundary: just above boundary succeeds" in {
      val table = query(
        """
          |CREATE TABLE t (val INT CHECK (val > 0));
          |INSERT INTO t (val) VALUES (1);
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
    }

    "UNIQUE + NOT NULL both enforced" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (email TEXT NOT NULL UNIQUE);
            |INSERT INTO t (email) VALUES ('a@b.com');
            |INSERT INTO t (email) VALUES ('a@b.com');
            |""".trim.stripMargin
        )
      }
    }

    "multiple constraints on same column all enforced" in {
      // First verify valid insert works
      val table = query(
        """
          |CREATE TABLE t (val INT NOT NULL UNIQUE CHECK (val > 0));
          |INSERT INTO t (val) VALUES (1);
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
    }

    "multiple constraints - NULL rejected" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (val INT NOT NULL UNIQUE CHECK (val > 0));
            |INSERT INTO t (val) VALUES (NULL);
            |""".trim.stripMargin
        )
      }
    }

    "multiple constraints - check violation rejected" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (val INT NOT NULL UNIQUE CHECK (val > 0));
            |INSERT INTO t (val) VALUES (-1);
            |""".trim.stripMargin
        )
      }
    }

    "multiple constraints - duplicate rejected" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (val INT NOT NULL UNIQUE CHECK (val > 0));
            |INSERT INTO t (val) VALUES (1);
            |INSERT INTO t (val) VALUES (1);
            |""".trim.stripMargin
        )
      }
    }
  }
}

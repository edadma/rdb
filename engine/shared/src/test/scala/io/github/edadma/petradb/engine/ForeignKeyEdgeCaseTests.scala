package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import io.github.edadma.dal.{IntType => DIntType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ForeignKeyEdgeCaseTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE parent (id INT PRIMARY KEY, name TEXT);
      |CREATE TABLE child (id INT PRIMARY KEY, parent_id INT REFERENCES parent(id), val TEXT);
      |INSERT INTO parent (id, name) VALUES (1, 'Alpha'), (2, 'Beta');
      |""".trim.stripMargin

  // ── FK INSERT enforcement ─────────────────────────────────────────

  "FK INSERT enforcement" - {
    "insert with valid FK succeeds" in {
      val table = query(
        s"""
          |$setup
          |INSERT INTO child (id, parent_id, val) VALUES (1, 1, 'x');
          |SELECT val FROM child;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("x")
    }

    "insert with invalid FK fails" in {
      an[Exception] should be thrownBy {
        results(
          s"""
            |$setup
            |INSERT INTO child (id, parent_id, val) VALUES (1, 999, 'x');
            |""".trim.stripMargin
        )
      }
    }

    "insert with NULL FK succeeds (NULLs bypass FK check)" in {
      val table = query(
        s"""
          |$setup
          |INSERT INTO child (id, parent_id, val) VALUES (1, NULL, 'orphan');
          |SELECT val FROM child;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("orphan")
    }

    "multi-row insert fails if any FK is invalid" in {
      an[Exception] should be thrownBy {
        results(
          s"""
            |$setup
            |INSERT INTO child (id, parent_id, val) VALUES (1, 1, 'ok'), (2, 999, 'bad');
            |""".trim.stripMargin
        )
      }
    }
  }

  // ── FK DELETE enforcement ─────────────────────────────────────────

  "FK DELETE enforcement" - {
    "delete parent with child references fails (RESTRICT default)" in {
      an[Exception] should be thrownBy {
        results(
          s"""
            |$setup
            |INSERT INTO child (id, parent_id, val) VALUES (1, 1, 'x');
            |DELETE FROM parent WHERE id = 1;
            |""".trim.stripMargin
        )
      }
    }

    "delete parent without child references succeeds" in {
      val res = results(
        s"""
          |$setup
          |INSERT INTO child (id, parent_id, val) VALUES (1, 1, 'x');
          |DELETE FROM parent WHERE id = 2;
          |""".trim.stripMargin
      )
      res.last shouldBe DeleteResult(1)
    }
  }

  // ── FK UPDATE enforcement ─────────────────────────────────────────

  "FK UPDATE enforcement" - {
    "update child FK to invalid value fails" in {
      an[Exception] should be thrownBy {
        results(
          s"""
            |$setup
            |INSERT INTO child (id, parent_id, val) VALUES (1, 1, 'x');
            |UPDATE child SET parent_id = 999 WHERE id = 1;
            |""".trim.stripMargin
        )
      }
    }

    "update child FK to valid value succeeds" in {
      val table = query(
        s"""
          |$setup
          |INSERT INTO child (id, parent_id, val) VALUES (1, 1, 'x');
          |UPDATE child SET parent_id = 2 WHERE id = 1;
          |SELECT parent_id FROM child;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 2)
    }

    "update child FK to NULL succeeds" in {
      val table = query(
        s"""
          |$setup
          |INSERT INTO child (id, parent_id, val) VALUES (1, 1, 'x');
          |UPDATE child SET parent_id = NULL WHERE id = 1;
          |SELECT parent_id FROM child;
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }

    "update parent PK with child references fails" in {
      an[Exception] should be thrownBy {
        results(
          s"""
            |$setup
            |INSERT INTO child (id, parent_id, val) VALUES (1, 1, 'x');
            |UPDATE parent SET id = 100 WHERE id = 1;
            |""".trim.stripMargin
        )
      }
    }
  }

  // ── CASCADE DELETE ────────────────────────────────────────────────

  "CASCADE DELETE" - {
    "deleting parent cascades to children" in {
      val table = query(
        """
          |CREATE TABLE p (id INT PRIMARY KEY);
          |CREATE TABLE c (id INT PRIMARY KEY, pid INT REFERENCES p(id) ON DELETE CASCADE);
          |INSERT INTO p (id) VALUES (1), (2);
          |INSERT INTO c (id, pid) VALUES (10, 1), (20, 1), (30, 2);
          |DELETE FROM p WHERE id = 1;
          |SELECT COUNT(*) FROM c;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
    }

    "cascading delete through multiple levels" in {
      val table = query(
        """
          |CREATE TABLE a (id INT PRIMARY KEY);
          |CREATE TABLE b (id INT PRIMARY KEY, aid INT REFERENCES a(id) ON DELETE CASCADE);
          |CREATE TABLE c (id INT PRIMARY KEY, bid INT REFERENCES b(id) ON DELETE CASCADE);
          |INSERT INTO a (id) VALUES (1);
          |INSERT INTO b (id, aid) VALUES (10, 1);
          |INSERT INTO c (id, bid) VALUES (100, 10);
          |DELETE FROM a WHERE id = 1;
          |SELECT COUNT(*) FROM c;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 0)
    }
  }

  // ── SET NULL on DELETE ────────────────────────────────────────────

  "SET NULL on DELETE" - {
    "deleting parent sets child FK to NULL" in {
      val table = query(
        """
          |CREATE TABLE p (id INT PRIMARY KEY);
          |CREATE TABLE c (id INT PRIMARY KEY, pid INT REFERENCES p(id) ON DELETE SET NULL);
          |INSERT INTO p (id) VALUES (1);
          |INSERT INTO c (id, pid) VALUES (10, 1);
          |DELETE FROM p WHERE id = 1;
          |SELECT pid FROM c WHERE id = 10;
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }
  }

  // ── FK DDL errors ─────────────────────────────────────────────────

  "FK DDL errors" - {
    "FK referencing nonexistent table fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE c (id INT, pid INT REFERENCES nonexistent(id));
            |""".trim.stripMargin
        )
      }
    }

    "FK referencing nonexistent column fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE p (id INT PRIMARY KEY);
            |CREATE TABLE c (id INT, pid INT REFERENCES p(nonexistent));
            |""".trim.stripMargin
        )
      }
    }
  }
}

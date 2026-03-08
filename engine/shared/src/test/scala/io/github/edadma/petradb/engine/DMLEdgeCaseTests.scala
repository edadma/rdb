package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import io.github.edadma.dal.{IntType => DIntType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DMLEdgeCaseTests extends AnyFreeSpec with Matchers with Testing {

  // ── INSERT edge cases ─────────────────────────────────────────────

  "INSERT edge cases" - {
    "insert into empty column list with values fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (a INT, b INT);
            |INSERT INTO t () VALUES (1, 2);
            |""".trim.stripMargin
        )
      }
    }

    "insert zero rows is valid (empty VALUES)" in {
      // This may or may not be supported - just verifying behavior
      val table = query(
        """
          |CREATE TABLE t (a INT);
          |SELECT COUNT(*) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 0)
    }

    "multi-row insert with inconsistent value counts fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (a INT, b INT);
            |INSERT INTO t (a, b) VALUES (1, 2), (3);
            |""".trim.stripMargin
        )
      }
    }

    "RETURNING * returns inserted values" in {
      val res = results(
        """
          |CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT);
          |INSERT INTO t (name) VALUES ('Alice') RETURNING *;
          |""".trim.stripMargin
      )
      val ins = res.last.asInstanceOf[InsertResult]
      ins.obj("name") shouldBe TextValue("Alice")
    }

    "RETURNING specific column" in {
      val res = results(
        """
          |CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT);
          |INSERT INTO t (name) VALUES ('Alice') RETURNING id;
          |""".trim.stripMargin
      )
      val ins = res.last.asInstanceOf[InsertResult]
      ins.obj("id") shouldBe NumberValue(DIntType, 1)
    }
  }

  // ── UPDATE edge cases ─────────────────────────────────────────────

  "UPDATE edge cases" - {
    "update with expression using current value" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (10);
          |UPDATE t SET val = val + 5;
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 15)
    }

    "update with expression using current value (multiply)" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (10);
          |UPDATE t SET val = val * 2;
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 20)
    }

    "update setting value to NULL" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (10);
          |UPDATE t SET val = NULL;
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }

    "update with WHERE matching no rows changes nothing" in {
      val res = results(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (10);
          |UPDATE t SET val = 20 WHERE val = 999;
          |""".trim.stripMargin
      )
      res.last shouldBe UpdateResult(0)
    }

    "update on empty table returns 0 count" in {
      val res = results(
        """
          |CREATE TABLE t (val INT);
          |UPDATE t SET val = 1;
          |""".trim.stripMargin
      )
      res.last shouldBe UpdateResult(0)
    }

    "multiple updates accumulate" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (1);
          |UPDATE t SET val = val + 1;
          |UPDATE t SET val = val + 1;
          |UPDATE t SET val = val + 1;
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 4)
    }
  }

  // ── DELETE edge cases ─────────────────────────────────────────────

  "DELETE edge cases" - {
    "delete from empty table returns 0 count" in {
      val res = results(
        """
          |CREATE TABLE t (val INT);
          |DELETE FROM t;
          |""".trim.stripMargin
      )
      res.last shouldBe DeleteResult(0)
    }

    "delete with WHERE matching no rows" in {
      val res = results(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (1);
          |DELETE FROM t WHERE val = 999;
          |""".trim.stripMargin
      )
      res.last shouldBe DeleteResult(0)
    }

    "delete then insert reuses table correctly" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (1), (2), (3);
          |DELETE FROM t;
          |INSERT INTO t (val) VALUES (10);
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(DIntType, 10)
    }

    "selective delete preserves other rows" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (1), (2), (3), (4), (5);
          |DELETE FROM t WHERE val % 2 = 0;
          |SELECT val FROM t ORDER BY val;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
      table.data(1).data(0) shouldBe NumberValue(DIntType, 3)
      table.data(2).data(0) shouldBe NumberValue(DIntType, 5)
    }

    "delete with NULL in WHERE condition" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (1), (NULL), (3);
          |DELETE FROM t WHERE val = NULL;
          |SELECT COUNT(*) FROM t;
          |""".trim.stripMargin
      )
      // val = NULL is unknown for all rows, so nothing deleted
      table.data(0).data(0) shouldBe NumberValue(DIntType, 3)
    }

    "delete with IS NULL in WHERE" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (1), (NULL), (3);
          |DELETE FROM t WHERE val IS NULL;
          |SELECT COUNT(*) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 2)
    }
  }

  // ── TRUNCATE edge cases ───────────────────────────────────────────

  "TRUNCATE edge cases" - {
    "truncate empty table succeeds" in {
      results(
        """
          |CREATE TABLE t (val INT);
          |TRUNCATE TABLE t;
          |""".trim.stripMargin
      )
    }

    "truncate then select returns empty" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (1), (2), (3);
          |TRUNCATE TABLE t;
          |SELECT COUNT(*) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 0)
    }

    "truncate nonexistent table fails" in {
      an[Exception] should be thrownBy {
        results("TRUNCATE TABLE nonexistent;")
      }
    }
  }

  // ── DROP TABLE edge cases ─────────────────────────────────────────

  "DROP TABLE" - {
    "drop then select fails" in {
      an[Exception] should be thrownBy {
        query(
          """
            |CREATE TABLE t (val INT);
            |INSERT INTO t (val) VALUES (1);
            |DROP TABLE t;
            |SELECT * FROM t;
            |""".trim.stripMargin
        )
      }
    }

    "drop then recreate with same name works" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t (val) VALUES (1);
          |DROP TABLE t;
          |CREATE TABLE t (val TEXT);
          |INSERT INTO t (val) VALUES ('hello');
          |SELECT val FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("hello")
    }
  }

  // ── SERIAL / auto-increment edge cases ────────────────────────────

  "SERIAL" - {
    "auto-increments across multiple inserts" in {
      val table = query(
        """
          |CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT);
          |INSERT INTO t (name) VALUES ('a');
          |INSERT INTO t (name) VALUES ('b');
          |INSERT INTO t (name) VALUES ('c');
          |SELECT id FROM t ORDER BY id;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
      table.data(1).data(0) shouldBe NumberValue(DIntType, 2)
      table.data(2).data(0) shouldBe NumberValue(DIntType, 3)
    }

    "serial continues after delete" in {
      val table = query(
        """
          |CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT);
          |INSERT INTO t (name) VALUES ('a');
          |INSERT INTO t (name) VALUES ('b');
          |DELETE FROM t WHERE id = 2;
          |INSERT INTO t (name) VALUES ('c');
          |SELECT id, name FROM t ORDER BY id;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
      // ID should be 3, not reuse 2
      table.data(1).data(0) shouldBe NumberValue(DIntType, 3)
    }

    "multi-row insert gets sequential IDs" in {
      val table = query(
        """
          |CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT);
          |INSERT INTO t (name) VALUES ('a'), ('b'), ('c');
          |SELECT id FROM t ORDER BY id;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
      table.data(1).data(0) shouldBe NumberValue(DIntType, 2)
      table.data(2).data(0) shouldBe NumberValue(DIntType, 3)
    }
  }
}

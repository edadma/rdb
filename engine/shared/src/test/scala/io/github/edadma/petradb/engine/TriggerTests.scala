package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}
import io.github.edadma.cross_platform.{createTempFile, deleteFile}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TriggerTests extends AnyFreeSpec with Matchers with Testing {

  // ══════════════════════════════════════════════════════════════════
  // AFTER INSERT TRIGGER
  // ══════════════════════════════════════════════════════════════════

  "after insert trigger" - {
    "fires after INSERT and logs to audit table" in {
      val table = query(
        """
          |CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL);
          |CREATE TABLE audit (msg TEXT);
          |CREATE FUNCTION log_insert() RETURNS INT AS $$
          |BEGIN
          |  INSERT INTO audit VALUES ('row inserted');
          |  RETURN 0;
          |END $$ LANGUAGE plpgsql;
          |CREATE TRIGGER trg_log AFTER INSERT ON users FOR EACH ROW EXECUTE FUNCTION log_insert();
          |INSERT INTO users (name) VALUES ('Alice');
          |INSERT INTO users (name) VALUES ('Bob');
          |SELECT msg FROM audit;
          |""".stripMargin
      )
      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("row inserted")
    }

    "fires once per inserted row" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |CREATE TABLE counts (n INT);
          |INSERT INTO counts VALUES (0);
          |CREATE FUNCTION count_inserts() RETURNS INT AS $$
          |BEGIN
          |  UPDATE counts SET n = n + 1;
          |  RETURN 0;
          |END $$ LANGUAGE plpgsql;
          |CREATE TRIGGER trg_count AFTER INSERT ON t FOR EACH ROW EXECUTE FUNCTION count_inserts();
          |INSERT INTO t VALUES (1);
          |INSERT INTO t VALUES (2);
          |INSERT INTO t VALUES (3);
          |SELECT n FROM counts;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(3)
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // BEFORE INSERT TRIGGER — cancel
  // ══════════════════════════════════════════════════════════════════

  "before insert trigger" - {
    "returning NULL cancels the insert" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |CREATE FUNCTION block_insert() RETURNS INT AS $$
          |BEGIN
          |  RETURN NULL;
          |END $$ LANGUAGE plpgsql;
          |CREATE TRIGGER trg_block BEFORE INSERT ON t FOR EACH ROW EXECUTE FUNCTION block_insert();
          |INSERT INTO t VALUES (1);
          |INSERT INTO t VALUES (2);
          |SELECT COUNT(*) AS cnt FROM t;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(0)
    }

    "returning non-NULL allows the insert" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |CREATE FUNCTION allow_insert() RETURNS INT AS $$
          |BEGIN
          |  RETURN 1;
          |END $$ LANGUAGE plpgsql;
          |CREATE TRIGGER trg_allow BEFORE INSERT ON t FOR EACH ROW EXECUTE FUNCTION allow_insert();
          |INSERT INTO t VALUES (42);
          |SELECT val FROM t;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(42)
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // AFTER DELETE TRIGGER
  // ══════════════════════════════════════════════════════════════════

  "after delete trigger" - {
    "fires after DELETE" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, name TEXT);
          |CREATE TABLE audit (msg TEXT);
          |INSERT INTO t VALUES (1, 'Alice');
          |INSERT INTO t VALUES (2, 'Bob');
          |CREATE FUNCTION log_delete() RETURNS INT AS $$
          |BEGIN
          |  INSERT INTO audit VALUES ('row deleted');
          |  RETURN 0;
          |END $$ LANGUAGE plpgsql;
          |CREATE TRIGGER trg_log_del AFTER DELETE ON t FOR EACH ROW EXECUTE FUNCTION log_delete();
          |DELETE FROM t WHERE id = 1;
          |SELECT msg FROM audit;
          |""".stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("row deleted")
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // BEFORE DELETE TRIGGER — cancel
  // ══════════════════════════════════════════════════════════════════

  "before delete trigger" - {
    "returning NULL cancels the delete" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, protected BOOLEAN);
          |INSERT INTO t VALUES (1, true);
          |INSERT INTO t VALUES (2, false);
          |CREATE FUNCTION guard_delete() RETURNS INT AS $$
          |BEGIN
          |  RETURN NULL;
          |END $$ LANGUAGE plpgsql;
          |CREATE TRIGGER trg_guard BEFORE DELETE ON t FOR EACH ROW EXECUTE FUNCTION guard_delete();
          |DELETE FROM t WHERE id = 1;
          |SELECT COUNT(*) AS cnt FROM t;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(2)
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // MULTIPLE TRIGGERS
  // ══════════════════════════════════════════════════════════════════

  "multiple triggers" - {
    "multiple triggers on same event fire in order" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |CREATE TABLE log (step TEXT);
          |CREATE FUNCTION log_a() RETURNS INT AS $$
          |BEGIN INSERT INTO log VALUES ('A'); RETURN 0; END $$ LANGUAGE plpgsql;
          |CREATE FUNCTION log_b() RETURNS INT AS $$
          |BEGIN INSERT INTO log VALUES ('B'); RETURN 0; END $$ LANGUAGE plpgsql;
          |CREATE TRIGGER trg_a AFTER INSERT ON t FOR EACH ROW EXECUTE FUNCTION log_a();
          |CREATE TRIGGER trg_b AFTER INSERT ON t FOR EACH ROW EXECUTE FUNCTION log_b();
          |INSERT INTO t VALUES (1);
          |SELECT step FROM log;
          |""".stripMargin
      )
      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("A")
      table.data(1).data(0) shouldBe TextValue("B")
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // DROP TRIGGER
  // ══════════════════════════════════════════════════════════════════

  "drop trigger" - {
    "dropped trigger no longer fires" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |CREATE TABLE audit (msg TEXT);
          |CREATE FUNCTION log_it() RETURNS INT AS $$
          |BEGIN INSERT INTO audit VALUES ('fired'); RETURN 0; END $$ LANGUAGE plpgsql;
          |CREATE TRIGGER trg AFTER INSERT ON t FOR EACH ROW EXECUTE FUNCTION log_it();
          |INSERT INTO t VALUES (1);
          |DROP TRIGGER trg ON t;
          |INSERT INTO t VALUES (2);
          |SELECT COUNT(*) AS cnt FROM audit;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(1)
    }

    "DROP TRIGGER IF EXISTS on nonexistent succeeds" in {
      results(
        """
          |CREATE TABLE t (id INT);
          |DROP TRIGGER IF EXISTS nonexistent ON t;
          |""".stripMargin
      ).last shouldBe a[DropTriggerResult]
    }

    "DROP TRIGGER on nonexistent fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (id INT);
            |DROP TRIGGER nonexistent ON t;
            |""".stripMargin
        )
      }
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // TRIGGER WITH TG_OP
  // ══════════════════════════════════════════════════════════════════

  "tg_op variable" - {
    "provides operation name" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |CREATE TABLE audit (op TEXT);
          |CREATE FUNCTION log_op() RETURNS INT AS $$
          |BEGIN
          |  INSERT INTO audit VALUES (tg_op);
          |  RETURN 0;
          |END $$ LANGUAGE plpgsql;
          |CREATE TRIGGER trg_ins AFTER INSERT ON t FOR EACH ROW EXECUTE FUNCTION log_op();
          |CREATE TRIGGER trg_del AFTER DELETE ON t FOR EACH ROW EXECUTE FUNCTION log_op();
          |INSERT INTO t VALUES (1);
          |DELETE FROM t WHERE val = 1;
          |SELECT op FROM audit;
          |""".stripMargin
      )
      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("INSERT")
      table.data(1).data(0) shouldBe TextValue("DELETE")
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // PERSISTENCE
  // ══════════════════════════════════════════════════════════════════

  "persistence" - {
    "trigger survives close and reopen" in {
      import scala.compiletime.uninitialized
      val tmpFile = createTempFile("petradb_trigger_test_", ".db")
      deleteFile(tmpFile)

      try {
        // Create
        {
          val db = PersistentDB.create(tmpFile, 4096)
          given Session = db.connect()
          executeSQL(
            """
              |CREATE TABLE t (val INT);
              |CREATE TABLE audit (msg TEXT);
              |CREATE FUNCTION log_it() RETURNS INT AS $$
              |BEGIN INSERT INTO audit VALUES ('fired'); RETURN 0; END $$ LANGUAGE plpgsql;
              |CREATE TRIGGER trg AFTER INSERT ON t FOR EACH ROW EXECUTE FUNCTION log_it();
              |""".stripMargin
          )
          db.close()
        }

        // Reopen and test
        {
          val db = PersistentDB.open(tmpFile)
          given Session = db.connect()
          executeSQL("INSERT INTO t VALUES (1);")
          val r = executeSQL("SELECT msg FROM audit;").collect { case QueryResult(t) => t }.last
          r.data.length shouldBe 1
          r.data(0).data(0) shouldBe TextValue("fired")
          db.close()
        }
      } finally {
        try deleteFile(tmpFile) catch { case _: Exception => }
      }
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // NEGATIVE TESTS
  // ══════════════════════════════════════════════════════════════════

  "errors" - {
    "trigger on nonexistent table fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE FUNCTION f() RETURNS INT AS $$
            |BEGIN RETURN 0; END $$ LANGUAGE plpgsql;
            |CREATE TRIGGER trg AFTER INSERT ON nonexistent FOR EACH ROW EXECUTE FUNCTION f();
            |""".stripMargin
        )
      }
    }

    "trigger with nonexistent function fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (id INT);
            |CREATE TRIGGER trg AFTER INSERT ON t FOR EACH ROW EXECUTE FUNCTION nonexistent();
            |""".stripMargin
        )
      }
    }

    "duplicate trigger on same table fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (id INT);
            |CREATE FUNCTION f() RETURNS INT AS $$
            |BEGIN RETURN 0; END $$ LANGUAGE plpgsql;
            |CREATE TRIGGER trg AFTER INSERT ON t FOR EACH ROW EXECUTE FUNCTION f();
            |CREATE TRIGGER trg AFTER INSERT ON t FOR EACH ROW EXECUTE FUNCTION f();
            |""".stripMargin
        )
      }
    }
  }
}

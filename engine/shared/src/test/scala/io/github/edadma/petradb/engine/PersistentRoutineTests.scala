package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}
import io.github.edadma.cross_platform.{createTempFile, deleteFile}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterEach
import scala.compiletime.uninitialized

class PersistentRoutineTests extends AnyFreeSpec with Matchers with BeforeAndAfterEach {

  private var tmpFile: String = uninitialized

  override def beforeEach(): Unit =
    tmpFile = createTempFile("petradb_routine_test_", ".db")
    deleteFile(tmpFile)

  override def afterEach(): Unit =
    try deleteFile(tmpFile) catch { case _: Exception => }

  // ══════════════════════════════════════════════════════════════════
  // FUNCTION PERSISTENCE
  // ══════════════════════════════════════════════════════════════════

  "persistent functions" - {
    "function survives close and reopen" in {
      // Create DB with a function
      {
        val db = PersistentDB.create(tmpFile, 4096)
        given Session = db.connect()
        executeSQL(
          """
            |CREATE FUNCTION double_it(x INT) RETURNS INT AS $$
            |BEGIN
            |  RETURN x * 2;
            |END $$ LANGUAGE plpgsql;
            |""".stripMargin
        )
        db.close()
      }

      // Reopen and verify function works
      {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val results = executeSQL("SELECT double_it(21) AS val;")
        val table = results.collect { case QueryResult(t) => t }.last
        table.data(0).data(0).intValue shouldBe 42
        db.close()
      }
    }

    "complex function survives reopen" in {
      {
        val db = PersistentDB.create(tmpFile, 4096)
        given Session = db.connect()
        executeSQL(
          """
            |CREATE FUNCTION factorial(n INT) RETURNS INT AS $$
            |DECLARE
            |  result INT := 1;
            |  i INT := 1;
            |BEGIN
            |  WHILE i <= n LOOP
            |    result := result * i;
            |    i := i + 1;
            |  END LOOP;
            |  RETURN result;
            |END $$ LANGUAGE plpgsql;
            |""".stripMargin
        )
        db.close()
      }

      {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT factorial(5) AS val;").collect { case QueryResult(t) => t }.last
        table.data(0).data(0).intValue shouldBe 120
        db.close()
      }
    }

    "function with table data survives reopen" in {
      {
        val db = PersistentDB.create(tmpFile, 4096)
        given Session = db.connect()
        executeSQL(
          """
            |CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL);
            |INSERT INTO users (name) VALUES ('Alice');
            |INSERT INTO users (name) VALUES ('Bob');
            |CREATE FUNCTION user_count() RETURNS INT AS $$
            |DECLARE
            |  cnt INT;
            |BEGIN
            |  cnt := (SELECT COUNT(*) FROM users);
            |  RETURN cnt;
            |END $$ LANGUAGE plpgsql;
            |""".stripMargin
        )
        db.close()
      }

      {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT user_count() AS cnt;").collect { case QueryResult(t) => t }.last
        table.data(0).data(0).intValue shouldBe 2
        db.close()
      }
    }

    "dropped function does not survive reopen" in {
      {
        val db = PersistentDB.create(tmpFile, 4096)
        given Session = db.connect()
        executeSQL(
          """
            |CREATE FUNCTION f(x INT) RETURNS INT AS $$
            |BEGIN RETURN x; END $$ LANGUAGE plpgsql;
            |DROP FUNCTION f;
            |""".stripMargin
        )
        db.close()
      }

      {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        an[Exception] should be thrownBy {
          executeSQL("SELECT f(1);")
        }
        db.close()
      }
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // PROCEDURE PERSISTENCE
  // ══════════════════════════════════════════════════════════════════

  "persistent procedures" - {
    "procedure survives close and reopen" in {
      {
        val db = PersistentDB.create(tmpFile, 4096)
        given Session = db.connect()
        executeSQL(
          """
            |CREATE TABLE t (val INT);
            |CREATE PROCEDURE insert_val(v INT) AS $$
            |BEGIN
            |  INSERT INTO t VALUES (v);
            |END $$ LANGUAGE plpgsql;
            |""".stripMargin
        )
        db.close()
      }

      {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        executeSQL("CALL insert_val(42);")
        val table = executeSQL("SELECT val FROM t;").collect { case QueryResult(t) => t }.last
        table.data(0).data(0) shouldBe NumberValue(42)
        db.close()
      }
    }

    "procedure with loop survives reopen" in {
      {
        val db = PersistentDB.create(tmpFile, 4096)
        given Session = db.connect()
        executeSQL(
          """
            |CREATE TABLE t (val INT);
            |CREATE PROCEDURE fill(n INT) AS $$
            |DECLARE
            |  i INT;
            |BEGIN
            |  FOR i IN 1..n LOOP
            |    INSERT INTO t VALUES (i);
            |  END LOOP;
            |END $$ LANGUAGE plpgsql;
            |""".stripMargin
        )
        db.close()
      }

      {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        executeSQL("CALL fill(5);")
        val table = executeSQL("SELECT val FROM t ORDER BY val;").collect { case QueryResult(t) => t }.last
        table.data.map(_.data(0).intValue) shouldBe Vector(1, 2, 3, 4, 5)
        db.close()
      }
    }

    "dropped procedure does not survive reopen" in {
      {
        val db = PersistentDB.create(tmpFile, 4096)
        given Session = db.connect()
        executeSQL(
          """
            |CREATE TABLE t (val INT);
            |CREATE PROCEDURE p(x INT) AS $$
            |BEGIN INSERT INTO t VALUES (x); END $$ LANGUAGE plpgsql;
            |DROP PROCEDURE p;
            |""".stripMargin
        )
        db.close()
      }

      {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        an[Exception] should be thrownBy {
          executeSQL("CALL p(1);")
        }
        db.close()
      }
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // OR REPLACE PERSISTENCE
  // ══════════════════════════════════════════════════════════════════

  "replace persistence" - {
    "OR REPLACE function persists the replacement" in {
      {
        val db = PersistentDB.create(tmpFile, 4096)
        given Session = db.connect()
        executeSQL(
          """
            |CREATE FUNCTION f(x INT) RETURNS INT AS $$
            |BEGIN RETURN x; END $$ LANGUAGE plpgsql;
            |CREATE OR REPLACE FUNCTION f(x INT) RETURNS INT AS $$
            |BEGIN RETURN x * 10; END $$ LANGUAGE plpgsql;
            |""".stripMargin
        )
        db.close()
      }

      {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT f(5) AS val;").collect { case QueryResult(t) => t }.last
        table.data(0).data(0) shouldBe NumberValue(50)
        db.close()
      }
    }
  }
}

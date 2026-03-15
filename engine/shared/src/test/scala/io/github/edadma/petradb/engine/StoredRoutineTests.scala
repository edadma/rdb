package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class StoredRoutineTests extends AnyFreeSpec with Matchers with Testing {

  // ══════════════════════════════════════════════════════════════════
  // CREATE FUNCTION
  // ══════════════════════════════════════════════════════════════════

  "create function" - {
    "simple function with RETURN" in {
      val table = query(
        """
          |CREATE FUNCTION add_nums(a INT, b INT) RETURNS INT AS $$
          |BEGIN
          |  RETURN a + b;
          |END $$ LANGUAGE plpgsql;
          |SELECT add_nums(3, 4) AS result;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(7)
    }

    "function with DECLARE" in {
      val table = query(
        """
          |CREATE FUNCTION double_it(x INT) RETURNS INT AS $$
          |DECLARE
          |  result INT;
          |BEGIN
          |  result := x * 2;
          |  RETURN result;
          |END $$ LANGUAGE plpgsql;
          |SELECT double_it(21) AS val;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(42)
    }

    "function with conditional logic" in {
      val table = query(
        """
          |CREATE FUNCTION classify(n INT) RETURNS TEXT AS $$
          |BEGIN
          |  IF n > 100 THEN
          |    RETURN 'big';
          |  ELSIF n > 10 THEN
          |    RETURN 'medium';
          |  ELSE
          |    RETURN 'small';
          |  END IF;
          |END $$ LANGUAGE plpgsql;
          |SELECT classify(5) AS c1, classify(50) AS c2, classify(500) AS c3;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("small")
      table.data(0).data(1) shouldBe TextValue("medium")
      table.data(0).data(2) shouldBe TextValue("big")
    }

    "function with loop" in {
      val table = query(
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
          |SELECT factorial(5) AS val;
          |""".stripMargin
      )
      table.data(0).data(0).intValue shouldBe 120
    }

    "function used in WHERE clause" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, name TEXT);
          |INSERT INTO t VALUES (1, 'Alice');
          |INSERT INTO t VALUES (2, 'Bob');
          |INSERT INTO t VALUES (3, 'Carol');
          |CREATE FUNCTION is_even(n INT) RETURNS BOOLEAN AS $$
          |BEGIN
          |  RETURN n % 2 = 0;
          |END $$ LANGUAGE plpgsql;
          |SELECT name FROM t WHERE is_even(id);
          |""".stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("Bob")
    }

    "function with text parameter" in {
      val table = query(
        """
          |CREATE FUNCTION greet(name TEXT) RETURNS TEXT AS $$
          |BEGIN
          |  RETURN 'Hello, ' || name || '!';
          |END $$ LANGUAGE plpgsql;
          |SELECT greet('World') AS greeting;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("Hello, World!")
    }

    "function called multiple times in one query" in {
      val table = query(
        """
          |CREATE FUNCTION square(n INT) RETURNS INT AS $$
          |BEGIN
          |  RETURN n * n;
          |END $$ LANGUAGE plpgsql;
          |SELECT square(3) AS a, square(4) AS b, square(5) AS c;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(9)
      table.data(0).data(1) shouldBe NumberValue(16)
      table.data(0).data(2) shouldBe NumberValue(25)
    }

    "OR REPLACE overwrites existing function" in {
      val table = query(
        """
          |CREATE FUNCTION f(x INT) RETURNS INT AS $$
          |BEGIN RETURN x; END $$ LANGUAGE plpgsql;
          |CREATE OR REPLACE FUNCTION f(x INT) RETURNS INT AS $$
          |BEGIN RETURN x * 10; END $$ LANGUAGE plpgsql;
          |SELECT f(5) AS val;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(50)
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // CREATE PROCEDURE + CALL
  // ══════════════════════════════════════════════════════════════════

  "create procedure" - {
    "simple procedure with INSERT" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |CREATE PROCEDURE insert_val(v INT) AS $$
          |BEGIN
          |  INSERT INTO t VALUES (v);
          |END $$ LANGUAGE plpgsql;
          |CALL insert_val(42);
          |SELECT val FROM t;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(42)
    }

    "procedure with multiple parameters" in {
      val table = query(
        """
          |CREATE TABLE t (name TEXT, age INT);
          |CREATE PROCEDURE add_person(n TEXT, a INT) AS $$
          |BEGIN
          |  INSERT INTO t VALUES (n, a);
          |END $$ LANGUAGE plpgsql;
          |CALL add_person('Alice', 30);
          |CALL add_person('Bob', 25);
          |SELECT name, age FROM t ORDER BY name;
          |""".stripMargin
      )
      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1) shouldBe NumberValue(30)
      table.data(1).data(0) shouldBe TextValue("Bob")
    }

    "procedure with loop" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |CREATE PROCEDURE insert_range(lo INT, hi INT) AS $$
          |DECLARE
          |  i INT;
          |BEGIN
          |  FOR i IN lo..hi LOOP
          |    INSERT INTO t VALUES (i);
          |  END LOOP;
          |END $$ LANGUAGE plpgsql;
          |CALL insert_range(1, 5);
          |SELECT val FROM t ORDER BY val;
          |""".stripMargin
      )
      table.data.map(_.data(0).intValue) shouldBe Vector(1, 2, 3, 4, 5)
    }

    "procedure with conditional logic" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT);
          |CREATE PROCEDURE categorize(n INT) AS $$
          |BEGIN
          |  IF n > 0 THEN
          |    INSERT INTO t VALUES ('positive');
          |  ELSIF n = 0 THEN
          |    INSERT INTO t VALUES ('zero');
          |  ELSE
          |    INSERT INTO t VALUES ('negative');
          |  END IF;
          |END $$ LANGUAGE plpgsql;
          |CALL categorize(5);
          |CALL categorize(0);
          |CALL categorize(-3);
          |SELECT val FROM t;
          |""".stripMargin
      )
      table.data.map(_.data(0).string) shouldBe Vector("positive", "zero", "negative")
    }

    "OR REPLACE overwrites existing procedure" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |CREATE PROCEDURE p(x INT) AS $$
          |BEGIN INSERT INTO t VALUES (x); END $$ LANGUAGE plpgsql;
          |CREATE OR REPLACE PROCEDURE p(x INT) AS $$
          |BEGIN INSERT INTO t VALUES (x * 100); END $$ LANGUAGE plpgsql;
          |CALL p(5);
          |SELECT val FROM t;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(500)
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // DROP FUNCTION / DROP PROCEDURE
  // ══════════════════════════════════════════════════════════════════

  "drop" - {
    "DROP FUNCTION removes function" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE FUNCTION f(x INT) RETURNS INT AS $$
            |BEGIN RETURN x; END $$ LANGUAGE plpgsql;
            |DROP FUNCTION f;
            |SELECT f(1);
            |""".stripMargin
        )
      }
    }

    "DROP FUNCTION IF EXISTS on nonexistent succeeds" in {
      results("DROP FUNCTION IF EXISTS nonexistent;").last shouldBe a[DropFunctionResult]
    }

    "DROP FUNCTION on nonexistent fails" in {
      an[Exception] should be thrownBy {
        results("DROP FUNCTION nonexistent;")
      }
    }

    "DROP PROCEDURE removes procedure" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE PROCEDURE p(x INT) AS $$
            |BEGIN NULL; END $$ LANGUAGE plpgsql;
            |DROP PROCEDURE p;
            |CALL p(1);
            |""".stripMargin
        )
      }
    }

    "DROP PROCEDURE IF EXISTS on nonexistent succeeds" in {
      results("DROP PROCEDURE IF EXISTS nonexistent;").last shouldBe a[DropProcedureResult]
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // FUNCTION CALLING FUNCTION
  // ══════════════════════════════════════════════════════════════════

  "composition" - {
    "function calls another function" in {
      val table = query(
        """
          |CREATE FUNCTION double_it(x INT) RETURNS INT AS $$
          |BEGIN RETURN x * 2; END $$ LANGUAGE plpgsql;
          |CREATE FUNCTION quadruple(x INT) RETURNS INT AS $$
          |BEGIN RETURN double_it(double_it(x)); END $$ LANGUAGE plpgsql;
          |SELECT quadruple(5) AS val;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(20)
    }

    "procedure calls function" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |CREATE FUNCTION triple(x INT) RETURNS INT AS $$
          |BEGIN RETURN x * 3; END $$ LANGUAGE plpgsql;
          |CREATE PROCEDURE store_triple(x INT) AS $$
          |BEGIN
          |  INSERT INTO t VALUES (triple(x));
          |END $$ LANGUAGE plpgsql;
          |CALL store_triple(7);
          |SELECT val FROM t;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(21)
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // FUNCTION WITH SIDE EFFECTS
  // ══════════════════════════════════════════════════════════════════

  "side effects" - {
    "function can INSERT" in {
      val table = query(
        """
          |CREATE TABLE audit_log (msg TEXT);
          |CREATE FUNCTION log_and_return(x INT) RETURNS INT AS $$
          |BEGIN
          |  INSERT INTO audit_log VALUES ('called with ' || x::TEXT);
          |  RETURN x;
          |END $$ LANGUAGE plpgsql;
          |SELECT log_and_return(42) AS val;
          |SELECT msg FROM audit_log;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("called with 42")
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // NEGATIVE TESTS
  // ══════════════════════════════════════════════════════════════════

  // ══════════════════════════════════════════════════════════════════
  // SOURCE ROUNDTRIP
  // ══════════════════════════════════════════════════════════════════

  "source roundtrip" - {
    "function source can be re-executed" in {
      val session = setupSession(
        """
          |CREATE FUNCTION add_nums(a INT, b INT) RETURNS INT AS $$
          |BEGIN
          |  RETURN a + b;
          |END $$ LANGUAGE plpgsql;
          |""".stripMargin
      )
      given Session = session

      // Get the stored source
      val sf = session.db.storedFunctions("add_nums")
      val source = sf.source

      // Drop the function and re-create from source
      executeSQL("DROP FUNCTION add_nums;")
      executeSQL(source + ";")

      // Verify it works
      val table = executeSQL("SELECT add_nums(10, 20) AS val;").collect { case QueryResult(t) => t }.last
      table.data(0).data(0) shouldBe NumberValue(30)
    }

    "procedure source can be re-executed" in {
      val session = setupSession(
        """
          |CREATE TABLE t (val INT);
          |CREATE PROCEDURE insert_doubled(x INT) AS $$
          |BEGIN
          |  INSERT INTO t VALUES (x * 2);
          |END $$ LANGUAGE plpgsql;
          |""".stripMargin
      )
      given Session = session

      val sp = session.db.storedProcedures("insert_doubled")
      val source = sp.source
      executeSQL("DROP PROCEDURE insert_doubled;")
      executeSQL(source + ";")

      executeSQL("CALL insert_doubled(21);")
      val table = executeSQL("SELECT val FROM t;").collect { case QueryResult(t) => t }.last
      table.data(0).data(0) shouldBe NumberValue(42)
    }

    "complex function source roundtrips" in {
      val session = setupSession(
        """
          |CREATE FUNCTION classify(n INT) RETURNS TEXT AS $$
          |DECLARE
          |  label TEXT;
          |BEGIN
          |  IF n > 100 THEN
          |    label := 'big';
          |  ELSIF n > 10 THEN
          |    label := 'medium';
          |  ELSE
          |    label := 'small';
          |  END IF;
          |  RETURN label;
          |END $$ LANGUAGE plpgsql;
          |""".stripMargin
      )
      given Session = session

      val source = session.db.storedFunctions("classify").source
      executeSQL("DROP FUNCTION classify;")
      executeSQL(source + ";")

      val table = executeSQL("SELECT classify(5) AS c1, classify(50) AS c2, classify(500) AS c3;")
        .collect { case QueryResult(t) => t }.last
      table.data(0).data(0) shouldBe TextValue("small")
      table.data(0).data(1) shouldBe TextValue("medium")
      table.data(0).data(2) shouldBe TextValue("big")
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // NEGATIVE TESTS
  // ══════════════════════════════════════════════════════════════════

  "errors" - {
    "duplicate function without OR REPLACE fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE FUNCTION f(x INT) RETURNS INT AS $$
            |BEGIN RETURN x; END $$ LANGUAGE plpgsql;
            |CREATE FUNCTION f(x INT) RETURNS INT AS $$
            |BEGIN RETURN x; END $$ LANGUAGE plpgsql;
            |""".stripMargin
        )
      }
    }

    "duplicate procedure without OR REPLACE fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE PROCEDURE p(x INT) AS $$
            |BEGIN NULL; END $$ LANGUAGE plpgsql;
            |CREATE PROCEDURE p(x INT) AS $$
            |BEGIN NULL; END $$ LANGUAGE plpgsql;
            |""".stripMargin
        )
      }
    }

    "CALL nonexistent procedure fails" in {
      an[Exception] should be thrownBy {
        results("CALL nonexistent(1);")
      }
    }

    "wrong argument count to function fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE FUNCTION f(x INT) RETURNS INT AS $$
            |BEGIN RETURN x; END $$ LANGUAGE plpgsql;
            |SELECT f(1, 2);
            |""".stripMargin
        )
      }
    }

    "wrong argument count to procedure fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE PROCEDURE p(x INT) AS $$
            |BEGIN NULL; END $$ LANGUAGE plpgsql;
            |CALL p(1, 2);
            |""".stripMargin
        )
      }
    }
  }
}

package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class PLBlockTests extends AnyFreeSpec with Matchers with Testing {

  // ══════════════════════════════════════════════════════════════════
  // STATEMENT SEQUENCES
  // ══════════════════════════════════════════════════════════════════

  "statement sequences" - {
    "multiple SQL statements in a block" in {
      val table = query(
        """
          |CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT);
          |DO $$
          |BEGIN
          |  INSERT INTO t (name) VALUES ('Alice');
          |  INSERT INTO t (name) VALUES ('Bob');
          |  INSERT INTO t (name) VALUES ('Carol');
          |END $$;
          |SELECT name FROM t ORDER BY id;
          |""".stripMargin
      )
      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(1).data(0) shouldBe TextValue("Bob")
      table.data(2).data(0) shouldBe TextValue("Carol")
    }

    "empty block" in {
      results(
        """
          |DO $$
          |BEGIN
          |END $$;
          |""".stripMargin
      ).last shouldBe DoBlockResult
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // DECLARE AND VARIABLES
  // ══════════════════════════════════════════════════════════════════

  "variables" - {
    "declare and use variable in INSERT" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |DO $$
          |DECLARE
          |  x INT := 42;
          |BEGIN
          |  INSERT INTO t (val) VALUES (x);
          |END $$;
          |SELECT val FROM t;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(42)
    }

    "declare with default NULL" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |DO $$
          |DECLARE
          |  x INT;
          |BEGIN
          |  INSERT INTO t (val) VALUES (x);
          |END $$;
          |SELECT val FROM t;
          |""".stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }

    "variable assignment" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |DO $$
          |DECLARE
          |  x INT := 10;
          |BEGIN
          |  x := x + 32;
          |  INSERT INTO t (val) VALUES (x);
          |END $$;
          |SELECT val FROM t;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(42)
    }

    "multiple variables" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b TEXT);
          |DO $$
          |DECLARE
          |  num INT := 5;
          |  txt TEXT := 'hello';
          |BEGIN
          |  INSERT INTO t (a, b) VALUES (num, txt);
          |END $$;
          |SELECT a, b FROM t;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(5)
      table.data(0).data(1) shouldBe TextValue("hello")
    }

    "variable used in WHERE clause" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, name TEXT);
          |INSERT INTO t VALUES (1, 'Alice');
          |INSERT INTO t VALUES (2, 'Bob');
          |DO $$
          |DECLARE
          |  target_id INT := 1;
          |BEGIN
          |  DELETE FROM t WHERE id = target_id;
          |END $$;
          |SELECT name FROM t;
          |""".stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("Bob")
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // IF / ELSIF / ELSE
  // ══════════════════════════════════════════════════════════════════

  "if statements" - {
    "IF true THEN" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT);
          |DO $$
          |DECLARE
          |  x INT := 10;
          |BEGIN
          |  IF x > 5 THEN
          |    INSERT INTO t VALUES ('big');
          |  END IF;
          |END $$;
          |SELECT val FROM t;
          |""".stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("big")
    }

    "IF false THEN — body skipped" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT);
          |DO $$
          |DECLARE
          |  x INT := 3;
          |BEGIN
          |  IF x > 5 THEN
          |    INSERT INTO t VALUES ('big');
          |  END IF;
          |END $$;
          |SELECT val FROM t;
          |""".stripMargin
      )
      table.data.length shouldBe 0
    }

    "IF ... ELSE" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT);
          |DO $$
          |DECLARE
          |  x INT := 3;
          |BEGIN
          |  IF x > 5 THEN
          |    INSERT INTO t VALUES ('big');
          |  ELSE
          |    INSERT INTO t VALUES ('small');
          |  END IF;
          |END $$;
          |SELECT val FROM t;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("small")
    }

    "IF ... ELSIF ... ELSE" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT);
          |DO $$
          |DECLARE
          |  x INT := 5;
          |BEGIN
          |  IF x > 10 THEN
          |    INSERT INTO t VALUES ('big');
          |  ELSIF x > 3 THEN
          |    INSERT INTO t VALUES ('medium');
          |  ELSE
          |    INSERT INTO t VALUES ('small');
          |  END IF;
          |END $$;
          |SELECT val FROM t;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("medium")
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // WHILE LOOP
  // ══════════════════════════════════════════════════════════════════

  "while loop" - {
    "basic counting loop" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |DO $$
          |DECLARE
          |  i INT := 1;
          |BEGIN
          |  WHILE i <= 5 LOOP
          |    INSERT INTO t VALUES (i);
          |    i := i + 1;
          |  END LOOP;
          |END $$;
          |SELECT val FROM t ORDER BY val;
          |""".stripMargin
      )
      table.data.length shouldBe 5
      table.data.map(_.data(0).intValue) shouldBe Vector(1, 2, 3, 4, 5)
    }

    "while false — body never executes" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |DO $$
          |DECLARE
          |  i INT := 10;
          |BEGIN
          |  WHILE i < 5 LOOP
          |    INSERT INTO t VALUES (i);
          |    i := i + 1;
          |  END LOOP;
          |END $$;
          |SELECT val FROM t;
          |""".stripMargin
      )
      table.data.length shouldBe 0
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // FOR RANGE LOOP
  // ══════════════════════════════════════════════════════════════════

  "for range loop" - {
    "basic range" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |DO $$
          |DECLARE
          |  i INT;
          |BEGIN
          |  FOR i IN 1..5 LOOP
          |    INSERT INTO t VALUES (i);
          |  END LOOP;
          |END $$;
          |SELECT val FROM t ORDER BY val;
          |""".stripMargin
      )
      table.data.length shouldBe 5
      table.data.map(_.data(0).intValue) shouldBe Vector(1, 2, 3, 4, 5)
    }

    "range with expressions" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |DO $$
          |DECLARE
          |  i INT;
          |  n INT := 3;
          |BEGIN
          |  FOR i IN 1..n LOOP
          |    INSERT INTO t VALUES (i * 10);
          |  END LOOP;
          |END $$;
          |SELECT val FROM t ORDER BY val;
          |""".stripMargin
      )
      table.data.map(_.data(0).intValue) shouldBe Vector(10, 20, 30)
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // FOR QUERY LOOP
  // ══════════════════════════════════════════════════════════════════

  "for query loop" - {
    "iterate over query results" in {
      val table = query(
        """
          |CREATE TABLE src (id INT, name TEXT);
          |CREATE TABLE dst (greeting TEXT);
          |INSERT INTO src VALUES (1, 'Alice');
          |INSERT INTO src VALUES (2, 'Bob');
          |DO $$
          |DECLARE
          |  n TEXT;
          |BEGIN
          |  FOR n IN SELECT name FROM src ORDER BY id LOOP
          |    INSERT INTO dst VALUES (n);
          |  END LOOP;
          |END $$;
          |SELECT greeting FROM dst ORDER BY greeting;
          |""".stripMargin
      )
      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(1).data(0) shouldBe TextValue("Bob")
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // RETURN
  // ══════════════════════════════════════════════════════════════════

  "return" - {
    "RETURN exits block early" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |DO $$
          |BEGIN
          |  INSERT INTO t VALUES (1);
          |  RETURN;
          |  INSERT INTO t VALUES (2);
          |END $$;
          |SELECT val FROM t;
          |""".stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(1)
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // RAISE
  // ══════════════════════════════════════════════════════════════════

  "raise" - {
    "RAISE EXCEPTION throws error" in {
      an[RaiseException] should be thrownBy {
        results(
          """
            |DO $$
            |BEGIN
            |  RAISE EXCEPTION 'something went wrong';
            |END $$;
            |""".stripMargin
        )
      }
    }

    "RAISE EXCEPTION with format args" in {
      val e = the[RaiseException] thrownBy {
        results(
          """
            |DO $$
            |DECLARE
            |  x INT := 42;
            |BEGIN
            |  RAISE EXCEPTION 'value is %', x;
            |END $$;
            |""".stripMargin
        )
      }
      e.getMessage shouldBe "value is 42"
    }

    "RAISE EXCEPTION stops execution" in {
      val session = setupSession(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t VALUES (1);
          |""".stripMargin
      )
      given Session = session
      an[RaiseException] should be thrownBy {
        executeSQL(
          """
            |DO $$
            |BEGIN
            |  RAISE EXCEPTION 'stop';
            |  DELETE FROM t;
            |END $$;
            |""".stripMargin
        )
      }
      // The DELETE should not have executed
      val rows = executeSQL("SELECT COUNT(*) AS cnt FROM t;").collect { case QueryResult(t) => t }.last
      rows.data(0).data(0).intValue shouldBe 1
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // PERFORM
  // ══════════════════════════════════════════════════════════════════

  "perform" - {
    "PERFORM executes query and discards result" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |INSERT INTO t VALUES (1);
          |INSERT INTO t VALUES (2);
          |DO $$
          |BEGIN
          |  PERFORM SELECT * FROM t;
          |END $$;
          |SELECT COUNT(*) AS cnt FROM t;
          |""".stripMargin
      )
      table.data(0).data(0).intValue shouldBe 2
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // EXCEPTION HANDLING
  // ══════════════════════════════════════════════════════════════════

  "exception handling" - {
    "catches duplicate_object" in {
      results(
        """
          |CREATE TABLE t (id INT);
          |DO $$
          |BEGIN
          |  CREATE TABLE t (id INT);
          |EXCEPTION
          |  WHEN duplicate_object THEN NULL;
          |END $$;
          |""".stripMargin
      ).last shouldBe DoBlockResult
    }

    "catches others" in {
      results(
        """
          |DO $$
          |BEGIN
          |  CREATE TABLE nonexistent_ref (id INT REFERENCES no_such_table(id));
          |EXCEPTION
          |  WHEN others THEN NULL;
          |END $$;
          |""".stripMargin
      ).last shouldBe DoBlockResult
    }

    "uncaught exception propagates" in {
      an[Exception] should be thrownBy {
        results(
          """
            |DO $$
            |BEGIN
            |  CREATE TABLE t (id INT);
            |  CREATE TABLE t (id INT);
            |END $$;
            |""".stripMargin
        )
      }
    }

    "exception handler body executes" in {
      val table = query(
        """
          |CREATE TABLE t (val TEXT);
          |CREATE TABLE t2 (id INT);
          |DO $$
          |BEGIN
          |  CREATE TABLE t2 (id INT);
          |EXCEPTION
          |  WHEN duplicate_object THEN
          |    INSERT INTO t VALUES ('caught');
          |END $$;
          |SELECT val FROM t;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("caught")
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // COMBINED PATTERNS
  // ══════════════════════════════════════════════════════════════════

  "combined" - {
    "loop with conditional insert" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |DO $$
          |DECLARE
          |  i INT;
          |BEGIN
          |  FOR i IN 1..10 LOOP
          |    IF i % 2 = 0 THEN
          |      INSERT INTO t VALUES (i);
          |    END IF;
          |  END LOOP;
          |END $$;
          |SELECT val FROM t ORDER BY val;
          |""".stripMargin
      )
      table.data.map(_.data(0).intValue) shouldBe Vector(2, 4, 6, 8, 10)
    }

    "accumulate sum in variable" in {
      val table = query(
        """
          |CREATE TABLE t (val INT);
          |DO $$
          |DECLARE
          |  i INT;
          |  total INT := 0;
          |BEGIN
          |  FOR i IN 1..5 LOOP
          |    total := total + i;
          |  END LOOP;
          |  INSERT INTO t VALUES (total);
          |END $$;
          |SELECT val FROM t;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(15)
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // NEGATIVE TESTS
  // ══════════════════════════════════════════════════════════════════

  "errors" - {
    "undeclared variable fails" in {
      an[Exception] should be thrownBy {
        results(
          """
            |CREATE TABLE t (val INT);
            |DO $$
            |BEGIN
            |  INSERT INTO t VALUES (undeclared_var);
            |END $$;
            |""".stripMargin
        )
      }
    }

    "parse error in block" in {
      an[Exception] should be thrownBy {
        results(
          """
            |DO $$
            |BEGIN
            |  INVALID SYNTAX HERE;
            |END $$;
            |""".stripMargin
        )
      }
    }
  }
}

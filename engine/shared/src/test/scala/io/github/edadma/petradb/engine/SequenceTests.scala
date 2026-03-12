package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SequenceTests extends AnyFreeSpec with Matchers:

  private def query(sql: String): TableValue =
    given Session = new MemoryDB().connect()
    executeSQL(sql).collect { case QueryResult(t) => t }.last

  private def results(sql: String): Seq[Result] =
    given Session = new MemoryDB().connect()
    executeSQL(sql)

  private def test(sql: String): String =
    given Session = new MemoryDB().connect()
    try executeSQL(sql).toString
    catch case e: RuntimeException => e.getMessage

  "CREATE SEQUENCE" - {
    "basic creation" in {
      val res = results("CREATE SEQUENCE my_seq;")
      res.last shouldBe a[CreateSequenceResult]
    }

    "with options" in {
      val res = results("CREATE SEQUENCE my_seq INCREMENT BY 5 START WITH 100 MINVALUE 1 MAXVALUE 1000;")
      res.last shouldBe a[CreateSequenceResult]
    }

    "IF NOT EXISTS" in {
      val res = results(
        """CREATE SEQUENCE my_seq;
          |CREATE SEQUENCE IF NOT EXISTS my_seq;""".stripMargin
      )
      // Should not error on second create
      res.length shouldBe 2
    }

    "duplicate name fails" in {
      an[Exception] should be thrownBy {
        results(
          """CREATE SEQUENCE my_seq;
            |CREATE SEQUENCE my_seq;""".stripMargin
        )
      }
    }
  }

  "DROP SEQUENCE" - {
    "basic drop" in {
      val res = results(
        """CREATE SEQUENCE my_seq;
          |DROP SEQUENCE my_seq;""".stripMargin
      )
      res.last shouldBe a[DropSequenceResult]
    }

    "IF EXISTS on non-existent" in {
      val res = results("DROP SEQUENCE IF EXISTS no_such_seq;")
      res.last shouldBe a[DropSequenceResult]
    }

    "drop non-existent fails" in {
      an[Exception] should be thrownBy {
        results("DROP SEQUENCE no_such_seq;")
      }
    }
  }

  "NEXTVAL" - {
    "returns sequential values" in {
      val table = query(
        """CREATE SEQUENCE my_seq;
          |SELECT nextval('my_seq') AS v;""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(1)
    }

    "increments on each call" in {
      val table = query(
        """CREATE SEQUENCE my_seq;
          |SELECT nextval('my_seq'), nextval('my_seq'), nextval('my_seq');""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(0).data(1) shouldBe NumberValue(2)
      table.data(0).data(2) shouldBe NumberValue(3)
    }

    "respects INCREMENT BY" in {
      val table = query(
        """CREATE SEQUENCE my_seq INCREMENT BY 10 START WITH 5;
          |SELECT nextval('my_seq'), nextval('my_seq');""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(5)
      table.data(0).data(1) shouldBe NumberValue(15)
    }

    "respects START WITH" in {
      val table = query(
        """CREATE SEQUENCE my_seq START WITH 42;
          |SELECT nextval('my_seq');""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(42)
    }
  }

  "CURRVAL" - {
    "returns last value after nextval" in {
      val table = query(
        """CREATE SEQUENCE my_seq;
          |SELECT nextval('my_seq');
          |SELECT currval('my_seq');""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(1)
    }

    "errors if nextval not called" in {
      an[Exception] should be thrownBy {
        results(
          """CREATE SEQUENCE my_seq;
            |SELECT currval('my_seq');""".stripMargin
        )
      }
    }
  }

  "SETVAL" - {
    "sets current value" in {
      val table = query(
        """CREATE SEQUENCE my_seq;
          |SELECT setval('my_seq', 50);
          |SELECT nextval('my_seq');""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(51)
    }

    "with is_called=false" in {
      val table = query(
        """CREATE SEQUENCE my_seq;
          |SELECT setval('my_seq', 50, false);
          |SELECT nextval('my_seq');""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(50)
    }
  }

  "LASTVAL" - {
    "returns last value from any sequence" in {
      val table = query(
        """CREATE SEQUENCE seq_a;
          |CREATE SEQUENCE seq_b;
          |SELECT nextval('seq_a');
          |SELECT nextval('seq_b');
          |SELECT lastval();""".stripMargin
      )
      // Last sequence used was seq_b
      table.data(0).data(0) shouldBe NumberValue(1)
    }

    "errors if no sequence used" in {
      an[Exception] should be thrownBy {
        results("SELECT lastval();")
      }
    }
  }

  "SERIAL creates backing sequence" - {
    "SERIAL column auto-increments via sequence" in {
      val table = query(
        """CREATE TABLE t (id SERIAL, name TEXT);
          |INSERT INTO t (name) VALUES ('a'), ('b'), ('c');
          |SELECT id FROM t ORDER BY id;""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(1).data(0) shouldBe NumberValue(2)
      table.data(2).data(0) shouldBe NumberValue(3)
    }

    "nextval works on backing sequence" in {
      val table = query(
        """CREATE TABLE t (id SERIAL, name TEXT);
          |INSERT INTO t (name) VALUES ('a');
          |SELECT nextval('t_id_seq');""".stripMargin
      )
      // After insert of 1 row, nextval should return 2
      table.data(0).data(0) shouldBe NumberValue(2)
    }

    "currval works on backing sequence" in {
      val table = query(
        """CREATE TABLE t (id SERIAL, name TEXT);
          |INSERT INTO t (name) VALUES ('a');
          |SELECT currval('t_id_seq');""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(1)
    }

    "DROP TABLE cascades to drop owned sequences" in {
      an[Exception] should be thrownBy {
        results(
          """CREATE TABLE t (id SERIAL, name TEXT);
            |DROP TABLE t;
            |SELECT nextval('t_id_seq');""".stripMargin
        )
      }
    }

    "TRUNCATE resets serial sequence" in {
      val table = query(
        """CREATE TABLE t (id SERIAL, name TEXT);
          |INSERT INTO t (name) VALUES ('a'), ('b'), ('c');
          |TRUNCATE TABLE t;
          |INSERT INTO t (name) VALUES ('x');
          |SELECT id FROM t;""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(1)
    }
  }

  "SHOW SEQUENCES" - {
    "lists all sequences" in {
      val table = query(
        """CREATE SEQUENCE seq_a;
          |CREATE SEQUENCE seq_b;
          |SHOW SEQUENCES;""".stripMargin
      )
      table.data.length shouldBe 2
    }

    "shows backing sequences from SERIAL" in {
      val table = query(
        """CREATE TABLE t (id SERIAL, name TEXT);
          |SHOW SEQUENCES;""".stripMargin
      )
      table.data.length shouldBe 1
    }
  }

  "Transaction rollback" - {
    "rollback resets sequence state" in {
      val table = query(
        """CREATE TABLE t (id SERIAL, name TEXT);
          |INSERT INTO t (name) VALUES ('a');
          |BEGIN;
          |INSERT INTO t (name) VALUES ('b');
          |ROLLBACK;
          |INSERT INTO t (name) VALUES ('c');
          |SELECT id, name FROM t ORDER BY id;""".stripMargin
      )
      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(1).data(0) shouldBe NumberValue(2)
    }

    "rollback of CREATE SEQUENCE" in {
      an[Exception] should be thrownBy {
        results(
          """BEGIN;
            |CREATE SEQUENCE my_seq;
            |ROLLBACK;
            |SELECT nextval('my_seq');""".stripMargin
        )
      }
    }
  }

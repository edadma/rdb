package io.github.edadma.rdb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import java.io.{ByteArrayOutputStream, PrintStream}

class PreparedStatementTests extends AnyFreeSpec with Matchers:

  private def suppressStderr[A](block: => A): A =
    val devNull = new PrintStream(new ByteArrayOutputStream())
    Console.withErr(devNull)(block)

  private def query(sql: String): TableValue =
    given DB = new MemoryDB
    executeSQL(sql).collect { case QueryResult(t) => t }.last

  private def results(sql: String): Seq[Result] =
    given DB = new MemoryDB
    executeSQL(sql)

  "SQL PREPARE/EXECUTE" - {
    "prepares and executes a SELECT with parameters" in {
      val table = query(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |INSERT INTO t (id, name) VALUES (2, 'Bob');
          |INSERT INTO t (id, name) VALUES (3, 'Carol');
          |PREPARE q AS SELECT * FROM t WHERE id = $1;
          |EXECUTE q(2);
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("Bob")
    }

    "executes with different parameter values" in {
      given db: DB = new MemoryDB

      executeSQL(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |INSERT INTO t (id, name) VALUES (2, 'Bob');
          |INSERT INTO t (id, name) VALUES (3, 'Carol');
          |PREPARE q AS SELECT * FROM t WHERE id = $1;
          |""".trim.stripMargin
      )

      val r1 = executeSQL("EXECUTE q(1)").collect { case QueryResult(t) => t }.last
      r1.data.length shouldBe 1
      r1.data(0).data(1) shouldBe TextValue("Alice")

      val r2 = executeSQL("EXECUTE q(3)").collect { case QueryResult(t) => t }.last
      r2.data.length shouldBe 1
      r2.data(0).data(1) shouldBe TextValue("Carol")
    }

    "prepares and executes INSERT with parameters" in {
      val table = query(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |PREPARE ins AS INSERT INTO t (id, name) VALUES ($1, $2);
          |EXECUTE ins(1, 'Alice');
          |EXECUTE ins(2, 'Bob');
          |SELECT * FROM t ORDER BY id;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(1) shouldBe TextValue("Alice")
      table.data(1).data(1) shouldBe TextValue("Bob")
    }

    "prepares and executes UPDATE with parameters" in {
      val table = query(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |INSERT INTO t (id, name) VALUES (2, 'Bob');
          |PREPARE upd AS UPDATE t SET name = $1 WHERE id = $2;
          |EXECUTE upd('Alicia', 1);
          |SELECT * FROM t WHERE id = 1;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("Alicia")
    }

    "prepares and executes DELETE with parameters" in {
      val table = query(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |INSERT INTO t (id, name) VALUES (2, 'Bob');
          |PREPARE del AS DELETE FROM t WHERE id = $1;
          |EXECUTE del(1);
          |SELECT * FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("Bob")
    }

    "DEALLOCATE removes a prepared statement" in {
      val res = results(
        """
          |PREPARE q AS SELECT 1;
          |DEALLOCATE q;
          |""".trim.stripMargin
      )

      res.last shouldBe a[DeallocateResult]
    }

    "DEALLOCATE PREPARE syntax works" in {
      val res = results(
        """
          |PREPARE q AS SELECT 1;
          |DEALLOCATE PREPARE q;
          |""".trim.stripMargin
      )

      res.last shouldBe a[DeallocateResult]
    }

    "EXECUTE non-existent statement fails" in {
      assertThrows[RuntimeException] {
        suppressStderr {
          given DB = new MemoryDB
          executeSQL("EXECUTE nonexistent(1)")
        }
      }
    }

    "DEALLOCATE non-existent statement fails" in {
      assertThrows[RuntimeException] {
        suppressStderr {
          given DB = new MemoryDB
          executeSQL("DEALLOCATE nonexistent")
        }
      }
    }

    "duplicate PREPARE fails" in {
      assertThrows[RuntimeException] {
        suppressStderr {
          given DB = new MemoryDB
          executeSQL(
            """
              |PREPARE q AS SELECT 1;
              |PREPARE q AS SELECT 2;
              |""".trim.stripMargin
          )
        }
      }
    }
  }

  "Programmatic API" - {
    "db.prepare returns PreparedStatement" in {
      given db: DB = new MemoryDB
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT)")

      val ps = db.prepare("SELECT * FROM t WHERE id = $1")
      ps shouldBe a[PreparedStatement]
    }

    "ps.execute returns correct results" in {
      given db: DB = new MemoryDB
      executeSQL(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |INSERT INTO t (id, name) VALUES (2, 'Bob');
          |""".trim.stripMargin
      )

      val ps = db.prepare("SELECT * FROM t WHERE id = $1")
      val r = ps.execute(NumberValue(1))
      val table = r.collect { case QueryResult(t) => t }.last

      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("Alice")
    }

    "multiple executions with different params" in {
      given db: DB = new MemoryDB
      executeSQL(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |INSERT INTO t (id, name) VALUES (2, 'Bob');
          |INSERT INTO t (id, name) VALUES (3, 'Carol');
          |""".trim.stripMargin
      )

      val ps = db.prepare("SELECT * FROM t WHERE id = $1")

      for (id, expected) <- Seq((1, "Alice"), (2, "Bob"), (3, "Carol")) do
        val table = ps.execute(NumberValue(id)).collect { case QueryResult(t) => t }.last
        table.data.length shouldBe 1
        table.data(0).data(1) shouldBe TextValue(expected)
    }

    "parameters in INSERT VALUES" in {
      given db: DB = new MemoryDB
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT)")

      val ps = db.prepare("INSERT INTO t (id, name) VALUES ($1, $2)")
      ps.execute(NumberValue(1), TextValue("Alice"))
      ps.execute(NumberValue(2), TextValue("Bob"))

      val table = executeSQL("SELECT * FROM t ORDER BY id").collect { case QueryResult(t) => t }.last
      table.data.length shouldBe 2
      table.data(0).data(1) shouldBe TextValue("Alice")
      table.data(1).data(1) shouldBe TextValue("Bob")
    }

    "parameters in UPDATE SET and WHERE" in {
      given db: DB = new MemoryDB
      executeSQL(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |INSERT INTO t (id, name) VALUES (2, 'Bob');
          |""".trim.stripMargin
      )

      val ps = db.prepare("UPDATE t SET name = $1 WHERE id = $2")
      ps.execute(TextValue("Alicia"), NumberValue(1))

      val table = executeSQL("SELECT * FROM t WHERE id = 1").collect { case QueryResult(t) => t }.last
      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("Alicia")
    }

    "parameterCount returns correct count" in {
      given db: DB = new MemoryDB
      val ps1 = db.prepare("SELECT * FROM t WHERE id = $1")
      ps1.parameterCount shouldBe 1

      val ps2 = db.prepare("INSERT INTO t (a, b, c) VALUES ($1, $2, $3)")
      ps2.parameterCount shouldBe 3

      val ps3 = db.prepare("SELECT 1")
      ps3.parameterCount shouldBe 0
    }
  }

  "Parameter validation" - {
    "unbound parameter $3 when only 2 params provided" in {
      assertThrows[RuntimeException] {
        suppressStderr {
          given db: DB = new MemoryDB
          executeSQL(
            """
              |CREATE TABLE t (id INTEGER, name TEXT);
              |INSERT INTO t (id, name) VALUES (1, 'Alice');
              |""".trim.stripMargin
          )
          val ps = db.prepare("SELECT * FROM t WHERE id = $3")
          ps.execute(NumberValue(1), NumberValue(2))
        }
      }
    }

    "$1 and $2 are parsed correctly" in {
      val table = query(
        """
          |CREATE TABLE t (a INTEGER, b INTEGER);
          |INSERT INTO t (a, b) VALUES (10, 20);
          |INSERT INTO t (a, b) VALUES (30, 40);
          |PREPARE q AS SELECT * FROM t WHERE a = $1 AND b = $2;
          |EXECUTE q(10, 20);
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(10)
      table.data(0).data(1) shouldBe NumberValue(20)
    }
  }

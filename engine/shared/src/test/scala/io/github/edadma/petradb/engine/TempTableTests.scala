package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TempTableTests extends AnyFreeSpec with Matchers:

  private def query(sql: String): TableValue =
    given Session = new MemoryDB().connect()
    executeSQL(sql).collect { case QueryResult(t) => t }.last

  private def results(sql: String): Seq[Result] =
    given Session = new MemoryDB().connect()
    executeSQL(sql)

  "CREATE TEMP TABLE, INSERT, SELECT" in {
    val t = query(
      """CREATE TEMP TABLE t (id INT, name TEXT);
        |INSERT INTO t VALUES (1, 'alice'), (2, 'bob');
        |SELECT * FROM t ORDER BY id""".stripMargin
    )
    t.data.map(_.data.toList) shouldBe Vector(
      List(NumberValue(1), TextValue("alice")),
      List(NumberValue(2), TextValue("bob")),
    )
  }

  "TEMPORARY keyword works" in {
    val t = query(
      """CREATE TEMPORARY TABLE t (id INT, name TEXT);
        |INSERT INTO t VALUES (1, 'x');
        |SELECT * FROM t""".stripMargin
    )
    t.data.length shouldBe 1
  }

  "IF NOT EXISTS" in {
    val r = results(
      """CREATE TEMP TABLE t (id INT);
        |CREATE TEMP TABLE IF NOT EXISTS t (id INT);
        |SELECT 1""".stripMargin
    )
    // Should not throw — IF NOT EXISTS suppresses duplicate error
    r.length shouldBe 3
  }

  "temp table shadows regular table of same name" in {
    val t = query(
      """CREATE TABLE t (id INT, val TEXT);
        |INSERT INTO t VALUES (1, 'regular');
        |CREATE TEMP TABLE t (id INT, val TEXT);
        |INSERT INTO t VALUES (2, 'temp');
        |SELECT * FROM t""".stripMargin
    )
    t.data.length shouldBe 1
    t.data.head.data(0) shouldBe NumberValue(2)
    t.data.head.data(1) shouldBe TextValue("temp")
  }

  "UPDATE on temp table" in {
    val t = query(
      """CREATE TEMP TABLE t (id INT, name TEXT);
        |INSERT INTO t VALUES (1, 'old');
        |UPDATE t SET name = 'new' WHERE id = 1;
        |SELECT name FROM t""".stripMargin
    )
    t.data.head.data(0) shouldBe TextValue("new")
  }

  "DELETE on temp table" in {
    val t = query(
      """CREATE TEMP TABLE t (id INT);
        |INSERT INTO t VALUES (1), (2), (3);
        |DELETE FROM t WHERE id = 2;
        |SELECT * FROM t ORDER BY id""".stripMargin
    )
    t.data.map(_.data(0)) shouldBe Vector(NumberValue(1), NumberValue(3))
  }

  "DROP TABLE on temp table" in {
    // Drop the temp table, then create a regular table with the same name
    val t = query(
      """CREATE TEMP TABLE t (id INT);
        |INSERT INTO t VALUES (1);
        |DROP TABLE t;
        |CREATE TABLE t (id INT);
        |INSERT INTO t VALUES (99);
        |SELECT * FROM t""".stripMargin
    )
    t.data.head.data(0) shouldBe NumberValue(99)
  }

  "temp table with PRIMARY KEY and serial" in {
    val t = query(
      """CREATE TEMP TABLE t (id SERIAL PRIMARY KEY, name TEXT);
        |INSERT INTO t (name) VALUES ('a'), ('b'), ('c');
        |SELECT * FROM t ORDER BY id""".stripMargin
    )
    t.data.length shouldBe 3
    t.data.map(_.data(0)) shouldBe Vector(NumberValue(1), NumberValue(2), NumberValue(3))
  }

  "error: duplicate temp table name" in {
    assertThrows[SchemaException] {
      results(
        """CREATE TEMP TABLE t (id INT);
          |CREATE TEMP TABLE t (id INT)""".stripMargin
      )
    }
  }

  "TRUNCATE temp table" in {
    val t = query(
      """CREATE TEMP TABLE t (id INT);
        |INSERT INTO t VALUES (1), (2);
        |TRUNCATE t;
        |SELECT * FROM t""".stripMargin
    )
    t.data.length shouldBe 0
  }

  "JOIN between temp table and regular table" in {
    val t = query(
      """CREATE TABLE users (id INT, name TEXT);
        |INSERT INTO users VALUES (1, 'alice'), (2, 'bob');
        |CREATE TEMP TABLE orders (user_id INT, product TEXT);
        |INSERT INTO orders VALUES (1, 'widget'), (2, 'gadget');
        |SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id ORDER BY u.name""".stripMargin
    )
    t.data.length shouldBe 2
    t.data(0).data(0) shouldBe TextValue("alice")
    t.data(0).data(1) shouldBe TextValue("widget")
    t.data(1).data(0) shouldBe TextValue("bob")
    t.data(1).data(1) shouldBe TextValue("gadget")
  }

  "INSERT SELECT into temp table from regular table" in {
    val t = query(
      """CREATE TABLE src (id INT, val TEXT);
        |INSERT INTO src VALUES (1, 'a'), (2, 'b');
        |CREATE TEMP TABLE dst (id INT, val TEXT);
        |INSERT INTO dst SELECT * FROM src;
        |SELECT * FROM dst ORDER BY id""".stripMargin
    )
    t.data.length shouldBe 2
  }

  "CREATE TEMP TABLE allowed inside transaction" in {
    val t = query(
      """BEGIN;
        |CREATE TEMP TABLE t (id INT, name TEXT);
        |INSERT INTO t VALUES (1, 'inside txn');
        |COMMIT;
        |SELECT * FROM t""".stripMargin
    )
    t.data.length shouldBe 1
    t.data.head.data(1) shouldBe TextValue("inside txn")
  }

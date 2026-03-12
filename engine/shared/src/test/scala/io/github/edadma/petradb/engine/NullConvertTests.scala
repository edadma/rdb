package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class NullConvertTests extends AnyFreeSpec with Matchers with Testing {

  "UPDATE SET column to NULL via prepared statement" - {
    "TEXT column" in {
      given session: Session = new MemoryDB().connect()
      executeSQL(
        """CREATE TABLE t (id INT, v TEXT);
          |INSERT INTO t VALUES (1, 'hello');""".stripMargin
      )
      val ps = session.prepare("UPDATE t SET v = $1 WHERE id = $2")
      ps.execute(NullValue(), NumberValue(1))
      val t = executeSQL("SELECT v FROM t WHERE id = 1").collect { case QueryResult(t) => t }.last
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "VARCHAR column" in {
      given session: Session = new MemoryDB().connect()
      executeSQL(
        """CREATE TABLE t (id INT, v VARCHAR(50));
          |INSERT INTO t VALUES (1, 'hello');""".stripMargin
      )
      val ps = session.prepare("UPDATE t SET v = $1 WHERE id = $2")
      ps.execute(NullValue(), NumberValue(1))
      val t = executeSQL("SELECT v FROM t WHERE id = 1").collect { case QueryResult(t) => t }.last
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "CHAR column" in {
      given session: Session = new MemoryDB().connect()
      executeSQL(
        """CREATE TABLE t (id INT, v CHAR(10));
          |INSERT INTO t VALUES (1, 'hello');""".stripMargin
      )
      val ps = session.prepare("UPDATE t SET v = $1 WHERE id = $2")
      ps.execute(NullValue(), NumberValue(1))
      val t = executeSQL("SELECT v FROM t WHERE id = 1").collect { case QueryResult(t) => t }.last
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "BOOLEAN column" in {
      given session: Session = new MemoryDB().connect()
      executeSQL(
        """CREATE TABLE t (id INT, v BOOLEAN);
          |INSERT INTO t VALUES (1, true);""".stripMargin
      )
      val ps = session.prepare("UPDATE t SET v = $1 WHERE id = $2")
      ps.execute(NullValue(), NumberValue(1))
      val t = executeSQL("SELECT v FROM t WHERE id = 1").collect { case QueryResult(t) => t }.last
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "DATE column" in {
      given session: Session = new MemoryDB().connect()
      executeSQL(
        """CREATE TABLE t (id INT, v DATE);
          |INSERT INTO t VALUES (1, '2026-01-01');""".stripMargin
      )
      val ps = session.prepare("UPDATE t SET v = $1 WHERE id = $2")
      ps.execute(NullValue(), NumberValue(1))
      val t = executeSQL("SELECT v FROM t WHERE id = 1").collect { case QueryResult(t) => t }.last
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "TIMESTAMP column" in {
      given session: Session = new MemoryDB().connect()
      executeSQL(
        """CREATE TABLE t (id INT, v TIMESTAMP);
          |INSERT INTO t VALUES (1, '2026-01-01 12:00:00');""".stripMargin
      )
      val ps = session.prepare("UPDATE t SET v = $1 WHERE id = $2")
      ps.execute(NullValue(), NumberValue(1))
      val t = executeSQL("SELECT v FROM t WHERE id = 1").collect { case QueryResult(t) => t }.last
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "UUID column" in {
      given session: Session = new MemoryDB().connect()
      executeSQL(
        """CREATE TABLE t (id INT, v UUID);
          |INSERT INTO t VALUES (1, gen_random_uuid());""".stripMargin
      )
      val ps = session.prepare("UPDATE t SET v = $1 WHERE id = $2")
      ps.execute(NullValue(), NumberValue(1))
      val t = executeSQL("SELECT v FROM t WHERE id = 1").collect { case QueryResult(t) => t }.last
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "JSON column" in {
      given session: Session = new MemoryDB().connect()
      executeSQL(
        """CREATE TABLE t (id INT, v JSON);
          |INSERT INTO t VALUES (1, '{"a":1}');""".stripMargin
      )
      val ps = session.prepare("UPDATE t SET v = $1 WHERE id = $2")
      ps.execute(NullValue(), NumberValue(1))
      val t = executeSQL("SELECT v FROM t WHERE id = 1").collect { case QueryResult(t) => t }.last
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "NUMERIC column" in {
      given session: Session = new MemoryDB().connect()
      executeSQL(
        """CREATE TABLE t (id INT, v NUMERIC(10,2));
          |INSERT INTO t VALUES (1, 19.99);""".stripMargin
      )
      val ps = session.prepare("UPDATE t SET v = $1 WHERE id = $2")
      ps.execute(NullValue(), NumberValue(1))
      val t = executeSQL("SELECT v FROM t WHERE id = 1").collect { case QueryResult(t) => t }.last
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "ENUM column" in {
      given session: Session = new MemoryDB().connect()
      executeSQL(
        """CREATE TYPE color AS ENUM ('red', 'green', 'blue');
          |CREATE TABLE t (id INT, v color);
          |INSERT INTO t VALUES (1, 'red');""".stripMargin
      )
      val ps = session.prepare("UPDATE t SET v = $1 WHERE id = $2")
      ps.execute(NullValue(), NumberValue(1))
      val t = executeSQL("SELECT v FROM t WHERE id = 1").collect { case QueryResult(t) => t }.last
      t.data(0).data(0) shouldBe a[NullValue]
    }
  }

  "INSERT with NULL into typed columns via prepared statement" - {
    "TEXT column" in {
      given session: Session = new MemoryDB().connect()
      executeSQL("CREATE TABLE t (id INT, v TEXT)")
      session.prepare("INSERT INTO t VALUES ($1, $2)").execute(NumberValue(1), NullValue())
      val t = executeSQL("SELECT v FROM t").collect { case QueryResult(t) => t }.last
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "DATE column" in {
      given session: Session = new MemoryDB().connect()
      executeSQL("CREATE TABLE t (id INT, v DATE)")
      session.prepare("INSERT INTO t VALUES ($1, $2)").execute(NumberValue(1), NullValue())
      val t = executeSQL("SELECT v FROM t").collect { case QueryResult(t) => t }.last
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "JSON column" in {
      given session: Session = new MemoryDB().connect()
      executeSQL("CREATE TABLE t (id INT, v JSON)")
      session.prepare("INSERT INTO t VALUES ($1, $2)").execute(NumberValue(1), NullValue())
      val t = executeSQL("SELECT v FROM t").collect { case QueryResult(t) => t }.last
      t.data(0).data(0) shouldBe a[NullValue]
    }

    "ENUM column" in {
      given session: Session = new MemoryDB().connect()
      executeSQL(
        """CREATE TYPE status AS ENUM ('on', 'off');
          |CREATE TABLE t (id INT, v status);""".stripMargin
      )
      session.prepare("INSERT INTO t VALUES ($1, $2)").execute(NumberValue(1), NullValue())
      val t = executeSQL("SELECT v FROM t").collect { case QueryResult(t) => t }.last
      t.data(0).data(0) shouldBe a[NullValue]
    }
  }
}

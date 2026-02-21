package io.github.edadma.rdb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MemoryTransactionTests extends AnyFreeSpec with Matchers:

  private def withDB(f: Session => Unit): Unit =
    val db = new MemoryDB
    f(db.connect())

  "MemoryDB Transactions" - {
    "BEGIN/COMMIT preserves inserts" in withDB { db =>
      given Session = db
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("BEGIN;")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
      executeSQL("INSERT INTO t (id, name) VALUES (2, 'Bob');")
      executeSQL("COMMIT;")

      val table = executeSQL("SELECT * FROM t ORDER BY id;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 2
      table.data(0).data(1) shouldBe TextValue("Alice")
      table.data(1).data(1) shouldBe TextValue("Bob")
    }

    "ROLLBACK undoes inserts" in withDB { db =>
      given Session = db
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'before');")
      executeSQL("BEGIN;")
      executeSQL("INSERT INTO t (id, name) VALUES (2, 'inside');")
      executeSQL("ROLLBACK;")

      val table = executeSQL("SELECT * FROM t;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("before")
    }

    "ROLLBACK undoes deletes" in withDB { db =>
      given Session = db
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
      executeSQL("BEGIN;")
      executeSQL("DELETE FROM t WHERE id = 1;")
      executeSQL("ROLLBACK;")

      val table = executeSQL("SELECT * FROM t;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(1)
    }

    "ROLLBACK undoes updates" in withDB { db =>
      given Session = db
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
      executeSQL("BEGIN;")
      executeSQL("UPDATE t SET name = 'CHANGED' WHERE id = 1;")
      executeSQL("ROLLBACK;")

      val table = executeSQL("SELECT * FROM t WHERE id = 1;").collect { case QueryResult(t) => t }.head
      table.data(0).data(1) shouldBe TextValue("Alice")
    }

    "ROLLBACK restores auto-increment state" in withDB { db =>
      given Session = db
      executeSQL("CREATE TABLE t (id SERIAL, name TEXT);")
      executeSQL("INSERT INTO t (name) VALUES ('first');")
      executeSQL("BEGIN;")
      executeSQL("INSERT INTO t (name) VALUES ('second');")
      executeSQL("ROLLBACK;")

      executeSQL("INSERT INTO t (name) VALUES ('actual_second');")
      val table = executeSQL("SELECT id, name FROM t ORDER BY id;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(1).data(0) shouldBe NumberValue(2)
    }

    "ROLLBACK restores unique index" in withDB { db =>
      given Session = db
      executeSQL("CREATE TABLE t (id INTEGER, PRIMARY KEY (id));")
      executeSQL("INSERT INTO t (id) VALUES (1);")
      executeSQL("BEGIN;")
      executeSQL("INSERT INTO t (id) VALUES (2);")
      executeSQL("ROLLBACK;")

      // Key 2 should be available after rollback
      executeSQL("INSERT INTO t (id) VALUES (2);")
      val table = executeSQL("SELECT * FROM t ORDER BY id;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 2
    }

    "ROLLBACK restores non-unique index" in withDB { db =>
      given Session = db
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("CREATE INDEX idx ON t (name);")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
      executeSQL("BEGIN;")
      executeSQL("INSERT INTO t (id, name) VALUES (2, 'Alice');")
      executeSQL("INSERT INTO t (id, name) VALUES (3, 'Bob');")
      executeSQL("ROLLBACK;")

      val table = executeSQL("SELECT * FROM t;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
    }

    "multiple DML in single transaction then rollback" in withDB { db =>
      given Session = db
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY (id));")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
      executeSQL("INSERT INTO t (id, name) VALUES (2, 'Bob');")
      executeSQL("INSERT INTO t (id, name) VALUES (3, 'Charlie');")

      executeSQL("BEGIN;")
      executeSQL("UPDATE t SET name = 'ALICE' WHERE id = 1;")
      executeSQL("DELETE FROM t WHERE id = 2;")
      executeSQL("INSERT INTO t (id, name) VALUES (4, 'Dave');")
      executeSQL("ROLLBACK;")

      val table = executeSQL("SELECT * FROM t ORDER BY id;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 3
      table.data(0).data(1) shouldBe TextValue("Alice")
      table.data(1).data(1) shouldBe TextValue("Bob")
      table.data(2).data(1) shouldBe TextValue("Charlie")
    }

    "read-your-own-writes within transaction" in withDB { db =>
      given Session = db
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("BEGIN;")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'txn_row');")

      val table = executeSQL("SELECT * FROM t;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("txn_row")
      executeSQL("COMMIT;")
    }

    "error inside transaction marks it aborted" in withDB { db =>
      given Session = db
      executeSQL("CREATE TABLE t (id INTEGER NOT NULL, name TEXT);")
      executeSQL("BEGIN;")

      intercept[Exception] {
        executeSQL("INSERT INTO t (id, name) VALUES (NULL, 'bad');")
      }

      the[RuntimeException] thrownBy {
        executeSQL("INSERT INTO t (id, name) VALUES (1, 'good');")
      } should have message "current transaction is aborted, use ROLLBACK"

      executeSQL("ROLLBACK;")
    }

    "ROLLBACK after error allows new transaction" in withDB { db =>
      given Session = db
      executeSQL("CREATE TABLE t (id INTEGER NOT NULL, name TEXT);")
      executeSQL("BEGIN;")

      intercept[Exception] {
        executeSQL("INSERT INTO t (id, name) VALUES (NULL, 'bad');")
      }

      executeSQL("ROLLBACK;")

      executeSQL("BEGIN;")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'good');")
      executeSQL("COMMIT;")

      val table = executeSQL("SELECT * FROM t;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("good")
    }

    "COMMIT without BEGIN fails" in withDB { db =>
      given Session = db
      the[RuntimeException] thrownBy {
        executeSQL("COMMIT;")
      } should have message "no active transaction"
    }

    "ROLLBACK without BEGIN fails" in withDB { db =>
      given Session = db
      the[RuntimeException] thrownBy {
        executeSQL("ROLLBACK;")
      } should have message "no active transaction"
    }

    "DDL inside transaction fails" in withDB { db =>
      given Session = db
      executeSQL("CREATE TABLE t (id INTEGER);")
      executeSQL("BEGIN;")

      the[RuntimeException] thrownBy {
        executeSQL("CREATE TABLE t2 (id INTEGER);")
      } should have message "DDL not allowed inside a transaction"

      executeSQL("ROLLBACK;")
    }
  }

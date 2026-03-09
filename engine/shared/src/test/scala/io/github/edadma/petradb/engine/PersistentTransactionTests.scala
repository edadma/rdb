package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

class PersistentTransactionTests extends PersistentTestBase:

  "Transactions" - {
    "BEGIN/COMMIT persists inserts" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("CREATE TABLE t (id SERIAL, name TEXT);")
      executeSQL("BEGIN;")
      executeSQL("INSERT INTO t (name) VALUES ('Alice');")
      executeSQL("INSERT INTO t (name) VALUES ('Bob');")
      executeSQL("COMMIT;")

      val table = executeSQL("SELECT name FROM t ORDER BY id;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(1).data(0) shouldBe TextValue("Bob")
      db.close()
    }

    "ROLLBACK undoes inserts" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("CREATE TABLE t (id SERIAL, name TEXT);")
      executeSQL("INSERT INTO t (name) VALUES ('before');")
      executeSQL("BEGIN;")
      executeSQL("INSERT INTO t (name) VALUES ('inside');")
      executeSQL("ROLLBACK;")

      val table = executeSQL("SELECT name FROM t;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("before")
      db.close()
    }

    "ROLLBACK undoes deletes" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
      executeSQL("BEGIN;")
      executeSQL("DELETE FROM t WHERE id = 1;")
      executeSQL("ROLLBACK;")

      val table = executeSQL("SELECT name FROM t;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("Alice")
      db.close()
    }

    "ROLLBACK undoes updates" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
      executeSQL("BEGIN;")
      executeSQL("UPDATE t SET name = 'CHANGED' WHERE id = 1;")
      executeSQL("ROLLBACK;")

      val table = executeSQL("SELECT name FROM t WHERE id = 1;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("Alice")
      db.close()
    }

    "ROLLBACK resets auto-increment" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
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
      db.close()
    }

    "read-your-own-writes within transaction" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("BEGIN;")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'txn_row');")

      val table = executeSQL("SELECT name FROM t;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("txn_row")
      executeSQL("COMMIT;")
      db.close()
    }

    "multiple DML operations in single transaction" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
      executeSQL("INSERT INTO t (id, name) VALUES (2, 'Bob');")
      executeSQL("INSERT INTO t (id, name) VALUES (3, 'Charlie');")

      executeSQL("BEGIN;")
      executeSQL("UPDATE t SET name = 'ALICE' WHERE id = 1;")
      executeSQL("DELETE FROM t WHERE id = 2;")
      executeSQL("INSERT INTO t (id, name) VALUES (4, 'Dave');")
      executeSQL("COMMIT;")

      val table = executeSQL("SELECT id, name FROM t ORDER BY id;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 3
      table.data(0).data(1) shouldBe TextValue("ALICE")
      table.data(1).data(1) shouldBe TextValue("Charlie")
      table.data(2).data(1) shouldBe TextValue("Dave")
      db.close()
    }

    "transaction survives reopen after commit" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id SERIAL, name TEXT);")
        executeSQL("BEGIN;")
        executeSQL("INSERT INTO t (name) VALUES ('persisted');")
        executeSQL("COMMIT;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT name FROM t;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 1
        table.data(0).data(0) shouldBe TextValue("persisted")
        db.close()
      }
    }

    "DDL-only transaction succeeds" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("BEGIN;")
      executeSQL("CREATE TABLE t (id INTEGER);")
      executeSQL("COMMIT;")

      val table = executeSQL("SELECT * FROM t;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 0
      db.close()
    }

    "DDL after DDL in same transaction succeeds" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("BEGIN;")
      executeSQL("CREATE TABLE t1 (id INTEGER);")
      executeSQL("CREATE TABLE t2 (name TEXT);")
      executeSQL("COMMIT;")

      executeSQL("SELECT * FROM t1;").collect { case QueryResult(t) => t }.head.data.length shouldBe 0
      executeSQL("SELECT * FROM t2;").collect { case QueryResult(t) => t }.head.data.length shouldBe 0
      db.close()
    }

    "DML then DDL in same transaction succeeds" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("CREATE TABLE t (id INTEGER);")
      executeSQL("BEGIN;")
      executeSQL("INSERT INTO t VALUES (1);")
      executeSQL("CREATE TABLE t2 (id INTEGER);")
      executeSQL("COMMIT;")

      val table = executeSQL("SELECT * FROM t;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      executeSQL("SELECT * FROM t2;").collect { case QueryResult(t) => t }.head.data.length shouldBe 0
      db.close()
    }

    "DML then DDL then ROLLBACK — everything is rolled back" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("CREATE TABLE t (id INTEGER);")
      executeSQL("BEGIN;")
      executeSQL("INSERT INTO t VALUES (1);")
      executeSQL("CREATE TABLE t2 (id INTEGER);")
      executeSQL("ROLLBACK;")

      val table = executeSQL("SELECT * FROM t;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 0
      assertThrows[Exception] { executeSQL("SELECT * FROM t2;") }
      db.close()
    }

    "DDL then DML then ROLLBACK — everything is rolled back" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("BEGIN;")
      executeSQL("CREATE TABLE t (id INTEGER);")
      executeSQL("INSERT INTO t VALUES (1);")
      executeSQL("ROLLBACK;")

      assertThrows[Exception] { executeSQL("SELECT * FROM t;") }
      db.close()
    }

    "interleaved DDL and DML commits atomically" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("BEGIN;")
      executeSQL("CREATE TABLE t1 (id SERIAL, name TEXT);")
      executeSQL("INSERT INTO t1 (name) VALUES ('Alice');")
      executeSQL("CREATE TABLE t2 (id SERIAL, title TEXT);")
      executeSQL("INSERT INTO t2 (title) VALUES ('Hello');")
      executeSQL("COMMIT;")

      val t1 = executeSQL("SELECT name FROM t1;").collect { case QueryResult(t) => t }.head
      t1.data.length shouldBe 1
      t1.data(0).data(0) shouldBe TextValue("Alice")
      val t2 = executeSQL("SELECT title FROM t2;").collect { case QueryResult(t) => t }.head
      t2.data.length shouldBe 1
      t2.data(0).data(0) shouldBe TextValue("Hello")
      db.close()
    }

    "interleaved DDL and DML rolls back atomically" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("BEGIN;")
      executeSQL("CREATE TABLE t1 (id INTEGER);")
      executeSQL("INSERT INTO t1 VALUES (1);")
      executeSQL("CREATE TABLE t2 (id INTEGER);")
      executeSQL("INSERT INTO t2 VALUES (2);")
      executeSQL("ROLLBACK;")

      assertThrows[Exception] { executeSQL("SELECT * FROM t1;") }
      assertThrows[Exception] { executeSQL("SELECT * FROM t2;") }
      db.close()
    }

    "interleaved DDL/DML survives reopen after commit" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("BEGIN;")
        executeSQL("CREATE TABLE t1 (id SERIAL, name TEXT);")
        executeSQL("INSERT INTO t1 (name) VALUES ('persisted');")
        executeSQL("CREATE TABLE t2 (id INTEGER);")
        executeSQL("COMMIT;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val t1 = executeSQL("SELECT name FROM t1;").collect { case QueryResult(t) => t }.head
        t1.data.length shouldBe 1
        t1.data(0).data(0) shouldBe TextValue("persisted")
        executeSQL("SELECT * FROM t2;").collect { case QueryResult(t) => t }.head.data.length shouldBe 0
        db.close()
      }
    }

    "CREATE INDEX inside transaction with DML" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("BEGIN;")
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
      executeSQL("CREATE INDEX idx ON t (name);")
      executeSQL("COMMIT;")

      val table = executeSQL("SELECT * FROM t WHERE name = 'Alice';").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      db.close()
    }

    "ROLLBACK after CREATE INDEX undoes index and table" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("BEGIN;")
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("CREATE INDEX idx ON t (name);")
      executeSQL("ROLLBACK;")

      assertThrows[Exception] { executeSQL("SELECT * FROM t;") }
      db.close()
    }

    "auto-commit works as before" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'auto');")

      val table = executeSQL("SELECT name FROM t;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("auto")
      db.close()
    }

    "COMMIT without BEGIN fails" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("CREATE TABLE t (id INTEGER);")

      the[RuntimeException] thrownBy {
        executeSQL("COMMIT;")
      } should have message "no active transaction"

      db.close()
    }

    "ROLLBACK without BEGIN fails" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("CREATE TABLE t (id INTEGER);")

      the[RuntimeException] thrownBy {
        executeSQL("ROLLBACK;")
      } should have message "no active transaction"

      db.close()
    }

    "error inside transaction marks it aborted" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("CREATE TABLE t (id INTEGER NOT NULL, name TEXT);")
      executeSQL("BEGIN;")

      // Insert a NULL into a NOT NULL column — should fail
      intercept[Exception] {
        executeSQL("INSERT INTO t (id, name) VALUES (NULL, 'bad');")
      }

      // Subsequent DML should be rejected
      the[RuntimeException] thrownBy {
        executeSQL("INSERT INTO t (id, name) VALUES (1, 'good');")
      } should have message "current transaction is aborted, use ROLLBACK"

      // COMMIT should be rejected
      the[RuntimeException] thrownBy {
        executeSQL("COMMIT;")
      } should have message "current transaction is aborted, use ROLLBACK"

      // ROLLBACK should work
      executeSQL("ROLLBACK;")
      db.close()
    }

    "ROLLBACK after error allows new transaction" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("CREATE TABLE t (id INTEGER NOT NULL, name TEXT);")
      executeSQL("BEGIN;")

      intercept[Exception] {
        executeSQL("INSERT INTO t (id, name) VALUES (NULL, 'bad');")
      }

      executeSQL("ROLLBACK;")

      // New transaction should work fine
      executeSQL("BEGIN;")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'good');")
      executeSQL("COMMIT;")

      val table = executeSQL("SELECT name FROM t;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("good")
      db.close()
    }

    "SELECT in aborted transaction fails" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("CREATE TABLE t (id INTEGER NOT NULL);")
      executeSQL("INSERT INTO t (id) VALUES (1);")
      executeSQL("BEGIN;")

      intercept[Exception] {
        executeSQL("INSERT INTO t (id) VALUES (NULL);")
      }

      the[RuntimeException] thrownBy {
        executeSQL("SELECT * FROM t;")
      } should have message "current transaction is aborted, use ROLLBACK"

      executeSQL("ROLLBACK;")
      db.close()
    }

    "ROLLBACK restores unique index — re-insert same key" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY (id));")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
      executeSQL("BEGIN;")
      executeSQL("INSERT INTO t (id, name) VALUES (2, 'Bob');")
      executeSQL("ROLLBACK;")

      // Key 2 should be available again after rollback
      executeSQL("INSERT INTO t (id, name) VALUES (2, 'Charlie');")
      val table = executeSQL("SELECT * FROM t ORDER BY id;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 2
      table.data(1).data(1) shouldBe TextValue("Charlie")
      db.close()
    }

    "ROLLBACK restores non-unique index state" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("CREATE INDEX idx ON t (name);")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
      executeSQL("BEGIN;")
      executeSQL("INSERT INTO t (id, name) VALUES (2, 'Alice');")
      executeSQL("INSERT INTO t (id, name) VALUES (3, 'Bob');")
      executeSQL("ROLLBACK;")

      // Only the pre-transaction row should remain
      val table = executeSQL("SELECT * FROM t;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(1)

      // Insert should still work after rollback
      executeSQL("INSERT INTO t (id, name) VALUES (4, 'Dave');")
      val table2 = executeSQL("SELECT * FROM t ORDER BY id;").collect { case QueryResult(t) => t }.head
      table2.data.length shouldBe 2
      db.close()
    }

    "ROLLBACK after delete restores index entry" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY (id));")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
      executeSQL("INSERT INTO t (id, name) VALUES (2, 'Bob');")
      executeSQL("BEGIN;")
      executeSQL("DELETE FROM t WHERE id = 1;")
      executeSQL("ROLLBACK;")

      // Both rows should still be present and PK constraint still intact
      val table = executeSQL("SELECT * FROM t ORDER BY id;").collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 2

      // Key 1 should still be in the index — duplicate insert should fail
      assertThrows[RuntimeException] {
        executeSQL("INSERT INTO t (id, name) VALUES (1, 'Duplicate');")
      }
      db.close()
    }
  }

package io.github.edadma.rdb

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

    "ROLLBACK does not reset auto-increment (PostgreSQL semantics)" in {
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
      table.data(1).data(0) shouldBe NumberValue(3)
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

    "DDL inside transaction fails" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL("CREATE TABLE t (id INTEGER);")
      executeSQL("BEGIN;")

      the[RuntimeException] thrownBy {
        executeSQL("CREATE TABLE t2 (id INTEGER);")
      } should have message "DDL not allowed inside a transaction"

      executeSQL("ROLLBACK;")
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
      suppressStderr {
        intercept[Exception] {
          executeSQL("INSERT INTO t (id, name) VALUES (NULL, 'bad');")
        }
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

      suppressStderr {
        intercept[Exception] {
          executeSQL("INSERT INTO t (id, name) VALUES (NULL, 'bad');")
        }
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

      suppressStderr {
        intercept[Exception] {
          executeSQL("INSERT INTO t (id) VALUES (NULL);")
        }
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

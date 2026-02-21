package io.github.edadma.rdb

class PersistentDDLTests extends PersistentTestBase:

  // ── DDL persistence ─────────────────────────────────────────────────

  "DDL persistence" - {
    "ALTER TABLE ADD COLUMN survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER);")
        executeSQL("INSERT INTO t (id) VALUES (1);")
        executeSQL("ALTER TABLE t ADD COLUMN name TEXT DEFAULT 'unknown';")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT id, name FROM t;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 1
        table.data(0).data(0).string shouldBe "1"
        table.data(0).data(1) shouldBe TextValue("unknown")
        db.close()
      }
    }

    "ALTER TABLE DROP COLUMN survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, name TEXT, email TEXT);")
        executeSQL("INSERT INTO t (id, name, email) VALUES (1, 'Alice', 'alice@test.com');")
        executeSQL("ALTER TABLE t DROP COLUMN email;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT id, name FROM t;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 1
        table.data(0).data(0).string shouldBe "1"
        table.data(0).data(1) shouldBe TextValue("Alice")
        db.close()
      }
    }

    "ALTER COLUMN TYPE survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, v INTEGER);")
        executeSQL("INSERT INTO t (id, v) VALUES (1, 42);")
        executeSQL("ALTER TABLE t ALTER COLUMN v TYPE TEXT;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TextValue("42")
        db.close()
      }
    }

    "ALTER COLUMN SET DEFAULT survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, status TEXT);")
        executeSQL("ALTER TABLE t ALTER COLUMN status SET DEFAULT 'active';")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        executeSQL("INSERT INTO t (id) VALUES (1);")
        val table = executeSQL("SELECT status FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TextValue("active")
        db.close()
      }
    }

    "ALTER COLUMN DROP DEFAULT survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, status TEXT DEFAULT 'active');")
        executeSQL("ALTER TABLE t ALTER COLUMN status DROP DEFAULT;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        executeSQL("INSERT INTO t (id) VALUES (1);")
        val table = executeSQL("SELECT status FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0).isNull shouldBe true
        db.close()
      }
    }

    "ALTER COLUMN SET NOT NULL survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
        executeSQL("ALTER TABLE t ALTER COLUMN name SET NOT NULL;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        assertThrows[RuntimeException] {
          executeSQL("INSERT INTO t (id) VALUES (1);")
        }
        db.close()
      }
    }

    "ALTER COLUMN DROP NOT NULL survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, name TEXT NOT NULL);")
        executeSQL("ALTER TABLE t ALTER COLUMN name DROP NOT NULL;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        executeSQL("INSERT INTO t (id) VALUES (1);")
        val table = executeSQL("SELECT name FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0).isNull shouldBe true
        db.close()
      }
    }

    "ADD/DROP CONSTRAINT survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
        executeSQL("ALTER TABLE t ADD CONSTRAINT uq_name UNIQUE (name);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        // Verify table still works
        executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
        val table = executeSQL("SELECT * FROM t;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 1
        // Drop the constraint and verify it persists
        executeSQL("ALTER TABLE t DROP CONSTRAINT uq_name;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT * FROM t;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 1
        db.close()
      }
    }

    "RENAME TABLE survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE old_name (id INTEGER);")
        executeSQL("INSERT INTO old_name (id) VALUES (1);")
        executeSQL("ALTER TABLE old_name RENAME TO new_name;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        (db hasTable "old_name") shouldBe false
        (db hasTable "new_name") shouldBe true
        val table = executeSQL("SELECT id FROM new_name;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe NumberValue(1)
        db.close()
      }
    }

    "RENAME COLUMN survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, old_col TEXT);")
        executeSQL("INSERT INTO t (id, old_col) VALUES (1, 'val');")
        executeSQL("ALTER TABLE t RENAME COLUMN old_col TO new_col;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT new_col FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TextValue("val")
        db.close()
      }
    }

    "ADD COLUMN then insert new rows with that column" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER);")
        executeSQL("INSERT INTO t (id) VALUES (1);")
        executeSQL("ALTER TABLE t ADD COLUMN name TEXT;")
        executeSQL("INSERT INTO t (id, name) VALUES (2, 'Bob');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT id, name FROM t ORDER BY id;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 2
        table.data(0).data(1).isNull shouldBe true
        table.data(1).data(1) shouldBe TextValue("Bob")
        db.close()
      }
    }
  }

  // ── DROP TABLE ──────────────────────────────────────────────────────

  "DROP TABLE" - {
    "table gone after reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t1 (id INTEGER);")
        executeSQL("CREATE TABLE t2 (id INTEGER);")
        executeSQL("INSERT INTO t1 (id) VALUES (1);")
        executeSQL("INSERT INTO t2 (id) VALUES (2);")
        executeSQL("DROP TABLE t1;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        (db hasTable "t1") shouldBe false
        (db hasTable "t2") shouldBe true
        val table = executeSQL("SELECT id FROM t2;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0).string shouldBe "2"
        db.close()
      }
    }

    "drop and recreate table with same name" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, old_col TEXT);")
        executeSQL("INSERT INTO t (id, old_col) VALUES (1, 'old');")
        executeSQL("DROP TABLE t;")
        executeSQL("CREATE TABLE t (id INTEGER, new_col BOOLEAN);")
        executeSQL("INSERT INTO t (id, new_col) VALUES (2, true);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT id, new_col FROM t;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 1
        table.data(0).data(0) shouldBe NumberValue(2)
        table.data(0).data(1) shouldBe BooleanValue(true)
        db.close()
      }
    }

    "DROP TYPE persists" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TYPE mood AS ENUM ('happy', 'sad');")
        executeSQL("DROP TYPE mood;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        (db hasType "mood") shouldBe false
        db.close()
      }
    }
  }

package io.github.edadma.rdb

class PersistentDMLTests extends PersistentTestBase:

  // ── DML persistence ─────────────────────────────────────────────────

  "DML persistence" - {
    "UPDATE survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE users (id INTEGER, name TEXT);")
        executeSQL("INSERT INTO users (id, name) VALUES (1, 'Alice');")
        executeSQL("INSERT INTO users (id, name) VALUES (2, 'Bob');")
        executeSQL("UPDATE users SET name = 'Alicia' WHERE id = 1;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT id, name FROM users ORDER BY id;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 2
        table.data(0).data(1) shouldBe TextValue("Alicia")
        table.data(1).data(1) shouldBe TextValue("Bob")
        db.close()
      }
    }

    "DELETE survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE users (id INTEGER, name TEXT);")
        executeSQL("INSERT INTO users (id, name) VALUES (1, 'Alice');")
        executeSQL("INSERT INTO users (id, name) VALUES (2, 'Bob');")
        executeSQL("INSERT INTO users (id, name) VALUES (3, 'Charlie');")
        executeSQL("DELETE FROM users WHERE id = 2;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT id, name FROM users ORDER BY id;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 2
        table.data(0).data(1) shouldBe TextValue("Alice")
        table.data(1).data(1) shouldBe TextValue("Charlie")
        db.close()
      }
    }

    "DELETE all rows then reopen yields empty table" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER);")
        executeSQL("INSERT INTO t (id) VALUES (1);")
        executeSQL("INSERT INTO t (id) VALUES (2);")
        executeSQL("INSERT INTO t (id) VALUES (3);")
        executeSQL("DELETE FROM t;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT * FROM t;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 0
        // Insert still works after deleting all
        executeSQL("INSERT INTO t (id) VALUES (99);")
        val table2 = executeSQL("SELECT id FROM t;").collect { case QueryResult(t) => t }.head
        table2.data(0).data(0) shouldBe NumberValue(99)
        db.close()
      }
    }

    "UPDATE short text to long text (inline to chain)" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, v TEXT);")
        executeSQL("INSERT INTO t (id, v) VALUES (1, 'short');")
        val longVal = "x" * 200
        db.getTable("t").get.asInstanceOf[PersistentTable]
        executeSQL(s"UPDATE t SET v = '${longVal}' WHERE id = 1;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TextValue("x" * 200)
        db.close()
      }
    }

    "UPDATE long text to short text (chain to inline)" in {
      val longVal = "y" * 200

      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, v TEXT);")
        db.getTable("t").get.insert(Map("id" -> NumberValue(1), "v" -> TextValue(longVal)), None)
        executeSQL("UPDATE t SET v = 'short' WHERE id = 1;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TextValue("short")
        db.close()
      }
    }

    "multiple updates to same row" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, v INTEGER);")
        executeSQL("INSERT INTO t (id, v) VALUES (1, 10);")
        executeSQL("UPDATE t SET v = 20 WHERE id = 1;")
        executeSQL("UPDATE t SET v = 30 WHERE id = 1;")
        executeSQL("UPDATE t SET v = 40 WHERE id = 1;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe NumberValue(40)
        db.close()
      }
    }
  }

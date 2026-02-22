package io.github.edadma.petradb

class PersistentBasicTests extends PersistentTestBase:

  // ── Basic persistence ───────────────────────────────────────────────

  "Basic persistence" - {
    "create table, insert, close, reopen, query" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE users (id SERIAL, name TEXT, PRIMARY KEY (id));")
        executeSQL("INSERT INTO users (name) VALUES ('Alice');")
        executeSQL("INSERT INTO users (name) VALUES ('Bob');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val results = executeSQL("SELECT id, name FROM users ORDER BY id;")
        val table = results.collect { case QueryResult(t) => t }.head

        table.data.length shouldBe 2
        table.data(0).data(1) shouldBe TextValue("Alice")
        table.data(1).data(1) shouldBe TextValue("Bob")
        db.close()
      }
    }

    "empty table persists" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE empty_table (id INTEGER, name TEXT);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val results = executeSQL("SELECT * FROM empty_table;")
        val table = results.collect { case QueryResult(t) => t }.head

        table.data.length shouldBe 0
        db.close()
      }
    }
  }

  // ── Auto-increment persistence ──────────────────────────────────────

  "Auto-increment persistence" - {
    "serial counter persists across reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE items (id SERIAL, name TEXT, PRIMARY KEY (id));")
        executeSQL("INSERT INTO items (name) VALUES ('first');")
        executeSQL("INSERT INTO items (name) VALUES ('second');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        executeSQL("INSERT INTO items (name) VALUES ('third');")
        val results = executeSQL("SELECT id, name FROM items ORDER BY id;")
        val table = results.collect { case QueryResult(t) => t }.head

        table.data.length shouldBe 3
        table.data(0).data(0) shouldBe NumberValue(1)
        table.data(1).data(0) shouldBe NumberValue(2)
        table.data(2).data(0) shouldBe NumberValue(3)
        db.close()
      }
    }

    "smallserial counter persists" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE ss (id SMALLSERIAL, name TEXT, PRIMARY KEY (id));")
        executeSQL("INSERT INTO ss (name) VALUES ('a');")
        executeSQL("INSERT INTO ss (name) VALUES ('b');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        executeSQL("INSERT INTO ss (name) VALUES ('c');")
        val table = executeSQL("SELECT id FROM ss ORDER BY id;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 3
        table.data(2).data(0) shouldBe NumberValue(3)
        db.close()
      }
    }

    "bigserial counter persists" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE bs (id BIGSERIAL, name TEXT, PRIMARY KEY (id));")
        executeSQL("INSERT INTO bs (name) VALUES ('a');")
        executeSQL("INSERT INTO bs (name) VALUES ('b');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        executeSQL("INSERT INTO bs (name) VALUES ('c');")
        val table = executeSQL("SELECT id FROM bs ORDER BY id;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 3
        table.data(2).data(0) shouldBe NumberValue(3)
        db.close()
      }
    }
  }

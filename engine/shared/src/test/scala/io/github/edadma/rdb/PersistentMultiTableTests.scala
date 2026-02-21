package io.github.edadma.rdb

class PersistentMultiTableTests extends PersistentTestBase:

  // ── Multiple tables ─────────────────────────────────────────────────

  "Multiple tables" - {
    "multiple tables persist independently" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE users (id SERIAL, name TEXT, PRIMARY KEY (id));")
        executeSQL("CREATE TABLE products (id SERIAL, title TEXT, price INTEGER, PRIMARY KEY (id));")
        executeSQL("INSERT INTO users (name) VALUES ('Alice');")
        executeSQL("INSERT INTO products (title, price) VALUES ('Widget', 100);")
        executeSQL("INSERT INTO products (title, price) VALUES ('Gadget', 200);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val users = executeSQL("SELECT name FROM users;").collect { case QueryResult(t) => t }.head
        val products = executeSQL("SELECT title, price FROM products ORDER BY price;").collect { case QueryResult(t) => t }.head

        users.data.length shouldBe 1
        users.data(0).data(0) shouldBe TextValue("Alice")

        products.data.length shouldBe 2
        products.data(0).data(0) shouldBe TextValue("Widget")
        products.data(1).data(0) shouldBe TextValue("Gadget")
        db.close()
      }
    }
  }

  // ── Slotted page edge cases ─────────────────────────────────────────

  "Slotted page edge cases" - {
    "many rows spanning multiple data pages" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, payload TEXT);")
        // Insert enough rows to fill multiple pages (each row ~100 bytes serialized)
        for i <- 1 to 100 do
          executeSQL(s"INSERT INTO t (id, payload) VALUES ($i, '${("a" * 50)}');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT id FROM t ORDER BY id;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 100
        table.data(0).data(0) shouldBe NumberValue(1)
        table.data(99).data(0) shouldBe NumberValue(100)
        db.close()
      }
    }

    "insert after delete reuses space" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, v TEXT);")
        for i <- 1 to 20 do
          executeSQL(s"INSERT INTO t (id, v) VALUES ($i, 'row$i');")
        // Delete some rows to create tombstones
        executeSQL("DELETE FROM t WHERE id <= 10;")
        // Insert new rows
        for i <- 21 to 30 do
          executeSQL(s"INSERT INTO t (id, v) VALUES ($i, 'new$i');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT id FROM t ORDER BY id;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 20
        table.data(0).data(0) shouldBe NumberValue(11)
        table.data(19).data(0) shouldBe NumberValue(30)
        db.close()
      }
    }

    "small page size forces more pages" in {
      val smallPageSize = 256

      locally {
        val db = PersistentDB.create(tmpFile, smallPageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
        for i <- 1 to 20 do
          executeSQL(s"INSERT INTO t (id, name) VALUES ($i, 'name_$i');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT id FROM t ORDER BY id;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 20
        table.data(0).data(0) shouldBe NumberValue(1)
        table.data(19).data(0) shouldBe NumberValue(20)
        db.close()
      }
    }
  }

package io.github.edadma.rdb

class PersistentLargeValueTests extends PersistentTestBase:

  // ── Large values / chain edge cases ─────────────────────────────────

  "Large values" - {
    "large TEXT uses chain storage" in {
      val longText = "x" * 500

      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE docs (id INTEGER, content TEXT);")
        db.getTable("docs").get.insert(Map("id" -> NumberValue(1), "content" -> TextValue(longText)), None)
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT id, content FROM docs;").collect { case QueryResult(t) => t }.head
        table.data(0).data(1) shouldBe TextValue(longText)
        db.close()
      }
    }

    "large BYTEA uses chain storage" in {
      val bigData = (0 until 500).map(i => (i % 256).toByte).toArray

      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v BYTEA);")
        db.getTable("t").get.insert(Map("v" -> ByteaValue(bigData)), None)
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        val result = table.data(0).data(0).asInstanceOf[ByteaValue].data
        result shouldBe bigData
        db.close()
      }
    }

    "TEXT exactly at inline threshold (64 bytes)" in {
      val exact = "a" * 64

      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v TEXT);")
        db.getTable("t").get.insert(Map("v" -> TextValue(exact)), None)
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TextValue(exact)
        db.close()
      }
    }

    "TEXT one byte over inline threshold (65 bytes)" in {
      val overBy1 = "a" * 65

      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v TEXT);")
        db.getTable("t").get.insert(Map("v" -> TextValue(overBy1)), None)
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TextValue(overBy1)
        db.close()
      }
    }

    "very large TEXT spanning multiple chain pages" in {
      val huge = "x" * 20000 // ~5 chain pages at 4096 page size

      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v TEXT);")
        db.getTable("t").get.insert(Map("v" -> TextValue(huge)), None)
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TextValue(huge)
        db.close()
      }
    }
  }

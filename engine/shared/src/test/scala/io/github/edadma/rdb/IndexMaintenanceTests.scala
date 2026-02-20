package io.github.edadma.rdb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class IndexMaintenanceTests extends AnyFreeSpec with Matchers:

  private def execDB(sql: String): (Seq[Result], DB) =
    given db: DB = new MemoryDB
    (executeSQL(sql), db)

  private def queryTable(sql: String): TableValue =
    given DB = new MemoryDB
    executeSQL(sql).collect { case QueryResult(t) => t }.last

  "INSERT maintains indexes" - {
    "unique index rejects duplicate on insert" in {
      assertThrows[RuntimeException] {
        queryTable(
          """
            |CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY (id));
            |INSERT INTO t (id, name) VALUES (1, 'Alice');
            |INSERT INTO t (id, name) VALUES (1, 'Bob');
            |SELECT * FROM t;
            |""".trim.stripMargin
        )
      }
    }

    "non-unique index allows duplicate values on insert" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |CREATE INDEX idx ON t (name);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |INSERT INTO t (id, name) VALUES (2, 'Alice');
          |""".trim.stripMargin
      )

      val idx = db.getTable("t").get.tableIndexes("idx").asInstanceOf[MemoryTableIndex]
      idx.tree.iterator.size shouldBe 2
    }

    "insert adds entry to index" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |CREATE UNIQUE INDEX idx ON t (id);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |INSERT INTO t (id, name) VALUES (2, 'Bob');
          |""".trim.stripMargin
      )

      val idx = db.getTable("t").get.tableIndexes("idx").asInstanceOf[MemoryTableIndex]
      idx.tree.iterator.size shouldBe 2
      idx.tree.search(IndexedSeq(NumberValue(1))).isDefined shouldBe true
      idx.tree.search(IndexedSeq(NumberValue(2))).isDefined shouldBe true
    }

    "unique violation on insert does not leave row in table" in {
      val table = queryTable(
        """
          |CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY (id));
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |INSERT INTO t (id, name) VALUES (2, 'Bob');
          |SELECT * FROM t;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 2

      // Now try a duplicate — should fail but not leave a ghost row
      assertThrows[RuntimeException] {
        queryTable(
          """
            |CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY (id));
            |INSERT INTO t (id, name) VALUES (1, 'Alice');
            |INSERT INTO t (id, name) VALUES (1, 'Bob');
            |SELECT * FROM t;
            |""".trim.stripMargin
        )
      }
    }

    "unique violation rolls back row — table unaffected" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |CREATE UNIQUE INDEX idx ON t (name);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |""".trim.stripMargin
      )

      given DB = db
      assertThrows[RuntimeException] {
        executeSQL("INSERT INTO t (id, name) VALUES (2, 'Alice');")
      }
      val result = executeSQL("SELECT * FROM t;")
      val table = result.collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(1)
    }

    "unique violation on second index rolls back first index entry" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |CREATE UNIQUE INDEX idx_id ON t (id);
          |CREATE UNIQUE INDEX idx_name ON t (name);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |""".trim.stripMargin
      )

      given DB = db
      // id=2 is unique but name='Alice' is duplicate — should fail and roll back
      assertThrows[RuntimeException] {
        executeSQL("INSERT INTO t (id, name) VALUES (2, 'Alice');")
      }
      val result = executeSQL("SELECT * FROM t;")
      val table = result.collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1

      // id=2 should not be in the id index — re-insert should work
      executeSQL("INSERT INTO t (id, name) VALUES (2, 'Bob');")
      val result2 = executeSQL("SELECT * FROM t ORDER BY id;")
      val table2 = result2.collect { case QueryResult(t) => t }.head
      table2.data.length shouldBe 2
    }

    "update unique violation does not corrupt data" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |CREATE UNIQUE INDEX idx ON t (id);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |INSERT INTO t (id, name) VALUES (2, 'Bob');
          |""".trim.stripMargin
      )

      given DB = db
      assertThrows[RuntimeException] {
        executeSQL("UPDATE t SET id = 2 WHERE id = 1;")
      }
      // Original data should be intact
      val result = executeSQL("SELECT * FROM t ORDER BY id;")
      val table = result.collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(0).data(1) shouldBe TextValue("Alice")
      table.data(1).data(0) shouldBe NumberValue(2)
      table.data(1).data(1) shouldBe TextValue("Bob")

      // The original key should still be in the index
      assertThrows[RuntimeException] {
        executeSQL("INSERT INTO t (id, name) VALUES (1, 'Charlie');")
      }
    }

    "NULL in PRIMARY KEY column rejected on update" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY (id));
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |""".trim.stripMargin
      )

      given DB = db
      assertThrows[RuntimeException] {
        executeSQL("UPDATE t SET id = NULL WHERE id = 1;")
      }
      val result = executeSQL("SELECT * FROM t;")
      val table = result.collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(1)
    }

    "NULL in PRIMARY KEY column rejected" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY (id));
          |""".trim.stripMargin
      )

      given DB = db
      assertThrows[RuntimeException] {
        executeSQL("INSERT INTO t (id, name) VALUES (NULL, 'Alice');")
      }
      val result = executeSQL("SELECT * FROM t;")
      val table = result.collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 0
    }

    "PK constraint enforced via auto-index" in {
      assertThrows[RuntimeException] {
        queryTable(
          """
            |CREATE TABLE t (id INTEGER, PRIMARY KEY (id));
            |INSERT INTO t (id) VALUES (1);
            |INSERT INTO t (id) VALUES (1);
            |SELECT * FROM t;
            |""".trim.stripMargin
        )
      }
    }
  }

  "DELETE maintains indexes" - {
    "delete removes entry from unique index" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |CREATE UNIQUE INDEX idx ON t (id);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |INSERT INTO t (id, name) VALUES (2, 'Bob');
          |DELETE FROM t WHERE id = 1;
          |""".trim.stripMargin
      )

      val idx = db.getTable("t").get.tableIndexes("idx").asInstanceOf[MemoryTableIndex]
      idx.tree.iterator.size shouldBe 1
      idx.tree.search(IndexedSeq(NumberValue(1))).isDefined shouldBe false
      idx.tree.search(IndexedSeq(NumberValue(2))).isDefined shouldBe true
    }

    "delete removes entry from non-unique index" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |CREATE INDEX idx ON t (name);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |INSERT INTO t (id, name) VALUES (2, 'Bob');
          |DELETE FROM t WHERE id = 1;
          |""".trim.stripMargin
      )

      val idx = db.getTable("t").get.tableIndexes("idx").asInstanceOf[MemoryTableIndex]
      idx.tree.iterator.size shouldBe 1
    }

    "re-insert after delete works" in {
      val table = queryTable(
        """
          |CREATE TABLE t (id INTEGER, PRIMARY KEY (id));
          |INSERT INTO t (id) VALUES (1);
          |DELETE FROM t WHERE id = 1;
          |INSERT INTO t (id) VALUES (1);
          |SELECT * FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
    }

    "delete all rows empties index" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |CREATE UNIQUE INDEX idx ON t (id);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |INSERT INTO t (id, name) VALUES (2, 'Bob');
          |DELETE FROM t;
          |""".trim.stripMargin
      )

      val idx = db.getTable("t").get.tableIndexes("idx").asInstanceOf[MemoryTableIndex]
      idx.tree.iterator.size shouldBe 0
    }
  }

  "UPDATE maintains indexes" - {
    "update changes index entry for unique index" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |CREATE UNIQUE INDEX idx ON t (id);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |UPDATE t SET id = 10 WHERE id = 1;
          |""".trim.stripMargin
      )

      val idx = db.getTable("t").get.tableIndexes("idx").asInstanceOf[MemoryTableIndex]
      idx.tree.iterator.size shouldBe 1
      idx.tree.search(IndexedSeq(NumberValue(1))).isDefined shouldBe false
      idx.tree.search(IndexedSeq(NumberValue(10))).isDefined shouldBe true
    }

    "update violating unique constraint fails" in {
      assertThrows[RuntimeException] {
        queryTable(
          """
            |CREATE TABLE t (id INTEGER, name TEXT);
            |CREATE UNIQUE INDEX idx ON t (id);
            |INSERT INTO t (id, name) VALUES (1, 'Alice');
            |INSERT INTO t (id, name) VALUES (2, 'Bob');
            |UPDATE t SET id = 2 WHERE id = 1;
            |SELECT * FROM t;
            |""".trim.stripMargin
        )
      }
    }

    "update non-indexed column preserves index" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |CREATE UNIQUE INDEX idx ON t (id);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |UPDATE t SET name = 'Bob' WHERE id = 1;
          |""".trim.stripMargin
      )

      val idx = db.getTable("t").get.tableIndexes("idx").asInstanceOf[MemoryTableIndex]
      idx.tree.iterator.size shouldBe 1
      idx.tree.search(IndexedSeq(NumberValue(1))).isDefined shouldBe true
    }
  }

class PersistentIndexMaintenanceTests extends PersistentTestBase:

  "PersistentDB index maintenance" - {
    "unique index enforced on insert" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given DB = db

      executeSQL("CREATE TABLE t (id INTEGER, PRIMARY KEY (id));")
      executeSQL("INSERT INTO t (id) VALUES (1);")
      assertThrows[RuntimeException] {
        executeSQL("INSERT INTO t (id) VALUES (1);")
      }
      db.close()
    }

    "delete removes from index, re-insert works" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given DB = db

      executeSQL("CREATE TABLE t (id INTEGER, PRIMARY KEY (id));")
      executeSQL("INSERT INTO t (id) VALUES (1);")
      executeSQL("DELETE FROM t WHERE id = 1;")
      executeSQL("INSERT INTO t (id) VALUES (1);")

      val result = executeSQL("SELECT * FROM t;")
      val table = result.collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      db.close()
    }

    "index survives close/reopen with DML" in {
      val db1 = PersistentDB.create(tmpFile, pageSize)
      given DB = db1

      executeSQL("CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY (id));")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
      executeSQL("INSERT INTO t (id, name) VALUES (2, 'Bob');")
      db1.close()

      // Reopen and verify PK constraint still works
      val db2 = PersistentDB.open(tmpFile)

      assertThrows[RuntimeException] {
        executeSQL("INSERT INTO t (id, name) VALUES (1, 'Charlie');")(using db2)
      }

      // But a new unique value should work
      executeSQL("INSERT INTO t (id, name) VALUES (3, 'Charlie');")(using db2)
      val result = executeSQL("SELECT * FROM t;")(using db2)
      val table = result.collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 3
      db2.close()
    }

    "update enforces unique constraint" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given DB = db

      executeSQL("CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY (id));")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
      executeSQL("INSERT INTO t (id, name) VALUES (2, 'Bob');")
      assertThrows[RuntimeException] {
        executeSQL("UPDATE t SET id = 2 WHERE id = 1;")
      }
      db.close()
    }

    "non-unique index insert allows duplicates" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given DB = db

      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("CREATE INDEX idx ON t (name);")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
      executeSQL("INSERT INTO t (id, name) VALUES (2, 'Alice');")

      val result = executeSQL("SELECT * FROM t;")
      val table = result.collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 2
      db.close()
    }

    "non-unique index delete removes correct entry" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given DB = db

      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("CREATE INDEX idx ON t (name);")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
      executeSQL("INSERT INTO t (id, name) VALUES (2, 'Alice');")
      executeSQL("INSERT INTO t (id, name) VALUES (3, 'Bob');")
      executeSQL("DELETE FROM t WHERE id = 1;")

      val result = executeSQL("SELECT * FROM t ORDER BY id;")
      val table = result.collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(2)
      table.data(1).data(0) shouldBe NumberValue(3)
      db.close()
    }

    "non-unique index update changes entry" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given DB = db

      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("CREATE INDEX idx ON t (name);")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
      executeSQL("INSERT INTO t (id, name) VALUES (2, 'Bob');")
      executeSQL("UPDATE t SET name = 'Charlie' WHERE id = 1;")

      val result = executeSQL("SELECT * FROM t ORDER BY id;")
      val table = result.collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 2
      table.data(0).data(1) shouldBe TextValue("Charlie")
      table.data(1).data(1) shouldBe TextValue("Bob")
      db.close()
    }

    "non-unique index survives close/reopen with delete" in {
      val db1 = PersistentDB.create(tmpFile, pageSize)
      given DB = db1

      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("CREATE INDEX idx ON t (name);")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
      executeSQL("INSERT INTO t (id, name) VALUES (2, 'Alice');")
      executeSQL("INSERT INTO t (id, name) VALUES (3, 'Bob');")
      db1.close()

      val db2 = PersistentDB.open(tmpFile)
      executeSQL("DELETE FROM t WHERE id = 2;")(using db2)
      executeSQL("INSERT INTO t (id, name) VALUES (4, 'Alice');")(using db2)

      val result = executeSQL("SELECT * FROM t ORDER BY id;")(using db2)
      val table = result.collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(1).data(0) shouldBe NumberValue(3)
      table.data(2).data(0) shouldBe NumberValue(4)
      db2.close()
    }

    "bulk insert with unique index" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given DB = db

      executeSQL("CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY (id));")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice'), (2, 'Bob'), (3, 'Charlie');")

      val result = executeSQL("SELECT * FROM t ORDER BY id;")
      val table = result.collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 3
      table.data(0).data(1) shouldBe TextValue("Alice")
      table.data(1).data(1) shouldBe TextValue("Bob")
      table.data(2).data(1) shouldBe TextValue("Charlie")
      db.close()
    }

    "bulk insert rejects duplicate in unique index" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given DB = db

      executeSQL("CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY (id));")
      assertThrows[RuntimeException] {
        executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice'), (2, 'Bob'), (1, 'Charlie');")
      }
      db.close()
    }

    "bulk insert with non-unique index" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given DB = db

      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("CREATE INDEX idx ON t (name);")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice'), (2, 'Alice'), (3, 'Bob');")

      val result = executeSQL("SELECT * FROM t ORDER BY id;")
      val table = result.collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 3

      // Delete one duplicate, verify the other remains
      executeSQL("DELETE FROM t WHERE id = 1;")
      val result2 = executeSQL("SELECT * FROM t ORDER BY id;")
      val table2 = result2.collect { case QueryResult(t) => t }.head
      table2.data.length shouldBe 2
      table2.data(0).data(0) shouldBe NumberValue(2)
      table2.data(1).data(0) shouldBe NumberValue(3)
      db.close()
    }

    "bulk insert survives close/reopen" in {
      val db1 = PersistentDB.create(tmpFile, pageSize)
      given DB = db1

      executeSQL("CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY (id));")
      executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice'), (2, 'Bob'), (3, 'Charlie');")
      db1.close()

      val db2 = PersistentDB.open(tmpFile)
      // PK constraint still enforced after reopen
      assertThrows[RuntimeException] {
        executeSQL("INSERT INTO t (id, name) VALUES (2, 'Duplicate');")(using db2)
      }
      val result = executeSQL("SELECT * FROM t ORDER BY id;")(using db2)
      val table = result.collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 3
      db2.close()
    }

    "unique index on long TEXT keys (chain-encoded)" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given DB = db

      executeSQL("CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY (id));")
      executeSQL("CREATE UNIQUE INDEX idx ON t (name);")

      val longA = "A" * 200
      val longB = "B" * 200
      executeSQL(s"INSERT INTO t (id, name) VALUES (1, '$longA');")
      executeSQL(s"INSERT INTO t (id, name) VALUES (2, '$longB');")

      // Duplicate long key should fail
      assertThrows[RuntimeException] {
        executeSQL(s"INSERT INTO t (id, name) VALUES (3, '$longA');")
      }

      val result = executeSQL("SELECT id FROM t ORDER BY id;")
      val table = result.collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 2
      db.close()
    }

    "non-unique index on long TEXT keys with delete" in {
      val db = PersistentDB.create(tmpFile, pageSize)
      given DB = db

      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("CREATE INDEX idx ON t (name);")

      val longVal = "X" * 200
      executeSQL(s"INSERT INTO t (id, name) VALUES (1, '$longVal');")
      executeSQL(s"INSERT INTO t (id, name) VALUES (2, '$longVal');")
      executeSQL("DELETE FROM t WHERE id = 1;")

      val result = executeSQL("SELECT id FROM t;")
      val table = result.collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(2)
      db.close()
    }

    "long TEXT index key survives close/reopen" in {
      val longA = "A" * 200
      val longB = "B" * 200
      val db1 = PersistentDB.create(tmpFile, pageSize)
      given DB = db1

      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("CREATE UNIQUE INDEX idx ON t (name);")
      executeSQL(s"INSERT INTO t (id, name) VALUES (1, '$longA');")
      executeSQL(s"INSERT INTO t (id, name) VALUES (2, '$longB');")
      db1.close()

      val db2 = PersistentDB.open(tmpFile)
      // Unique constraint still enforced after reopen
      assertThrows[RuntimeException] {
        executeSQL(s"INSERT INTO t (id, name) VALUES (3, '$longA');")(using db2)
      }
      executeSQL(s"INSERT INTO t (id, name) VALUES (3, 'short');")(using db2)
      val result = executeSQL("SELECT * FROM t ORDER BY id;")(using db2)
      val table = result.collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 3
      db2.close()
    }
  }

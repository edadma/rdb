package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.github.edadma.cross_platform.{createTempFile, deleteFile}
class IndexDDLTests extends AnyFreeSpec with Matchers:

  private def exec(sql: String): Seq[Result] =
    given Session = new MemoryDB().connect()
    executeSQL(sql)

  private def execDB(sql: String): (Seq[Result], DB) =
    given session: Session = new MemoryDB().connect()
    (executeSQL(sql), session.db)

  "CREATE INDEX" - {
    "creates a non-unique index on a table" in {
      val results = exec(
        """
          |CREATE TABLE users (id INTEGER, name TEXT);
          |CREATE INDEX idx_users_name ON users (name);
          |""".trim.stripMargin
      )

      results should contain(CreateIndexResult("idx_users_name"))
    }

    "creates a unique index on a table" in {
      val results = exec(
        """
          |CREATE TABLE users (id INTEGER, name TEXT);
          |CREATE UNIQUE INDEX idx_users_id ON users (id);
          |""".trim.stripMargin
      )

      results should contain(CreateIndexResult("idx_users_id"))
    }

    "creates a multi-column index" in {
      val results = exec(
        """
          |CREATE TABLE orders (customer_id INTEGER, product_id INTEGER, qty INTEGER);
          |CREATE INDEX idx_orders_cust_prod ON orders (customer_id, product_id);
          |""".trim.stripMargin
      )

      results should contain(CreateIndexResult("idx_orders_cust_prod"))
    }

    "index is registered on the DB" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE users (id INTEGER, name TEXT);
          |CREATE INDEX idx_users_name ON users (name);
          |""".trim.stripMargin
      )

      db.hasIndex("idx_users_name") shouldBe true
      db.hasIndex("nonexistent") shouldBe false
    }

    "index is registered on the table" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE users (id INTEGER, name TEXT);
          |CREATE INDEX idx_users_name ON users (name);
          |""".trim.stripMargin
      )

      val table = db.getTable("users").get
      table.tableIndexes.contains("idx_users_name") shouldBe true
    }

    "fails on non-existent table" in {
      assertThrows[RuntimeException] {
        exec("CREATE INDEX idx ON nonexistent (col);")
      }
    }

    "fails on non-existent column" in {
      assertThrows[RuntimeException] {
        exec(
          """
            |CREATE TABLE t (id INTEGER);
            |CREATE INDEX idx ON t (nonexistent);
            |""".trim.stripMargin
        )
      }
    }

    "fails on duplicate index name" in {
      assertThrows[RuntimeException] {
        exec(
          """
            |CREATE TABLE t (id INTEGER, name TEXT);
            |CREATE INDEX idx ON t (id);
            |CREATE INDEX idx ON t (name);
            |""".trim.stripMargin
        )
      }
    }

    "populates index from existing data" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |INSERT INTO t (id, name) VALUES (2, 'Bob');
          |CREATE UNIQUE INDEX idx_t_id ON t (id);
          |""".trim.stripMargin
      )

      db.hasIndex("idx_t_id") shouldBe true
      val table = db.getTable("t").get
      val idx = table.tableIndexes("idx_t_id").asInstanceOf[MemoryTableIndex]
      // The tree should have 2 entries
      idx.tree.iterator.size shouldBe 2
    }

    "unique index rejects duplicate values on creation" in {
      assertThrows[RuntimeException] {
        exec(
          """
            |CREATE TABLE t (id INTEGER, name TEXT);
            |INSERT INTO t (id, name) VALUES (1, 'Alice');
            |INSERT INTO t (id, name) VALUES (1, 'Bob');
            |CREATE UNIQUE INDEX idx_t_id ON t (id);
            |""".trim.stripMargin
        )
      }
    }

    "non-unique index allows duplicate values" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |INSERT INTO t (id, name) VALUES (1, 'Bob');
          |CREATE INDEX idx_t_id ON t (id);
          |""".trim.stripMargin
      )

      db.hasIndex("idx_t_id") shouldBe true
      val table = db.getTable("t").get
      val idx = table.tableIndexes("idx_t_id").asInstanceOf[MemoryTableIndex]
      idx.tree.iterator.size shouldBe 2
    }
  }

  "CREATE INDEX ... USING" - {
    "USING btree succeeds" in {
      val results = exec(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |CREATE INDEX idx ON t USING btree (name);
          |""".trim.stripMargin
      )
      results should contain(CreateIndexResult("idx"))
    }

    "USING BTREE (case insensitive) succeeds" in {
      val results = exec(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |CREATE INDEX idx ON t USING BTREE (name);
          |""".trim.stripMargin
      )
      results should contain(CreateIndexResult("idx"))
    }

    "USING with unique index succeeds" in {
      val results = exec(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |CREATE UNIQUE INDEX idx ON t USING btree (id);
          |""".trim.stripMargin
      )
      results should contain(CreateIndexResult("idx"))
    }

    "USING hash fails" in {
      assertThrows[SchemaException] {
        exec(
          """
            |CREATE TABLE t (id INTEGER);
            |CREATE INDEX idx ON t USING hash (id);
            |""".trim.stripMargin
        )
      }
    }

    "USING gin fails" in {
      assertThrows[SchemaException] {
        exec(
          """
            |CREATE TABLE t (id INTEGER);
            |CREATE INDEX idx ON t USING gin (id);
            |""".trim.stripMargin
        )
      }
    }

    "USING gist fails" in {
      assertThrows[SchemaException] {
        exec(
          """
            |CREATE TABLE t (id INTEGER);
            |CREATE INDEX idx ON t USING gist (id);
            |""".trim.stripMargin
        )
      }
    }

    "omitting USING defaults to btree (existing behavior)" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |CREATE INDEX idx ON t (name);
          |""".trim.stripMargin
      )
      db.hasIndex("idx") shouldBe true
    }

    "USING btree multi-column index" in {
      val results = exec(
        """
          |CREATE TABLE t (a INTEGER, b INTEGER, c TEXT);
          |CREATE INDEX idx ON t USING btree (a, b);
          |""".trim.stripMargin
      )
      results should contain(CreateIndexResult("idx"))
    }
  }

  "DROP INDEX" - {
    "drops an existing index" in {
      val (results, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |CREATE INDEX idx_t_name ON t (name);
          |DROP INDEX idx_t_name;
          |""".trim.stripMargin
      )

      results.last shouldBe DropIndexResult("idx_t_name")
      db.hasIndex("idx_t_name") shouldBe false
    }

    "DROP INDEX IF EXISTS on non-existent index succeeds" in {
      val results = exec("DROP INDEX IF EXISTS nonexistent;")
      results should contain(DropIndexResult("nonexistent"))
    }

    "DROP INDEX on non-existent index fails" in {
      assertThrows[RuntimeException] {
        exec("DROP INDEX nonexistent;")
      }
    }

    "cleans up table index references on drop" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER);
          |CREATE INDEX idx_t_id ON t (id);
          |DROP INDEX idx_t_id;
          |""".trim.stripMargin
      )

      val table = db.getTable("t").get
      table.tableIndexes.contains("idx_t_id") shouldBe false
    }
  }

  "DROP TABLE cleans up indexes" - {
    "indexes for table are removed on drop" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |CREATE INDEX idx_t_id ON t (id);
          |CREATE INDEX idx_t_name ON t (name);
          |DROP TABLE t;
          |""".trim.stripMargin
      )

      db.hasIndex("idx_t_id") shouldBe false
      db.hasIndex("idx_t_name") shouldBe false
    }
  }

  "PRIMARY KEY auto-index" - {
    "auto-creates unique index for primary key" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY (id));
          |""".trim.stripMargin
      )

      db.hasIndex("t_pkey") shouldBe true
      val table = db.getTable("t").get
      table.tableIndexes.contains("t_pkey") shouldBe true
    }

    "auto-created PK index is unique" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, PRIMARY KEY (id));
          |""".trim.stripMargin
      )

      val table = db.getTable("t").get
      val idx = table.tableIndexes("t_pkey")
      idx.meta.unique shouldBe true
    }

    "PK index is cleaned up on drop table" in {
      val (_, db) = execDB(
        """
          |CREATE TABLE t (id INTEGER, PRIMARY KEY (id));
          |DROP TABLE t;
          |""".trim.stripMargin
      )

      db.hasIndex("t_pkey") shouldBe false
    }
  }

class PersistentIndexDDLTests extends PersistentTestBase:

  "PersistentDB CREATE INDEX" - {
    "creates index and persists across close/reopen" in {
      val db1 = PersistentDB.create(tmpFile, pageSize)
      given Session = db1.connect()

      executeSQL(
        """
          |CREATE TABLE users (id INTEGER, name TEXT);
          |INSERT INTO users (id, name) VALUES (1, 'Alice');
          |INSERT INTO users (id, name) VALUES (2, 'Bob');
          |CREATE UNIQUE INDEX idx_users_id ON users (id);
          |""".trim.stripMargin
      )

      db1.hasIndex("idx_users_id") shouldBe true
      db1.close()

      // Reopen and verify index persists
      val db2 = PersistentDB.open(tmpFile)
      db2.hasIndex("idx_users_id") shouldBe true

      val table = db2.getTable("users").get
      table.tableIndexes.contains("idx_users_id") shouldBe true

      val idx = table.tableIndexes("idx_users_id")
      idx.meta.unique shouldBe true
      idx.meta.columns shouldBe Seq("id")
      db2.close()
    }

    "PK auto-index persists across close/reopen" in {
      val db1 = PersistentDB.create(tmpFile, pageSize)
      given Session = db1.connect()

      executeSQL("CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY (id));")
      db1.hasIndex("t_pkey") shouldBe true
      db1.close()

      val db2 = PersistentDB.open(tmpFile)
      db2.hasIndex("t_pkey") shouldBe true

      val table = db2.getTable("t").get
      table.tableIndexes.contains("t_pkey") shouldBe true
      table.tableIndexes("t_pkey").meta.unique shouldBe true
      db2.close()
    }

    "DROP INDEX persists across close/reopen" in {
      val db1 = PersistentDB.create(tmpFile, pageSize)
      given Session = db1.connect()

      executeSQL(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |CREATE INDEX idx ON t (name);
          |DROP INDEX idx;
          |""".trim.stripMargin
      )

      db1.hasIndex("idx") shouldBe false
      db1.close()

      val db2 = PersistentDB.open(tmpFile)
      db2.hasIndex("idx") shouldBe false
      db2.close()
    }
  }

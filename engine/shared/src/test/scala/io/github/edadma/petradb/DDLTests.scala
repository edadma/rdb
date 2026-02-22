package io.github.edadma.petradb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import java.io.{ByteArrayOutputStream, PrintStream}

class DDLTests extends AnyFreeSpec with Matchers:

  /** Suppresses stderr output during block execution (for expected error messages). */
  private def suppressStderr[A](block: => A): A =
    val devNull = new PrintStream(new ByteArrayOutputStream())
    val oldErr = Console.err
    Console.withErr(devNull)(block)

  private def test(sql: String): String =
    given Session = new MemoryDB().connect()
    try {
      executeSQL(sql).toString
    } catch {
      case e: RuntimeException => e.getMessage
    }

  private def testExpectingError(sql: String): String =
    given Session = new MemoryDB().connect()
    suppressStderr {
      try {
        executeSQL(sql).toString
      } catch {
        case e: RuntimeException => e.getMessage
      }
    }

  private def testExpectingException(sql: String): Unit =
    given Session = new MemoryDB().connect()
    suppressStderr { executeSQL(sql) }

  private def query(sql: String): TableValue =
    given Session = new MemoryDB().connect()
    executeSQL(sql).collect { case QueryResult(t) => t }.last

  "ALTER TABLE ADD COLUMN" - {
    "adds column to empty table" in {
      val result = test(
        """
          |CREATE TABLE users (id SERIAL, PRIMARY KEY (id));
          |ALTER TABLE users ADD COLUMN name TEXT;
          |INSERT INTO users (name) VALUES ('Alice');
          |SELECT name FROM users;
          |""".trim.stripMargin
      )

      result should include("AlterTableResult")
      result should include("Alice")
    }

    "adds column to table with existing data" in {
      val table = query(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |INSERT INTO t (id, name) VALUES (2, 'Bob');
          |ALTER TABLE t ADD COLUMN age INTEGER;
          |SELECT id, name, age FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(2).isNull shouldBe true
      table.data(1).data(2).isNull shouldBe true
    }

    "adds column with default value populating existing rows" in {
      val table = query(
        """
          |CREATE TABLE t (id INTEGER);
          |INSERT INTO t (id) VALUES (1);
          |INSERT INTO t (id) VALUES (2);
          |ALTER TABLE t ADD COLUMN status TEXT DEFAULT 'active';
          |SELECT id, status FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(1) shouldBe TextValue("active")
      table.data(1).data(1) shouldBe TextValue("active")
    }

    "insert uses new column after add" in {
      val table = query(
        """
          |CREATE TABLE t (id INTEGER);
          |ALTER TABLE t ADD COLUMN name TEXT;
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |INSERT INTO t (id, name) VALUES (2, 'Bob');
          |SELECT id, name FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(1) shouldBe TextValue("Alice")
      table.data(1).data(1) shouldBe TextValue("Bob")
    }

    "fails when adding duplicate column" in {
      assertThrows[RuntimeException] {
        testExpectingException(
          """
            |CREATE TABLE t (id INTEGER, name TEXT);
            |ALTER TABLE t ADD COLUMN name TEXT;
            |""".trim.stripMargin
        )
      }
    }
  }

  "ALTER TABLE DROP COLUMN" - {
    "drops column from table" in {
      val table = query(
        """
          |CREATE TABLE t (id INTEGER, name TEXT, email TEXT);
          |INSERT INTO t (id, name, email) VALUES (1, 'Alice', 'alice@test.com');
          |ALTER TABLE t DROP COLUMN email;
          |SELECT id, name FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.meta.width shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(0).data(1) shouldBe TextValue("Alice")
    }

    "drops middle column preserving data" in {
      val table = query(
        """
          |CREATE TABLE t (a INTEGER, b TEXT, c INTEGER);
          |INSERT INTO t (a, b, c) VALUES (1, 'x', 10);
          |INSERT INTO t (a, b, c) VALUES (2, 'y', 20);
          |ALTER TABLE t DROP COLUMN b;
          |SELECT a, c FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.meta.width shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(0).data(1) shouldBe NumberValue(10)
      table.data(1).data(0) shouldBe NumberValue(2)
      table.data(1).data(1) shouldBe NumberValue(20)
    }

    "fails when dropping primary key column" in {
      assertThrows[RuntimeException] {
        testExpectingException(
          """
            |CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY (id));
            |ALTER TABLE t DROP COLUMN id;
            |""".trim.stripMargin
        )
      }
    }

    "fails when dropping non-existent column" in {
      assertThrows[RuntimeException] {
        testExpectingException(
          """
            |CREATE TABLE t (id INTEGER);
            |ALTER TABLE t DROP COLUMN nonexistent;
            |""".trim.stripMargin
        )
      }
    }
  }

  "ALTER TABLE ALTER COLUMN" - {
    "changes column type with data conversion" in {
      val table = query(
        """
          |CREATE TABLE t (id INTEGER, value INTEGER);
          |INSERT INTO t (id, value) VALUES (1, 42);
          |ALTER TABLE t ALTER COLUMN value TYPE TEXT;
          |SELECT id, value FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("42")
    }

    "sets default value" in {
      val table = query(
        """
          |CREATE TABLE t (id INTEGER, status TEXT);
          |ALTER TABLE t ALTER COLUMN status SET DEFAULT 'active';
          |INSERT INTO t (id) VALUES (1);
          |SELECT id, status FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("active")
    }

    "drops default value" in {
      val table = query(
        """
          |CREATE TABLE t (id INTEGER, status TEXT DEFAULT 'active');
          |ALTER TABLE t ALTER COLUMN status DROP DEFAULT;
          |INSERT INTO t (id) VALUES (1);
          |SELECT id, status FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(1).isNull shouldBe true
    }

    "sets NOT NULL constraint" in {
      assertThrows[RuntimeException] {
        testExpectingException(
          """
            |CREATE TABLE t (id INTEGER, name TEXT);
            |ALTER TABLE t ALTER COLUMN name SET NOT NULL;
            |INSERT INTO t (id) VALUES (1);
            |""".trim.stripMargin
        )
      }
    }

    "SET NOT NULL fails when column has existing nulls" in {
      assertThrows[RuntimeException] {
        testExpectingException(
          """
            |CREATE TABLE t (id INTEGER, name TEXT);
            |INSERT INTO t (id) VALUES (1);
            |ALTER TABLE t ALTER COLUMN name SET NOT NULL;
            |""".trim.stripMargin
        )
      }
    }

    "drops NOT NULL constraint" in {
      val table = query(
        """
          |CREATE TABLE t (id INTEGER, name TEXT NOT NULL);
          |ALTER TABLE t ALTER COLUMN name DROP NOT NULL;
          |INSERT INTO t (id) VALUES (1);
          |SELECT id, name FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(1).isNull shouldBe true
    }

    "fails on non-existent column" in {
      assertThrows[RuntimeException] {
        testExpectingException(
          """
            |CREATE TABLE t (id INTEGER);
            |ALTER TABLE t ALTER COLUMN nonexistent TYPE TEXT;
            |""".trim.stripMargin
        )
      }
    }
  }

  "ALTER TABLE ADD/DROP CONSTRAINT" - {
    "adds unique constraint" in {
      val result = test(
        """
          |CREATE TABLE users (id SERIAL, name TEXT, PRIMARY KEY (id));
          |ALTER TABLE users ADD CONSTRAINT unique_name UNIQUE (name);
          |""".trim.stripMargin
      )

      result should include("AlterTableResult")
    }

    "adds primary key constraint" in {
      val result = test(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |ALTER TABLE t ADD CONSTRAINT pk_t PRIMARY KEY (id);
          |""".trim.stripMargin
      )

      result should include("AlterTableResult")
    }

    "adds foreign key constraint" in {
      val result = test(
        """
          |CREATE TABLE parents (id INTEGER, PRIMARY KEY (id));
          |CREATE TABLE children (id INTEGER, parent_id INTEGER);
          |ALTER TABLE children ADD CONSTRAINT fk_parent FOREIGN KEY (parent_id) REFERENCES parents (id);
          |""".trim.stripMargin
      )

      result should include("AlterTableResult")
    }

    "fails adding second primary key" in {
      assertThrows[RuntimeException] {
        testExpectingException(
          """
            |CREATE TABLE t (id INTEGER, PRIMARY KEY (id));
            |ALTER TABLE t ADD CONSTRAINT pk2 PRIMARY KEY (id);
            |""".trim.stripMargin
        )
      }
    }

    "drops constraint" in {
      val result = test(
        """
          |CREATE TABLE users (
          |  id SERIAL,
          |  name TEXT,
          |  PRIMARY KEY (id),
          |  CONSTRAINT unique_name UNIQUE (name)
          |);
          |ALTER TABLE users DROP CONSTRAINT unique_name;
          |""".trim.stripMargin
      )

      result should include("AlterTableResult")
    }

    "fails dropping non-existent constraint" in {
      assertThrows[RuntimeException] {
        testExpectingException(
          """
            |CREATE TABLE t (id INTEGER);
            |ALTER TABLE t DROP CONSTRAINT nonexistent;
            |""".trim.stripMargin
        )
      }
    }
  }

  "ALTER TABLE RENAME TO" - {
    "renames table" in {
      val table = query(
        """
          |CREATE TABLE users (id INTEGER, name TEXT);
          |INSERT INTO users (id, name) VALUES (1, 'Alice');
          |ALTER TABLE users RENAME TO customers;
          |SELECT id, name FROM customers;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("Alice")
    }

    "old name no longer works after rename" in {
      assertThrows[RuntimeException] {
        testExpectingException(
          """
            |CREATE TABLE users (id INTEGER);
            |ALTER TABLE users RENAME TO customers;
            |SELECT * FROM users;
            |""".trim.stripMargin
        )
      }
    }
  }

  "ALTER TABLE RENAME COLUMN" - {
    "renames column" in {
      val table = query(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |ALTER TABLE t RENAME COLUMN name TO full_name;
          |SELECT id, full_name FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("Alice")
    }

    "old column name no longer works after rename" in {
      assertThrows[RuntimeException] {
        testExpectingException(
          """
            |CREATE TABLE t (id INTEGER, name TEXT);
            |ALTER TABLE t RENAME COLUMN name TO full_name;
            |SELECT name FROM t;
            |""".trim.stripMargin
        )
      }
    }

    "fails on non-existent column" in {
      assertThrows[RuntimeException] {
        testExpectingException(
          """
            |CREATE TABLE t (id INTEGER);
            |ALTER TABLE t RENAME COLUMN nonexistent TO new_name;
            |""".trim.stripMargin
        )
      }
    }
  }

  "Multiple sequential ALTER TABLE operations" - {
    "add column then drop it" in {
      val table = query(
        """
          |CREATE TABLE t (id INTEGER);
          |INSERT INTO t (id) VALUES (1);
          |ALTER TABLE t ADD COLUMN temp TEXT DEFAULT 'x';
          |ALTER TABLE t DROP COLUMN temp;
          |SELECT id FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.meta.width shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(1)
    }

    "rename table then rename column then query" in {
      val table = query(
        """
          |CREATE TABLE t (id INTEGER, name TEXT);
          |INSERT INTO t (id, name) VALUES (1, 'Alice');
          |ALTER TABLE t RENAME TO t2;
          |ALTER TABLE t2 RENAME COLUMN name TO label;
          |SELECT id, label FROM t2;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(0).data(1) shouldBe TextValue("Alice")
    }

    "add column then alter its type" in {
      val table = query(
        """
          |CREATE TABLE t (id INTEGER);
          |INSERT INTO t (id) VALUES (1);
          |ALTER TABLE t ADD COLUMN value INTEGER DEFAULT 42;
          |ALTER TABLE t ALTER COLUMN value TYPE TEXT;
          |SELECT id, value FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("42")
    }
  }

  "DROP commands" - {
    "parses DROP TABLE syntax" in {
      val result = test(
        """
          |CREATE TABLE test_table (id SERIAL, PRIMARY KEY (id));
          |DROP TABLE test_table;
          |""".trim.stripMargin
      )

      result should include("CreateTableResult")
      result should include("DropTableResult")
    }

    "CREATE TABLE IF NOT EXISTS on new table" in {
      val result = test(
        """
          |CREATE TABLE IF NOT EXISTS t (id INT);
          |""".trim.stripMargin
      )

      result should include("CreateTableResult")
    }

    "CREATE TABLE IF NOT EXISTS on existing table is no-op" in {
      val result = test(
        """
          |CREATE TABLE t (id INT);
          |CREATE TABLE IF NOT EXISTS t (id INT, name TEXT);
          |""".trim.stripMargin
      )

      result should include("CreateTableResult")
      result should not include "duplicate"
    }

    "CREATE TABLE without IF NOT EXISTS on existing table fails" in {
      assertThrows[RuntimeException] {
        testExpectingException(
          """
            |CREATE TABLE t (id INT);
            |CREATE TABLE t (id INT);
            |""".trim.stripMargin
        )
      }
    }

    "parses DROP TABLE IF EXISTS syntax" in {
      val result = test(
        """
          |DROP TABLE IF EXISTS nonexistent_table;
          |""".trim.stripMargin
      )

      result should include("DropTableResult")
    }

    "parses DROP TABLE CASCADE syntax" in {
      val result = test(
        """
          |CREATE TABLE test_table (id SERIAL, PRIMARY KEY (id));
          |DROP TABLE test_table CASCADE;
          |""".trim.stripMargin
      )

      result should include("CreateTableResult")
      result should include("DropTableResult")
    }

    "parses DROP INDEX syntax" in {
      val result = testExpectingError(
        """
          |DROP INDEX test_index;
          |""".trim.stripMargin
      )

      result should include("index 'test_index' not found")
    }

    "parses DROP INDEX IF EXISTS syntax" in {
      val result = test(
        """
          |DROP INDEX IF EXISTS nonexistent_index;
          |""".trim.stripMargin
      )

      result should include("DropIndexResult")
    }

    "parses DROP TYPE syntax" in {
      val result = test(
        """
          |CREATE TYPE color AS ENUM ('red', 'green', 'blue');
          |DROP TYPE color;
          |""".trim.stripMargin
      )

      result should include("CreateTypeResult")
      result should include("DropTypeResult")
    }

    "parses DROP TYPE IF EXISTS syntax" in {
      val result = test(
        """
          |DROP TYPE IF EXISTS nonexistent_type;
          |""".trim.stripMargin
      )

      result should include("DropTypeResult")
    }

    "parses DROP TYPE CASCADE syntax" in {
      val result = test(
        """
          |CREATE TYPE status AS ENUM ('active', 'inactive');
          |DROP TYPE status CASCADE;
          |""".trim.stripMargin
      )

      result should include("CreateTypeResult")
      result should include("DropTypeResult")
    }
  }

  "Error handling" - {
    "fails on DROP TABLE for non-existent table without IF EXISTS" in {
      assertThrows[RuntimeException] {
        testExpectingException("DROP TABLE nonexistent_table;")
      }
    }

    "fails on DROP TYPE for non-existent type without IF EXISTS" in {
      assertThrows[RuntimeException] {
        testExpectingException("DROP TYPE nonexistent_type;")
      }
    }

    "fails on ALTER TABLE for non-existent table" in {
      assertThrows[RuntimeException] {
        testExpectingException("ALTER TABLE nonexistent_table ADD COLUMN name TEXT;")
      }
    }
  }

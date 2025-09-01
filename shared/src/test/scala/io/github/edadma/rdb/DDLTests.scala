package io.github.edadma.rdb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DDLTests extends AnyFreeSpec with Matchers:

  private def test(sql: String): String =
    implicit val db: DB = new MemoryDB
    try {
      executeSQL(sql).toString
    } catch {
      case e: RuntimeException => e.getMessage
    }
  
  private def testExpectingException(sql: String): Unit =
    implicit val db: DB = new MemoryDB
    executeSQL(sql)

  "ALTER TABLE command" - {
    "parses ADD COLUMN syntax" in {
      val result = test(
        """
          |CREATE TABLE users (id SERIAL, PRIMARY KEY (id));
          |ALTER TABLE users ADD COLUMN name TEXT;
          |""".trim.stripMargin
      )
      
      result should include("ALTER TABLE ADD COLUMN not implemented yet")
    }

    "parses ADD CONSTRAINT syntax" in {
      val result = test(
        """
          |CREATE TABLE users (id SERIAL, name TEXT, PRIMARY KEY (id));
          |ALTER TABLE users ADD CONSTRAINT unique_name UNIQUE (name);
          |""".trim.stripMargin
      )
      
      result should include("ALTER TABLE ADD CONSTRAINT not implemented yet")
    }

    "parses DROP COLUMN syntax" in {
      val result = test(
        """
          |CREATE TABLE users (id SERIAL, name TEXT, email TEXT, PRIMARY KEY (id));
          |ALTER TABLE users DROP COLUMN email;
          |""".trim.stripMargin
      )
      
      result should include("ALTER TABLE DROP COLUMN not implemented yet")
    }

    "parses DROP CONSTRAINT syntax" in {
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
      
      result should include("ALTER TABLE DROP CONSTRAINT not implemented yet")
    }

    "parses ALTER COLUMN TYPE syntax" in {
      val result = test(
        """
          |CREATE TABLE users (id SERIAL, name TEXT, PRIMARY KEY (id));
          |ALTER TABLE users ALTER COLUMN name TYPE TEXT;
          |""".trim.stripMargin
      )
      
      result should include("ALTER TABLE ALTER COLUMN not implemented yet")
    }

    "parses ALTER COLUMN SET DEFAULT syntax" in {
      val result = test(
        """
          |CREATE TABLE users (id SERIAL, status TEXT, PRIMARY KEY (id));
          |ALTER TABLE users ALTER COLUMN status SET DEFAULT 'active';
          |""".trim.stripMargin
      )
      
      result should include("ALTER TABLE ALTER COLUMN not implemented yet")
    }

    "parses ALTER COLUMN DROP DEFAULT syntax" in {
      val result = test(
        """
          |CREATE TABLE users (id SERIAL, status TEXT DEFAULT 'inactive', PRIMARY KEY (id));
          |ALTER TABLE users ALTER COLUMN status DROP DEFAULT;
          |""".trim.stripMargin
      )
      
      result should include("ALTER TABLE ALTER COLUMN not implemented yet")
    }

    "parses RENAME TO syntax" in {
      val result = test(
        """
          |CREATE TABLE users (id SERIAL, PRIMARY KEY (id));
          |ALTER TABLE users RENAME TO customers;
          |""".trim.stripMargin
      )
      
      result should include("ALTER TABLE RENAME TO not implemented yet")
    }

    "parses RENAME COLUMN syntax" in {
      val result = test(
        """
          |CREATE TABLE users (id SERIAL, name TEXT, PRIMARY KEY (id));
          |ALTER TABLE users RENAME COLUMN name TO full_name;
          |""".trim.stripMargin
      )
      
      result should include("ALTER TABLE RENAME COLUMN not implemented yet")
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
      val result = test(
        """
          |DROP INDEX test_index;
          |""".trim.stripMargin
      )
      
      result should include("indexes not implemented yet")
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
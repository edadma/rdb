package io.github.edadma.rdb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ForeignKeyTests extends AnyFreeSpec with Matchers:

  private def execDB(sql: String): (Seq[Result], DB) =
    given db: DB = new MemoryDB
    (executeSQL(sql), db)

  private def setup: DB =
    given db: DB = new MemoryDB
    executeSQL(
      """CREATE TABLE departments (
        |  id INTEGER,
        |  name TEXT NOT NULL,
        |  PRIMARY KEY (id)
        |);
        |INSERT INTO departments (id, name) VALUES (1, 'Engineering');
        |INSERT INTO departments (id, name) VALUES (2, 'Sales');
        |""".stripMargin
    )
    db

  // ── INSERT ──────────────────────────────────────────────────────

  "INSERT" - {
    "rejects insert when parent row does not exist" in {
      given db: DB = setup
      executeSQL(
        """CREATE TABLE employees (
          |  id SERIAL,
          |  name TEXT NOT NULL,
          |  dept_id INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (dept_id) REFERENCES departments(id)
          |);
          |""".stripMargin
      )
      assertThrows[RuntimeException] {
        executeSQL("INSERT INTO employees (name, dept_id) VALUES ('Alice', 99);")
      }
    }

    "allows insert when parent row exists" in {
      given db: DB = setup
      executeSQL(
        """CREATE TABLE employees (
          |  id SERIAL,
          |  name TEXT NOT NULL,
          |  dept_id INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (dept_id) REFERENCES departments(id)
          |);
          |INSERT INTO employees (name, dept_id) VALUES ('Alice', 1);
          |""".stripMargin
      )
      val t = executeSQL("SELECT name FROM employees;").collect { case QueryResult(t) => t }.head
      t.data.length shouldBe 1
    }

    "allows insert with NULL FK column" in {
      given db: DB = setup
      executeSQL(
        """CREATE TABLE employees (
          |  id SERIAL,
          |  name TEXT NOT NULL,
          |  dept_id INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (dept_id) REFERENCES departments(id)
          |);
          |INSERT INTO employees (name) VALUES ('Bob');
          |""".stripMargin
      )
      val t = executeSQL("SELECT name FROM employees;").collect { case QueryResult(t) => t }.head
      t.data.length shouldBe 1
    }

    "enforces inline column-level REFERENCES" in {
      given db: DB = setup
      executeSQL(
        """CREATE TABLE employees (
          |  id SERIAL,
          |  name TEXT NOT NULL,
          |  dept_id INTEGER REFERENCES departments(id),
          |  PRIMARY KEY (id)
          |);
          |""".stripMargin
      )
      assertThrows[RuntimeException] {
        executeSQL("INSERT INTO employees (name, dept_id) VALUES ('Alice', 99);")
      }
      // Valid insert works
      executeSQL("INSERT INTO employees (name, dept_id) VALUES ('Alice', 1);")
    }

    "composite FK enforcement" in {
      given db: DB = new MemoryDB
      executeSQL(
        """CREATE TABLE parent (
          |  a INTEGER,
          |  b INTEGER,
          |  PRIMARY KEY (a, b)
          |);
          |INSERT INTO parent (a, b) VALUES (1, 2);
          |CREATE TABLE child (
          |  id SERIAL,
          |  x INTEGER,
          |  y INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (x, y) REFERENCES parent(a, b)
          |);
          |""".stripMargin
      )
      // Valid
      executeSQL("INSERT INTO child (x, y) VALUES (1, 2);")
      // Invalid
      assertThrows[RuntimeException] {
        executeSQL("INSERT INTO child (x, y) VALUES (1, 3);")
      }
    }
  }

  // ── DELETE ──────────────────────────────────────────────────────

  "DELETE" - {
    "RESTRICT (default) rejects delete when child rows exist" in {
      given db: DB = setup
      executeSQL(
        """CREATE TABLE employees (
          |  id SERIAL,
          |  name TEXT NOT NULL,
          |  dept_id INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (dept_id) REFERENCES departments(id)
          |);
          |INSERT INTO employees (name, dept_id) VALUES ('Alice', 1);
          |""".stripMargin
      )
      assertThrows[RuntimeException] {
        executeSQL("DELETE FROM departments WHERE id = 1;")
      }
    }

    "allows delete when no children reference the row" in {
      given db: DB = setup
      executeSQL(
        """CREATE TABLE employees (
          |  id SERIAL,
          |  name TEXT NOT NULL,
          |  dept_id INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (dept_id) REFERENCES departments(id)
          |);
          |INSERT INTO employees (name, dept_id) VALUES ('Alice', 1);
          |""".stripMargin
      )
      // Delete dept 2 which has no children
      executeSQL("DELETE FROM departments WHERE id = 2;")
      val t = executeSQL("SELECT * FROM departments;").collect { case QueryResult(t) => t }.head
      t.data.length shouldBe 1
    }

    "CASCADE deletes child rows" in {
      given db: DB = setup
      executeSQL(
        """CREATE TABLE employees (
          |  id SERIAL,
          |  name TEXT NOT NULL,
          |  dept_id INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (dept_id) REFERENCES departments(id) ON DELETE CASCADE
          |);
          |INSERT INTO employees (name, dept_id) VALUES ('Alice', 1);
          |INSERT INTO employees (name, dept_id) VALUES ('Bob', 1);
          |INSERT INTO employees (name, dept_id) VALUES ('Carol', 2);
          |""".stripMargin
      )
      executeSQL("DELETE FROM departments WHERE id = 1;")
      val emps = executeSQL("SELECT * FROM employees;").collect { case QueryResult(t) => t }.head
      emps.data.length shouldBe 1
      emps.data(0).data(1).string shouldBe "Carol"
    }

    "CASCADE multi-level" in {
      given db: DB = new MemoryDB
      executeSQL(
        """CREATE TABLE a (id INTEGER, PRIMARY KEY (id));
          |INSERT INTO a (id) VALUES (1);
          |CREATE TABLE b (
          |  id INTEGER,
          |  a_id INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (a_id) REFERENCES a(id) ON DELETE CASCADE
          |);
          |INSERT INTO b (id, a_id) VALUES (10, 1);
          |CREATE TABLE c (
          |  id INTEGER,
          |  b_id INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (b_id) REFERENCES b(id) ON DELETE CASCADE
          |);
          |INSERT INTO c (id, b_id) VALUES (100, 10);
          |""".stripMargin
      )
      executeSQL("DELETE FROM a WHERE id = 1;")
      val bs = executeSQL("SELECT * FROM b;").collect { case QueryResult(t) => t }.head
      val cs = executeSQL("SELECT * FROM c;").collect { case QueryResult(t) => t }.head
      bs.data.length shouldBe 0
      cs.data.length shouldBe 0
    }

    "SET NULL sets child FK columns to NULL on parent delete" in {
      given db: DB = setup
      executeSQL(
        """CREATE TABLE employees (
          |  id SERIAL,
          |  name TEXT NOT NULL,
          |  dept_id INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (dept_id) REFERENCES departments(id) ON DELETE SET NULL
          |);
          |INSERT INTO employees (name, dept_id) VALUES ('Alice', 1);
          |""".stripMargin
      )
      executeSQL("DELETE FROM departments WHERE id = 1;")
      val emps = executeSQL("SELECT name, dept_id FROM employees;").collect { case QueryResult(t) => t }.head
      emps.data.length shouldBe 1
      emps.data(0).data(1).isNull shouldBe true
    }
  }

  // ── UPDATE ─────────────────────────────────────────────────────

  "UPDATE" - {
    "RESTRICT rejects update of referenced column when children exist" in {
      given db: DB = setup
      executeSQL(
        """CREATE TABLE employees (
          |  id SERIAL,
          |  name TEXT NOT NULL,
          |  dept_id INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (dept_id) REFERENCES departments(id)
          |);
          |INSERT INTO employees (name, dept_id) VALUES ('Alice', 1);
          |""".stripMargin
      )
      assertThrows[RuntimeException] {
        executeSQL("UPDATE departments SET id = 99 WHERE id = 1;")
      }
    }

    "CASCADE propagates new value to children" in {
      given db: DB = setup
      executeSQL(
        """CREATE TABLE employees (
          |  id SERIAL,
          |  name TEXT NOT NULL,
          |  dept_id INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (dept_id) REFERENCES departments(id) ON UPDATE CASCADE
          |);
          |INSERT INTO employees (name, dept_id) VALUES ('Alice', 1);
          |""".stripMargin
      )
      executeSQL("UPDATE departments SET id = 99 WHERE id = 1;")
      val emps = executeSQL("SELECT dept_id FROM employees;").collect { case QueryResult(t) => t }.head
      emps.data(0).data(0).string shouldBe "99"
    }

    "SET NULL on parent update" in {
      given db: DB = setup
      executeSQL(
        """CREATE TABLE employees (
          |  id SERIAL,
          |  name TEXT NOT NULL,
          |  dept_id INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (dept_id) REFERENCES departments(id) ON UPDATE SET NULL
          |);
          |INSERT INTO employees (name, dept_id) VALUES ('Alice', 1);
          |""".stripMargin
      )
      executeSQL("UPDATE departments SET id = 99 WHERE id = 1;")
      val emps = executeSQL("SELECT dept_id FROM employees;").collect { case QueryResult(t) => t }.head
      emps.data(0).data(0).isNull shouldBe true
    }

    "rejects update of FK column to non-existent parent" in {
      given db: DB = setup
      executeSQL(
        """CREATE TABLE employees (
          |  id SERIAL,
          |  name TEXT NOT NULL,
          |  dept_id INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (dept_id) REFERENCES departments(id)
          |);
          |INSERT INTO employees (name, dept_id) VALUES ('Alice', 1);
          |""".stripMargin
      )
      assertThrows[RuntimeException] {
        executeSQL("UPDATE employees SET dept_id = 99 WHERE name = 'Alice';")
      }
    }

    "allows update of non-FK columns freely" in {
      given db: DB = setup
      executeSQL(
        """CREATE TABLE employees (
          |  id SERIAL,
          |  name TEXT NOT NULL,
          |  dept_id INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (dept_id) REFERENCES departments(id)
          |);
          |INSERT INTO employees (name, dept_id) VALUES ('Alice', 1);
          |""".stripMargin
      )
      executeSQL("UPDATE employees SET name = 'Alicia' WHERE name = 'Alice';")
      val emps = executeSQL("SELECT name FROM employees;").collect { case QueryResult(t) => t }.head
      emps.data(0).data(0).string shouldBe "Alicia"
    }
  }

  // ── DDL ────────────────────────────────────────────────────────

  "DDL" - {
    "CREATE TABLE with FK to non-existent table fails" in {
      given db: DB = new MemoryDB
      assertThrows[RuntimeException] {
        executeSQL(
          """CREATE TABLE child (
            |  id INTEGER,
            |  parent_id INTEGER,
            |  FOREIGN KEY (parent_id) REFERENCES nonexistent(id)
            |);
            |""".stripMargin
        )
      }
    }

    "CREATE TABLE with FK to non-existent column fails" in {
      given db: DB = new MemoryDB
      executeSQL("CREATE TABLE parent (id INTEGER, PRIMARY KEY (id));")
      assertThrows[RuntimeException] {
        executeSQL(
          """CREATE TABLE child (
            |  id INTEGER,
            |  parent_id INTEGER,
            |  FOREIGN KEY (parent_id) REFERENCES parent(nope)
            |);
            |""".stripMargin
        )
      }
    }

    "DROP TABLE blocked when referenced by FK" in {
      given db: DB = setup
      executeSQL(
        """CREATE TABLE employees (
          |  id SERIAL,
          |  dept_id INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (dept_id) REFERENCES departments(id)
          |);
          |""".stripMargin
      )
      assertThrows[RuntimeException] {
        executeSQL("DROP TABLE departments;")
      }
    }

    "DROP TABLE CASCADE allowed when referenced" in {
      given db: DB = setup
      executeSQL(
        """CREATE TABLE employees (
          |  id SERIAL,
          |  dept_id INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (dept_id) REFERENCES departments(id)
          |);
          |""".stripMargin
      )
      executeSQL("DROP TABLE departments CASCADE;")
      db.hasTable("departments") shouldBe false
    }

    "ON DELETE CASCADE syntax parses and is stored" in {
      given db: DB = new MemoryDB
      executeSQL("CREATE TABLE parent (id INTEGER, PRIMARY KEY (id));")
      executeSQL(
        """CREATE TABLE child (
          |  id INTEGER,
          |  parent_id INTEGER,
          |  FOREIGN KEY (parent_id) REFERENCES parent(id) ON DELETE CASCADE ON UPDATE SET NULL
          |);
          |""".stripMargin
      )
      val t = db.getTable("child").get
      val fk = t.constraints.collect { case fk: ForeignKeySpec => fk }.head
      fk.onDelete shouldBe ReferentialAction.Cascade
      fk.onUpdate shouldBe ReferentialAction.SetNull
    }
  }

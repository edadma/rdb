package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

class PersistentForeignKeyTests extends PersistentTestBase:

  "FK enforcement survives close/reopen" in {
    locally {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL(
        """CREATE TABLE departments (id INTEGER, name TEXT, PRIMARY KEY (id));
          |INSERT INTO departments (id, name) VALUES (1, 'Engineering');
          |CREATE TABLE employees (
          |  id SERIAL,
          |  name TEXT NOT NULL,
          |  dept_id INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (dept_id) REFERENCES departments(id)
          |);
          |INSERT INTO employees (name, dept_id) VALUES ('Alice', 1);
          |""".stripMargin
      )
      db.close()
    }

    locally {
      val db = PersistentDB.open(tmpFile)
      given Session = db.connect()
      // FK enforcement still works after reopen
      assertThrows[RuntimeException] {
        executeSQL("INSERT INTO employees (name, dept_id) VALUES ('Bob', 99);")
      }
      // Valid insert still works
      executeSQL("INSERT INTO employees (name, dept_id) VALUES ('Bob', 1);")
      val t = executeSQL("SELECT name FROM employees ORDER BY name;").collect { case QueryResult(t) => t }.head
      t.data.length shouldBe 2
      db.close()
    }
  }

  "CASCADE action persists across reopen" in {
    locally {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL(
        """CREATE TABLE parent (id INTEGER, PRIMARY KEY (id));
          |INSERT INTO parent (id) VALUES (1);
          |CREATE TABLE child (
          |  id SERIAL,
          |  parent_id INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (parent_id) REFERENCES parent(id) ON DELETE CASCADE
          |);
          |INSERT INTO child (parent_id) VALUES (1);
          |""".stripMargin
      )
      db.close()
    }

    locally {
      val db = PersistentDB.open(tmpFile)
      given Session = db.connect()
      // CASCADE should still work
      executeSQL("DELETE FROM parent WHERE id = 1;")
      val t = executeSQL("SELECT * FROM child;").collect { case QueryResult(t) => t }.head
      t.data.length shouldBe 0
      db.close()
    }
  }

  "DROP TABLE CASCADE cleans up FK constraints and survives reopen" in {
    locally {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL(
        """CREATE TABLE departments (id INTEGER, name TEXT, PRIMARY KEY (id));
          |INSERT INTO departments (id, name) VALUES (1, 'Engineering');
          |CREATE TABLE employees (
          |  id SERIAL,
          |  name TEXT NOT NULL,
          |  dept_id INTEGER,
          |  PRIMARY KEY (id),
          |  FOREIGN KEY (dept_id) REFERENCES departments(id)
          |);
          |INSERT INTO employees (name, dept_id) VALUES ('Alice', 1);
          |DROP TABLE departments CASCADE;
          |""".stripMargin
      )
      db.close()
    }

    locally {
      val db = PersistentDB.open(tmpFile)
      given session: Session = db.connect()
      // Parent table should be gone
      session.db.hasTable("departments") shouldBe false
      // Child table should still exist with no FK constraints
      val empTable = session.db.getTable("employees").get
      val fks = empTable.constraints.collect { case fk: ForeignKeySpec => fk }
      fks shouldBe empty
      // Should accept inserts with arbitrary dept_id since FK is gone
      executeSQL("INSERT INTO employees (name, dept_id) VALUES ('Bob', 999);")
      db.close()
    }
  }

  "DROP TABLE IF EXISTS CASCADE cleans up FK constraints and survives reopen" in {
    locally {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL(
        """CREATE TABLE parent (id INTEGER, PRIMARY KEY (id));
          |INSERT INTO parent (id) VALUES (1);
          |CREATE TABLE child (
          |  id SERIAL,
          |  parent_id INTEGER REFERENCES parent(id),
          |  PRIMARY KEY (id)
          |);
          |INSERT INTO child (parent_id) VALUES (1);
          |DROP TABLE IF EXISTS parent CASCADE;
          |""".stripMargin
      )
      db.close()
    }

    locally {
      val db = PersistentDB.open(tmpFile)
      given session: Session = db.connect()
      session.db.hasTable("parent") shouldBe false
      // Column-level FK should be cleared
      val childTable = session.db.getTable("child").get
      val parentIdCol = childTable.columns(childTable.columnMap("parent_id"))
      parentIdCol.fk shouldBe None
      // Should accept arbitrary values
      executeSQL("INSERT INTO child (parent_id) VALUES (999);")
      db.close()
    }
  }

  "inline column-level FK enforced after reopen" in {
    locally {
      val db = PersistentDB.create(tmpFile, pageSize)
      given Session = db.connect()
      executeSQL(
        """CREATE TABLE parent (id INTEGER, PRIMARY KEY (id));
          |INSERT INTO parent (id) VALUES (1);
          |CREATE TABLE child (
          |  id SERIAL,
          |  parent_id INTEGER REFERENCES parent(id),
          |  PRIMARY KEY (id)
          |);
          |""".stripMargin
      )
      db.close()
    }

    locally {
      val db = PersistentDB.open(tmpFile)
      given Session = db.connect()
      assertThrows[RuntimeException] {
        executeSQL("INSERT INTO child (parent_id) VALUES (99);")
      }
      executeSQL("INSERT INTO child (parent_id) VALUES (1);")
      db.close()
    }
  }

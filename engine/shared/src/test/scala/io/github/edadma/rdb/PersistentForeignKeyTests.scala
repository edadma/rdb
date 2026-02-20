package io.github.edadma.rdb

class PersistentForeignKeyTests extends PersistentTestBase:

  "FK enforcement survives close/reopen" in {
    locally {
      val db = PersistentDB.create(tmpFile, pageSize)
      given DB = db
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
      given DB = db
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
      given DB = db
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
      given DB = db
      // CASCADE should still work
      executeSQL("DELETE FROM parent WHERE id = 1;")
      val t = executeSQL("SELECT * FROM child;").collect { case QueryResult(t) => t }.head
      t.data.length shouldBe 0
      db.close()
    }
  }

  "inline column-level FK enforced after reopen" in {
    locally {
      val db = PersistentDB.create(tmpFile, pageSize)
      given DB = db
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
      given DB = db
      assertThrows[RuntimeException] {
        executeSQL("INSERT INTO child (parent_id) VALUES (99);")
      }
      executeSQL("INSERT INTO child (parent_id) VALUES (1);")
      db.close()
    }
  }

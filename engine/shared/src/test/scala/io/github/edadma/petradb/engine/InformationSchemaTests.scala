package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class InformationSchemaTests extends AnyFreeSpec with Matchers with Testing {

  // ── information_schema.tables ──────────────────────────────────────

  "information_schema.tables" - {
    "lists all tables" in {
      val table = query(
        """
          |CREATE TABLE users (id INT PRIMARY KEY, name TEXT);
          |CREATE TABLE orders (id INT PRIMARY KEY, user_id INT);
          |SELECT table_name FROM information_schema.tables ORDER BY table_name;
          |""".trim.stripMargin
      )
      table.data.map(_.data(0)) shouldBe Vector(TextValue("orders"), TextValue("users"))
    }

    "returns correct columns" in {
      val table = query(
        """
          |CREATE TABLE t (a INT);
          |SELECT table_catalog, table_schema, table_name, table_type FROM information_schema.tables;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data shouldBe Vector(TextValue("petradb"), TextValue("public"), TextValue("t"), TextValue("BASE TABLE"))
    }

    "returns empty result with no tables" in {
      val table = query("SELECT * FROM information_schema.tables;")
      table.data.length shouldBe 0
    }

    "reflects dropped tables" in {
      val table = query(
        """
          |CREATE TABLE t1 (a INT);
          |CREATE TABLE t2 (b INT);
          |DROP TABLE t1;
          |SELECT table_name FROM information_schema.tables;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("t2")
    }
  }

  // ── information_schema.columns ─────────────────────────────────────

  "information_schema.columns" - {
    "lists columns with correct metadata" in {
      val table = query(
        """
          |CREATE TABLE t (id SERIAL PRIMARY KEY, name VARCHAR(100) NOT NULL, score DOUBLE);
          |SELECT column_name, ordinal_position, is_nullable, data_type
          |FROM information_schema.columns
          |WHERE table_name = 't'
          |ORDER BY ordinal_position;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 3

      // id column
      table.data(0).data(0) shouldBe TextValue("id")
      table.data(0).data(1) shouldBe NumberValue(1)
      table.data(0).data(2) shouldBe TextValue("NO")
      table.data(0).data(3) shouldBe TextValue("integer")

      // name column
      table.data(1).data(0) shouldBe TextValue("name")
      table.data(1).data(1) shouldBe NumberValue(2)
      table.data(1).data(2) shouldBe TextValue("NO")
      table.data(1).data(3) shouldBe TextValue("character varying")

      // score column
      table.data(2).data(0) shouldBe TextValue("score")
      table.data(2).data(1) shouldBe NumberValue(3)
      table.data(2).data(2) shouldBe TextValue("YES")
      table.data(2).data(3) shouldBe TextValue("double precision")
    }

    "reports character_maximum_length for VARCHAR" in {
      val table = query(
        """
          |CREATE TABLE t (name VARCHAR(50));
          |SELECT character_maximum_length FROM information_schema.columns WHERE table_name = 't';
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(50)
    }

    "reports numeric_precision and scale" in {
      val table = query(
        """
          |CREATE TABLE t (price NUMERIC(10, 2));
          |SELECT numeric_precision, numeric_scale FROM information_schema.columns WHERE table_name = 't';
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(10)
      table.data(0).data(1) shouldBe NumberValue(2)
    }

    "returns NULL for non-applicable length/precision" in {
      val table = query(
        """
          |CREATE TABLE t (active BOOLEAN);
          |SELECT character_maximum_length, numeric_precision, numeric_scale
          |FROM information_schema.columns WHERE table_name = 't';
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
      table.data(0).data(1).isNull shouldBe true
      table.data(0).data(2).isNull shouldBe true
    }

    "lists columns from multiple tables" in {
      val table = query(
        """
          |CREATE TABLE t1 (a INT);
          |CREATE TABLE t2 (b TEXT, c BOOLEAN);
          |SELECT table_name, column_name
          |FROM information_schema.columns
          |ORDER BY table_name, ordinal_position;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 3
      table.data(0).data shouldBe Vector(TextValue("t1"), TextValue("a"))
      table.data(1).data shouldBe Vector(TextValue("t2"), TextValue("b"))
      table.data(2).data shouldBe Vector(TextValue("t2"), TextValue("c"))
    }
  }

  // ── information_schema.table_constraints ───────────────────────────

  "information_schema.table_constraints" - {
    "lists PRIMARY KEY constraint" in {
      val table = query(
        """
          |CREATE TABLE t (id INT PRIMARY KEY, name TEXT);
          |SELECT constraint_name, constraint_type
          |FROM information_schema.table_constraints
          |WHERE table_name = 't';
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("t_pkey")
      table.data(0).data(1) shouldBe TextValue("PRIMARY KEY")
    }

    "lists UNIQUE constraint" in {
      val table = query(
        """
          |CREATE TABLE t (id INT PRIMARY KEY, email TEXT UNIQUE);
          |SELECT constraint_name, constraint_type
          |FROM information_schema.table_constraints
          |WHERE table_name = 't' AND constraint_type = 'UNIQUE';
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("UNIQUE")
    }

    "lists FOREIGN KEY constraint" in {
      val table = query(
        """
          |CREATE TABLE parent (id INT PRIMARY KEY);
          |CREATE TABLE child (id INT PRIMARY KEY, parent_id INT REFERENCES parent(id));
          |SELECT constraint_name, constraint_type
          |FROM information_schema.table_constraints
          |WHERE table_name = 'child' AND constraint_type = 'FOREIGN KEY';
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("FOREIGN KEY")
    }

    "lists CHECK constraint" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, age INT CHECK (age > 0));
          |SELECT constraint_type
          |FROM information_schema.table_constraints
          |WHERE table_name = 't';
          |""".trim.stripMargin
      )
      table.data.exists(_.data(0) == TextValue("CHECK")) shouldBe true
    }

    "returns empty for unconstrained table" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b TEXT);
          |SELECT * FROM information_schema.table_constraints WHERE table_name = 't';
          |""".trim.stripMargin
      )
      table.data.length shouldBe 0
    }
  }

  // ── information_schema.key_column_usage ────────────────────────────

  "information_schema.key_column_usage" - {
    "lists primary key columns" in {
      val table = query(
        """
          |CREATE TABLE t (id INT PRIMARY KEY, name TEXT);
          |SELECT constraint_name, column_name, ordinal_position
          |FROM information_schema.key_column_usage
          |WHERE table_name = 't';
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("t_pkey")
      table.data(0).data(1) shouldBe TextValue("id")
      table.data(0).data(2) shouldBe NumberValue(1)
    }

    "lists composite primary key columns" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b INT, PRIMARY KEY (a, b));
          |SELECT column_name, ordinal_position
          |FROM information_schema.key_column_usage
          |WHERE table_name = 't'
          |ORDER BY ordinal_position;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 2
      table.data(0).data shouldBe Vector(TextValue("a"), NumberValue(1))
      table.data(1).data shouldBe Vector(TextValue("b"), NumberValue(2))
    }
  }

  // ── information_schema.referential_constraints ─────────────────────

  "information_schema.referential_constraints" - {
    "lists foreign key relationships" in {
      val table = query(
        """
          |CREATE TABLE parent (id INT PRIMARY KEY);
          |CREATE TABLE child (id INT PRIMARY KEY, parent_id INT REFERENCES parent(id));
          |SELECT constraint_name, unique_constraint_name, update_rule, delete_rule
          |FROM information_schema.referential_constraints
          |WHERE constraint_name LIKE '%child%';
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("parent_pkey")
      table.data(0).data(2) shouldBe TextValue("NO ACTION")
      table.data(0).data(3) shouldBe TextValue("NO ACTION")
    }

    "reports CASCADE actions" in {
      val table = query(
        """
          |CREATE TABLE parent (id INT PRIMARY KEY);
          |CREATE TABLE child (id INT PRIMARY KEY, parent_id INT REFERENCES parent(id) ON DELETE CASCADE);
          |SELECT delete_rule FROM information_schema.referential_constraints;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("CASCADE")
    }
  }

  // ── information_schema.schemata ────────────────────────────────────

  "information_schema.schemata" - {
    "lists schemas" in {
      val table = query("SELECT schema_name FROM information_schema.schemata ORDER BY schema_name;")
      table.data.map(_.data(0)) shouldBe Vector(TextValue("information_schema"), TextValue("public"))
    }
  }

  // ── Aliasing and joins ─────────────────────────────────────────────

  "aliasing" - {
    "works with alias" in {
      val table = query(
        """
          |CREATE TABLE t (a INT);
          |SELECT ist.table_name FROM information_schema.tables ist;
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("t")
    }

    "works in join with real table" in {
      val table = query(
        """
          |CREATE TABLE t (id INT PRIMARY KEY, name TEXT);
          |INSERT INTO t (id, name) VALUES (1, 'hello');
          |SELECT t.name, c.column_name
          |FROM t
          |JOIN information_schema.columns c ON c.table_name = 't' AND c.column_name = 'name';
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("hello")
      table.data(0).data(1) shouldBe TextValue("name")
    }
  }

  // ── Error handling ─────────────────────────────────────────────────

  "error handling" - {
    "unsupported information_schema table fails" in {
      an[Exception] should be thrownBy {
        query("SELECT * FROM information_schema.nonexistent;")
      }
    }
  }

  // ── WHERE filtering ────────────────────────────────────────────────

  "WHERE filtering" - {
    "filters by table_name" in {
      val table = query(
        """
          |CREATE TABLE users (id INT, name TEXT);
          |CREATE TABLE orders (id INT, total DOUBLE);
          |SELECT column_name FROM information_schema.columns WHERE table_name = 'users' ORDER BY ordinal_position;
          |""".trim.stripMargin
      )
      table.data.map(_.data(0)) shouldBe Vector(TextValue("id"), TextValue("name"))
    }

    "filters by data_type" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, name TEXT, active BOOLEAN);
          |SELECT column_name FROM information_schema.columns WHERE data_type = 'text';
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("name")
    }

    "filters by is_nullable" in {
      val table = query(
        """
          |CREATE TABLE t (id INT NOT NULL, name TEXT);
          |SELECT column_name FROM information_schema.columns
          |WHERE table_name = 't' AND is_nullable = 'YES';
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("name")
    }
  }
}

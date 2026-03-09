package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SchemaTests extends AnyFreeSpec with Matchers with Testing:

  "CREATE SCHEMA" - {
    "creates a new schema" in {
      val table = query(
        """CREATE SCHEMA myschema;
          |SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'myschema';
          |""".stripMargin
      )
      table.data.length shouldBe 1
    }

    "IF NOT EXISTS on existing schema succeeds silently" in {
      query(
        """CREATE SCHEMA myschema;
          |CREATE SCHEMA IF NOT EXISTS myschema;
          |SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'myschema';
          |""".stripMargin
      ).data.length shouldBe 1
    }

    "public schema exists by default" in {
      query("SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'public'").data.length shouldBe 1
    }

    "information_schema is listed in schemata" in {
      query("SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'information_schema'").data.length shouldBe 1
    }

    "newly created schema appears in schemata" in {
      val table = query(
        """CREATE SCHEMA drizzle;
          |SELECT schema_name FROM information_schema.schemata ORDER BY schema_name;
          |""".stripMargin
      )
      val names = table.data.map(_.data(0).string)
      names should contain("drizzle")
      names should contain("public")
      names should contain("information_schema")
    }
  }

  "Schema-qualified CREATE TABLE" - {
    "creates table in non-public schema" in {
      query(
        """CREATE SCHEMA myschema;
          |CREATE TABLE myschema.users (id SERIAL PRIMARY KEY, name TEXT NOT NULL);
          |SELECT * FROM myschema.users;
          |""".stripMargin
      ).data.length shouldBe 0
    }

    "schema-qualified table appears in information_schema.tables" in {
      val table = query(
        """CREATE SCHEMA myschema;
          |CREATE TABLE myschema.users (id SERIAL PRIMARY KEY, name TEXT);
          |SELECT table_schema, table_name FROM information_schema.tables WHERE table_schema = 'myschema';
          |""".stripMargin
      )
      table.data.length shouldBe 1
      table.data.head.data(0).string shouldBe "myschema"
      table.data.head.data(1).string shouldBe "users"
    }

    "same table name in different schemas" in {
      val t1 = query(
        """CREATE SCHEMA s1;
          |CREATE SCHEMA s2;
          |CREATE TABLE s1.items (id INTEGER, name TEXT);
          |CREATE TABLE s2.items (id INTEGER, label TEXT);
          |INSERT INTO s1.items VALUES (1, 'alpha');
          |INSERT INTO s2.items VALUES (2, 'beta');
          |SELECT name FROM s1.items;
          |""".stripMargin
      )
      t1.data.head.data(0).string shouldBe "alpha"

      val t2 = query(
        """CREATE SCHEMA s1;
          |CREATE SCHEMA s2;
          |CREATE TABLE s1.items (id INTEGER, name TEXT);
          |CREATE TABLE s2.items (id INTEGER, label TEXT);
          |INSERT INTO s1.items VALUES (1, 'alpha');
          |INSERT INTO s2.items VALUES (2, 'beta');
          |SELECT label FROM s2.items;
          |""".stripMargin
      )
      t2.data.head.data(0).string shouldBe "beta"
    }

    "unqualified table defaults to public schema" in {
      val table = query(
        """CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT);
          |SELECT table_schema, table_name FROM information_schema.tables WHERE table_name = 'users';
          |""".stripMargin
      )
      table.data.length shouldBe 1
      table.data.head.data(0).string shouldBe "public"
    }
  }

  "Schema-qualified DML" - {
    "INSERT into schema-qualified table" in {
      val table = query(
        """CREATE SCHEMA myschema;
          |CREATE TABLE myschema.log (id SERIAL PRIMARY KEY, msg TEXT);
          |INSERT INTO myschema.log (msg) VALUES ('hello');
          |SELECT msg FROM myschema.log;
          |""".stripMargin
      )
      table.data.head.data(0).string shouldBe "hello"
    }

    "UPDATE schema-qualified table" in {
      val table = query(
        """CREATE SCHEMA myschema;
          |CREATE TABLE myschema.data (id INTEGER, val TEXT);
          |INSERT INTO myschema.data VALUES (1, 'old');
          |UPDATE myschema.data SET val = 'new' WHERE id = 1;
          |SELECT val FROM myschema.data WHERE id = 1;
          |""".stripMargin
      )
      table.data.head.data(0).string shouldBe "new"
    }

    "DELETE from schema-qualified table" in {
      val table = query(
        """CREATE SCHEMA myschema;
          |CREATE TABLE myschema.data (id INTEGER, val TEXT);
          |INSERT INTO myschema.data VALUES (1, 'a');
          |INSERT INTO myschema.data VALUES (2, 'b');
          |DELETE FROM myschema.data WHERE id = 1;
          |SELECT * FROM myschema.data;
          |""".stripMargin
      )
      table.data.length shouldBe 1
    }

    "TRUNCATE schema-qualified table" in {
      val table = query(
        """CREATE SCHEMA myschema;
          |CREATE TABLE myschema.data (id INTEGER);
          |INSERT INTO myschema.data VALUES (1);
          |TRUNCATE myschema.data;
          |SELECT * FROM myschema.data;
          |""".stripMargin
      )
      table.data.length shouldBe 0
    }
  }

  "Schema-qualified DDL" - {
    "DROP TABLE with schema qualifier" in {
      val table = query(
        """CREATE SCHEMA myschema;
          |CREATE TABLE myschema.temp (id INTEGER);
          |DROP TABLE myschema.temp;
          |SELECT table_name FROM information_schema.tables WHERE table_schema = 'myschema';
          |""".stripMargin
      )
      table.data.length shouldBe 0
    }

    "DROP TABLE IF EXISTS with schema qualifier" in {
      // Should not throw
      results(
        """CREATE SCHEMA myschema;
          |DROP TABLE IF EXISTS myschema.nonexistent;
          |""".stripMargin
      )
    }

    "ALTER TABLE with schema qualifier" in {
      val table = query(
        """CREATE SCHEMA myschema;
          |CREATE TABLE myschema.data (id INTEGER);
          |ALTER TABLE myschema.data ADD COLUMN name TEXT;
          |INSERT INTO myschema.data VALUES (1, 'test');
          |SELECT name FROM myschema.data;
          |""".stripMargin
      )
      table.data.head.data(0).string shouldBe "test"
    }

    "CREATE INDEX on schema-qualified table" in {
      val table = query(
        """CREATE SCHEMA myschema;
          |CREATE TABLE myschema.data (id INTEGER, name TEXT);
          |CREATE INDEX idx_data_name ON myschema.data (name);
          |INSERT INTO myschema.data VALUES (1, 'hello');
          |SELECT name FROM myschema.data WHERE name = 'hello';
          |""".stripMargin
      )
      table.data.length shouldBe 1
    }
  }

  "Schema-qualified information_schema" - {
    "columns shows correct schema" in {
      val table = query(
        """CREATE SCHEMA myschema;
          |CREATE TABLE myschema.t (a INTEGER, b TEXT);
          |SELECT table_schema, column_name FROM information_schema.columns WHERE table_schema = 'myschema' ORDER BY ordinal_position;
          |""".stripMargin
      )
      table.data.length shouldBe 2
      table.data(0).data(0).string shouldBe "myschema"
      table.data(0).data(1).string shouldBe "a"
      table.data(1).data(1).string shouldBe "b"
    }

    "table_constraints shows correct schema" in {
      val table = query(
        """CREATE SCHEMA myschema;
          |CREATE TABLE myschema.t (id INTEGER PRIMARY KEY);
          |SELECT constraint_schema, constraint_type FROM information_schema.table_constraints WHERE table_schema = 'myschema';
          |""".stripMargin
      )
      table.data.length shouldBe 1
      table.data.head.data(0).string shouldBe "myschema"
      table.data.head.data(1).string shouldBe "PRIMARY KEY"
    }
  }

  "Drizzle migration pattern" - {
    "full Drizzle migration flow" in {
      val table = query(
        """CREATE SCHEMA IF NOT EXISTS "drizzle";
          |CREATE TABLE "drizzle"."__drizzle_migrations" (
          |  id SERIAL PRIMARY KEY,
          |  hash TEXT,
          |  created_at BIGINT
          |);
          |INSERT INTO "drizzle"."__drizzle_migrations" (hash, created_at) VALUES ('abc123', 1709913600000);
          |SELECT hash, created_at FROM "drizzle"."__drizzle_migrations";
          |""".stripMargin
      )
      table.data.length shouldBe 1
      table.data.head.data(0).string shouldBe "abc123"
    }

    "migrations table appears in correct schema" in {
      val table = query(
        """CREATE SCHEMA IF NOT EXISTS "drizzle";
          |CREATE TABLE "drizzle"."__drizzle_migrations" (
          |  id SERIAL PRIMARY KEY,
          |  hash TEXT,
          |  created_at BIGINT
          |);
          |SELECT table_schema, table_name FROM information_schema.tables WHERE table_name = '__drizzle_migrations';
          |""".stripMargin
      )
      table.data.length shouldBe 1
      table.data.head.data(0).string shouldBe "drizzle"
    }

    "idempotent CREATE SCHEMA IF NOT EXISTS" in {
      query(
        """CREATE SCHEMA IF NOT EXISTS drizzle;
          |CREATE SCHEMA IF NOT EXISTS drizzle;
          |CREATE SCHEMA IF NOT EXISTS drizzle;
          |SELECT schema_name FROM information_schema.schemata WHERE schema_name = 'drizzle';
          |""".stripMargin
      ).data.length shouldBe 1
    }

    "empty migrations check returns no rows" in {
      val table = query(
        """CREATE SCHEMA IF NOT EXISTS "drizzle";
          |CREATE TABLE "drizzle"."__drizzle_migrations" (
          |  id SERIAL PRIMARY KEY,
          |  hash TEXT,
          |  created_at BIGINT
          |);
          |SELECT id, hash, created_at FROM "drizzle"."__drizzle_migrations" ORDER BY created_at;
          |""".stripMargin
      )
      table.data.length shouldBe 0
    }
  }

  "DO $$ blocks" - {
    "DO block with ALTER TABLE ADD CONSTRAINT FOREIGN KEY" in {
      results(
        """CREATE TABLE "users" (id SERIAL PRIMARY KEY, name TEXT NOT NULL);
          |CREATE TABLE "posts" (id SERIAL PRIMARY KEY, user_id INTEGER, title TEXT);
          |DO $$ BEGIN
          |  ALTER TABLE "posts" ADD CONSTRAINT "posts_user_id_fk" FOREIGN KEY ("user_id") REFERENCES "users"("id") ON DELETE cascade ON UPDATE no action;
          |EXCEPTION
          |  WHEN duplicate_object THEN null;
          |END $$;
          |""".stripMargin
      )
    }

    "DO block idempotent — runs twice without error" in {
      results(
        """CREATE TABLE "users" (id SERIAL PRIMARY KEY, name TEXT NOT NULL);
          |CREATE TABLE "posts" (id SERIAL PRIMARY KEY, user_id INTEGER, title TEXT);
          |DO $$ BEGIN
          |  ALTER TABLE "posts" ADD CONSTRAINT "posts_user_id_fk" FOREIGN KEY ("user_id") REFERENCES "users"("id") ON DELETE cascade ON UPDATE no action;
          |EXCEPTION
          |  WHEN duplicate_object THEN null;
          |END $$;
          |DO $$ BEGIN
          |  ALTER TABLE "posts" ADD CONSTRAINT "posts_user_id_fk" FOREIGN KEY ("user_id") REFERENCES "users"("id") ON DELETE cascade ON UPDATE no action;
          |EXCEPTION
          |  WHEN duplicate_object THEN null;
          |END $$;
          |""".stripMargin
      )
    }

    "DO block with schema-qualified REFERENCES" in {
      results(
        """CREATE SCHEMA app;
          |CREATE TABLE app."users" (id SERIAL PRIMARY KEY, name TEXT NOT NULL);
          |CREATE TABLE app."posts" (id SERIAL PRIMARY KEY, user_id INTEGER, title TEXT);
          |DO $$ BEGIN
          |  ALTER TABLE app."posts" ADD CONSTRAINT "posts_user_id_fk" FOREIGN KEY ("user_id") REFERENCES app."users"("id") ON DELETE no action ON UPDATE no action;
          |EXCEPTION
          |  WHEN duplicate_object THEN null;
          |END $$;
          |""".stripMargin
      )
    }

    "DO block with CREATE TYPE idempotent" in {
      results(
        """DO $$ BEGIN
          |  CREATE TYPE status AS ENUM ('active', 'inactive');
          |EXCEPTION
          |  WHEN duplicate_object THEN null;
          |END $$;
          |DO $$ BEGIN
          |  CREATE TYPE status AS ENUM ('active', 'inactive');
          |EXCEPTION
          |  WHEN duplicate_object THEN null;
          |END $$;
          |""".stripMargin
      )
    }

    "DO block FK constraint is enforced" in {
      val table = query(
        """CREATE TABLE "users" (id SERIAL PRIMARY KEY, name TEXT NOT NULL);
          |CREATE TABLE "posts" (id SERIAL PRIMARY KEY, user_id INTEGER, title TEXT);
          |DO $$ BEGIN
          |  ALTER TABLE "posts" ADD CONSTRAINT "posts_user_id_fk" FOREIGN KEY ("user_id") REFERENCES "users"("id") ON DELETE cascade ON UPDATE no action;
          |EXCEPTION
          |  WHEN duplicate_object THEN null;
          |END $$;
          |INSERT INTO "users" (name) VALUES ('Alice');
          |INSERT INTO "posts" (user_id, title) VALUES (1, 'Hello');
          |SELECT title FROM "posts";
          |""".stripMargin
      )
      table.data.head.data(0).string shouldBe "Hello"
    }
  }

  "SHOW commands with schema qualifier" - {
    "SHOW COLUMNS from schema-qualified table" in {
      val table = query(
        """CREATE SCHEMA myschema;
          |CREATE TABLE myschema.t (id INTEGER, name TEXT);
          |SHOW COLUMNS FROM myschema.t;
          |""".stripMargin
      )
      table.data.length shouldBe 2
    }

    "SHOW PRIMARY KEY from schema-qualified table" in {
      val table = query(
        """CREATE SCHEMA myschema;
          |CREATE TABLE myschema.t (id INTEGER PRIMARY KEY);
          |SHOW PRIMARY KEY myschema.t;
          |""".stripMargin
      )
      table.data.length shouldBe 1
    }
  }

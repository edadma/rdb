package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

// Tests that exercise the exact SQL patterns Drizzle ORM generates for
// relational queries: json_build_array, json_agg, coalesce, LATERAL
// subqueries with parameters, LEFT JOIN ... ON true, etc.
class DrizzleRelationalPatternTests extends AnyFreeSpec with Matchers:

  private val schema =
    """
      |CREATE TABLE authors (id SERIAL PRIMARY KEY, name TEXT NOT NULL);
      |CREATE TABLE posts (id SERIAL PRIMARY KEY, title TEXT NOT NULL, author_id INTEGER NOT NULL);
      |CREATE TABLE comments (id SERIAL PRIMARY KEY, body TEXT NOT NULL, post_id INTEGER NOT NULL);
      |INSERT INTO authors (name) VALUES ('Alice'), ('Bob'), ('Carol');
      |INSERT INTO posts (title, author_id) VALUES
      |  ('Alice Post 1', 1), ('Alice Post 2', 1), ('Bob Post 1', 2);
      |INSERT INTO comments (body, post_id) VALUES
      |  ('Great!', 1), ('Nice!', 1), ('Cool!', 3);
      |""".trim.stripMargin

  private def withDB(f: Session => Unit): Unit =
    val db = new MemoryDB
    f(db.connect())

  // ── json_build_array / json_build_object ─────────────────────────

  "json_build_array" - {
    "builds array from column values" in withDB { implicit s =>
      executeSQL(schema)
      val table = executeSQL("SELECT json_build_array(id, name) AS row FROM authors ORDER BY id;")
        .collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe a[ArrayValue]
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.length shouldBe 2
      arr.data(0) shouldBe NumberValue(1)
      arr.data(1) shouldBe TextValue("Alice")
    }

    "builds array with mixed types including null" in withDB { implicit s =>
      executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
      executeSQL("INSERT INTO t VALUES (1, NULL);")
      val table = executeSQL("SELECT json_build_array(id, name) AS row FROM t;")
        .collect { case QueryResult(t) => t }.head
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data(1) shouldBe a[NullValue]
    }

    "empty array" in withDB { implicit s =>
      val table = executeSQL("SELECT json_build_array() AS row;")
        .collect { case QueryResult(t) => t }.head
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.length shouldBe 0
    }
  }

  "json_build_object" - {
    "builds object from key-value pairs" in withDB { implicit s =>
      executeSQL(schema)
      val table = executeSQL("SELECT json_build_object('id', id, 'name', name) AS obj FROM authors WHERE id = 1;")
        .collect { case QueryResult(t) => t }.head
      val obj = table.data(0).data(0).asInstanceOf[ObjectValue]
      obj.properties.toMap shouldBe Map("id" -> NumberValue(1), "name" -> TextValue("Alice"))
    }

    "odd number of arguments fails" in withDB { implicit s =>
      assertThrows[Exception] {
        executeSQL("SELECT json_build_object('id', 1, 'name');")
      }
    }
  }

  // ── json_agg + coalesce pattern (Drizzle's collection aggregation) ─

  "json_agg with coalesce" - {
    "aggregates rows into JSON array" in withDB { implicit s =>
      executeSQL(schema)
      val table = executeSQL(
        """SELECT coalesce(json_agg(json_build_array(p.id, p.title)), '[]'::json) AS posts
          |FROM posts p WHERE p.author_id = 1;""".stripMargin)
        .collect { case QueryResult(t) => t }.head
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.length shouldBe 2
    }

    "coalesce returns empty array for no matching rows" in withDB { implicit s =>
      executeSQL(schema)
      val table = executeSQL(
        """SELECT coalesce(json_agg(json_build_array(p.id, p.title)), '[]'::json) AS posts
          |FROM posts p WHERE p.author_id = 999;""".stripMargin)
        .collect { case QueryResult(t) => t }.head
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.length shouldBe 0
    }
  }

  // ── LATERAL subquery with json_build_array (Drizzle pattern) ─────

  "LATERAL with json_build_array" - {
    "one-to-many: authors → posts via LEFT JOIN LATERAL ON true" in withDB { implicit s =>
      executeSQL(schema)
      val table = executeSQL(
        """SELECT a.id, a.name, "posts_lateral"."data" AS posts
          |FROM authors a
          |LEFT JOIN LATERAL (
          |  SELECT coalesce(json_agg(json_build_array(p.id, p.title, p.author_id)), '[]'::json) AS "data"
          |  FROM posts p WHERE p.author_id = a.id
          |) AS "posts_lateral" ON true
          |ORDER BY a.id;""".stripMargin)
        .collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 3

      // Alice: 2 posts
      val alicePosts = table.data(0).data(2).asInstanceOf[ArrayValue]
      alicePosts.data.length shouldBe 2

      // Bob: 1 post
      val bobPosts = table.data(1).data(2).asInstanceOf[ArrayValue]
      bobPosts.data.length shouldBe 1

      // Carol: 0 posts (coalesce → empty array)
      val carolPosts = table.data(2).data(2).asInstanceOf[ArrayValue]
      carolPosts.data.length shouldBe 0
    }

    "many-to-one: posts → author via LEFT JOIN LATERAL ON true" in withDB { implicit s =>
      executeSQL(schema)
      val table = executeSQL(
        """SELECT p.id, p.title, "author_lateral"."data" AS author
          |FROM posts p
          |LEFT JOIN LATERAL (
          |  SELECT json_build_array(a.id, a.name) AS "data"
          |  FROM authors a WHERE a.id = p.author_id
          |) AS "author_lateral" ON true
          |ORDER BY p.id;""".stripMargin)
        .collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 3
      // Each post's author data is a json array [id, name]
      val author1 = table.data(0).data(2).asInstanceOf[ArrayValue]
      author1.data(1) shouldBe TextValue("Alice")
      val author3 = table.data(2).data(2).asInstanceOf[ArrayValue]
      author3.data(1) shouldBe TextValue("Bob")
    }

    "nested: authors → posts → comments via two LATERAL joins" in withDB { implicit s =>
      executeSQL(schema)
      val table = executeSQL(
        """SELECT a.id, a.name, "posts_lateral"."data" AS posts
          |FROM authors a
          |LEFT JOIN LATERAL (
          |  SELECT coalesce(json_agg(json_build_array(
          |    p.id, p.title, p.author_id,
          |    (SELECT coalesce(json_agg(json_build_array(c.id, c.body, c.post_id)), '[]'::json) FROM comments c WHERE c.post_id = p.id)
          |  )), '[]'::json) AS "data"
          |  FROM posts p WHERE p.author_id = a.id
          |) AS "posts_lateral" ON true
          |WHERE a.name = 'Alice'
          |ORDER BY a.id;""".stripMargin)
        .collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1

      val posts = table.data(0).data(2).asInstanceOf[ArrayValue]
      posts.data.length shouldBe 2

      // First post's comments (nested array is the 4th element of each post array)
      val post1 = posts.data(0).asInstanceOf[ArrayValue]
      val post1Comments = post1.data(3).asInstanceOf[ArrayValue]
      post1Comments.data.length shouldBe 2 // "Great!" and "Nice!"

      // Second post has no comments
      val post2 = posts.data(1).asInstanceOf[ArrayValue]
      val post2Comments = post2.data(3).asInstanceOf[ArrayValue]
      post2Comments.data.length shouldBe 0
    }
  }

  // ── Parameterized LATERAL subqueries (prepared statement pattern) ─

  "parameterized LATERAL subqueries" - {
    "parameter in WHERE of LATERAL subquery" in withDB { implicit s =>
      executeSQL(schema)
      executeSQL(
        """PREPARE q AS
          |SELECT a.id, a.name, "posts_lateral"."data" AS posts
          |FROM authors a
          |LEFT JOIN LATERAL (
          |  SELECT coalesce(json_agg(json_build_array(p.id, p.title)), '[]'::json) AS "data"
          |  FROM posts p WHERE p.author_id = a.id
          |) AS "posts_lateral" ON true
          |WHERE a.name = $1
          |ORDER BY a.id;""".stripMargin)
      val table = executeSQL("EXECUTE q('Alice');")
        .collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("Alice")
      val posts = table.data(0).data(2).asInstanceOf[ArrayValue]
      posts.data.length shouldBe 2
    }

    "parameter in outer WHERE with LATERAL aggregation" in withDB { implicit s =>
      executeSQL(schema)
      executeSQL(
        """PREPARE q AS
          |SELECT p.id, p.title, "comments_lateral"."data" AS comments
          |FROM posts p
          |LEFT JOIN LATERAL (
          |  SELECT coalesce(json_agg(json_build_array(c.id, c.body)), '[]'::json) AS "data"
          |  FROM comments c WHERE c.post_id = p.id
          |) AS "comments_lateral" ON true
          |WHERE p.author_id = $1
          |ORDER BY p.id;""".stripMargin)
      val table = executeSQL("EXECUTE q(1);")
        .collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 2

      // Alice Post 1 has 2 comments
      val post1Comments = table.data(0).data(2).asInstanceOf[ArrayValue]
      post1Comments.data.length shouldBe 2

      // Alice Post 2 has 0 comments
      val post2Comments = table.data(1).data(2).asInstanceOf[ArrayValue]
      post2Comments.data.length shouldBe 0
    }

    "multiple parameters across outer and lateral contexts" in withDB { implicit s =>
      executeSQL(schema)
      executeSQL(
        """PREPARE q AS
          |SELECT a.id, a.name, "posts_lateral"."data" AS posts
          |FROM authors a
          |LEFT JOIN LATERAL (
          |  SELECT coalesce(json_agg(json_build_array(p.id, p.title)), '[]'::json) AS "data"
          |  FROM posts p WHERE p.author_id = a.id AND p.title != $2
          |) AS "posts_lateral" ON true
          |WHERE a.name = $1
          |ORDER BY a.id;""".stripMargin)
      val table = executeSQL("EXECUTE q('Alice', 'Alice Post 2');")
        .collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 1
      val posts = table.data(0).data(2).asInstanceOf[ArrayValue]
      posts.data.length shouldBe 1 // only Alice Post 1 (Post 2 excluded)
    }

    "prepared statement reuse with different parameter values" in withDB { implicit s =>
      executeSQL(schema)
      executeSQL(
        """PREPARE q AS
          |SELECT a.name, "posts_lateral"."data" AS posts
          |FROM authors a
          |LEFT JOIN LATERAL (
          |  SELECT coalesce(json_agg(json_build_array(p.id, p.title)), '[]'::json) AS "data"
          |  FROM posts p WHERE p.author_id = a.id
          |) AS "posts_lateral" ON true
          |WHERE a.name = $1;""".stripMargin)

      // Execute with Alice
      val t1 = executeSQL("EXECUTE q('Alice');").collect { case QueryResult(t) => t }.head
      t1.data(0).data(0) shouldBe TextValue("Alice")
      t1.data(0).data(1).asInstanceOf[ArrayValue].data.length shouldBe 2

      // Reuse with Bob — verifies parameters are correctly re-substituted
      val t2 = executeSQL("EXECUTE q('Bob');").collect { case QueryResult(t) => t }.head
      t2.data(0).data(0) shouldBe TextValue("Bob")
      t2.data(0).data(1).asInstanceOf[ArrayValue].data.length shouldBe 1

      // Reuse with Carol — no posts
      val t3 = executeSQL("EXECUTE q('Carol');").collect { case QueryResult(t) => t }.head
      t3.data(0).data(0) shouldBe TextValue("Carol")
      t3.data(0).data(1).asInstanceOf[ArrayValue].data.length shouldBe 0
    }
  }

  // ── Edge cases ───────────────────────────────────────────────────

  "nullable foreign key" - {
    "LEFT JOIN LATERAL with nullable FK returns null for unlinked rows" in withDB { implicit s =>
      executeSQL(
        """CREATE TABLE categories (id SERIAL PRIMARY KEY, name TEXT NOT NULL);
          |CREATE TABLE items (id SERIAL PRIMARY KEY, name TEXT NOT NULL, category_id INTEGER);
          |INSERT INTO categories (name) VALUES ('Electronics'), ('Books');
          |INSERT INTO items (name, category_id) VALUES ('Phone', 1), ('Orphan', NULL), ('Laptop', 1);
          |""".stripMargin)
      val table = executeSQL(
        """SELECT i.id, i.name, "cat_lateral"."data" AS category
          |FROM items i
          |LEFT JOIN LATERAL (
          |  SELECT json_build_array(c.id, c.name) AS "data"
          |  FROM categories c WHERE c.id = i.category_id
          |) AS "cat_lateral" ON true
          |ORDER BY i.id;""".stripMargin)
        .collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 3
      // Phone → Electronics
      table.data(0).data(2) shouldBe a[ArrayValue]
      // Orphan → NULL category
      table.data(1).data(2) shouldBe a[NullValue]
      // Laptop → Electronics
      table.data(2).data(2) shouldBe a[ArrayValue]
    }
  }

  "self-referential relation" - {
    "employee → manager via LEFT JOIN LATERAL" in withDB { implicit s =>
      executeSQL(
        """CREATE TABLE employees (id SERIAL PRIMARY KEY, name TEXT NOT NULL, manager_id INTEGER);
          |INSERT INTO employees (name, manager_id) VALUES ('CEO', NULL), ('VP', 1), ('Dev', 2);
          |""".stripMargin)
      val table = executeSQL(
        """SELECT e.id, e.name, "mgr_lateral"."data" AS manager
          |FROM employees e
          |LEFT JOIN LATERAL (
          |  SELECT json_build_array(m.id, m.name) AS "data"
          |  FROM employees m WHERE m.id = e.manager_id
          |) AS "mgr_lateral" ON true
          |ORDER BY e.id;""".stripMargin)
        .collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 3
      // CEO has no manager
      table.data(0).data(2) shouldBe a[NullValue]
      // VP's manager is CEO
      val vpMgr = table.data(1).data(2).asInstanceOf[ArrayValue]
      vpMgr.data(1) shouldBe TextValue("CEO")
      // Dev's manager is VP
      val devMgr = table.data(2).data(2).asInstanceOf[ArrayValue]
      devMgr.data(1) shouldBe TextValue("VP")
    }

    "self-referential one-to-many (manager → reports)" in withDB { implicit s =>
      executeSQL(
        """CREATE TABLE employees (id SERIAL PRIMARY KEY, name TEXT NOT NULL, manager_id INTEGER);
          |INSERT INTO employees (name, manager_id) VALUES ('CEO', NULL), ('VP1', 1), ('VP2', 1), ('Dev', 2);
          |""".stripMargin)
      val table = executeSQL(
        """SELECT e.id, e.name, "reports_lateral"."data" AS reports
          |FROM employees e
          |LEFT JOIN LATERAL (
          |  SELECT coalesce(json_agg(json_build_array(r.id, r.name)), '[]'::json) AS "data"
          |  FROM employees r WHERE r.manager_id = e.id
          |) AS "reports_lateral" ON true
          |ORDER BY e.id;""".stripMargin)
        .collect { case QueryResult(t) => t }.head
      table.data.length shouldBe 4
      // CEO has 2 reports
      table.data(0).data(2).asInstanceOf[ArrayValue].data.length shouldBe 2
      // VP1 has 1 report (Dev)
      table.data(1).data(2).asInstanceOf[ArrayValue].data.length shouldBe 1
      // VP2 has 0 reports
      table.data(2).data(2).asInstanceOf[ArrayValue].data.length shouldBe 0
      // Dev has 0 reports
      table.data(3).data(2).asInstanceOf[ArrayValue].data.length shouldBe 0
    }
  }

  // ── Cross-backend: PersistentDB ──────────────────────────────────

  "PersistentDB" - {
    "LATERAL + json_build_array works with persistent storage" in {
      import io.github.edadma.cross_platform.{createTempFile, deleteFile}
      val tmpFile = createTempFile("petradb_drizzle_test_", ".db")
      deleteFile(tmpFile)

      try
        val db = PersistentDB.create(tmpFile, 4096)
        given Session = db.connect()
        executeSQL(schema)
        val table = executeSQL(
          """SELECT a.id, a.name, "posts_lateral"."data" AS posts
            |FROM authors a
            |LEFT JOIN LATERAL (
            |  SELECT coalesce(json_agg(json_build_array(p.id, p.title, p.author_id)), '[]'::json) AS "data"
            |  FROM posts p WHERE p.author_id = a.id
            |) AS "posts_lateral" ON true
            |ORDER BY a.id;""".stripMargin)
          .collect { case QueryResult(t) => t }.head

        table.data.length shouldBe 3
        table.data(0).data(2).asInstanceOf[ArrayValue].data.length shouldBe 2 // Alice: 2 posts
        table.data(1).data(2).asInstanceOf[ArrayValue].data.length shouldBe 1 // Bob: 1 post
        table.data(2).data(2).asInstanceOf[ArrayValue].data.length shouldBe 0 // Carol: 0 posts
        db.close()
      finally
        try deleteFile(tmpFile) catch case _: Exception => ()
    }

    "parameterized LATERAL survives reopen" in {
      import io.github.edadma.cross_platform.{createTempFile, deleteFile}
      val tmpFile = createTempFile("petradb_drizzle_test2_", ".db")
      deleteFile(tmpFile)

      try
        locally {
          val db = PersistentDB.create(tmpFile, 4096)
          given Session = db.connect()
          executeSQL(schema)
          executeSQL(
            """PREPARE q AS
              |SELECT a.name, "posts_lateral"."data" AS posts
              |FROM authors a
              |LEFT JOIN LATERAL (
              |  SELECT coalesce(json_agg(json_build_array(p.id, p.title)), '[]'::json) AS "data"
              |  FROM posts p WHERE p.author_id = a.id
              |) AS "posts_lateral" ON true
              |WHERE a.name = $1;""".stripMargin)
          val table = executeSQL("EXECUTE q('Alice');").collect { case QueryResult(t) => t }.head
          table.data(0).data(1).asInstanceOf[ArrayValue].data.length shouldBe 2
          db.close()
        }

        // Reopen and verify data is intact (prepared stmt doesn't persist, but data does)
        locally {
          val db = PersistentDB.open(tmpFile)
          given Session = db.connect()
          val table = executeSQL(
            """SELECT a.name, "posts_lateral"."data" AS posts
              |FROM authors a
              |LEFT JOIN LATERAL (
              |  SELECT coalesce(json_agg(json_build_array(p.id, p.title)), '[]'::json) AS "data"
              |  FROM posts p WHERE p.author_id = a.id
              |) AS "posts_lateral" ON true
              |WHERE a.name = 'Bob';""".stripMargin)
            .collect { case QueryResult(t) => t }.head
          table.data(0).data(0) shouldBe TextValue("Bob")
          table.data(0).data(1).asInstanceOf[ArrayValue].data.length shouldBe 1
          db.close()
        }
      finally
        try deleteFile(tmpFile) catch case _: Exception => ()
    }
  }

package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class AdvancedIndexTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, email TEXT NOT NULL, status TEXT NOT NULL, age INT);
      |INSERT INTO users (name, email, status, age) VALUES ('Alice', 'alice@test.com', 'active', 30);
      |INSERT INTO users (name, email, status, age) VALUES ('Bob', 'bob@test.com', 'inactive', 25);
      |INSERT INTO users (name, email, status, age) VALUES ('Carol', 'CAROL@test.com', 'active', 35);
      |INSERT INTO users (name, email, status, age) VALUES ('Dave', 'dave@test.com', 'inactive', 28);
      |INSERT INTO users (name, email, status, age) VALUES ('Eve', 'eve@test.com', 'active', 32);
      |""".trim.stripMargin

  // ══════════════════════════════════════════════════════════════════
  // PARTIAL INDEXES
  // ══════════════════════════════════════════════════════════════════

  "partial indexes" - {
    "CREATE INDEX ... WHERE parses and creates" in {
      val session = setupSession(setup)
      given Session = session

      executeSQL("CREATE INDEX idx_active_email ON users (email) WHERE status = 'active';")
      val showResult = executeSQL("SHOW INDEXES users;").collect { case QueryResult(t) => t }.last
      val idxRow = showResult.data.find(r => r.data(0).string == "idx_active_email")
      idxRow shouldBe defined
    }

    "partial unique index allows duplicates outside the WHERE condition" in {
      // Unique on email, but only for active users
      // Two inactive users could theoretically have the same email (if not constrained elsewhere)
      val session = setupSession(
        """
          |CREATE TABLE accounts (id SERIAL PRIMARY KEY, email TEXT NOT NULL, active BOOLEAN NOT NULL);
          |INSERT INTO accounts (email, active) VALUES ('test@x.com', true);
          |INSERT INTO accounts (email, active) VALUES ('test@x.com', false);
          |""".stripMargin
      )
      given Session = session

      // This should succeed — only one active row has 'test@x.com'
      executeSQL("CREATE UNIQUE INDEX idx_unique_active_email ON accounts (email) WHERE active = true;")

      // Insert another inactive with same email — should succeed (not in index)
      executeSQL("INSERT INTO accounts (email, active) VALUES ('test@x.com', false);")
      val count = executeSQL("SELECT COUNT(*) AS cnt FROM accounts WHERE email = 'test@x.com';")
        .collect { case QueryResult(t) => t }.last
      count.data(0).data(0).intValue shouldBe 3
    }

    "partial unique index rejects duplicates within the WHERE condition" in {
      val session = setupSession(
        """
          |CREATE TABLE accounts (id SERIAL PRIMARY KEY, email TEXT NOT NULL, active BOOLEAN NOT NULL);
          |INSERT INTO accounts (email, active) VALUES ('test@x.com', true);
          |""".stripMargin
      )
      given Session = session

      executeSQL("CREATE UNIQUE INDEX idx_unique_active_email ON accounts (email) WHERE active = true;")

      // Insert another active with same email — should fail
      an[Exception] should be thrownBy {
        executeSQL("INSERT INTO accounts (email, active) VALUES ('test@x.com', true);")
      }
    }

    "partial index creation fails on duplicate keys in existing data" in {
      val session = setupSession(
        """
          |CREATE TABLE t (id SERIAL PRIMARY KEY, status TEXT, val INT);
          |INSERT INTO t (status, val) VALUES ('a', 1);
          |INSERT INTO t (status, val) VALUES ('a', 1);
          |""".stripMargin
      )
      given Session = session

      an[Exception] should be thrownBy {
        executeSQL("CREATE UNIQUE INDEX idx ON t (val) WHERE status = 'a';")
      }
    }

    "partial index only indexes rows matching WHERE" in {
      val session = setupSession(setup)
      given Session = session

      executeSQL("CREATE INDEX idx_active_name ON users (name) WHERE status = 'active';")

      // Query with matching WHERE uses the index
      val plan = executeSQL("EXPLAIN SELECT * FROM users WHERE name = 'Alice' AND status = 'active';")
        .collect { case ExplainResult(p) => p }.last
      plan should include("Index Scan")
    }

    "partial index is NOT used when query doesn't include index WHERE condition" in {
      val session = setupSession(setup)
      given Session = session

      executeSQL("CREATE INDEX idx_active_name ON users (name) WHERE status = 'active';")

      // Query without status = 'active' should NOT use the partial index
      val plan = executeSQL("EXPLAIN SELECT * FROM users WHERE name = 'Alice';")
        .collect { case ExplainResult(p) => p }.last
      plan should include("Seq Scan")
    }

    "partial index query returns correct results" in {
      val session = setupSession(setup)
      given Session = session

      executeSQL("CREATE INDEX idx_active_name ON users (name) WHERE status = 'active';")

      val rows = executeSQL("SELECT name FROM users WHERE name = 'Alice' AND status = 'active';")
        .collect { case QueryResult(t) => t }.last
      rows.data.length shouldBe 1
      rows.data(0).data(0) shouldBe TextValue("Alice")
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // EXPRESSION INDEXES
  // ══════════════════════════════════════════════════════════════════

  "expression indexes" - {
    "CREATE INDEX with expression parses and creates" in {
      val session = setupSession(setup)
      given Session = session

      executeSQL("CREATE INDEX idx_lower_email ON users ((lower(email)));")
      val showResult = executeSQL("SHOW INDEXES users;").collect { case QueryResult(t) => t }.last
      val idxRow = showResult.data.find(r => r.data(0).string == "idx_lower_email")
      idxRow shouldBe defined
    }

    "expression index is used for matching WHERE clause" in {
      val session = setupSession(setup)
      given Session = session

      executeSQL("CREATE INDEX idx_lower_email ON users ((lower(email)));")

      val plan = executeSQL("EXPLAIN SELECT * FROM users WHERE lower(email) = 'carol@test.com';")
        .collect { case ExplainResult(p) => p }.last
      plan should include("Index Scan")
    }

    "expression index returns correct results" in {
      val session = setupSession(setup)
      given Session = session

      executeSQL("CREATE INDEX idx_lower_email ON users ((lower(email)));")

      // Carol's email is stored as 'CAROL@test.com' but lower() should find it
      val rows = executeSQL("SELECT name FROM users WHERE lower(email) = 'carol@test.com';")
        .collect { case QueryResult(t) => t }.last
      rows.data.length shouldBe 1
      rows.data(0).data(0) shouldBe TextValue("Carol")
    }

    "unique expression index rejects duplicates on computed value" in {
      val session = setupSession(setup)
      given Session = session

      executeSQL("CREATE UNIQUE INDEX idx_lower_email ON users ((lower(email)));")

      // Insert with email that differs in case but has same lower() — should fail
      an[Exception] should be thrownBy {
        executeSQL("INSERT INTO users (name, email, status, age) VALUES ('Frank', 'Alice@Test.Com', 'active', 40);")
      }
    }

    "expression index with arithmetic expression" in {
      val session = setupSession(setup)
      given Session = session

      executeSQL("CREATE INDEX idx_age_x2 ON users ((age * 2));")

      // age*2: Alice=60, Bob=50, Carol=70, Dave=56, Eve=64
      val rows = executeSQL("SELECT name FROM users WHERE age * 2 = 60;")
        .collect { case QueryResult(t) => t }.last
      rows.data.length shouldBe 1
      rows.data(0).data(0) shouldBe TextValue("Alice")
    }

    "expression index is NOT used for non-matching expressions" in {
      val session = setupSession(setup)
      given Session = session

      executeSQL("CREATE INDEX idx_lower_email ON users ((lower(email)));")

      // upper(email) doesn't match the index on lower(email)
      val plan = executeSQL("EXPLAIN SELECT * FROM users WHERE upper(email) = 'CAROL@TEST.COM';")
        .collect { case ExplainResult(p) => p }.last
      plan should include("Seq Scan")
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // COMBINED: PARTIAL + EXPRESSION
  // ══════════════════════════════════════════════════════════════════

  "combined partial + expression index" - {
    "creates and uses partial expression index" in {
      val session = setupSession(setup)
      given Session = session

      executeSQL("CREATE INDEX idx_active_lower_name ON users ((lower(name))) WHERE status = 'active';")

      val plan = executeSQL("EXPLAIN SELECT * FROM users WHERE lower(name) = 'alice' AND status = 'active';")
        .collect { case ExplainResult(p) => p }.last
      plan should include("Index Scan")
    }

    "partial expression index returns correct results" in {
      val session = setupSession(setup)
      given Session = session

      executeSQL("CREATE INDEX idx_active_lower_name ON users ((lower(name))) WHERE status = 'active';")

      val rows = executeSQL("SELECT name FROM users WHERE lower(name) = 'alice' AND status = 'active';")
        .collect { case QueryResult(t) => t }.last
      rows.data.length shouldBe 1
      rows.data(0).data(0) shouldBe TextValue("Alice")
    }

    "partial expression index not used without WHERE match" in {
      val session = setupSession(setup)
      given Session = session

      executeSQL("CREATE INDEX idx_active_lower_name ON users ((lower(name))) WHERE status = 'active';")

      // Missing status = 'active', so partial index shouldn't be used
      val plan = executeSQL("EXPLAIN SELECT * FROM users WHERE lower(name) = 'alice';")
        .collect { case ExplainResult(p) => p }.last
      plan should include("Seq Scan")
    }
  }
}

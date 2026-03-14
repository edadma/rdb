package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class QualifiedColumnTests extends AnyFreeSpec with Matchers with Testing {

  val setup =
    """CREATE TABLE users (id INT, name TEXT, dept_id INT);
      |CREATE TABLE departments (id INT, dname TEXT);
      |INSERT INTO departments VALUES (1, 'eng'), (2, 'sales');
      |INSERT INTO users VALUES (1, 'alice', 1), (2, 'bob', 2), (3, 'charlie', 1);
      |""".stripMargin

  // ── Basic qualified column in WHERE ───────────────────────────────

  "qualified columns in WHERE" - {
    "table.column in simple WHERE" in {
      val table = query(s"$setup SELECT users.name FROM users WHERE users.id = 1;")
      table.data(0).data(0).string shouldBe "alice"
    }

    "table.column with comparison" in {
      val table = query(s"$setup SELECT users.name FROM users WHERE users.id > 1 ORDER BY users.id;")
      table.data.map(_.data(0).string) shouldBe IndexedSeq("bob", "charlie")
    }

    "table.column in AND condition" in {
      val table = query(s"$setup SELECT users.name FROM users WHERE users.id >= 1 AND users.dept_id = 1 ORDER BY users.id;")
      table.data.map(_.data(0).string) shouldBe IndexedSeq("alice", "charlie")
    }
  }

  // ── Qualified columns in SELECT ───────────────────────────────────

  "qualified columns in SELECT" - {
    "table.column in projection" in {
      val table = query(s"$setup SELECT users.id, users.name FROM users WHERE id = 2;")
      table.data(0).data(0).intValue shouldBe 2
      table.data(0).data(1).string shouldBe "bob"
    }

    "mixed qualified and unqualified" in {
      val table = query(s"$setup SELECT users.name, dept_id FROM users WHERE users.id = 1;")
      table.data(0).data(0).string shouldBe "alice"
      table.data(0).data(1).intValue shouldBe 1
    }
  }

  // ── Qualified columns in JOINs ────────────────────────────────────

  "qualified columns in JOINs" - {
    "qualified columns in JOIN ON" in {
      val table = query(
        s"""$setup SELECT users.name, departments.dname
           |FROM users
           |JOIN departments ON users.dept_id = departments.id
           |ORDER BY users.id;
           |""".stripMargin
      )
      table.data.map(r => (r.data(0).string, r.data(1).string)) shouldBe
        IndexedSeq(("alice", "eng"), ("bob", "sales"), ("charlie", "eng"))
    }

    "qualified columns in WHERE with JOIN" in {
      val table = query(
        s"""$setup SELECT users.name
           |FROM users
           |JOIN departments ON users.dept_id = departments.id
           |WHERE departments.dname = 'eng'
           |ORDER BY users.id;
           |""".stripMargin
      )
      table.data.map(_.data(0).string) shouldBe IndexedSeq("alice", "charlie")
    }

    "qualified column disambiguates after JOIN" in {
      val table = query(
        s"""$setup SELECT users.id, departments.id
           |FROM users
           |JOIN departments ON users.dept_id = departments.id
           |WHERE users.id = 1;
           |""".stripMargin
      )
      table.data(0).data(0).intValue shouldBe 1
      table.data(0).data(1).intValue shouldBe 1
    }
  }

  // ── Qualified columns in subqueries ───────────────────────────────

  "qualified columns in subqueries" - {
    "table.column in subquery WHERE" in {
      val table = query(
        s"""$setup SELECT users.name FROM users
           |WHERE users.dept_id IN (SELECT departments.id FROM departments WHERE departments.dname = 'eng')
           |ORDER BY users.id;
           |""".stripMargin
      )
      table.data.map(_.data(0).string) shouldBe IndexedSeq("alice", "charlie")
    }

    "correlated subquery with qualified outer reference" in {
      val table = query(
        s"""$setup SELECT u.name FROM users u
           |WHERE EXISTS (SELECT 1 FROM departments d WHERE d.id = u.dept_id AND d.dname = 'sales');
           |""".stripMargin
      )
      table.data.map(_.data(0).string) shouldBe IndexedSeq("bob")
    }

    "qualified column in subquery projection" in {
      val table = query(
        s"""$setup SELECT departments.dname FROM departments
           |WHERE departments.id = (SELECT users.dept_id FROM users WHERE users.id = 1);
           |""".stripMargin
      )
      table.data(0).data(0).string shouldBe "eng"
    }

    "scalar subquery with qualified column" in {
      val table = query(
        s"""$setup SELECT users.name,
           |  (SELECT departments.dname FROM departments WHERE departments.id = users.dept_id) AS dept
           |FROM users
           |ORDER BY users.id;
           |""".stripMargin
      )
      table.data.map(r => (r.data(0).string, r.data(1).string)) shouldBe
        IndexedSeq(("alice", "eng"), ("bob", "sales"), ("charlie", "eng"))
    }
  }

  // ── Qualified columns with aliases ────────────────────────────────

  "qualified columns with aliases" - {
    "alias.column in WHERE" in {
      val table = query(s"$setup SELECT u.name FROM users u WHERE u.id = 1;")
      table.data(0).data(0).string shouldBe "alice"
    }

    "alias.column in ORDER BY" in {
      val table = query(s"$setup SELECT u.name FROM users u ORDER BY u.id DESC;")
      table.data(0).data(0).string shouldBe "charlie"
    }

    "alias.column in JOIN with alias" in {
      val table = query(
        s"""$setup SELECT u.name, d.dname
           |FROM users u
           |JOIN departments d ON u.dept_id = d.id
           |WHERE d.dname = 'eng'
           |ORDER BY u.id;
           |""".stripMargin
      )
      table.data.map(r => (r.data(0).string, r.data(1).string)) shouldBe
        IndexedSeq(("alice", "eng"), ("charlie", "eng"))
    }
  }

  // ── Qualified columns in ORDER BY ─────────────────────────────────

  "qualified columns in ORDER BY" - {
    "table.column in ORDER BY" in {
      val table = query(s"$setup SELECT name FROM users ORDER BY users.id DESC;")
      table.data.map(_.data(0).string) shouldBe IndexedSeq("charlie", "bob", "alice")
    }
  }

  // ── Qualified columns in UPDATE/DELETE ─────────────────────────────

  "qualified columns in UPDATE" - {
    "table.column in UPDATE WHERE" in {
      val res = results(
        s"""$setup UPDATE users SET name = 'alicia' WHERE users.id = 1;
           |SELECT name FROM users WHERE id = 1;
           |""".stripMargin
      )
      val table = res.collect { case QueryResult(t) => t }.last
      table.data(0).data(0).string shouldBe "alicia"
    }
  }

  "qualified columns in DELETE" - {
    "table.column in DELETE WHERE" in {
      val res = results(
        s"""$setup DELETE FROM users WHERE users.id = 3;
           |SELECT count(*) FROM users;
           |""".stripMargin
      )
      val table = res.collect { case QueryResult(t) => t }.last
      table.data(0).data(0).intValue shouldBe 2
    }
  }
}

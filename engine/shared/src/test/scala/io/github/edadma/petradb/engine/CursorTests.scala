package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CursorTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, age INT, active BOOLEAN NOT NULL DEFAULT true);
      |INSERT INTO users (name, age) VALUES ('Alice', 30);
      |INSERT INTO users (name, age) VALUES ('Bob', 25);
      |INSERT INTO users (name, age) VALUES ('Carol', 35);
      |INSERT INTO users (name, age, active) VALUES ('Dave', null, false);
      |INSERT INTO users (name, age) VALUES ('Eve', 28);
      |""".trim.stripMargin

  // ── Basic step/column access ────────────────────────────────────

  "step and column access" - {
    "step returns true for each row, false at end" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name, age FROM users ORDER BY name;")

      cursor.step() shouldBe true
      cursor.columnText(0) shouldBe "Alice"
      cursor.columnInt(1) shouldBe 30

      cursor.step() shouldBe true
      cursor.columnText(0) shouldBe "Bob"

      cursor.step() shouldBe true
      cursor.columnText(0) shouldBe "Carol"

      cursor.step() shouldBe true
      cursor.columnText(0) shouldBe "Dave"

      cursor.step() shouldBe true
      cursor.columnText(0) shouldBe "Eve"

      cursor.step() shouldBe false
      cursor.close()
    }

    "columnInt reads integer values" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT age FROM users WHERE name = 'Alice';")
      cursor.step() shouldBe true
      cursor.columnInt(0) shouldBe 30
      cursor.close()
    }

    "columnDouble reads numeric values" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT age * 1.5 FROM users WHERE name = 'Alice';")
      cursor.step() shouldBe true
      cursor.columnDouble(0) shouldBe 45.0
      cursor.close()
    }

    "columnText reads text values" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name FROM users WHERE name = 'Bob';")
      cursor.step() shouldBe true
      cursor.columnText(0) shouldBe "Bob"
      cursor.close()
    }

    "columnBoolean reads boolean values" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT active FROM users WHERE name = 'Dave';")
      cursor.step() shouldBe true
      cursor.columnBoolean(0) shouldBe false
      cursor.close()
    }

    "columnIsNull detects NULL" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT age FROM users WHERE name = 'Dave';")
      cursor.step() shouldBe true
      cursor.columnIsNull(0) shouldBe true
      cursor.close()
    }

    "columnIsNull returns false for non-NULL" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT age FROM users WHERE name = 'Alice';")
      cursor.step() shouldBe true
      cursor.columnIsNull(0) shouldBe false
      cursor.close()
    }
  }

  // ── Metadata ────────────────────────────────────────────────────

  "metadata" - {
    "columnCount reflects query columns" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name, age, active FROM users;")
      cursor.columnCount shouldBe 3
      cursor.close()
    }

    "columnName returns correct names" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name, age FROM users;")
      cursor.columnName(0) shouldBe "name"
      cursor.columnName(1) shouldBe "age"
      cursor.close()
    }

    "columnType returns correct types" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name, age, active FROM users;")
      cursor.columnType(0) shouldBe TextType
      cursor.columnType(1) shouldBe IntegerType
      cursor.columnType(2) shouldBe BooleanType
      cursor.close()
    }

    "metadata available before first step" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name FROM users;")
      cursor.columnCount shouldBe 1
      cursor.columnName(0) shouldBe "name"
      cursor.close()
    }
  }

  // ── fetch and move ──────────────────────────────────────────────

  "fetch" - {
    "fetches multiple rows at once" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name FROM users ORDER BY name;")
      val rows = cursor.fetch(3)
      rows.length shouldBe 3
      rows(0).data(0).string shouldBe "Alice"
      rows(1).data(0).string shouldBe "Bob"
      rows(2).data(0).string shouldBe "Carol"
      cursor.close()
    }

    "fetch returns remaining when less than n available" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name FROM users ORDER BY name;")
      cursor.fetch(3) // consume first 3
      val remaining = cursor.fetch(10) // ask for 10 but only 2 left
      remaining.length shouldBe 2
      remaining(0).data(0).string shouldBe "Dave"
      remaining(1).data(0).string shouldBe "Eve"
      cursor.close()
    }

    "fetch returns empty when no rows left" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name FROM users ORDER BY name;")
      cursor.fetch(5)
      cursor.fetch(1).length shouldBe 0
      cursor.close()
    }
  }

  "move" - {
    "skips rows" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name FROM users ORDER BY name;")
      cursor.move(3) shouldBe 3
      cursor.step() shouldBe true
      cursor.columnText(0) shouldBe "Dave"
      cursor.close()
    }

    "returns actual count when fewer rows available" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name FROM users ORDER BY name;")
      cursor.move(10) shouldBe 5
      cursor.step() shouldBe false
      cursor.close()
    }
  }

  // ── rowCount ────────────────────────────────────────────────────

  "rowCount" - {
    "tracks consumed rows" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name FROM users;")
      cursor.rowCount shouldBe 0
      cursor.step()
      cursor.rowCount shouldBe 1
      cursor.step()
      cursor.rowCount shouldBe 2
      cursor.close()
    }
  }

  // ── toSeq ───────────────────────────────────────────────────────

  "toSeq" - {
    "materializes all rows" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name FROM users ORDER BY name;")
      val rows = cursor.toSeq
      rows.length shouldBe 5
      rows.map(_.data(0).string) shouldBe Seq("Alice", "Bob", "Carol", "Dave", "Eve")
      cursor.close()
    }

    "materializes remaining rows after partial consumption" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name FROM users ORDER BY name;")
      cursor.step() // consume Alice
      val remaining = cursor.toSeq
      remaining.length shouldBe 4
      remaining(0).data(0).string shouldBe "Bob"
      cursor.close()
    }
  }

  // ── Parameterized cursors ───────────────────────────────────────

  "parameterized" - {
    "cursor with $1 parameter" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name, age FROM users WHERE age > $1 ORDER BY name;", IndexedSeq(28))
      cursor.step() shouldBe true
      cursor.columnText(0) shouldBe "Alice" // age 30
      cursor.step() shouldBe true
      cursor.columnText(0) shouldBe "Carol" // age 35
      cursor.step() shouldBe false
      cursor.close()
    }

    "cursor with multiple parameters" in {
      val session = setupSession(setup)
      val cursor = session.openCursor(
        "SELECT name FROM users WHERE age >= $1 AND age <= $2 ORDER BY name;",
        IndexedSeq(25, 30),
      )
      val names = cursor.toSeq.map(_.data(0).string)
      names shouldBe Seq("Alice", "Bob", "Eve")
      cursor.close()
    }
  }

  // ── Empty results ───────────────────────────────────────────────

  "empty results" - {
    "step returns false immediately" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name FROM users WHERE age > 100;")
      cursor.step() shouldBe false
      cursor.close()
    }

    "metadata still available" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name, age FROM users WHERE age > 100;")
      cursor.columnCount shouldBe 2
      cursor.columnName(0) shouldBe "name"
      cursor.step() shouldBe false
      cursor.close()
    }
  }

  // ── Error cases ─────────────────────────────────────────────────

  "errors" - {
    "columnValue before step throws" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name FROM users;")
      an[IllegalStateException] should be thrownBy {
        cursor.columnText(0)
      }
      cursor.close()
    }

    "step after close throws" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name FROM users;")
      cursor.close()
      an[IllegalArgumentException] should be thrownBy {
        cursor.step()
      }
    }

    "fetch after close throws" in {
      val session = setupSession(setup)
      val cursor = session.openCursor("SELECT name FROM users;")
      cursor.close()
      an[IllegalArgumentException] should be thrownBy {
        cursor.fetch(1)
      }
    }
  }

  // ── Lazy evaluation ─────────────────────────────────────────────

  "laziness" - {
    "cursor does not materialize all rows on open" in {
      val session = setupSession(setup)
      // If this materializes all rows, there's no way to test laziness directly,
      // but we can verify that partial consumption works correctly
      val cursor = session.openCursor("SELECT name FROM users ORDER BY name;")
      cursor.step() shouldBe true
      cursor.columnText(0) shouldBe "Alice"
      // Close without consuming remaining — no error
      cursor.close()
      cursor.isClosed shouldBe true
    }
  }
}

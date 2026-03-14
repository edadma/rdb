package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ValueWindowFunctionTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE sales (id SERIAL PRIMARY KEY, dept TEXT NOT NULL, employee TEXT NOT NULL, amount INT NOT NULL);
      |INSERT INTO sales (dept, employee, amount) VALUES ('eng', 'Alice', 100);
      |INSERT INTO sales (dept, employee, amount) VALUES ('eng', 'Bob', 200);
      |INSERT INTO sales (dept, employee, amount) VALUES ('eng', 'Carol', 150);
      |INSERT INTO sales (dept, employee, amount) VALUES ('mkt', 'Dave', 300);
      |INSERT INTO sales (dept, employee, amount) VALUES ('mkt', 'Eve', 250);
      |""".trim.stripMargin

  // ── FIRST_VALUE ──────────────────────────────────────────────────

  "FIRST_VALUE" - {
    "returns first value in ordered partition" in {
      val table = query(
        s"$setup SELECT employee, dept, amount, FIRST_VALUE(employee) OVER (PARTITION BY dept ORDER BY amount) AS first_emp FROM sales ORDER BY dept, amount;"
      )
      // eng partition ordered by amount: Alice(100), Carol(150), Bob(200) → first is Alice
      val engRows = table.data.filter(r => r.data(1).string == "eng")
      engRows.foreach(r => r.data(3).string shouldBe "Alice")

      // mkt partition ordered by amount: Eve(250), Dave(300) → first is Eve
      val mktRows = table.data.filter(r => r.data(1).string == "mkt")
      mktRows.foreach(r => r.data(3).string shouldBe "Eve")
    }

    "returns first value without partition (whole table)" in {
      val table = query(
        s"$setup SELECT employee, FIRST_VALUE(amount) OVER (ORDER BY amount) AS first_amt FROM sales ORDER BY amount;"
      )
      // Whole table ordered by amount: first is 100
      table.data.foreach(r => r.data(1).intValue shouldBe 100)
    }

    "returns first value with unbounded frame" in {
      val table = query(
        s"$setup SELECT employee, amount, FIRST_VALUE(employee) OVER (ORDER BY amount ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS first_emp FROM sales ORDER BY amount;"
      )
      table.data.foreach(r => r.data(2).string shouldBe "Alice")
    }
  }

  // ── LAST_VALUE ───────────────────────────────────────────────────

  "LAST_VALUE" - {
    "returns current row with default frame (RANGE BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)" in {
      val table = query(
        s"$setup SELECT employee, amount, LAST_VALUE(employee) OVER (ORDER BY amount) AS last_emp FROM sales ORDER BY amount;"
      )
      // Default frame is UNBOUNDED PRECEDING to CURRENT ROW, so last_value = current row's value
      table.data(0).data(2).string shouldBe "Alice"   // amount=100
      table.data(1).data(2).string shouldBe "Carol"   // amount=150
      table.data(2).data(2).string shouldBe "Bob"     // amount=200
    }

    "returns last value with unbounded frame" in {
      val table = query(
        s"$setup SELECT employee, amount, LAST_VALUE(employee) OVER (ORDER BY amount ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS last_emp FROM sales ORDER BY amount;"
      )
      // With full frame, last value is always Dave (amount=300)
      table.data.foreach(r => r.data(2).string shouldBe "Dave")
    }

    "returns last value per partition with unbounded frame" in {
      val table = query(
        s"$setup SELECT employee, dept, amount, LAST_VALUE(employee) OVER (PARTITION BY dept ORDER BY amount ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS last_emp FROM sales ORDER BY dept, amount;"
      )
      // eng partition: Alice(100), Carol(150), Bob(200) → last is Bob
      val engRows = table.data.filter(r => r.data(1).string == "eng")
      engRows.foreach(r => r.data(3).string shouldBe "Bob")

      // mkt partition: Eve(250), Dave(300) → last is Dave
      val mktRows = table.data.filter(r => r.data(1).string == "mkt")
      mktRows.foreach(r => r.data(3).string shouldBe "Dave")
    }
  }

  // ── NTH_VALUE ────────────────────────────────────────────────────

  "NTH_VALUE" - {
    "returns nth value from start of frame" in {
      val table = query(
        s"$setup SELECT employee, amount, NTH_VALUE(employee, 2) OVER (ORDER BY amount ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS second_emp FROM sales ORDER BY amount;"
      )
      // Ordered: Alice(100), Carol(150), Bob(200), Eve(250), Dave(300)
      // 2nd value is Carol
      table.data.foreach(r => r.data(2).string shouldBe "Carol")
    }

    "returns null when n exceeds frame size" in {
      val table = query(
        s"$setup SELECT employee, dept, NTH_VALUE(employee, 5) OVER (PARTITION BY dept ORDER BY amount ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS fifth_emp FROM sales ORDER BY dept, amount;"
      )
      // eng has 3 rows, mkt has 2 rows — neither has a 5th value
      table.data.foreach(r => r.data(2).isNull shouldBe true)
    }

    "returns first value when n=1" in {
      val table = query(
        s"$setup SELECT employee, amount, NTH_VALUE(employee, 1) OVER (ORDER BY amount ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS first_emp FROM sales ORDER BY amount;"
      )
      table.data.foreach(r => r.data(2).string shouldBe "Alice")
    }

    "returns nth value per partition" in {
      val table = query(
        s"$setup SELECT employee, dept, amount, NTH_VALUE(employee, 2) OVER (PARTITION BY dept ORDER BY amount ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS second_emp FROM sales ORDER BY dept, amount;"
      )
      // eng partition: Alice(100), Carol(150), Bob(200) → 2nd is Carol
      val engRows = table.data.filter(r => r.data(1).string == "eng")
      engRows.foreach(r => r.data(3).string shouldBe "Carol")

      // mkt partition: Eve(250), Dave(300) → 2nd is Dave
      val mktRows = table.data.filter(r => r.data(1).string == "mkt")
      mktRows.foreach(r => r.data(3).string shouldBe "Dave")
    }

    "returns null when n exceeds current frame with default frame" in {
      val table = query(
        s"$setup SELECT employee, amount, NTH_VALUE(employee, 3) OVER (ORDER BY amount) AS third_emp FROM sales ORDER BY amount;"
      )
      // Default frame: UNBOUNDED PRECEDING to CURRENT ROW
      // Row 0 (Alice, 100): frame has 1 row → null
      table.data(0).data(2).isNull shouldBe true
      // Row 1 (Carol, 150): frame has 2 rows → null
      table.data(1).data(2).isNull shouldBe true
      // Row 2 (Bob, 200): frame has 3 rows → 3rd is Bob
      table.data(2).data(2).string shouldBe "Bob"
      // Row 3+: frame has 4+ rows → 3rd is still Bob
      table.data(3).data(2).string shouldBe "Bob"
    }
  }

  // ── Combined with other window functions ─────────────────────────

  "combined" - {
    "FIRST_VALUE with ROW_NUMBER" in {
      val table = query(
        s"$setup SELECT employee, amount, ROW_NUMBER() OVER (ORDER BY amount) AS rn, FIRST_VALUE(employee) OVER (ORDER BY amount) AS first_emp FROM sales ORDER BY amount;"
      )
      table.data(0).data(2).intValue shouldBe 1
      table.data(0).data(3).string shouldBe "Alice"
      table.data(4).data(2).intValue shouldBe 5
      table.data(4).data(3).string shouldBe "Alice"
    }
  }

  // ── Error cases ──────────────────────────────────────────────────

  "errors" - {
    "FIRST_VALUE with no arguments fails" in {
      an[Exception] should be thrownBy {
        query(s"$setup SELECT FIRST_VALUE() OVER (ORDER BY amount) FROM sales;")
      }
    }

    "LAST_VALUE with no arguments fails" in {
      an[Exception] should be thrownBy {
        query(s"$setup SELECT LAST_VALUE() OVER (ORDER BY amount) FROM sales;")
      }
    }

    "NTH_VALUE with wrong number of arguments fails" in {
      an[Exception] should be thrownBy {
        query(s"$setup SELECT NTH_VALUE(employee) OVER (ORDER BY amount) FROM sales;")
      }
    }

    "NTH_VALUE with three arguments fails" in {
      an[Exception] should be thrownBy {
        query(s"$setup SELECT NTH_VALUE(employee, 2, 'x') OVER (ORDER BY amount) FROM sales;")
      }
    }
  }
}

package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class OrderByAliasTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE emp (name TEXT, dept TEXT, salary INT);
      |INSERT INTO emp (name, dept, salary) VALUES
      |  ('Alice', 'Eng', 90000),
      |  ('Bob', 'Eng', 70000),
      |  ('Carol', 'Sales', 80000),
      |  ('Dave', 'Sales', 60000);
      |""".trim.stripMargin

  def names(t: TableValue): Vector[String] =
    t.data.map(_.data(0).asInstanceOf[TextValue].s).toVector

  // ── Non-grouped, non-windowed: ORDER BY alias ─────────────────────

  "ORDER BY alias on simple expression" in {
    val t = query(
      s"""$setup
         |SELECT name, salary * 2 AS double_sal FROM emp ORDER BY double_sal;
         |""".stripMargin
    )
    names(t) shouldBe Vector("Dave", "Bob", "Carol", "Alice")
  }

  "ORDER BY alias DESC" in {
    val t = query(
      s"""$setup
         |SELECT name, salary AS sal FROM emp ORDER BY sal DESC;
         |""".stripMargin
    )
    names(t) shouldBe Vector("Alice", "Carol", "Bob", "Dave")
  }

  "ORDER BY alias mixed with column name" in {
    val t = query(
      s"""$setup
         |SELECT name, salary AS sal FROM emp ORDER BY dept, sal DESC;
         |""".stripMargin
    )
    // Eng: Alice(90k), Bob(70k); Sales: Carol(80k), Dave(60k)
    names(t) shouldBe Vector("Alice", "Bob", "Carol", "Dave")
  }

  // ── Non-grouped, windowed: ORDER BY alias on window function ──────

  "ORDER BY alias on RANK() window function" in {
    val t = query(
      s"""$setup
         |SELECT name, RANK() OVER (ORDER BY salary DESC) AS rnk FROM emp ORDER BY rnk;
         |""".stripMargin
    )
    names(t) shouldBe Vector("Alice", "Carol", "Bob", "Dave")
    t.data.map(_.data(1).intValue).toVector shouldBe Vector(1, 2, 3, 4)
  }

  "ORDER BY alias on ROW_NUMBER() window function" in {
    val t = query(
      s"""$setup
         |SELECT name, ROW_NUMBER() OVER (PARTITION BY dept ORDER BY salary DESC) AS rn
         |FROM emp ORDER BY rn, name;
         |""".stripMargin
    )
    // rn=1: Alice(Eng), Carol(Sales); rn=2: Bob(Eng), Dave(Sales)
    names(t) shouldBe Vector("Alice", "Carol", "Bob", "Dave")
  }

  "ORDER BY alias DESC on window function" in {
    val t = query(
      s"""$setup
         |SELECT name, RANK() OVER (ORDER BY salary DESC) AS rnk FROM emp ORDER BY rnk DESC;
         |""".stripMargin
    )
    names(t) shouldBe Vector("Dave", "Bob", "Carol", "Alice")
  }

  "ORDER BY alias on DENSE_RANK() window function" in {
    val t = query(
      s"""$setup
         |SELECT name, DENSE_RANK() OVER (ORDER BY dept) AS dr FROM emp ORDER BY dr DESC, name;
         |""".stripMargin
    )
    // dept='Sales' gets rank 2, 'Eng' gets rank 1 → DESC: Sales first
    names(t) shouldBe Vector("Carol", "Dave", "Alice", "Bob")
  }

  // ── Grouped: ORDER BY alias (already worked, regression test) ─────

  "ORDER BY alias on aggregate" in {
    val t = query(
      s"""$setup
         |SELECT dept, SUM(salary) AS total FROM emp GROUP BY dept ORDER BY total;
         |""".stripMargin
    )
    t.data.map(_.data(0).asInstanceOf[TextValue].s).toVector shouldBe Vector("Sales", "Eng")
    t.data.map(_.data(1).intValue).toVector shouldBe Vector(140000, 160000)
  }

  "ORDER BY alias on grouped query with COUNT" in {
    val t = query(
      s"""$setup
         |SELECT dept, COUNT(*) AS cnt FROM emp GROUP BY dept ORDER BY cnt DESC;
         |""".stripMargin
    )
    t.data.map(_.data(0).asInstanceOf[TextValue].s).toVector shouldBe Vector("Eng", "Sales")
  }

  // ── Ordinal still works alongside alias ───────────────────────────

  "ORDER BY ordinal still works in windowed query" in {
    val t = query(
      s"""$setup
         |SELECT name, RANK() OVER (ORDER BY salary DESC) AS rnk FROM emp ORDER BY 2;
         |""".stripMargin
    )
    names(t) shouldBe Vector("Alice", "Carol", "Bob", "Dave")
  }
}

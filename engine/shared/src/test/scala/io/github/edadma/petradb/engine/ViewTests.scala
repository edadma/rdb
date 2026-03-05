package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ViewTests extends AnyFreeSpec with Matchers with Testing {

  "basic view creation and querying" in {
    val t = query(
      """
        |CREATE TABLE employees (id INT, name TEXT, salary INT);
        |INSERT INTO employees (id, name, salary) VALUES (1, 'Alice', 50000);
        |INSERT INTO employees (id, name, salary) VALUES (2, 'Bob', 60000);
        |CREATE VIEW emp_view AS SELECT name, salary FROM employees;
        |SELECT * FROM emp_view;
        |""".trim.stripMargin,
    )
    assert(t.data.length == 2)
    assert(t.meta.columns.map(_.name).toSet == Set("name", "salary"))
  }

  "view with WHERE clause" in {
    val t = query(
      """
        |CREATE TABLE items (id INT, name TEXT, price INT);
        |INSERT INTO items (id, name, price) VALUES (1, 'Widget', 10);
        |INSERT INTO items (id, name, price) VALUES (2, 'Gadget', 25);
        |INSERT INTO items (id, name, price) VALUES (3, 'Gizmo', 5);
        |CREATE VIEW expensive AS SELECT name, price FROM items WHERE price > 8;
        |SELECT * FROM expensive ORDER BY price;
        |""".trim.stripMargin,
    )
    assert(t.data.length == 2)
    assert(t.data(0).data(0).string == "Widget")
    assert(t.data(1).data(0).string == "Gadget")
  }

  "view with joins" in {
    val t = query(
      """
        |CREATE TABLE departments (id INT PRIMARY KEY, name TEXT);
        |CREATE TABLE staff (id INT, name TEXT, dept_id INT);
        |INSERT INTO departments (id, name) VALUES (1, 'Engineering');
        |INSERT INTO departments (id, name) VALUES (2, 'Sales');
        |INSERT INTO staff (id, name, dept_id) VALUES (1, 'Alice', 1);
        |INSERT INTO staff (id, name, dept_id) VALUES (2, 'Bob', 2);
        |CREATE VIEW staff_dept AS SELECT s.name AS staff_name, d.name AS dept_name FROM staff s INNER JOIN departments d ON s.dept_id = d.id;
        |SELECT * FROM staff_dept ORDER BY staff_name;
        |""".trim.stripMargin,
    )
    assert(t.data.length == 2)
    assert(t.data(0).data(0).string == "Alice")
    assert(t.data(0).data(1).string == "Engineering")
    assert(t.data(1).data(0).string == "Bob")
    assert(t.data(1).data(1).string == "Sales")
  }

  "view with aggregation" in {
    val t = query(
      """
        |CREATE TABLE sales (id INT, region TEXT, amount INT);
        |INSERT INTO sales (id, region, amount) VALUES (1, 'North', 100);
        |INSERT INTO sales (id, region, amount) VALUES (2, 'North', 200);
        |INSERT INTO sales (id, region, amount) VALUES (3, 'South', 150);
        |CREATE VIEW region_totals AS SELECT region, SUM(amount) AS total FROM sales GROUP BY region;
        |SELECT * FROM region_totals ORDER BY region;
        |""".trim.stripMargin,
    )
    assert(t.data.length == 2)
    assert(t.data(0).data(0).string == "North")
    assert(t.data(0).data(1).string == "300")
    assert(t.data(1).data(0).string == "South")
    assert(t.data(1).data(1).string == "150")
  }

  "nested views" in {
    val t = query(
      """
        |CREATE TABLE nums (n INT);
        |INSERT INTO nums (n) VALUES (1);
        |INSERT INTO nums (n) VALUES (2);
        |INSERT INTO nums (n) VALUES (3);
        |INSERT INTO nums (n) VALUES (4);
        |INSERT INTO nums (n) VALUES (5);
        |CREATE VIEW odds AS SELECT n FROM nums WHERE n % 2 = 1;
        |CREATE VIEW big_odds AS SELECT n FROM odds WHERE n > 2;
        |SELECT * FROM big_odds ORDER BY n;
        |""".trim.stripMargin,
    )
    assert(t.data.length == 2)
    assert(t.data(0).data(0).string == "3")
    assert(t.data(1).data(0).string == "5")
  }

  "CREATE OR REPLACE VIEW" in {
    val t = query(
      """
        |CREATE TABLE t (a INT, b INT);
        |INSERT INTO t (a, b) VALUES (1, 10);
        |INSERT INTO t (a, b) VALUES (2, 20);
        |CREATE VIEW v AS SELECT a FROM t;
        |CREATE OR REPLACE VIEW v AS SELECT b FROM t;
        |SELECT * FROM v ORDER BY b;
        |""".trim.stripMargin,
    )
    assert(t.data.length == 2)
    assert(t.data(0).data(0).string == "10")
    assert(t.data(1).data(0).string == "20")
  }

  "DROP VIEW" in {
    val r = results(
      """
        |CREATE TABLE t (x INT);
        |CREATE VIEW v AS SELECT x FROM t;
        |DROP VIEW v;
        |""".trim.stripMargin,
    )
    assert(r.last.isInstanceOf[DropViewResult])
  }

  "DROP VIEW IF EXISTS on nonexistent" in {
    val r = results(
      """
        |DROP VIEW IF EXISTS no_such_view;
        |""".trim.stripMargin,
    )
    assert(r.last.isInstanceOf[DropViewResult])
  }

  "DROP VIEW error on nonexistent" in {
    assertThrows[PetraException] {
      results("DROP VIEW no_such_view")
    }
  }

  "view name conflicts with table" in {
    assertThrows[PetraException] {
      results(
        """
          |CREATE TABLE t (x INT);
          |CREATE VIEW t AS SELECT 1;
          |""".trim.stripMargin,
      )
    }
  }

  "duplicate view name without OR REPLACE" in {
    assertThrows[PetraException] {
      results(
        """
          |CREATE TABLE t (x INT);
          |CREATE VIEW v AS SELECT x FROM t;
          |CREATE VIEW v AS SELECT x FROM t;
          |""".trim.stripMargin,
      )
    }
  }

  "view with ORDER BY and LIMIT" in {
    val t = query(
      """
        |CREATE TABLE t (n INT);
        |INSERT INTO t (n) VALUES (3);
        |INSERT INTO t (n) VALUES (1);
        |INSERT INTO t (n) VALUES (2);
        |CREATE VIEW v AS SELECT n FROM t ORDER BY n LIMIT 2;
        |SELECT * FROM v;
        |""".trim.stripMargin,
    )
    assert(t.data.length == 2)
    assert(t.data(0).data(0).string == "1")
    assert(t.data(1).data(0).string == "2")
  }

  "view with UNION" in {
    val t = query(
      """
        |CREATE TABLE a (x INT);
        |CREATE TABLE b (x INT);
        |INSERT INTO a (x) VALUES (1);
        |INSERT INTO a (x) VALUES (2);
        |INSERT INTO b (x) VALUES (2);
        |INSERT INTO b (x) VALUES (3);
        |CREATE VIEW combined AS SELECT x FROM a UNION SELECT x FROM b;
        |SELECT * FROM combined ORDER BY x;
        |""".trim.stripMargin,
    )
    assert(t.data.length == 3)
    assert(t.data(0).data(0).string == "1")
    assert(t.data(1).data(0).string == "2")
    assert(t.data(2).data(0).string == "3")
  }

  "exprToSQL round-trip for simple SELECT" in {
    val sql = "SELECT name, salary FROM employees WHERE salary > 50000"
    val parsed = SQLParser.parseQuery(sql)
    val reconstructed = exprToSQL(parsed)
    val reparsed = SQLParser.parseQuery(reconstructed)
    assert(reparsed.isInstanceOf[SQLSelectExpr])
  }

  "exprToSQL round-trip for SELECT with ORDER BY" in {
    val sql = "SELECT a, b FROM t ORDER BY a DESC LIMIT 10"
    val parsed = SQLParser.parseQuery(sql)
    val reconstructed = exprToSQL(parsed)
    assert(reconstructed.contains("ORDER BY"))
    assert(reconstructed.contains("LIMIT"))
    // Re-parse should succeed (may be CompoundQueryExpr or SQLSelectExpr depending on how parser groups it)
    val reparsed = SQLParser.parseQuery(reconstructed)
    assert(reparsed.isInstanceOf[CompoundQueryExpr] || reparsed.isInstanceOf[SQLSelectExpr])
  }

  "exprToSQL round-trip for UNION" in {
    val sql = "SELECT x FROM a UNION SELECT x FROM b"
    val parsed = SQLParser.parseQuery(sql)
    val reconstructed = exprToSQL(parsed)
    assert(reconstructed.contains("UNION"))
    SQLParser.parseQuery(reconstructed)
  }

  "view reflects live data changes" in {
    val t = query(
      """
        |CREATE TABLE t (n INT);
        |INSERT INTO t (n) VALUES (1);
        |CREATE VIEW v AS SELECT n FROM t;
        |INSERT INTO t (n) VALUES (2);
        |SELECT * FROM v ORDER BY n;
        |""".trim.stripMargin,
    )
    assert(t.data.length == 2)
    assert(t.data(0).data(0).string == "1")
    assert(t.data(1).data(0).string == "2")
  }
}

package io.github.edadma.petradb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class FastparseEdgeCaseTests extends AnyFreeSpec with Matchers with Testing:

  "large integer literals don't overflow" in {
    val table = query("""
      CREATE TABLE t (id BIGINT);
      INSERT INTO t (id) VALUES (2147483648);
      INSERT INTO t (id) VALUES (9999999999);
      SELECT id FROM t ORDER BY id;
    """)
    table.data.length shouldBe 2
  }

  "dot-qualified column names aren't confused with decimals" in {
    val table = query("""
      CREATE TABLE t (id INTEGER, value TEXT);
      INSERT INTO t (id, value) VALUES (1, 'hello');
      SELECT t.id, t.value FROM t;
    """)
    table.data.length shouldBe 1
    table.data(0).data(0) shouldBe NumberValue(1)
    table.data(0).data(1) shouldBe TextValue("hello")
  }

  "column named e near a string literal" in {
    val table = query("""
      CREATE TABLE t (e TEXT);
      INSERT INTO t (e) VALUES ('hello');
      SELECT e FROM t WHERE e = 'hello';
    """)
    table.data.length shouldBe 1
    table.data(0).data(0) shouldBe TextValue("hello")
  }

  "column named name (non-reserved word)" in {
    val table = query("""
      CREATE TABLE t (id INTEGER, name TEXT, value TEXT);
      INSERT INTO t (id, name, value) VALUES (1, 'alice', 'x');
      SELECT name, value FROM t WHERE name = 'alice';
    """)
    table.data.length shouldBe 1
    table.data(0).data(0) shouldBe TextValue("alice")
    table.data(0).data(1) shouldBe TextValue("x")
  }

  "reserved words cannot be used as column identifiers" in {
    a[ParseException] should be thrownBy {
      results("""
        CREATE TABLE t (date DATE, time TIME, text TEXT, integer INTEGER);
      """)
    }
  }

  "empty and whitespace-only input to parseCommands" in {
    SQLParser.parseCommands("") shouldBe empty
    SQLParser.parseCommands("   ") shouldBe empty
    SQLParser.parseCommands("-- just a comment") shouldBe empty
    SQLParser.parseCommands("/* block comment */") shouldBe empty
  }

  "strings containing comment-like content" in {
    val table = query("""
      CREATE TABLE t (v TEXT);
      INSERT INTO t (v) VALUES ('not -- a comment');
      INSERT INTO t (v) VALUES ('not /* a comment */ either');
      SELECT v FROM t ORDER BY v;
    """)
    table.data.length shouldBe 2
    table.data(0).data(0) shouldBe TextValue("not -- a comment")
    table.data(1).data(0) shouldBe TextValue("not /* a comment */ either")
  }

  "doubled single-quote escaping" in {
    val table = query("""
      CREATE TABLE t (v TEXT);
      INSERT INTO t (v) VALUES ('it''s');
      INSERT INTO t (v) VALUES ('he said ''hello''');
      INSERT INTO t (v) VALUES ('');
      SELECT v FROM t ORDER BY v;
    """)
    table.data.length shouldBe 3
    table.data(0).data(0) shouldBe TextValue("")
    table.data(1).data(0) shouldBe TextValue("he said 'hello'")
    table.data(2).data(0) shouldBe TextValue("it's")
  }

  "decimal literal edge cases" in {
    val table = query("""
      SELECT CAST(0.5 AS DOUBLE PRECISION) AS a,
             CAST(.5 AS DOUBLE PRECISION) AS b,
             CAST(5e2 AS DOUBLE PRECISION) AS c,
             CAST(1.5e-3 AS DOUBLE PRECISION) AS d;
    """)
    table.data.length shouldBe 1
    table.data(0).data(0) shouldBe NumberValue(0.5)
    table.data(0).data(1) shouldBe NumberValue(0.5)
    table.data(0).data(2) shouldBe NumberValue(500.0)
    table.data(0).data(3) shouldBe NumberValue(0.0015)
  }

  "nested block comments" in {
    val table = query("/* outer /* inner */ still comment */ SELECT 1 AS v;")
    table.data.length shouldBe 1
    table.data(0).data(0) shouldBe NumberValue(1)
  }

  "semicolons and multi-statement edge cases" in {
    // basic cases
    SQLParser.parseCommands("SELECT 1 AS a; SELECT 2 AS b;").length shouldBe 2
    SQLParser.parseCommands("SELECT 1 AS a;").length shouldBe 1
    SQLParser.parseCommands("SELECT 1 AS a").length shouldBe 1

    // double semicolons (empty statement slots are ignored)
    SQLParser.parseCommands("SELECT 1 AS a;; SELECT 2 AS b").length shouldBe 2
    SQLParser.parseCommands("SELECT 1 AS a;;; SELECT 2 AS b").length shouldBe 2

    // bare semicolons (no commands, returns empty)
    SQLParser.parseCommands(";") shouldBe empty
    SQLParser.parseCommands("  ;  ") shouldBe empty
    SQLParser.parseCommands(";;;") shouldBe empty

    // leading semicolons before a command
    SQLParser.parseCommands("; SELECT 1 AS a").length shouldBe 1
    SQLParser.parseCommands(";; SELECT 1 AS a; SELECT 2 AS b").length shouldBe 2

    // trailing semicolons
    SQLParser.parseCommands("SELECT 1 AS a;;;").length shouldBe 1
  }

  "bookstore.sql integration" in {
    val sql = scala.io.Source.fromFile("examples/bookstore/bookstore.sql").mkString
    given Session = new MemoryDB().connect()
    val res = executeSQL(sql)
    res should not be empty
  }

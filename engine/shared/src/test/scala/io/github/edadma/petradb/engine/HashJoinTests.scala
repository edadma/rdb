package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class HashJoinTests extends AnyFreeSpec with Matchers:

  private def setupSession(sql: String): Session =
    given session: Session = new MemoryDB().connect()
    executeSQL(sql)
    session

  private def findProcess[T](proc: Process)(pf: PartialFunction[Process, T]): Option[T] =
    if pf.isDefinedAt(proc) then Some(pf(proc))
    else
      proc match
        case p: ProjectProcess                    => findProcess(p.input)(pf)
        case p: SeqScanProcess                    => findProcess(p.input)(pf)
        case p: SortProcess                       => findProcess(p.input)(pf)
        case p: AggregateProcess                  => findProcess(p.input)(pf)
        case p: TakeProcess                       => findProcess(p.input)(pf)
        case _: DropProcess                       => None
        case p: DistinctProcess                   => findProcess(p.input)(pf)
        case p: HavingProcess                     => findProcess(p.input)(pf)
        case p: AliasProcess                      => findProcess(p.input)(pf)
        case p: ColumnAliasProcess                => findProcess(p.input)(pf)
        case p: CrossProcess                      => findProcess(p.input1)(pf).orElse(findProcess(p.input2)(pf))
        case p: LeftCrossJoinProcess              => findProcess(p.input1)(pf).orElse(findProcess(p.input2)(pf))
        case p: RightCrossJoinProcess             => findProcess(p.input1)(pf).orElse(findProcess(p.input2)(pf))
        case p: IndexNestedLoopJoinProcess        => findProcess(p.outer)(pf)
        case p: LeftIndexNestedLoopJoinProcess    => findProcess(p.outer)(pf)
        case p: RightIndexNestedLoopJoinProcess   => findProcess(p.outer)(pf)
        case p: HashJoinProcess                   => findProcess(p.build)(pf).orElse(findProcess(p.probe)(pf))
        case p: LeftHashJoinProcess               => findProcess(p.build)(pf).orElse(findProcess(p.probe)(pf))
        case p: RightHashJoinProcess              => findProcess(p.build)(pf).orElse(findProcess(p.probe)(pf))
        case p: FullHashJoinProcess               => findProcess(p.build)(pf).orElse(findProcess(p.probe)(pf))
        case p: WindowProcess                     => findProcess(p.input)(pf)
        case p: FullCrossJoinProcess              => findProcess(p.input1)(pf).orElse(findProcess(p.input2)(pf))
        case _                                    => None

  val setup: String =
    """
      |CREATE TABLE departments (
      |  id INT,
      |  name TEXT
      |);
      |CREATE TABLE employees (
      |  id INT,
      |  name TEXT,
      |  dept_id INT
      |);
      |INSERT INTO departments (id, name) VALUES (1, 'Engineering'), (2, 'Marketing'), (3, 'Sales');
      |INSERT INTO employees (id, name, dept_id) VALUES
      |  (1, 'Alice', 1),
      |  (2, 'Bob', 1),
      |  (3, 'Carol', 2),
      |  (4, 'Dave', 4);
      |""".trim.stripMargin

  "plan selection" - {

    "INNER JOIN on unindexed columns selects HashJoinProcess" in {
      given session: Session = setupSession(setup)
      val proc = procRewrite(SQLParser.parseQuery(
        "SELECT e.name, d.name FROM employees e INNER JOIN departments d ON e.dept_id = d.id ORDER BY e.name"))
      findProcess(proc) { case p: HashJoinProcess => p } shouldBe defined
      findProcess(proc) { case _: CrossProcess => true } shouldBe None
    }

    "LEFT JOIN on unindexed columns selects LeftHashJoinProcess" in {
      given session: Session = setupSession(setup)
      val proc = procRewrite(SQLParser.parseQuery(
        "SELECT e.name, d.name FROM employees e LEFT JOIN departments d ON e.dept_id = d.id ORDER BY e.name"))
      findProcess(proc) { case p: LeftHashJoinProcess => p } shouldBe defined
    }

    "RIGHT JOIN on unindexed columns selects RightHashJoinProcess" in {
      given session: Session = setupSession(setup)
      val proc = procRewrite(SQLParser.parseQuery(
        "SELECT e.name, d.name FROM employees e RIGHT JOIN departments d ON e.dept_id = d.id ORDER BY d.name"))
      findProcess(proc) { case p: RightHashJoinProcess => p } shouldBe defined
    }

    "FULL JOIN on unindexed columns selects FullHashJoinProcess" in {
      given session: Session = setupSession(setup)
      val proc = procRewrite(SQLParser.parseQuery(
        "SELECT e.name, d.name FROM employees e FULL JOIN departments d ON e.dept_id = d.id"))
      findProcess(proc) { case p: FullHashJoinProcess => p } shouldBe defined
    }

    "index join still preferred over hash join when index available" in {
      given session: Session = setupSession(setup + "\nCREATE INDEX idx_dept_id ON departments (id);")
      val proc = procRewrite(SQLParser.parseQuery(
        "SELECT e.name, d.name FROM employees e INNER JOIN departments d ON e.dept_id = d.id ORDER BY e.name"))
      findProcess(proc) { case p: IndexNestedLoopJoinProcess => p } shouldBe defined
      findProcess(proc) { case _: HashJoinProcess => true } shouldBe None
    }

    "non-equijoin falls back to cross product" in {
      given session: Session = setupSession(setup)
      val proc = procRewrite(SQLParser.parseQuery(
        "SELECT * FROM employees e INNER JOIN departments d ON e.dept_id > d.id"))
      findProcess(proc) { case _: HashJoinProcess => true } shouldBe None
      findProcess(proc) { case _: CrossProcess => true } shouldBe defined
    }
  }

  "result correctness" - {

    "INNER JOIN returns matching rows" in {
      given session: Session = setupSession(setup)
      val result = executeQuery(
        "SELECT e.name, d.name FROM employees e INNER JOIN departments d ON e.dept_id = d.id ORDER BY e.name")
      val data = result.table.data
      data.length shouldBe 3
      data(0).data(0) shouldBe TextValue("Alice")
      data(0).data(1) shouldBe TextValue("Engineering")
      data(1).data(0) shouldBe TextValue("Bob")
      data(1).data(1) shouldBe TextValue("Engineering")
      data(2).data(0) shouldBe TextValue("Carol")
      data(2).data(1) shouldBe TextValue("Marketing")
    }

    "INNER JOIN with residual condition" in {
      given session: Session = setupSession(setup)
      val result = executeQuery(
        "SELECT e.name, d.name FROM employees e JOIN departments d ON e.dept_id = d.id AND d.name = 'Engineering' ORDER BY e.name")
      val data = result.table.data
      data.length shouldBe 2
      data(0).data(0) shouldBe TextValue("Alice")
      data(1).data(0) shouldBe TextValue("Bob")
    }

    "LEFT JOIN with unmatched left rows" in {
      given session: Session = setupSession(setup)
      val result = executeQuery(
        "SELECT e.name, d.name FROM employees e LEFT JOIN departments d ON e.dept_id = d.id ORDER BY e.name")
      val data = result.table.data
      data.length shouldBe 4
      data(0).data(0) shouldBe TextValue("Alice")
      data(0).data(1) shouldBe TextValue("Engineering")
      data(3).data(0) shouldBe TextValue("Dave")
      data(3).data(1).isNull shouldBe true
    }

    "RIGHT JOIN with unmatched right rows" in {
      given session: Session = setupSession(setup)
      val result = executeQuery(
        "SELECT e.name, d.name FROM employees e RIGHT JOIN departments d ON e.dept_id = d.id ORDER BY d.name")
      val data = result.table.data
      data.length shouldBe 4
      data.map(_.data(1)) should contain(TextValue("Sales"))
      val salesRow = data.find(_.data(1) == TextValue("Sales")).get
      salesRow.data(0).isNull shouldBe true
    }

    "FULL JOIN with unmatched rows on both sides" in {
      given session: Session = setupSession(setup)
      val result = executeQuery(
        "SELECT e.name, d.name FROM employees e FULL JOIN departments d ON e.dept_id = d.id ORDER BY e.name, d.name")
      val data = result.table.data
      // Alice+Engineering, Bob+Engineering, Carol+Marketing, Dave+NULL, NULL+Sales
      data.length shouldBe 5
      // Dave has no dept
      val daveRow = data.find(_.data(0) == TextValue("Dave")).get
      daveRow.data(1).isNull shouldBe true
      // Sales has no employee
      val salesRow = data.find(_.data(1) == TextValue("Sales")).get
      salesRow.data(0).isNull shouldBe true
    }

    "multi-key equijoin" in {
      given session: Session = setupSession(
        """
          |CREATE TABLE t1 (a INT, b INT, val TEXT);
          |CREATE TABLE t2 (x INT, y INT, val TEXT);
          |INSERT INTO t1 (a, b, val) VALUES (1, 10, 'A'), (2, 20, 'B'), (1, 20, 'C');
          |INSERT INTO t2 (x, y, val) VALUES (1, 10, 'X'), (2, 20, 'Y'), (3, 30, 'Z');
          |""".trim.stripMargin)
      val result = executeQuery(
        "SELECT t1.val, t2.val FROM t1 JOIN t2 ON t1.a = t2.x AND t1.b = t2.y ORDER BY t1.val")
      val data = result.table.data
      data.length shouldBe 2
      data(0).data(0) shouldBe TextValue("A")
      data(0).data(1) shouldBe TextValue("X")
      data(1).data(0) shouldBe TextValue("B")
      data(1).data(1) shouldBe TextValue("Y")
    }

    "large dataset correctness" in {
      given session: Session = setupSession(
        """
          |CREATE TABLE big_left (id INT, category INT);
          |CREATE TABLE big_right (id INT, category INT);
          |""".trim.stripMargin)
      // Insert 500 rows per side, matching on category (10 categories)
      for i <- 1 to 500 do
        executeSQL(s"INSERT INTO big_left (id, category) VALUES ($i, ${i % 10});")
      for i <- 1 to 500 do
        executeSQL(s"INSERT INTO big_right (id, category) VALUES ($i, ${i % 10});")
      val result = executeQuery(
        "SELECT COUNT(*) FROM big_left l JOIN big_right r ON l.category = r.category")
      // Each category has 50 rows on each side → 50*50 = 2500 per category, 10 categories = 25000
      result.table.data(0).data(0).intValue shouldBe 25000
    }
  }

  "EXPLAIN output" - {

    "shows Hash Join for INNER JOIN" in {
      given session: Session = setupSession(setup)
      val results = executeSQL("EXPLAIN SELECT * FROM employees e JOIN departments d ON e.dept_id = d.id")
      val plan = results.last.asInstanceOf[ExplainResult].plan
      plan should include("Hash Join")
    }

    "shows Hash Left Join for LEFT JOIN" in {
      given session: Session = setupSession(setup)
      val results = executeSQL("EXPLAIN SELECT * FROM employees e LEFT JOIN departments d ON e.dept_id = d.id")
      val plan = results.last.asInstanceOf[ExplainResult].plan
      plan should include("Hash Left Join")
    }

    "shows Hash Right Join for RIGHT JOIN" in {
      given session: Session = setupSession(setup)
      val results = executeSQL("EXPLAIN SELECT * FROM employees e RIGHT JOIN departments d ON e.dept_id = d.id")
      val plan = results.last.asInstanceOf[ExplainResult].plan
      plan should include("Hash Right Join")
    }

    "shows Hash Full Join for FULL JOIN" in {
      given session: Session = setupSession(setup)
      val results = executeSQL("EXPLAIN SELECT * FROM employees e FULL JOIN departments d ON e.dept_id = d.id")
      val plan = results.last.asInstanceOf[ExplainResult].plan
      plan should include("Hash Full Join")
    }
  }

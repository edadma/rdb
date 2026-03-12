package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class IndexJoinTests extends AnyFreeSpec with Matchers:

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
      |CREATE INDEX idx_dept_id ON departments (id);
      |""".trim.stripMargin

  "Index join optimization" - {
    "INNER JOIN uses index when available" in {
      given session: Session = setupSession(setup)
      val proc = procRewrite(SQLParser.parseQuery(
        "SELECT e.name, d.name FROM employees e INNER JOIN departments d ON e.dept_id = d.id ORDER BY e.name"))
      findProcess(proc) { case p: IndexNestedLoopJoinProcess => p } shouldBe defined
    }

    "INNER JOIN produces correct results" in {
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

    "LEFT JOIN uses index when available" in {
      given session: Session = setupSession(setup)
      val proc = procRewrite(SQLParser.parseQuery(
        "SELECT e.name, d.name FROM employees e LEFT JOIN departments d ON e.dept_id = d.id ORDER BY e.name"))
      findProcess(proc) { case p: LeftIndexNestedLoopJoinProcess => p } shouldBe defined
    }

    "LEFT JOIN produces correct results with NULLs" in {
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

    "RIGHT JOIN uses index when available" in {
      given session: Session = setupSession(
        setup +
        "\nCREATE INDEX idx_emp_dept ON employees (dept_id);")
      val proc = procRewrite(SQLParser.parseQuery(
        "SELECT e.name, d.name FROM employees e RIGHT JOIN departments d ON e.dept_id = d.id ORDER BY d.name"))
      findProcess(proc) { case p: RightIndexNestedLoopJoinProcess => p } shouldBe defined
    }

    "RIGHT JOIN produces correct results with NULLs" in {
      given session: Session = setupSession(
        setup +
        "\nCREATE INDEX idx_emp_dept ON employees (dept_id);")
      val result = executeQuery(
        "SELECT e.name, d.name FROM employees e RIGHT JOIN departments d ON e.dept_id = d.id ORDER BY d.name")
      val data = result.table.data
      data.length shouldBe 4
      data.map(_.data(1)) should contain(TextValue("Sales"))
      // Sales department has no employees → left side NULL
      val salesRow = data.find(_.data(1) == TextValue("Sales")).get
      salesRow.data(0).isNull shouldBe true
    }

    "falls back to hash join without index" in {
      given session: Session = setupSession(
        """
          |CREATE TABLE t1 (a INT, b TEXT);
          |CREATE TABLE t2 (x INT, y TEXT);
          |INSERT INTO t1 (a, b) VALUES (1, 'one');
          |INSERT INTO t2 (x, y) VALUES (1, 'uno');
          |""".trim.stripMargin)
      val proc = procRewrite(SQLParser.parseQuery(
        "SELECT * FROM t1 JOIN t2 ON t1.a = t2.x"))
      findProcess(proc) { case _: IndexNestedLoopJoinProcess => true } shouldBe None
      findProcess(proc) { case _: HashJoinProcess => true } shouldBe defined
    }

    "INNER JOIN swaps sides when left is indexed" in {
      given session: Session = setupSession(
        setup +
        "\nCREATE INDEX idx_emp_dept ON employees (dept_id);")
      // departments drives, employees is indexed inner
      val proc = procRewrite(SQLParser.parseQuery(
        "SELECT * FROM employees e INNER JOIN departments d ON e.dept_id = d.id"))
      // Should still use index join (either side)
      findProcess(proc) { case p: IndexNestedLoopJoinProcess => p } shouldBe defined
    }

    "handles residual conditions" in {
      given session: Session = setupSession(setup)
      val result = executeQuery(
        "SELECT e.name, d.name FROM employees e JOIN departments d ON e.dept_id = d.id AND d.name = 'Engineering' ORDER BY e.name")
      val data = result.table.data
      data.length shouldBe 2
      data(0).data(0) shouldBe TextValue("Alice")
      data(1).data(0) shouldBe TextValue("Bob")
    }
  }

  "findProcess recursion" - {

    "recurses through WindowProcess to find HashJoinProcess inside" in {
      given session: Session = setupSession(
        """
          |CREATE TABLE t1 (a INT, b TEXT);
          |CREATE TABLE t2 (x INT, y TEXT);
          |INSERT INTO t1 (a, b) VALUES (1, 'one');
          |INSERT INTO t2 (x, y) VALUES (1, 'uno');
          |""".trim.stripMargin)
      val proc = procRewrite(SQLParser.parseQuery(
        "SELECT t1.b, ROW_NUMBER() OVER (ORDER BY t1.a) FROM t1 JOIN t2 ON t1.a = t2.x"))
      findProcess(proc) { case _: HashJoinProcess => true } shouldBe defined
    }

    "recurses through FullCrossJoinProcess to find Table on input1 side" in {
      given session: Session = setupSession(
        """
          |CREATE TABLE left_t (a INT);
          |CREATE TABLE right_t (x INT);
          |INSERT INTO left_t (a) VALUES (1);
          |INSERT INTO right_t (x) VALUES (1);
          |""".trim.stripMargin)
      val proc = procRewrite(SQLParser.parseQuery(
        "SELECT * FROM left_t FULL JOIN right_t ON left_t.a > right_t.x"))
      findProcess(proc) { case t: Table if t.name == "left_t" => t } shouldBe defined
    }

    "recurses through FullCrossJoinProcess to find Table on input2 side" in {
      given session: Session = setupSession(
        """
          |CREATE TABLE left_t (a INT);
          |CREATE TABLE right_t (x INT);
          |INSERT INTO left_t (a) VALUES (1);
          |INSERT INTO right_t (x) VALUES (1);
          |""".trim.stripMargin)
      val proc = procRewrite(SQLParser.parseQuery(
        "SELECT * FROM left_t FULL JOIN right_t ON left_t.a > right_t.x"))
      findProcess(proc) { case t: Table if t.name == "right_t" => t } shouldBe defined
    }
  }

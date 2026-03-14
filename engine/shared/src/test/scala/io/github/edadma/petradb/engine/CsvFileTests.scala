package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterAll
import io.github.edadma.cross_platform.{createTempFile, deleteFile, writeFile as cpWriteFile}

class CsvFileTests extends AnyFreeSpec with Matchers with Testing with BeforeAndAfterAll {

  private val tempFiles = scala.collection.mutable.ArrayBuffer[String]()

  private def makeCsv(name: String, content: String): String = {
    val path = createTempFile(s"petradb-csv-$name-", ".csv")
    cpWriteFile(path, content)
    tempFiles += path
    path
  }

  override def afterAll(): Unit =
    for path <- tempFiles do
      try deleteFile(path) catch { case _: Exception => }

  private val withHeader = makeCsv("with_header",
    "name,age,city\nAlice,30,NYC\nBob,25,LA\nCarol,35,Chicago")

  private val noHeader = makeCsv("no_header",
    "Alice,30,NYC\nBob,25,LA\nCarol,35,Chicago")

  private val pipeDelim = makeCsv("pipe_delim",
    "name|age|city\nAlice|30|NYC\nBob|25|LA")

  private val singleCol = makeCsv("single_col",
    "value\nhello\nworld")

  private val emptyFields = makeCsv("empty_fields",
    "name,age,city\nAlice,,NYC\n,25,\nCarol,35,Chicago")

  private val quoted = makeCsv("quoted",
    "name,description\nWidget,\"A small, useful device\"\nGadget,\"Has \"\"special\"\" features\"")

  private val numbers = makeCsv("numbers",
    "id,amount\n1,100\n2,200\n3,150")

  private val headerOnly = makeCsv("header_only",
    "name,age,city")

  // ── Basic queries ───────────────────────────────────────────────

  "csv_file" - {
    "reads CSV with header" in {
      val table = query(s"SELECT * FROM csv_file('$withHeader');")
      table.data.length shouldBe 3
      table.meta.columns.map(_.name) shouldBe Vector("name", "age", "city")
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1) shouldBe TextValue("30")
      table.data(0).data(2) shouldBe TextValue("NYC")
      table.data(2).data(0) shouldBe TextValue("Carol")
    }

    "reads CSV without header" in {
      val table = query(s"SELECT * FROM csv_file('$noHeader', false);")
      table.data.length shouldBe 3
      table.meta.columns.map(_.name) shouldBe Vector("column1", "column2", "column3")
      table.data(0).data(0) shouldBe TextValue("Alice")
    }

    "reads CSV with custom delimiter" in {
      val table = query(s"SELECT * FROM csv_file('$pipeDelim', true, '|');")
      table.data.length shouldBe 2
      table.meta.columns.map(_.name) shouldBe Vector("name", "age", "city")
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(1).data(2) shouldBe TextValue("LA")
    }

    "reads single column CSV" in {
      val table = query(s"SELECT * FROM csv_file('$singleCol');")
      table.data.length shouldBe 2
      table.meta.columns.map(_.name) shouldBe Vector("value")
      table.data(0).data(0) shouldBe TextValue("hello")
    }

    "handles empty fields as NULL" in {
      val table = query(s"SELECT * FROM csv_file('$emptyFields');")
      table.data.length shouldBe 3
      table.data(0).data(1).isNull shouldBe true // Alice's age is empty
      table.data(1).data(0).isNull shouldBe true // second row name is empty
      table.data(1).data(2).isNull shouldBe true // second row city is empty
      table.data(2).data(0) shouldBe TextValue("Carol") // third row is complete
    }

    "handles quoted fields" in {
      val table = query(s"SELECT * FROM csv_file('$quoted');")
      table.data.length shouldBe 2
      table.data(0).data(1) shouldBe TextValue("A small, useful device")
      table.data(1).data(1) shouldBe TextValue("""Has "special" features""")
    }

    "header only file returns no rows" in {
      val table = query(s"SELECT * FROM csv_file('$headerOnly');")
      table.data.length shouldBe 0
      table.meta.columns.map(_.name) shouldBe Vector("name", "age", "city")
    }
  }

  // ── WHERE, ORDER BY, LIMIT ──────────────────────────────────────

  "filtering and sorting" - {
    "WHERE clause filters rows" in {
      val table = query(s"SELECT * FROM csv_file('$withHeader') WHERE name = 'Bob';")
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("Bob")
    }

    "ORDER BY works" in {
      val table = query(s"SELECT name FROM csv_file('$withHeader') ORDER BY name;")
      table.data.map(_.data(0).string) shouldBe Vector("Alice", "Bob", "Carol")
    }

    "LIMIT works" in {
      val table = query(s"SELECT * FROM csv_file('$withHeader') LIMIT 2;")
      table.data.length shouldBe 2
    }
  }

  // ── CAST and expressions ────────────────────────────────────────

  "type casting" - {
    "cast text columns to numeric types" in {
      val table = query(s"SELECT id::int, amount::int FROM csv_file('$numbers') ORDER BY id::int;")
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(0).data(1) shouldBe NumberValue(100)
      table.data(2).data(1) shouldBe NumberValue(150)
    }

    "aggregate on cast values" in {
      val table = query(s"SELECT SUM(amount::int) AS total FROM csv_file('$numbers');")
      table.data(0).data(0) shouldBe NumberValue(450) // 100+200+150
    }

    "filter on cast values" in {
      val table = query(s"SELECT name FROM csv_file('$withHeader') WHERE age::int > 28;")
      table.data.length shouldBe 2 // Alice (30) and Carol (35)
      val names = table.data.map(_.data(0).string).sorted
      names shouldBe Vector("Alice", "Carol")
    }
  }

  // ── JOIN with real tables ───────────────────────────────────────

  "join with tables" - {
    "inner join CSV with database table" in {
      val table = query(
        s"""
           |CREATE TABLE departments (id INT, name TEXT);
           |INSERT INTO departments (id, name) VALUES (1, 'Engineering');
           |INSERT INTO departments (id, name) VALUES (2, 'Marketing');
           |SELECT d.name AS dept, c.name AS emp
           |FROM departments d
           |INNER JOIN csv_file('$withHeader') c ON c.city = 'NYC'
           |WHERE d.id = 1;
           |""".stripMargin)
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("Engineering")
      table.data(0).data(1) shouldBe TextValue("Alice")
    }
  }

  // ── Join multiple CSVs ───────────────────────────────────────────

  "join multiple CSVs" - {
    "inner join two CSV files" in {
      val employees = makeCsv("employees",
        "name,dept_id\nAlice,1\nBob,2\nCarol,1")

      val departments = makeCsv("departments",
        "id,department\n1,Engineering\n2,Marketing")

      val table = query(
        s"""SELECT e.name, d.department
           |FROM csv_file('$employees') e
           |INNER JOIN csv_file('$departments') d ON e.dept_id = d.id
           |ORDER BY e.name;""".stripMargin)
      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1) shouldBe TextValue("Engineering")
      table.data(1).data(0) shouldBe TextValue("Bob")
      table.data(1).data(1) shouldBe TextValue("Marketing")
      table.data(2).data(0) shouldBe TextValue("Carol")
      table.data(2).data(1) shouldBe TextValue("Engineering")
    }

    "join three CSV files" in {
      val orders = makeCsv("orders",
        "order_id,emp_name,product_id\n1,Alice,10\n2,Bob,20\n3,Alice,20")

      val products = makeCsv("products",
        "id,product_name,price\n10,Widget,25\n20,Gadget,50")

      val staff = makeCsv("staff",
        "name,title\nAlice,Manager\nBob,Engineer")

      val table = query(
        s"""SELECT o.order_id, s.title, p.product_name, p.price
           |FROM csv_file('$orders') o
           |INNER JOIN csv_file('$products') p ON o.product_id = p.id
           |INNER JOIN csv_file('$staff') s ON o.emp_name = s.name
           |ORDER BY o.order_id;""".stripMargin)
      table.data.length shouldBe 3
      table.data(0).data(1) shouldBe TextValue("Manager")
      table.data(0).data(2) shouldBe TextValue("Widget")
      table.data(1).data(1) shouldBe TextValue("Engineer")
      table.data(1).data(2) shouldBe TextValue("Gadget")
      table.data(2).data(1) shouldBe TextValue("Manager")
      table.data(2).data(2) shouldBe TextValue("Gadget")
    }
  }

  // ── Aliasing ────────────────────────────────────────────────────

  "aliasing" - {
    "table alias works" in {
      val table = query(s"SELECT f.name, f.age FROM csv_file('$withHeader') AS f WHERE f.name = 'Alice';")
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("Alice")
    }
  }

  // ── Error cases ─────────────────────────────────────────────────

  "errors" - {
    "fails on nonexistent file" in {
      an[Exception] should be thrownBy {
        query("SELECT * FROM csv_file('/tmp/petradb-nonexistent-file-xyz.csv');")
      }
    }

    "fails with no arguments" in {
      an[Exception] should be thrownBy {
        query("SELECT * FROM csv_file();")
      }
    }

    "fails with too many arguments" in {
      an[Exception] should be thrownBy {
        query(s"SELECT * FROM csv_file('$withHeader', true, ',', 'extra');")
      }
    }
  }
}

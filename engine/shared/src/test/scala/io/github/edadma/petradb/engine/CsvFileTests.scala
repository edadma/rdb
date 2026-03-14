package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterAll

import java.io.{File, PrintWriter}
import java.nio.file.Files
import scala.compiletime.uninitialized

class CsvFileTests extends AnyFreeSpec with Matchers with Testing with BeforeAndAfterAll {

  private var tmpDir: File = uninitialized

  override def beforeAll(): Unit = {
    tmpDir = Files.createTempDirectory("petradb-csv-test").toFile

    writeFile("with_header.csv",
      """name,age,city
        |Alice,30,NYC
        |Bob,25,LA
        |Carol,35,Chicago""".stripMargin)

    writeFile("no_header.csv",
      """Alice,30,NYC
        |Bob,25,LA
        |Carol,35,Chicago""".stripMargin)

    writeFile("pipe_delim.csv",
      """name|age|city
        |Alice|30|NYC
        |Bob|25|LA""".stripMargin)

    writeFile("single_col.csv",
      """value
        |hello
        |world""".stripMargin)

    writeFile("empty_fields.csv",
      """name,age,city
        |Alice,,NYC
        |,25,
        |Carol,35,Chicago""".stripMargin)

    writeFile("quoted.csv",
      """name,description
        |Widget,"A small, useful device"
        |Gadget,"Has ""special"" features"""".stripMargin)

    writeFile("numbers.csv",
      """id,amount
        |1,100
        |2,200
        |3,150""".stripMargin)

    writeFile("empty.csv", "")

    writeFile("header_only.csv", "name,age,city")
  }

  override def afterAll(): Unit = {
    if tmpDir != null then
      tmpDir.listFiles().foreach(_.delete())
      tmpDir.delete()
  }

  private def writeFile(name: String, content: String): Unit = {
    val pw = new PrintWriter(new File(tmpDir, name))
    pw.write(content)
    pw.close()
  }

  private def csvPath(name: String): String = new File(tmpDir, name).getAbsolutePath

  // ── Basic queries ───────────────────────────────────────────────

  "csv_file" - {
    "reads CSV with header" in {
      val table = query(s"SELECT * FROM csv_file('${csvPath("with_header.csv")}');")
      table.data.length shouldBe 3
      table.meta.columns.map(_.name) shouldBe Vector("name", "age", "city")
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1) shouldBe TextValue("30")
      table.data(0).data(2) shouldBe TextValue("NYC")
      table.data(2).data(0) shouldBe TextValue("Carol")
    }

    "reads CSV without header" in {
      val table = query(s"SELECT * FROM csv_file('${csvPath("no_header.csv")}', false);")
      table.data.length shouldBe 3
      table.meta.columns.map(_.name) shouldBe Vector("column1", "column2", "column3")
      table.data(0).data(0) shouldBe TextValue("Alice")
    }

    "reads CSV with custom delimiter" in {
      val table = query(s"SELECT * FROM csv_file('${csvPath("pipe_delim.csv")}', true, '|');")
      table.data.length shouldBe 2
      table.meta.columns.map(_.name) shouldBe Vector("name", "age", "city")
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(1).data(2) shouldBe TextValue("LA")
    }

    "reads single column CSV" in {
      val table = query(s"SELECT * FROM csv_file('${csvPath("single_col.csv")}');")
      table.data.length shouldBe 2
      table.meta.columns.map(_.name) shouldBe Vector("value")
      table.data(0).data(0) shouldBe TextValue("hello")
    }

    "handles empty fields as NULL" in {
      val table = query(s"SELECT * FROM csv_file('${csvPath("empty_fields.csv")}');")
      table.data.length shouldBe 3
      table.data(0).data(1).isNull shouldBe true // Alice's age is empty
      table.data(1).data(0).isNull shouldBe true // second row name is empty
      table.data(1).data(2).isNull shouldBe true // second row city is empty
      table.data(2).data(0) shouldBe TextValue("Carol") // third row is complete
    }

    "handles quoted fields" in {
      val table = query(s"SELECT * FROM csv_file('${csvPath("quoted.csv")}');")
      table.data.length shouldBe 2
      table.data(0).data(1) shouldBe TextValue("A small, useful device")
      table.data(1).data(1) shouldBe TextValue("""Has "special" features""")
    }

    "header only file returns no rows" in {
      val table = query(s"SELECT * FROM csv_file('${csvPath("header_only.csv")}');")
      table.data.length shouldBe 0
      table.meta.columns.map(_.name) shouldBe Vector("name", "age", "city")
    }
  }

  // ── WHERE, ORDER BY, LIMIT ──────────────────────────────────────

  "filtering and sorting" - {
    "WHERE clause filters rows" in {
      val table = query(s"SELECT * FROM csv_file('${csvPath("with_header.csv")}') WHERE name = 'Bob';")
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("Bob")
    }

    "ORDER BY works" in {
      val table = query(s"SELECT name FROM csv_file('${csvPath("with_header.csv")}') ORDER BY name;")
      table.data.map(_.data(0).string) shouldBe Vector("Alice", "Bob", "Carol")
    }

    "LIMIT works" in {
      val table = query(s"SELECT * FROM csv_file('${csvPath("with_header.csv")}') LIMIT 2;")
      table.data.length shouldBe 2
    }
  }

  // ── CAST and expressions ────────────────────────────────────────

  "type casting" - {
    "cast text columns to numeric types" in {
      val table = query(s"SELECT id::int, amount::int FROM csv_file('${csvPath("numbers.csv")}') ORDER BY id::int;")
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(0).data(1) shouldBe NumberValue(100)
      table.data(2).data(1) shouldBe NumberValue(150)
    }

    "aggregate on cast values" in {
      val table = query(s"SELECT SUM(amount::int) AS total FROM csv_file('${csvPath("numbers.csv")}');")
      table.data(0).data(0) shouldBe NumberValue(450) // 100+200+150
    }

    "filter on cast values" in {
      val table = query(s"SELECT name FROM csv_file('${csvPath("with_header.csv")}') WHERE age::int > 28;")
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
           |INNER JOIN csv_file('${csvPath("with_header.csv")}') c ON c.city = 'NYC'
           |WHERE d.id = 1;
           |""".stripMargin)
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("Engineering")
      table.data(0).data(1) shouldBe TextValue("Alice")
    }
  }

  // ── Aliasing ────────────────────────────────────────────────────

  "aliasing" - {
    "table alias works" in {
      val table = query(s"SELECT f.name, f.age FROM csv_file('${csvPath("with_header.csv")}') AS f WHERE f.name = 'Alice';")
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("Alice")
    }
  }

  // ── Error cases ─────────────────────────────────────────────────

  "errors" - {
    "fails on nonexistent file" in {
      an[Exception] should be thrownBy {
        query(s"SELECT * FROM csv_file('${csvPath("nonexistent.csv")}');")
      }
    }

    "fails with no arguments" in {
      an[Exception] should be thrownBy {
        query("SELECT * FROM csv_file();")
      }
    }

    "fails with too many arguments" in {
      an[Exception] should be thrownBy {
        query(s"SELECT * FROM csv_file('${csvPath("with_header.csv")}', true, ',', 'extra');")
      }
    }
  }
}

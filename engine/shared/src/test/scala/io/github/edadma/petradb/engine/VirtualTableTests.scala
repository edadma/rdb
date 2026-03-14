package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterAll
import io.github.edadma.cross_platform.{createTempFile, deleteFile, writeFile as cpWriteFile}

class VirtualTableTests extends AnyFreeSpec with Matchers with Testing with BeforeAndAfterAll {

  private val tempFiles = scala.collection.mutable.ArrayBuffer[String]()

  private def makeCsv(name: String, content: String): String = {
    val path = createTempFile(s"petradb-vt-$name-", ".csv")
    cpWriteFile(path, content)
    tempFiles += path
    path
  }

  override def afterAll(): Unit =
    for path <- tempFiles do
      try deleteFile(path) catch { case _: Exception => }

  private val usersFile = makeCsv("users",
    "name,age,city\nAlice,30,NYC\nBob,25,LA\nCarol,35,Chicago")

  private val ordersFile = makeCsv("orders",
    "id,customer,amount\n1,Alice,100\n2,Bob,200\n3,Alice,150")

  // ══════════════════════════════════════════════════════════════════
  // BUILT-IN CSV MODULE
  // ══════════════════════════════════════════════════════════════════

  "csv virtual table" - {
    "CREATE VIRTUAL TABLE ... USING csv creates a queryable table" in {
      val session = setupSession(s"CREATE VIRTUAL TABLE people USING csv('$usersFile');")
      given Session = session

      val rows = executeSQL("SELECT * FROM people;").collect { case QueryResult(t) => t }.last
      rows.data.length shouldBe 3
      rows.meta.columns.map(_.name) shouldBe Vector("name", "age", "city")
      rows.data(0).data(0) shouldBe TextValue("Alice")
    }

    "appears in SHOW TABLES" in {
      val session = setupSession(s"CREATE VIRTUAL TABLE people USING csv('$usersFile');")
      given Session = session

      val tables = executeSQL("SHOW TABLES;").collect { case QueryResult(t) => t }.last
      val names = tables.data.map(_.data(0).string)
      names should contain("people")
    }

    "supports WHERE clause" in {
      val session = setupSession(s"CREATE VIRTUAL TABLE people USING csv('$usersFile');")
      given Session = session

      val rows = executeSQL("SELECT name FROM people WHERE city = 'NYC';")
        .collect { case QueryResult(t) => t }.last
      rows.data.length shouldBe 1
      rows.data(0).data(0) shouldBe TextValue("Alice")
    }

    "supports ORDER BY and LIMIT" in {
      val session = setupSession(s"CREATE VIRTUAL TABLE people USING csv('$usersFile');")
      given Session = session

      val rows = executeSQL("SELECT name FROM people ORDER BY name LIMIT 2;")
        .collect { case QueryResult(t) => t }.last
      rows.data.length shouldBe 2
      rows.data(0).data(0) shouldBe TextValue("Alice")
      rows.data(1).data(0) shouldBe TextValue("Bob")
    }

    "supports aggregates" in {
      val session = setupSession(s"CREATE VIRTUAL TABLE people USING csv('$usersFile');")
      given Session = session

      val rows = executeSQL("SELECT COUNT(*) AS cnt FROM people;")
        .collect { case QueryResult(t) => t }.last
      rows.data(0).data(0).intValue shouldBe 3
    }

    "supports CAST on text columns" in {
      val session = setupSession(s"CREATE VIRTUAL TABLE people USING csv('$usersFile');")
      given Session = session

      val rows = executeSQL("SELECT name, age::int FROM people WHERE age::int > 28 ORDER BY name;")
        .collect { case QueryResult(t) => t }.last
      rows.data.length shouldBe 2
      rows.data(0).data(0) shouldBe TextValue("Alice")
      rows.data(1).data(0) shouldBe TextValue("Carol")
    }

    "supports no_header option" in {
      val noHeaderFile = makeCsv("noheader", "Alice,30,NYC\nBob,25,LA")
      val session = setupSession(s"CREATE VIRTUAL TABLE raw USING csv('$noHeaderFile', 'no_header');")
      given Session = session

      val rows = executeSQL("SELECT * FROM raw;").collect { case QueryResult(t) => t }.last
      rows.meta.columns.map(_.name) shouldBe Vector("column1", "column2", "column3")
      rows.data.length shouldBe 2
    }

    "supports custom delimiter" in {
      val pipeFile = makeCsv("pipe", "name|age\nAlice|30\nBob|25")
      val session = setupSession(s"CREATE VIRTUAL TABLE piped USING csv('$pipeFile', 'header', '|');")
      given Session = session

      val rows = executeSQL("SELECT * FROM piped;").collect { case QueryResult(t) => t }.last
      rows.data.length shouldBe 2
      rows.data(0).data(0) shouldBe TextValue("Alice")
    }

    "can be dropped with DROP TABLE" in {
      val session = setupSession(s"CREATE VIRTUAL TABLE people USING csv('$usersFile');")
      given Session = session

      executeSQL("DROP TABLE people;")
      val tables = executeSQL("SHOW TABLES;").collect { case QueryResult(t) => t }.last
      val names = tables.data.map(_.data(0).string)
      names should not contain "people"
    }

    "INSERT fails on virtual table" in {
      val session = setupSession(s"CREATE VIRTUAL TABLE people USING csv('$usersFile');")
      given Session = session

      an[Exception] should be thrownBy {
        executeSQL("INSERT INTO people (name, age, city) VALUES ('Dave', '40', 'Boston');")
      }
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // JOIN VIRTUAL WITH REAL TABLES
  // ══════════════════════════════════════════════════════════════════

  "joins" - {
    "join virtual table with real table" in {
      val session = setupSession(
        s"""
           |CREATE VIRTUAL TABLE csv_orders USING csv('$ordersFile');
           |CREATE TABLE customers (id SERIAL PRIMARY KEY, name TEXT NOT NULL, tier TEXT NOT NULL);
           |INSERT INTO customers (name, tier) VALUES ('Alice', 'gold');
           |INSERT INTO customers (name, tier) VALUES ('Bob', 'silver');
           |""".stripMargin
      )
      given Session = session

      val rows = executeSQL(
        "SELECT c.tier, csv_orders.amount FROM csv_orders INNER JOIN customers c ON csv_orders.customer = c.name ORDER BY csv_orders.id;"
      ).collect { case QueryResult(t) => t }.last

      rows.data.length shouldBe 3
      rows.data(0).data(0) shouldBe TextValue("gold")   // Alice order
      rows.data(0).data(1) shouldBe TextValue("100")
      rows.data(1).data(0) shouldBe TextValue("silver")  // Bob order
    }

    "join two virtual tables" in {
      val session = setupSession(
        s"""
           |CREATE VIRTUAL TABLE csv_people USING csv('$usersFile');
           |CREATE VIRTUAL TABLE csv_orders USING csv('$ordersFile');
           |""".stripMargin
      )
      given Session = session

      val rows = executeSQL(
        "SELECT p.city, o.amount FROM csv_orders o INNER JOIN csv_people p ON o.customer = p.name ORDER BY o.id;"
      ).collect { case QueryResult(t) => t }.last

      rows.data.length shouldBe 3
      rows.data(0).data(0) shouldBe TextValue("NYC")    // Alice
      rows.data(1).data(0) shouldBe TextValue("LA")     // Bob
      rows.data(2).data(0) shouldBe TextValue("NYC")    // Alice again
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // CUSTOM MODULE VIA API
  // ══════════════════════════════════════════════════════════════════

  "custom module" - {
    "register and use a custom virtual table module" in {
      val db = new MemoryDB()

      // Register a custom module that generates a sequence
      db.registerVirtualTableModule("sequence", new VirtualTableModule {
        def create(tableName: String, args: Seq[String]): VirtualTableProvider = {
          val n = args.headOption.map(_.toInt).getOrElse(10)
          VirtualTableProvider(
            Seq(("n", NumberType)),
            () => (1 to n).iterator.map(i => IndexedSeq[Value](NumberValue(i))),
          )
        }
      })

      given Session = db.connect()

      executeSQL("CREATE VIRTUAL TABLE nums USING sequence('5');")
      val rows = executeSQL("SELECT * FROM nums;").collect { case QueryResult(t) => t }.last
      rows.data.length shouldBe 5
      rows.data(0).data(0) shouldBe NumberValue(1)
      rows.data(4).data(0) shouldBe NumberValue(5)
    }

    "custom module works with WHERE" in {
      val db = new MemoryDB()

      db.registerVirtualTableModule("sequence", new VirtualTableModule {
        def create(tableName: String, args: Seq[String]): VirtualTableProvider = {
          val n = args.headOption.map(_.toInt).getOrElse(10)
          VirtualTableProvider(
            Seq(("n", NumberType)),
            () => (1 to n).iterator.map(i => IndexedSeq[Value](NumberValue(i))),
          )
        }
      })

      given Session = db.connect()

      executeSQL("CREATE VIRTUAL TABLE nums USING sequence('10');")
      val rows = executeSQL("SELECT * FROM nums WHERE n > 7;").collect { case QueryResult(t) => t }.last
      rows.data.length shouldBe 3
      rows.data.map(_.data(0).intValue) shouldBe Vector(8, 9, 10)
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // ERROR CASES
  // ══════════════════════════════════════════════════════════════════

  "errors" - {
    "unknown module fails" in {
      an[Exception] should be thrownBy {
        query("CREATE VIRTUAL TABLE t USING nonexistent('arg');")
      }
    }

    "duplicate table name fails" in {
      an[Exception] should be thrownBy {
        query(s"CREATE TABLE t (id INT); CREATE VIRTUAL TABLE t USING csv('$usersFile');")
      }
    }

    "csv module with no args fails" in {
      an[Exception] should be thrownBy {
        query("CREATE VIRTUAL TABLE t USING csv();")
      }
    }
  }
}

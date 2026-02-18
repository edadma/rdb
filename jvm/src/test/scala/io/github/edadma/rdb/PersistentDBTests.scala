package io.github.edadma.rdb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterEach
import io.github.edadma.cross_platform.{createTempFile, deleteFile}

import java.time.{Duration, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, ZoneOffset}
import scala.compiletime.uninitialized

class PersistentDBTests extends AnyFreeSpec with Matchers with BeforeAndAfterEach:

  private var tmpFile: String = uninitialized

  override def beforeEach(): Unit =
    tmpFile = createTempFile("rdb_test_", ".db")
    deleteFile(tmpFile) // FilePageStore.create needs a non-existent path

  override def afterEach(): Unit =
    try deleteFile(tmpFile)
    catch case _: Exception => ()

  private val pageSize = 4096

  // ── Basic persistence ───────────────────────────────────────────────

  "Basic persistence" - {
    "create table, insert, close, reopen, query" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE users (id SERIAL, name TEXT, PRIMARY KEY (id));")
        executeSQL("INSERT INTO users (name) VALUES ('Alice');")
        executeSQL("INSERT INTO users (name) VALUES ('Bob');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val results = executeSQL("SELECT id, name FROM users ORDER BY id;")
        val table = results.collect { case QueryResult(t) => t }.head

        table.data.length shouldBe 2
        table.data(0).data(1) shouldBe TextValue("Alice")
        table.data(1).data(1) shouldBe TextValue("Bob")
        db.close()
      }
    }

    "empty table persists" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE empty_table (id INTEGER, name TEXT);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val results = executeSQL("SELECT * FROM empty_table;")
        val table = results.collect { case QueryResult(t) => t }.head

        table.data.length shouldBe 0
        db.close()
      }
    }
  }

  // ── Auto-increment persistence ──────────────────────────────────────

  "Auto-increment persistence" - {
    "serial counter persists across reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE items (id SERIAL, name TEXT, PRIMARY KEY (id));")
        executeSQL("INSERT INTO items (name) VALUES ('first');")
        executeSQL("INSERT INTO items (name) VALUES ('second');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        executeSQL("INSERT INTO items (name) VALUES ('third');")
        val results = executeSQL("SELECT id, name FROM items ORDER BY id;")
        val table = results.collect { case QueryResult(t) => t }.head

        table.data.length shouldBe 3
        table.data(0).data(0) shouldBe NumberValue(1)
        table.data(1).data(0) shouldBe NumberValue(2)
        table.data(2).data(0) shouldBe NumberValue(3)
        db.close()
      }
    }

    "smallserial counter persists" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE ss (id SMALLSERIAL, name TEXT, PRIMARY KEY (id));")
        executeSQL("INSERT INTO ss (name) VALUES ('a');")
        executeSQL("INSERT INTO ss (name) VALUES ('b');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        executeSQL("INSERT INTO ss (name) VALUES ('c');")
        val table = executeSQL("SELECT id FROM ss ORDER BY id;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 3
        table.data(2).data(0) shouldBe NumberValue(3)
        db.close()
      }
    }

    "bigserial counter persists" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE bs (id BIGSERIAL, name TEXT, PRIMARY KEY (id));")
        executeSQL("INSERT INTO bs (name) VALUES ('a');")
        executeSQL("INSERT INTO bs (name) VALUES ('b');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        executeSQL("INSERT INTO bs (name) VALUES ('c');")
        val table = executeSQL("SELECT id FROM bs ORDER BY id;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 3
        table.data(2).data(0) shouldBe NumberValue(3)
        db.close()
      }
    }
  }

  // ── Data type persistence (all types) ───────────────────────────────

  "Data type persistence" - {
    "SMALLINT roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v SMALLINT);")
        executeSQL("INSERT INTO t (v) VALUES (42);")
        executeSQL("INSERT INTO t (v) VALUES (-32768);")
        executeSQL("INSERT INTO t (v) VALUES (32767);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t ORDER BY v;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 3
        table.data(0).data(0).string shouldBe "-32768"
        table.data(1).data(0).string shouldBe "42"
        table.data(2).data(0).string shouldBe "32767"
        db.close()
      }
    }

    "INTEGER roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v INTEGER);")
        executeSQL("INSERT INTO t (v) VALUES (0);")
        executeSQL("INSERT INTO t (v) VALUES (-1);")
        executeSQL("INSERT INTO t (v) VALUES (2147483647);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t ORDER BY v;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 3
        table.data(0).data(0).string shouldBe "-1"
        table.data(1).data(0).string shouldBe "0"
        table.data(2).data(0).string shouldBe "2147483647"
        db.close()
      }
    }

    "BIGINT roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v BIGINT);")
        // Use API to insert a true long value that SQL parser can't handle
        db.getTable("t").get.insert(Map("v" -> NumberValue(io.github.edadma.dal.LongType, 9999999999L: java.lang.Long)), None)
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0).string shouldBe "9999999999"
        db.close()
      }
    }

    "DOUBLE roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v DOUBLE PRECISION);")
        executeSQL("INSERT INTO t (v) VALUES (3.14);")
        executeSQL("INSERT INTO t (v) VALUES (-0.001);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t ORDER BY v;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 2
        table.data(0).data(0).string shouldBe "-0.001"
        table.data(1).data(0).string shouldBe "3.14"
        db.close()
      }
    }

    "NUMERIC roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v NUMERIC(10,2));")
        executeSQL("INSERT INTO t (v) VALUES (12345.67);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0).string should include("12345.67")
        db.close()
      }
    }

    "BOOLEAN roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE flags (a BOOLEAN, b BOOLEAN);")
        executeSQL("INSERT INTO flags (a, b) VALUES (true, false);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT a, b FROM flags;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe BooleanValue(true)
        table.data(0).data(1) shouldBe BooleanValue(false)
        db.close()
      }
    }

    "TEXT roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v TEXT);")
        executeSQL("INSERT INTO t (v) VALUES ('hello world');")
        executeSQL("INSERT INTO t (v) VALUES ('');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 2
        table.data(0).data(0) shouldBe TextValue("hello world")
        table.data(1).data(0) shouldBe TextValue("")
        db.close()
      }
    }

    "CHAR(n) roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v CHAR(5));")
        executeSQL("INSERT INTO t (v) VALUES ('hi');")
        executeSQL("INSERT INTO t (v) VALUES ('abcdefgh');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TextValue("hi   ") // padded
        table.data(1).data(0) shouldBe TextValue("abcde") // truncated
        db.close()
      }
    }

    "DATE roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v DATE);")
        executeSQL("INSERT INTO t (v) VALUES ('2024-01-15');")
        executeSQL("INSERT INTO t (v) VALUES ('1970-01-01');")
        executeSQL("INSERT INTO t (v) VALUES ('2099-12-31');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t ORDER BY v;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe DateValue(LocalDate.of(1970, 1, 1))
        table.data(1).data(0) shouldBe DateValue(LocalDate.of(2024, 1, 15))
        table.data(2).data(0) shouldBe DateValue(LocalDate.of(2099, 12, 31))
        db.close()
      }
    }

    "TIME roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v TIME);")
        executeSQL("INSERT INTO t (v) VALUES ('14:30:00');")
        executeSQL("INSERT INTO t (v) VALUES ('00:00:00');")
        executeSQL("INSERT INTO t (v) VALUES ('23:59:59');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t ORDER BY v;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TimeValue(LocalTime.of(0, 0, 0))
        table.data(1).data(0) shouldBe TimeValue(LocalTime.of(14, 30, 0))
        table.data(2).data(0) shouldBe TimeValue(LocalTime.of(23, 59, 59))
        db.close()
      }
    }

    "TIMESTAMP roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v TIMESTAMP);")
        executeSQL("INSERT INTO t (v) VALUES ('2024-01-15 14:30:00');")
        executeSQL("INSERT INTO t (v) VALUES ('1970-01-01 00:00:00');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t ORDER BY v;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TimestampValue(LocalDateTime.of(1970, 1, 1, 0, 0, 0))
        table.data(1).data(0) shouldBe TimestampValue(LocalDateTime.of(2024, 1, 15, 14, 30, 0))
        db.close()
      }
    }

    "TIMESTAMP WITH TIME ZONE roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v TIMESTAMP WITH TIME ZONE);")
        executeSQL("INSERT INTO t (v) VALUES ('2024-06-15T10:30:00+05:30');")
        executeSQL("INSERT INTO t (v) VALUES ('2024-01-01T00:00:00Z');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t ORDER BY v;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 2
        // +05:30 sorts after Z (UTC) when comparing instants
        val v0 = table.data(0).data(0).asInstanceOf[TimestampTZValue]
        val v1 = table.data(1).data(0).asInstanceOf[TimestampTZValue]
        v0.t.getOffset shouldBe ZoneOffset.UTC
        v1.t.getOffset shouldBe ZoneOffset.ofHoursMinutes(5, 30)
        db.close()
      }
    }

    "INTERVAL roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v INTERVAL);")
        executeSQL("INSERT INTO t (v) VALUES ('2 days 3 hours');")
        executeSQL("INSERT INTO t (v) VALUES ('PT1H30M');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 2
        val i0 = table.data(0).data(0).asInstanceOf[IntervalValue]
        i0.d shouldBe Duration.ofDays(2).plusHours(3)
        val i1 = table.data(1).data(0).asInstanceOf[IntervalValue]
        i1.d shouldBe Duration.ofHours(1).plusMinutes(30)
        db.close()
      }
    }

    "BYTEA roundtrip" in {
      val testBytes = "Hello".getBytes("UTF-8")

      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v BYTEA);")
        db.getTable("t").get.insert(Map("v" -> ByteaValue(testBytes)), None)
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        val bytes = table.data(0).data(0).asInstanceOf[ByteaValue].data
        new String(bytes, "UTF-8") shouldBe "Hello"
        db.close()
      }
    }

    "UUID roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id UUID);")
        executeSQL("INSERT INTO t (id) VALUES ('550e8400-e29b-41d4-a716-446655440000');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT id FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe UUIDValue("550e8400-e29b-41d4-a716-446655440000")
        db.close()
      }
    }

    "UUID auto-generation persists" in {
      var uuid1: String = ""
      var uuid2: String = ""

      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id UUID, name TEXT);")
        executeSQL("INSERT INTO t (name) VALUES ('Alice');")
        executeSQL("INSERT INTO t (name) VALUES ('Bob');")
        val table = executeSQL("SELECT id FROM t;").collect { case QueryResult(t) => t }.head
        uuid1 = table.data(0).data(0).string
        uuid2 = table.data(1).data(0).string
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT id, name FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0).string shouldBe uuid1
        table.data(1).data(0).string shouldBe uuid2
        db.close()
      }
    }

    "JSON object roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v JSON);")
        executeSQL("""INSERT INTO t (v) VALUES ('{"name": "Alice", "age": 30}');""")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        val obj = table.data(0).data(0).asInstanceOf[ObjectValue]
        val propMap = obj.properties.toMap
        propMap("name") shouldBe TextValue("Alice")
        propMap("age") shouldBe NumberValue(30)
        db.close()
      }
    }

    "JSON array roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v JSON);")
        executeSQL("""INSERT INTO t (v) VALUES ('[1, 2, 3]');""")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
        arr.data.length shouldBe 3
        arr.data(0) shouldBe NumberValue(1)
        db.close()
      }
    }

    "typed INT[] array roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v INT[]);")
        executeSQL("INSERT INTO t (v) VALUES (ARRAY[10, 20, 30]);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
        arr.data.length shouldBe 3
        arr.data(0) shouldBe NumberValue(10)
        arr.data(1) shouldBe NumberValue(20)
        arr.data(2) shouldBe NumberValue(30)
        db.close()
      }
    }

    "ENUM type roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TYPE color AS ENUM ('red', 'green', 'blue');")
        executeSQL("CREATE TABLE t (c color);")
        executeSQL("INSERT INTO t (c) VALUES ('red');")
        executeSQL("INSERT INTO t (c) VALUES ('blue');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT c FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0).string shouldBe "red"
        table.data(1).data(0).string shouldBe "blue"
        // Verify the type is actually an EnumValue, not just text
        table.data(0).data(0) shouldBe a[EnumValue]
        db.close()
      }
    }

    "NULL values roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
        executeSQL("INSERT INTO t (id) VALUES (1);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT id, name FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0).string shouldBe "1"
        table.data(0).data(1).isNull shouldBe true
        db.close()
      }
    }

    "all-nulls row roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (a INTEGER, b TEXT, c BOOLEAN, d DATE);")
        db.getTable("t").get.insert(Map.empty, None)
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT * FROM t;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 1
        table.data(0).data.foreach(_.isNull shouldBe true)
        db.close()
      }
    }

    "multiple data types in single table" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL(
          """CREATE TABLE mixed (
            |  id SERIAL,
            |  name TEXT,
            |  active BOOLEAN,
            |  score DOUBLE PRECISION,
            |  created DATE,
            |  notes BYTEA,
            |  PRIMARY KEY (id)
            |);""".stripMargin
        )
        // Use API for bytea since SQL parser interprets \x as escape
        db.getTable("mixed").get.insert(
          Map(
            "name" -> TextValue("test"),
            "active" -> BooleanValue(true),
            "score" -> NumberValue(99.5),
            "created" -> DateValue(LocalDate.of(2024, 6, 15)),
            "notes" -> ByteaValue("AB".getBytes("UTF-8")),
          ),
          None,
        )
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT * FROM mixed;").collect { case QueryResult(t) => t }.head
        val row = table.data(0)
        row.data(0) shouldBe NumberValue(1) // serial
        row.data(1) shouldBe TextValue("test")
        row.data(2) shouldBe BooleanValue(true)
        row.data(3).string shouldBe "99.5"
        row.data(4) shouldBe DateValue(LocalDate.of(2024, 6, 15))
        new String(row.data(5).asInstanceOf[ByteaValue].data, "UTF-8") shouldBe "AB"
        db.close()
      }
    }
  }

  // ── Large values / chain edge cases ─────────────────────────────────

  "Large values" - {
    "large TEXT uses chain storage" in {
      val longText = "x" * 500

      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE docs (id INTEGER, content TEXT);")
        db.getTable("docs").get.insert(Map("id" -> NumberValue(1), "content" -> TextValue(longText)), None)
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT id, content FROM docs;").collect { case QueryResult(t) => t }.head
        table.data(0).data(1) shouldBe TextValue(longText)
        db.close()
      }
    }

    "large BYTEA uses chain storage" in {
      val bigData = (0 until 500).map(i => (i % 256).toByte).toArray

      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v BYTEA);")
        db.getTable("t").get.insert(Map("v" -> ByteaValue(bigData)), None)
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        val result = table.data(0).data(0).asInstanceOf[ByteaValue].data
        result shouldBe bigData
        db.close()
      }
    }

    "TEXT exactly at inline threshold (64 bytes)" in {
      val exact = "a" * 64

      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v TEXT);")
        db.getTable("t").get.insert(Map("v" -> TextValue(exact)), None)
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TextValue(exact)
        db.close()
      }
    }

    "TEXT one byte over inline threshold (65 bytes)" in {
      val overBy1 = "a" * 65

      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v TEXT);")
        db.getTable("t").get.insert(Map("v" -> TextValue(overBy1)), None)
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TextValue(overBy1)
        db.close()
      }
    }

    "very large TEXT spanning multiple chain pages" in {
      val huge = "x" * 20000 // ~5 chain pages at 4096 page size

      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (v TEXT);")
        db.getTable("t").get.insert(Map("v" -> TextValue(huge)), None)
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TextValue(huge)
        db.close()
      }
    }
  }

  // ── DML persistence ─────────────────────────────────────────────────

  "DML persistence" - {
    "UPDATE survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE users (id INTEGER, name TEXT);")
        executeSQL("INSERT INTO users (id, name) VALUES (1, 'Alice');")
        executeSQL("INSERT INTO users (id, name) VALUES (2, 'Bob');")
        executeSQL("UPDATE users SET name = 'Alicia' WHERE id = 1;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT id, name FROM users ORDER BY id;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 2
        table.data(0).data(1) shouldBe TextValue("Alicia")
        table.data(1).data(1) shouldBe TextValue("Bob")
        db.close()
      }
    }

    "DELETE survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE users (id INTEGER, name TEXT);")
        executeSQL("INSERT INTO users (id, name) VALUES (1, 'Alice');")
        executeSQL("INSERT INTO users (id, name) VALUES (2, 'Bob');")
        executeSQL("INSERT INTO users (id, name) VALUES (3, 'Charlie');")
        executeSQL("DELETE FROM users WHERE id = 2;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT id, name FROM users ORDER BY id;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 2
        table.data(0).data(1) shouldBe TextValue("Alice")
        table.data(1).data(1) shouldBe TextValue("Charlie")
        db.close()
      }
    }

    "DELETE all rows then reopen yields empty table" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER);")
        executeSQL("INSERT INTO t (id) VALUES (1);")
        executeSQL("INSERT INTO t (id) VALUES (2);")
        executeSQL("INSERT INTO t (id) VALUES (3);")
        executeSQL("DELETE FROM t;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT * FROM t;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 0
        // Insert still works after deleting all
        executeSQL("INSERT INTO t (id) VALUES (99);")
        val table2 = executeSQL("SELECT id FROM t;").collect { case QueryResult(t) => t }.head
        table2.data(0).data(0) shouldBe NumberValue(99)
        db.close()
      }
    }

    "UPDATE short text to long text (inline to chain)" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER, v TEXT);")
        executeSQL("INSERT INTO t (id, v) VALUES (1, 'short');")
        val longVal = "x" * 200
        db.getTable("t").get.asInstanceOf[PersistentTable]
        executeSQL(s"UPDATE t SET v = '${longVal}' WHERE id = 1;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TextValue("x" * 200)
        db.close()
      }
    }

    "UPDATE long text to short text (chain to inline)" in {
      val longVal = "y" * 200

      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER, v TEXT);")
        db.getTable("t").get.insert(Map("id" -> NumberValue(1), "v" -> TextValue(longVal)), None)
        executeSQL("UPDATE t SET v = 'short' WHERE id = 1;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TextValue("short")
        db.close()
      }
    }

    "multiple updates to same row" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER, v INTEGER);")
        executeSQL("INSERT INTO t (id, v) VALUES (1, 10);")
        executeSQL("UPDATE t SET v = 20 WHERE id = 1;")
        executeSQL("UPDATE t SET v = 30 WHERE id = 1;")
        executeSQL("UPDATE t SET v = 40 WHERE id = 1;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe NumberValue(40)
        db.close()
      }
    }
  }

  // ── DDL persistence ─────────────────────────────────────────────────

  "DDL persistence" - {
    "ALTER TABLE ADD COLUMN survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER);")
        executeSQL("INSERT INTO t (id) VALUES (1);")
        executeSQL("ALTER TABLE t ADD COLUMN name TEXT DEFAULT 'unknown';")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT id, name FROM t;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 1
        table.data(0).data(0).string shouldBe "1"
        table.data(0).data(1) shouldBe TextValue("unknown")
        db.close()
      }
    }

    "ALTER TABLE DROP COLUMN survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER, name TEXT, email TEXT);")
        executeSQL("INSERT INTO t (id, name, email) VALUES (1, 'Alice', 'alice@test.com');")
        executeSQL("ALTER TABLE t DROP COLUMN email;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT id, name FROM t;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 1
        table.data(0).data(0).string shouldBe "1"
        table.data(0).data(1) shouldBe TextValue("Alice")
        db.close()
      }
    }

    "ALTER COLUMN TYPE survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER, v INTEGER);")
        executeSQL("INSERT INTO t (id, v) VALUES (1, 42);")
        executeSQL("ALTER TABLE t ALTER COLUMN v TYPE TEXT;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TextValue("42")
        db.close()
      }
    }

    "ALTER COLUMN SET DEFAULT survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER, status TEXT);")
        executeSQL("ALTER TABLE t ALTER COLUMN status SET DEFAULT 'active';")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        executeSQL("INSERT INTO t (id) VALUES (1);")
        val table = executeSQL("SELECT status FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TextValue("active")
        db.close()
      }
    }

    "ALTER COLUMN DROP DEFAULT survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER, status TEXT DEFAULT 'active');")
        executeSQL("ALTER TABLE t ALTER COLUMN status DROP DEFAULT;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        executeSQL("INSERT INTO t (id) VALUES (1);")
        val table = executeSQL("SELECT status FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0).isNull shouldBe true
        db.close()
      }
    }

    "ALTER COLUMN SET NOT NULL survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
        executeSQL("ALTER TABLE t ALTER COLUMN name SET NOT NULL;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        assertThrows[RuntimeException] {
          executeSQL("INSERT INTO t (id) VALUES (1);")
        }
        db.close()
      }
    }

    "ALTER COLUMN DROP NOT NULL survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER, name TEXT NOT NULL);")
        executeSQL("ALTER TABLE t ALTER COLUMN name DROP NOT NULL;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        executeSQL("INSERT INTO t (id) VALUES (1);")
        val table = executeSQL("SELECT name FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0).isNull shouldBe true
        db.close()
      }
    }

    "ADD/DROP CONSTRAINT survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
        executeSQL("ALTER TABLE t ADD CONSTRAINT uq_name UNIQUE (name);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        // Verify table still works
        executeSQL("INSERT INTO t (id, name) VALUES (1, 'Alice');")
        val table = executeSQL("SELECT * FROM t;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 1
        // Drop the constraint and verify it persists
        executeSQL("ALTER TABLE t DROP CONSTRAINT uq_name;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT * FROM t;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 1
        db.close()
      }
    }

    "RENAME TABLE survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE old_name (id INTEGER);")
        executeSQL("INSERT INTO old_name (id) VALUES (1);")
        executeSQL("ALTER TABLE old_name RENAME TO new_name;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        (db hasTable "old_name") shouldBe false
        (db hasTable "new_name") shouldBe true
        val table = executeSQL("SELECT id FROM new_name;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe NumberValue(1)
        db.close()
      }
    }

    "RENAME COLUMN survives reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER, old_col TEXT);")
        executeSQL("INSERT INTO t (id, old_col) VALUES (1, 'val');")
        executeSQL("ALTER TABLE t RENAME COLUMN old_col TO new_col;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT new_col FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TextValue("val")
        db.close()
      }
    }

    "ADD COLUMN then insert new rows with that column" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER);")
        executeSQL("INSERT INTO t (id) VALUES (1);")
        executeSQL("ALTER TABLE t ADD COLUMN name TEXT;")
        executeSQL("INSERT INTO t (id, name) VALUES (2, 'Bob');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT id, name FROM t ORDER BY id;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 2
        table.data(0).data(1).isNull shouldBe true
        table.data(1).data(1) shouldBe TextValue("Bob")
        db.close()
      }
    }
  }

  // ── DROP TABLE ──────────────────────────────────────────────────────

  "DROP TABLE" - {
    "table gone after reopen" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t1 (id INTEGER);")
        executeSQL("CREATE TABLE t2 (id INTEGER);")
        executeSQL("INSERT INTO t1 (id) VALUES (1);")
        executeSQL("INSERT INTO t2 (id) VALUES (2);")
        executeSQL("DROP TABLE t1;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        (db hasTable "t1") shouldBe false
        (db hasTable "t2") shouldBe true
        val table = executeSQL("SELECT id FROM t2;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0).string shouldBe "2"
        db.close()
      }
    }

    "drop and recreate table with same name" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER, old_col TEXT);")
        executeSQL("INSERT INTO t (id, old_col) VALUES (1, 'old');")
        executeSQL("DROP TABLE t;")
        executeSQL("CREATE TABLE t (id INTEGER, new_col BOOLEAN);")
        executeSQL("INSERT INTO t (id, new_col) VALUES (2, true);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT id, new_col FROM t;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 1
        table.data(0).data(0) shouldBe NumberValue(2)
        table.data(0).data(1) shouldBe BooleanValue(true)
        db.close()
      }
    }

    "DROP TYPE persists" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TYPE mood AS ENUM ('happy', 'sad');")
        executeSQL("DROP TYPE mood;")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        (db hasType "mood") shouldBe false
        db.close()
      }
    }
  }

  // ── Multiple tables ─────────────────────────────────────────────────

  "Multiple tables" - {
    "multiple tables persist independently" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE users (id SERIAL, name TEXT, PRIMARY KEY (id));")
        executeSQL("CREATE TABLE products (id SERIAL, title TEXT, price INTEGER, PRIMARY KEY (id));")
        executeSQL("INSERT INTO users (name) VALUES ('Alice');")
        executeSQL("INSERT INTO products (title, price) VALUES ('Widget', 100);")
        executeSQL("INSERT INTO products (title, price) VALUES ('Gadget', 200);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val users = executeSQL("SELECT name FROM users;").collect { case QueryResult(t) => t }.head
        val products = executeSQL("SELECT title, price FROM products ORDER BY price;").collect { case QueryResult(t) => t }.head

        users.data.length shouldBe 1
        users.data(0).data(0) shouldBe TextValue("Alice")

        products.data.length shouldBe 2
        products.data(0).data(0) shouldBe TextValue("Widget")
        products.data(1).data(0) shouldBe TextValue("Gadget")
        db.close()
      }
    }
  }

  // ── Slotted page edge cases ─────────────────────────────────────────

  "Slotted page edge cases" - {
    "many rows spanning multiple data pages" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER, payload TEXT);")
        // Insert enough rows to fill multiple pages (each row ~100 bytes serialized)
        for i <- 1 to 100 do
          executeSQL(s"INSERT INTO t (id, payload) VALUES ($i, '${("a" * 50)}');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT id FROM t ORDER BY id;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 100
        table.data(0).data(0) shouldBe NumberValue(1)
        table.data(99).data(0) shouldBe NumberValue(100)
        db.close()
      }
    }

    "insert after delete reuses space" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER, v TEXT);")
        for i <- 1 to 20 do
          executeSQL(s"INSERT INTO t (id, v) VALUES ($i, 'row$i');")
        // Delete some rows to create tombstones
        executeSQL("DELETE FROM t WHERE id <= 10;")
        // Insert new rows
        for i <- 21 to 30 do
          executeSQL(s"INSERT INTO t (id, v) VALUES ($i, 'new$i');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT id FROM t ORDER BY id;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 20
        table.data(0).data(0) shouldBe NumberValue(11)
        table.data(19).data(0) shouldBe NumberValue(30)
        db.close()
      }
    }

    "small page size forces more pages" in {
      val smallPageSize = 256

      locally {
        val db = PersistentDB.create(tmpFile, smallPageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
        for i <- 1 to 20 do
          executeSQL(s"INSERT INTO t (id, name) VALUES ($i, 'name_$i');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT id FROM t ORDER BY id;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 20
        table.data(0).data(0) shouldBe NumberValue(1)
        table.data(19).data(0) shouldBe NumberValue(20)
        db.close()
      }
    }
  }

  // ── Reopen multiple times ───────────────────────────────────────────

  "Multiple reopen cycles" - {
    "three open/close cycles with inserts each time" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id SERIAL, name TEXT, PRIMARY KEY (id));")
        executeSQL("INSERT INTO t (name) VALUES ('a');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        executeSQL("INSERT INTO t (name) VALUES ('b');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        executeSQL("INSERT INTO t (name) VALUES ('c');")
        val table = executeSQL("SELECT id FROM t ORDER BY id;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 3
        table.data(0).data(0) shouldBe NumberValue(1)
        table.data(1).data(0) shouldBe NumberValue(2)
        table.data(2).data(0) shouldBe NumberValue(3)
        db.close()
      }
    }

    "open/close with mixed DML each cycle" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given DB = db
        executeSQL("CREATE TABLE t (id INTEGER, v TEXT);")
        executeSQL("INSERT INTO t (id, v) VALUES (1, 'a');")
        executeSQL("INSERT INTO t (id, v) VALUES (2, 'b');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        executeSQL("UPDATE t SET v = 'aa' WHERE id = 1;")
        executeSQL("DELETE FROM t WHERE id = 2;")
        executeSQL("INSERT INTO t (id, v) VALUES (3, 'c');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given DB = db
        val table = executeSQL("SELECT id, v FROM t ORDER BY id;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 2
        table.data(0).data(0) shouldBe NumberValue(1)
        table.data(0).data(1) shouldBe TextValue("aa")
        table.data(1).data(0) shouldBe NumberValue(3)
        table.data(1).data(1) shouldBe TextValue("c")
        db.close()
      }
    }
  }

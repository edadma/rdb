package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import java.time.{Duration, LocalDate, LocalDateTime, LocalTime, ZoneOffset}

class PersistentDataTypeTests extends PersistentTestBase:

  // ── Data type persistence (all types) ───────────────────────────────

  "Data type persistence" - {
    "SMALLINT roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (v SMALLINT);")
        executeSQL("INSERT INTO t (v) VALUES (42);")
        executeSQL("INSERT INTO t (v) VALUES (-32768);")
        executeSQL("INSERT INTO t (v) VALUES (32767);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
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
        given Session = db.connect()
        executeSQL("CREATE TABLE t (v INTEGER);")
        executeSQL("INSERT INTO t (v) VALUES (0);")
        executeSQL("INSERT INTO t (v) VALUES (-1);")
        executeSQL("INSERT INTO t (v) VALUES (2147483647);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
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
        given Session = db.connect()
        executeSQL("CREATE TABLE t (v BIGINT);")
        // Use API to insert a true long value that SQL parser can't handle
        db.getTable("t").get.insert(Map("v" -> NumberValue(io.github.edadma.dal.LongType, 9999999999L: java.lang.Long)), None)
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0).string shouldBe "9999999999"
        db.close()
      }
    }

    "DOUBLE roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (v DOUBLE PRECISION);")
        executeSQL("INSERT INTO t (v) VALUES (3.14);")
        executeSQL("INSERT INTO t (v) VALUES (-0.001);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
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
        given Session = db.connect()
        executeSQL("CREATE TABLE t (v NUMERIC(10,2));")
        executeSQL("INSERT INTO t (v) VALUES (12345.67);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0).string should include("12345.67")
        db.close()
      }
    }

    "BOOLEAN roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE flags (a BOOLEAN, b BOOLEAN);")
        executeSQL("INSERT INTO flags (a, b) VALUES (true, false);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT a, b FROM flags;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe BooleanValue(true)
        table.data(0).data(1) shouldBe BooleanValue(false)
        db.close()
      }
    }

    "TEXT roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (v TEXT);")
        executeSQL("INSERT INTO t (v) VALUES ('hello world');")
        executeSQL("INSERT INTO t (v) VALUES ('');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
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
        given Session = db.connect()
        executeSQL("CREATE TABLE t (v CHAR(5));")
        executeSQL("INSERT INTO t (v) VALUES ('hi');")
        executeSQL("INSERT INTO t (v) VALUES ('abcdefgh');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TextValue("hi   ") // padded
        table.data(1).data(0) shouldBe TextValue("abcde") // truncated
        db.close()
      }
    }

    "DATE roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (v DATE);")
        executeSQL("INSERT INTO t (v) VALUES ('2024-01-15');")
        executeSQL("INSERT INTO t (v) VALUES ('1970-01-01');")
        executeSQL("INSERT INTO t (v) VALUES ('2099-12-31');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
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
        given Session = db.connect()
        executeSQL("CREATE TABLE t (v TIME);")
        executeSQL("INSERT INTO t (v) VALUES ('14:30:00');")
        executeSQL("INSERT INTO t (v) VALUES ('00:00:00');")
        executeSQL("INSERT INTO t (v) VALUES ('23:59:59');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
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
        given Session = db.connect()
        executeSQL("CREATE TABLE t (v TIMESTAMP);")
        executeSQL("INSERT INTO t (v) VALUES ('2024-01-15 14:30:00');")
        executeSQL("INSERT INTO t (v) VALUES ('1970-01-01 00:00:00');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT v FROM t ORDER BY v;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0) shouldBe TimestampValue(LocalDateTime.of(1970, 1, 1, 0, 0, 0))
        table.data(1).data(0) shouldBe TimestampValue(LocalDateTime.of(2024, 1, 15, 14, 30, 0))
        db.close()
      }
    }

    "TIMESTAMP WITH TIME ZONE roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (v TIMESTAMP WITH TIME ZONE);")
        executeSQL("INSERT INTO t (v) VALUES ('2024-06-15T10:30:00+05:30');")
        executeSQL("INSERT INTO t (v) VALUES ('2024-01-01T00:00:00Z');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
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
        given Session = db.connect()
        executeSQL("CREATE TABLE t (v INTERVAL);")
        executeSQL("INSERT INTO t (v) VALUES ('2 days 3 hours');")
        executeSQL("INSERT INTO t (v) VALUES ('PT1H30M');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
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
        given Session = db.connect()
        executeSQL("CREATE TABLE t (v BYTEA);")
        db.getTable("t").get.insert(Map("v" -> ByteaValue(testBytes)), None)
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT v FROM t;").collect { case QueryResult(t) => t }.head
        val bytes = table.data(0).data(0).asInstanceOf[ByteaValue].data
        new String(bytes, "UTF-8") shouldBe "Hello"
        db.close()
      }
    }

    "UUID roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id UUID);")
        executeSQL("INSERT INTO t (id) VALUES ('550e8400-e29b-41d4-a716-446655440000');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
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
        given Session = db.connect()
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
        given Session = db.connect()
        val table = executeSQL("SELECT id, name FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0).string shouldBe uuid1
        table.data(1).data(0).string shouldBe uuid2
        db.close()
      }
    }

    "JSON object roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (v JSON);")
        executeSQL("""INSERT INTO t (v) VALUES ('{"name": "Alice", "age": 30}');""")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
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
        given Session = db.connect()
        executeSQL("CREATE TABLE t (v JSON);")
        executeSQL("""INSERT INTO t (v) VALUES ('[1, 2, 3]');""")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
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
        given Session = db.connect()
        executeSQL("CREATE TABLE t (v INT[]);")
        executeSQL("INSERT INTO t (v) VALUES (ARRAY[10, 20, 30]);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
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
        given Session = db.connect()
        executeSQL("CREATE TYPE color AS ENUM ('red', 'green', 'blue');")
        executeSQL("CREATE TABLE t (c color);")
        executeSQL("INSERT INTO t (c) VALUES ('red');")
        executeSQL("INSERT INTO t (c) VALUES ('blue');")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
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
        given Session = db.connect()
        executeSQL("CREATE TABLE t (id INTEGER, name TEXT);")
        executeSQL("INSERT INTO t (id) VALUES (1);")
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT id, name FROM t;").collect { case QueryResult(t) => t }.head
        table.data(0).data(0).string shouldBe "1"
        table.data(0).data(1).isNull shouldBe true
        db.close()
      }
    }

    "all-nulls row roundtrip" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
        executeSQL("CREATE TABLE t (a INTEGER, b TEXT, c BOOLEAN, d DATE);")
        db.getTable("t").get.insert(Map.empty, None)
        db.close()
      }

      locally {
        val db = PersistentDB.open(tmpFile)
        given Session = db.connect()
        val table = executeSQL("SELECT * FROM t;").collect { case QueryResult(t) => t }.head
        table.data.length shouldBe 1
        table.data(0).data.foreach(_.isNull shouldBe true)
        db.close()
      }
    }

    "multiple data types in single table" in {
      locally {
        val db = PersistentDB.create(tmpFile, pageSize)
        given Session = db.connect()
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
        given Session = db.connect()
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

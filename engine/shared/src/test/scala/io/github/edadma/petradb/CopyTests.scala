package io.github.edadma.petradb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.github.edadma.cross_platform.{createTempFile, deleteFile, writeFile, readFile}
import io.github.edadma.dal.{IntType => DIntType}

class CopyTests extends AnyFreeSpec with Matchers with Testing {

  def withTempFile(suffix: String)(f: String => Unit): Unit =
    val path = createTempFile("petradb-copy-test", suffix)
    try f(path)
    finally deleteFile(path)

  "COPY FROM" - {
    "imports CSV data" in withTempFile(".csv") { path =>
      writeFile(path, "Alice,30\nBob,25\n")
      val table = query(
        s"""
          |CREATE TABLE t (name TEXT, age INTEGER);
          |COPY t FROM '$path';
          |SELECT name, age FROM t ORDER BY name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1) shouldBe NumberValue(DIntType, 30)
      table.data(1).data(0) shouldBe TextValue("Bob")
      table.data(1).data(1) shouldBe NumberValue(DIntType, 25)
    }

    "imports CSV with HEADER" in withTempFile(".csv") { path =>
      writeFile(path, "name,age\nAlice,30\nBob,25\n")
      val table = query(
        s"""
          |CREATE TABLE t (name TEXT, age INTEGER);
          |COPY t FROM '$path' WITH (HEADER);
          |SELECT name, age FROM t ORDER BY name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(1).data(0) shouldBe TextValue("Bob")
    }

    "imports specific columns" in withTempFile(".csv") { path =>
      writeFile(path, "Alice\nBob\n")
      val table = query(
        s"""
          |CREATE TABLE t (id SERIAL, name TEXT, PRIMARY KEY (id));
          |COPY t (name) FROM '$path';
          |SELECT id, name FROM t ORDER BY name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(1) shouldBe TextValue("Alice")
      table.data(1).data(1) shouldBe TextValue("Bob")
    }

    "imports with tab delimiter" in withTempFile(".tsv") { path =>
      writeFile(path, "Alice\t30\nBob\t25\n")
      val table = query(
        s"""
          |CREATE TABLE t (name TEXT, age INTEGER);
          |COPY t FROM '$path' WITH (DELIMITER E'\t');
          |SELECT name, age FROM t ORDER BY name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1) shouldBe NumberValue(DIntType, 30)
    }

    "returns CopyResult with row count" in withTempFile(".csv") { path =>
      writeFile(path, "Alice,30\nBob,25\nCharlie,35\n")
      val res = results(
        s"""
          |CREATE TABLE t (name TEXT, age INTEGER);
          |COPY t FROM '$path';
          |""".trim.stripMargin
      )

      res.last shouldBe CopyResult(3)
    }

    "handles type conversion" in withTempFile(".csv") { path =>
      writeFile(path, "true,42,3.14,hello\n")
      val table = query(
        s"""
          |CREATE TABLE t (flag BOOLEAN, num INTEGER, dec DOUBLE, txt TEXT);
          |COPY t FROM '$path';
          |SELECT flag, num, dec, txt FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe BooleanValue(true)
      table.data(0).data(1) shouldBe NumberValue(DIntType, 42)
      table.data(0).data(3) shouldBe TextValue("hello")
    }

    "handles NULL (empty fields)" in withTempFile(".csv") { path =>
      writeFile(path, "Alice,\n,25\n")
      val table = query(
        s"""
          |CREATE TABLE t (name TEXT, age INTEGER);
          |COPY t FROM '$path';
          |SELECT name, age FROM t ORDER BY age NULLS FIRST;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1).isNull shouldBe true
      table.data(1).data(0).isNull shouldBe true
      table.data(1).data(1) shouldBe NumberValue(DIntType, 25)
    }

    "FK validation fails when parent missing" in withTempFile(".csv") { path =>
      writeFile(path, "1,999\n")
      an[Exception] should be thrownBy results(
        s"""
          |CREATE TABLE parents (id INTEGER, PRIMARY KEY (id));
          |INSERT INTO parents (id) VALUES (1), (2);
          |CREATE TABLE children (id INTEGER, parent_id INTEGER REFERENCES parents (id));
          |COPY children FROM '$path';
          |""".trim.stripMargin
      )
    }

    "error on nonexistent file" in {
      an[Exception] should be thrownBy results(
        s"""
          |CREATE TABLE t (name TEXT);
          |COPY t FROM '/tmp/petradb-nonexistent-file-99999.csv';
          |""".trim.stripMargin
      )
    }

    "HEADER with DELIMITER" in withTempFile(".csv") { path =>
      writeFile(path, "name|age\nAlice|30\n")
      val table = query(
        s"""
          |CREATE TABLE t (name TEXT, age INTEGER);
          |COPY t FROM '$path' WITH (HEADER, DELIMITER '|');
          |SELECT name, age FROM t;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1) shouldBe NumberValue(DIntType, 30)
    }
  }

  "COPY TO" - {
    "exports table to file" in withTempFile(".csv") { path =>
      results(
        s"""
          |CREATE TABLE t (name TEXT, age INTEGER);
          |INSERT INTO t (name, age) VALUES ('Alice', 30), ('Bob', 25);
          |COPY t TO '$path';
          |""".trim.stripMargin
      )

      val content = readFile(path)
      content should include("Alice")
      content should include("Bob")
    }

    "exports with HEADER" in withTempFile(".csv") { path =>
      results(
        s"""
          |CREATE TABLE t (name TEXT, age INTEGER);
          |INSERT INTO t (name, age) VALUES ('Alice', 30);
          |COPY t TO '$path' WITH (HEADER);
          |""".trim.stripMargin
      )

      val lines = readFile(path).trim.split("\n")
      lines(0) shouldBe "name,age"
      lines(1) should include("Alice")
    }

    "returns CopyResult" in withTempFile(".csv") { path =>
      val res = results(
        s"""
          |CREATE TABLE t (name TEXT);
          |INSERT INTO t (name) VALUES ('a'), ('b');
          |COPY t TO '$path';
          |""".trim.stripMargin
      )

      res.last shouldBe CopyResult(2)
    }

    "exports query result" in withTempFile(".csv") { path =>
      results(
        s"""
          |CREATE TABLE t (name TEXT, age INTEGER, active BOOLEAN);
          |INSERT INTO t (name, age, active) VALUES ('Alice', 30, true), ('Bob', 25, false), ('Charlie', 35, true);
          |COPY (SELECT name, age FROM t WHERE active = true ORDER BY name) TO '$path' WITH (HEADER);
          |""".trim.stripMargin
      )

      val lines = readFile(path).trim.split("\n")
      lines(0) shouldBe "name,age"
      lines.length shouldBe 3
      lines(1) should include("Alice")
      lines(2) should include("Charlie")
    }

    "exports with tab delimiter" in withTempFile(".tsv") { path =>
      results(
        s"""
          |CREATE TABLE t (name TEXT, age INTEGER);
          |INSERT INTO t (name, age) VALUES ('Alice', 30);
          |COPY t TO '$path' WITH (DELIMITER E'\t');
          |""".trim.stripMargin
      )

      val content = readFile(path).trim
      content should include("Alice\t30")
    }
  }

  "COPY round-trip" - {
    "export then import preserves data" in withTempFile(".csv") { path =>
      val table = query(
        s"""
          |CREATE TABLE src (name TEXT, age INTEGER);
          |INSERT INTO src (name, age) VALUES ('Alice', 30), ('Bob', 25);
          |COPY src TO '$path' WITH (HEADER);
          |CREATE TABLE dst (name TEXT, age INTEGER);
          |COPY dst FROM '$path' WITH (HEADER);
          |SELECT name, age FROM dst ORDER BY name;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(0).data(1) shouldBe NumberValue(DIntType, 30)
      table.data(1).data(0) shouldBe TextValue("Bob")
      table.data(1).data(1) shouldBe NumberValue(DIntType, 25)
    }
  }
}

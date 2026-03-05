package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import io.github.edadma.dal.{IntType => DIntType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class KnexGapTests extends AnyFreeSpec with Matchers with Testing {

  // ── SHOW COLUMNS FROM ──────────────────────────────────────────────

  "SHOW COLUMNS FROM" - {
    "returns column metadata with FROM keyword" in {
      val table = query(
        """
          |CREATE TABLE sc1 (id SERIAL, name TEXT NOT NULL, age INTEGER);
          |SHOW COLUMNS FROM sc1;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
      table.data.map(_.data(0)) should contain(TextValue("id"))
      table.data.map(_.data(0)) should contain(TextValue("name"))
      table.data.map(_.data(0)) should contain(TextValue("age"))
    }

    "works without FROM keyword (existing syntax)" in {
      val table = query(
        """
          |CREATE TABLE sc2 (x INT, y TEXT);
          |SHOW COLUMNS sc2;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
    }
  }

  // ── Multi-column INSERT RETURNING ──────────────────────────────────

  "INSERT RETURNING" - {
    "single column (existing behavior)" in {
      val table = query(
        """
          |CREATE TABLE ret1 (id SERIAL, name TEXT);
          |INSERT INTO ret1 (name) VALUES ('Alice') RETURNING id;
          |SELECT * FROM ret1;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("Alice")
    }

    "multiple columns" in {
      val res = results(
        """
          |CREATE TABLE ret2 (id SERIAL, name TEXT, age INT);
          |INSERT INTO ret2 (name, age) VALUES ('Bob', 30) RETURNING id, name;
          |""".trim.stripMargin
      )

      val insertResult = res.collect { case r: InsertResult => r }.last
      // The rows should contain both id and name
      insertResult.table.meta.columns.map(_.name) should contain allOf ("id", "name")
      insertResult.table.data.length shouldBe 1
    }

    "RETURNING *" in {
      val res = results(
        """
          |CREATE TABLE ret3 (id SERIAL, name TEXT, age INT);
          |INSERT INTO ret3 (name, age) VALUES ('Charlie', 25) RETURNING *;
          |""".trim.stripMargin
      )

      val insertResult = res.collect { case r: InsertResult => r }.last
      insertResult.table.meta.columns.map(_.name) should contain allOf ("id", "name", "age")
      insertResult.table.data.length shouldBe 1
    }

    "RETURNING with INSERT ... SELECT" in {
      val res = results(
        """
          |CREATE TABLE ret4a (id SERIAL, name TEXT);
          |INSERT INTO ret4a (name) VALUES ('Diana');
          |CREATE TABLE ret4b (id SERIAL, name TEXT);
          |INSERT INTO ret4b (name) SELECT name FROM ret4a RETURNING id, name;
          |""".trim.stripMargin
      )

      val insertResult = res.collect { case r: InsertResult => r }.last
      insertResult.table.meta.columns.map(_.name) should contain allOf ("id", "name")
    }

    "RETURNING with prepared statement" in {
      val res = results(
        """
          |CREATE TABLE ret5 (id SERIAL, name TEXT, age INT);
          |PREPARE ins AS INSERT INTO ret5 (name, age) VALUES ($1, $2) RETURNING id, name;
          |EXECUTE ins('Eve', 28);
          |""".trim.stripMargin
      )

      val insertResult = res.collect { case r: InsertResult => r }.last
      insertResult.table.meta.columns.map(_.name) should contain allOf ("id", "name")
    }
  }

  // ── Parameterized LIMIT / OFFSET ───────────────────────────────────

  "parameterized LIMIT/OFFSET" - {
    val setup: String =
      """
        |CREATE TABLE lim (id SERIAL, name TEXT);
        |INSERT INTO lim (name) VALUES ('Alice');
        |INSERT INTO lim (name) VALUES ('Bob');
        |INSERT INTO lim (name) VALUES ('Charlie');
        |INSERT INTO lim (name) VALUES ('Diana');
        |INSERT INTO lim (name) VALUES ('Eve');
        |""".trim.stripMargin

    "literal LIMIT works" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM lim ORDER BY id LIMIT 2;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("Alice")
      table.data(1).data(0) shouldBe TextValue("Bob")
    }

    "literal LIMIT and OFFSET" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM lim ORDER BY id LIMIT 2 OFFSET 1;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("Bob")
      table.data(1).data(0) shouldBe TextValue("Charlie")
    }

    "LIMIT with prepared statement parameter" in {
      val table = query(
        s"""
          |$setup
          |PREPARE q AS SELECT name FROM lim ORDER BY id LIMIT $$1;
          |EXECUTE q(3);
          |""".trim.stripMargin
      )

      table.data.length shouldBe 3
    }

    "LIMIT and OFFSET with prepared statement parameters" in {
      val table = query(
        s"""
          |$setup
          |PREPARE q AS SELECT name FROM lim ORDER BY id LIMIT $$1 OFFSET $$2;
          |EXECUTE q(2, 2);
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("Charlie")
      table.data(1).data(0) shouldBe TextValue("Diana")
    }

    "OFFSET only with prepared statement parameter" in {
      val table = query(
        s"""
          |$setup
          |PREPARE q AS SELECT name FROM lim ORDER BY id OFFSET $$1;
          |EXECUTE q(3);
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("Diana")
      table.data(1).data(0) shouldBe TextValue("Eve")
    }

    "expression in LIMIT" in {
      val table = query(
        s"""
          |$setup
          |SELECT name FROM lim ORDER BY id LIMIT 1 + 1;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
    }
  }
}

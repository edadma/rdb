package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}
import io.github.edadma.dal.{IntType => DIntType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class InsertReturningTests extends AnyFreeSpec with Matchers with Testing {

  "INSERT multi-row RETURNING" - {
    "returns all inserted rows with RETURNING *" in {
      val res = results(
        """
          |CREATE TABLE mr1 (id SERIAL, name TEXT NOT NULL);
          |INSERT INTO mr1 (name) VALUES ('Alice'), ('Bob'), ('Charlie') RETURNING *;
          |""".trim.stripMargin
      )

      val insertResult = res.collect { case r: InsertResult => r }.last
      insertResult.table.data.length shouldBe 3
      val names = insertResult.table.data.map(_.data(1)).collect { case TextValue(s) => s }.toSet
      names shouldBe Set("Alice", "Bob", "Charlie")
    }

    "returns all inserted rows with RETURNING id" in {
      val res = results(
        """
          |CREATE TABLE mr2 (id SERIAL, name TEXT NOT NULL);
          |INSERT INTO mr2 (name) VALUES ('A'), ('B') RETURNING id;
          |""".trim.stripMargin
      )

      val insertResult = res.collect { case r: InsertResult => r }.last
      insertResult.table.data.length shouldBe 2
      insertResult.table.meta.columns.map(_.name) shouldBe IndexedSeq("id")
    }

    "returns all inserted rows with RETURNING name" in {
      val res = results(
        """
          |CREATE TABLE mr3 (id SERIAL, name TEXT NOT NULL, age INT);
          |INSERT INTO mr3 (name, age) VALUES ('X', 10), ('Y', 20), ('Z', 30) RETURNING name, age;
          |""".trim.stripMargin
      )

      val insertResult = res.collect { case r: InsertResult => r }.last
      insertResult.table.data.length shouldBe 3
      insertResult.table.meta.columns.map(_.name) shouldBe IndexedSeq("name", "age")
    }
  }

  "INSERT...SELECT RETURNING" - {
    val setup =
      """
        |CREATE TABLE src (id SERIAL, name TEXT NOT NULL, value INT NOT NULL);
        |INSERT INTO src (name, value) VALUES ('a', 10), ('b', 20), ('c', 30);
        |CREATE TABLE dst (id SERIAL, name TEXT NOT NULL, value INT NOT NULL);
        |""".trim.stripMargin

    "returns all inserted rows with RETURNING *" in {
      val res = results(
        s"""
          |$setup
          |INSERT INTO dst (name, value) SELECT name, value FROM src RETURNING *;
          |""".trim.stripMargin
      )

      val insertResult = res.collect { case r: InsertResult => r }.last
      insertResult.table.data.length shouldBe 3
      val names = insertResult.table.data.map(_.data(1)).collect { case TextValue(s) => s }.toSet
      names shouldBe Set("a", "b", "c")
    }

    "returns all inserted rows with RETURNING id, name" in {
      val res = results(
        s"""
          |$setup
          |INSERT INTO dst (name, value) SELECT name, value FROM src WHERE value >= 20 RETURNING id, name;
          |""".trim.stripMargin
      )

      val insertResult = res.collect { case r: InsertResult => r }.last
      insertResult.table.data.length shouldBe 2
      insertResult.table.meta.columns.map(_.name) shouldBe IndexedSeq("id", "name")
    }
  }
}

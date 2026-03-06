package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class QualifiedStarTests extends AnyFreeSpec with Matchers with Testing {

  "table.* in SELECT" - {
    "single table qualified star" in {
      val table = query(
        """
          |CREATE TABLE t (a INT, b TEXT);
          |INSERT INTO t (a, b) VALUES (1, 'hello'), (2, 'world');
          |SELECT t.* FROM t ORDER BY a;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(0).data(1) shouldBe TextValue("hello")
      table.data(1).data(0) shouldBe NumberValue(2)
      table.data(1).data(1) shouldBe TextValue("world")
    }

    "qualified star with join" in {
      val table = query(
        """
          |CREATE TABLE authors (id SERIAL, name TEXT);
          |CREATE TABLE books (id SERIAL, title TEXT, author_id INT);
          |INSERT INTO authors (name) VALUES ('Alice'), ('Bob');
          |INSERT INTO books (title, author_id) VALUES ('Book A', 1), ('Book B', 2);
          |SELECT books.* FROM books INNER JOIN authors ON authors.id = books.author_id ORDER BY books.id;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(1) shouldBe TextValue("Book A")
      table.data(1).data(1) shouldBe TextValue("Book B")
    }

    "qualified star alongside other columns" in {
      val table = query(
        """
          |CREATE TABLE categories (id SERIAL, name TEXT);
          |CREATE TABLE items (id SERIAL, cat_id INT, label TEXT);
          |INSERT INTO categories (name) VALUES ('A');
          |INSERT INTO items (cat_id, label) VALUES (1, 'Item 1');
          |SELECT categories.*, items.label FROM categories INNER JOIN items ON items.cat_id = categories.id;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(0).data(1) shouldBe TextValue("A")
      table.data(0).data(2) shouldBe TextValue("Item 1")
    }

    "quoted table qualified star" in {
      val table = query(
        """
          |CREATE TABLE "my_table" (id SERIAL, val TEXT);
          |INSERT INTO "my_table" (val) VALUES ('test');
          |SELECT "my_table".* FROM "my_table";
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("test")
    }

    "multiple qualified stars from different tables" in {
      val table = query(
        """
          |CREATE TABLE t1 (a INT);
          |CREATE TABLE t2 (b TEXT);
          |INSERT INTO t1 (a) VALUES (42);
          |INSERT INTO t2 (b) VALUES ('hello');
          |SELECT t1.*, t2.* FROM t1, t2;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(42)
      table.data(0).data(1) shouldBe TextValue("hello")
    }

    "alias qualified star" in {
      val table = query(
        """
          |CREATE TABLE things (id SERIAL, name TEXT);
          |INSERT INTO things (name) VALUES ('widget');
          |SELECT th.* FROM things th;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 1
      table.data(0).data(1) shouldBe TextValue("widget")
    }
  }
}

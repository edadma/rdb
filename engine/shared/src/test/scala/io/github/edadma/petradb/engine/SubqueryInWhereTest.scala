package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SubqueryInWhereTest extends AnyFreeSpec with Matchers with Testing {

  val setup =
    """CREATE TABLE users (id INT, name TEXT);
      |CREATE TABLE posts (id INT, title TEXT, author INT);
      |INSERT INTO users VALUES (1, 'alice'), (2, 'bob'), (3, 'charlie');
      |INSERT INTO posts VALUES (1, 'post1', 1), (2, 'post2', 2);
      |""".stripMargin

  "correlated IN subquery" - {
    "unquoted correlated subquery referencing outer table" in {
      val table = query(
        s"""$setup SELECT users.id, users.name FROM users
           |WHERE users.id IN (
           |  SELECT posts.author FROM posts WHERE posts.author = users.id
           |) ORDER BY users.id;
           |""".stripMargin
      )
      table.data.map(_.data(0).intValue) shouldBe IndexedSeq(1, 2)
    }

    "quoted correlated subquery referencing outer table" in {
      val table = query(
        s"""$setup SELECT "users"."id", "users"."name" FROM "users"
           |WHERE "users"."id" IN (
           |  SELECT "posts"."author" FROM "posts" WHERE "posts"."author" = "users"."id"
           |) ORDER BY "users"."id";
           |""".stripMargin
      )
      table.data.map(_.data(0).intValue) shouldBe IndexedSeq(1, 2)
    }

    "correlated subquery with alias referencing outer table" in {
      val table = query(
        s"""$setup SELECT "users"."id", "users"."name" FROM "users"
           |WHERE "users"."id" IN (
           |  SELECT "p"."author" FROM "posts" AS "p" WHERE "p"."author" = "users"."id"
           |) ORDER BY "users"."id";
           |""".stripMargin
      )
      table.data.map(_.data(0).intValue) shouldBe IndexedSeq(1, 2)
    }

    "correlated subquery with dollar-sign alias" in {
      val table = query(
        s"""$setup SELECT "users"."id", "users"."name" FROM "users"
           |WHERE "users"."id" IN (
           |  SELECT "users$$posts"."author" FROM "posts" AS "users$$posts"
           |  WHERE "users$$posts"."author" = "users"."id"
           |) AND "users"."id" IN (1, 2, 3)
           |ORDER BY "users"."id" ASC NULLS FIRST;
           |""".stripMargin
      )
      table.data.map(_.data(0).intValue) shouldBe IndexedSeq(1, 2)
    }

    "correlated subquery with AND after IN subquery" in {
      val table = query(
        s"""$setup SELECT users.id FROM users
           |WHERE users.id IN (
           |  SELECT posts.author FROM posts WHERE posts.author = users.id
           |) AND users.id IN (1, 2, 3)
           |ORDER BY users.id;
           |""".stripMargin
      )
      table.data.map(_.data(0).intValue) shouldBe IndexedSeq(1, 2)
    }

    "simple non-correlated IN subquery" in {
      val table = query(
        s"""$setup SELECT users.id FROM users
           |WHERE users.id IN (SELECT posts.author FROM posts)
           |ORDER BY users.id;
           |""".stripMargin
      )
      table.data.map(_.data(0).intValue) shouldBe IndexedSeq(1, 2)
    }

    "EXISTS correlated subquery" in {
      val table = query(
        s"""$setup SELECT users.id FROM users
           |WHERE EXISTS (SELECT 1 FROM posts WHERE posts.author = users.id)
           |ORDER BY users.id;
           |""".stripMargin
      )
      table.data.map(_.data(0).intValue) shouldBe IndexedSeq(1, 2)
    }
  }

  "correlated subquery via parameterized executeSQL" - {
    "correlated IN subquery with empty params" in {
      given session: Session = new MemoryDB().connect()
      executeSQL(setup)
      val results = executeSQL(
        "SELECT users.id FROM users WHERE users.id IN (SELECT posts.author FROM posts WHERE posts.author = users.id) ORDER BY users.id",
        IndexedSeq.empty
      )
      val table = results.collect { case QueryResult(t) => t }.last
      table.data.map(_.data(0).intValue) shouldBe IndexedSeq(1, 2)
    }

    "correlated IN subquery with actual params" in {
      given session: Session = new MemoryDB().connect()
      executeSQL(setup)
      val results = executeSQL(
        "SELECT users.id FROM users WHERE users.id IN (SELECT posts.author FROM posts WHERE posts.author = users.id) AND users.id = ANY($1) ORDER BY users.id",
        IndexedSeq(Seq(1, 2, 3))
      )
      val table = results.collect { case QueryResult(t) => t }.last
      table.data.map(_.data(0).intValue) shouldBe IndexedSeq(1, 2)
    }

    "EXISTS correlated subquery with empty params" in {
      given session: Session = new MemoryDB().connect()
      executeSQL(setup)
      val results = executeSQL(
        "SELECT users.id FROM users WHERE EXISTS (SELECT 1 FROM posts WHERE posts.author = users.id) ORDER BY users.id",
        IndexedSeq.empty
      )
      val table = results.collect { case QueryResult(t) => t }.last
      table.data.map(_.data(0).intValue) shouldBe IndexedSeq(1, 2)
    }

    "correlated subquery via .head (OQL raw pattern)" in {
      given session: Session = new MemoryDB().connect()
      executeSQL(setup)
      // This mirrors PetraDBConnection.raw: executeSQL(sql, params).head
      val result = executeSQL(
        "SELECT users.id FROM users WHERE users.id IN (SELECT posts.author FROM posts WHERE posts.author = users.id) ORDER BY users.id",
        IndexedSeq[Any]()
      ).head
      val table = result.asInstanceOf[QueryResult].table
      table.data.map(_.data(0).intValue) shouldBe IndexedSeq(1, 2)
    }

    "exact OQL generated SQL pattern" in {
      given session: Session = new MemoryDB().connect()
      executeSQL(setup)
      val sql = """SELECT "users"."id", "users"."name" FROM "users" WHERE "users"."id" IN (SELECT "p"."author" FROM "posts" AS "p" WHERE "p"."author" = "users"."id") AND "users"."id" IN (1, 2, 3) ORDER BY "users"."id" ASC NULLS FIRST"""
      val result = executeSQL(sql, IndexedSeq[Any]()).head
      val table = result.asInstanceOf[QueryResult].table
      table.data.map(_.data(0).intValue) shouldBe IndexedSeq(1, 2)
    }
  }
}

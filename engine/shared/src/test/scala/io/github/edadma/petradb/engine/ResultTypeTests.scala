package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ResultTypeTests extends AnyFreeSpec with Matchers with Testing:

  "CreateViewResult" in {
    val r = results("CREATE TABLE t (x INT); CREATE VIEW v AS SELECT x FROM t")
    r.last shouldBe a[CreateViewResult]
    r.last.asInstanceOf[CreateViewResult].name shouldBe "v"
  }

  "DropViewResult" in {
    val r = results("CREATE TABLE t (x INT); CREATE VIEW v AS SELECT x FROM t; DROP VIEW v")
    r.last shouldBe a[DropViewResult]
    r.last.asInstanceOf[DropViewResult].name shouldBe "v"
  }

  "ExplainResult" in {
    val r = results("CREATE TABLE t (x INT); EXPLAIN SELECT x FROM t")
    r.last shouldBe a[ExplainResult]
    r.last.asInstanceOf[ExplainResult].plan should not be empty
  }

  "CreateTableResult" in {
    val r = results("CREATE TABLE t (x INT)")
    r.last shouldBe a[CreateTableResult]
    r.last.asInstanceOf[CreateTableResult].table shouldBe "t"
  }

  "DropTableResult" in {
    val r = results("CREATE TABLE t (x INT); DROP TABLE t")
    r.last shouldBe a[DropTableResult]
  }

  "InsertResult" in {
    val r = results("CREATE TABLE t (id SERIAL, x INT); INSERT INTO t (x) VALUES (1)")
    r.last shouldBe a[InsertResult]
  }

  "UpdateResult" in {
    val r = results("CREATE TABLE t (x INT); INSERT INTO t VALUES (1); UPDATE t SET x = 2")
    r.last shouldBe a[UpdateResult]
    r.last.asInstanceOf[UpdateResult].rows shouldBe 1
  }

  "DeleteResult" in {
    val r = results("CREATE TABLE t (x INT); INSERT INTO t VALUES (1); DELETE FROM t")
    r.last shouldBe a[DeleteResult]
    r.last.asInstanceOf[DeleteResult].rows shouldBe 1
  }

  "TruncateResult" in {
    val r = results("CREATE TABLE t (x INT); INSERT INTO t VALUES (1); TRUNCATE TABLE t")
    r.last shouldBe a[TruncateResult]
  }

  "AlterTableResult" in {
    val r = results("CREATE TABLE t (x INT); ALTER TABLE t ADD COLUMN y TEXT")
    r.last shouldBe a[AlterTableResult]
  }

  "QueryResult" in {
    val r = results("CREATE TABLE t (x INT); INSERT INTO t VALUES (1); SELECT * FROM t")
    r.last shouldBe a[QueryResult]
  }

  "CreateIndexResult" in {
    val r = results("CREATE TABLE t (x INT); CREATE INDEX idx ON t (x)")
    r.last shouldBe a[CreateIndexResult]
    r.last.asInstanceOf[CreateIndexResult].name shouldBe "idx"
  }

  "DropIndexResult" in {
    val r = results("CREATE TABLE t (x INT); CREATE INDEX idx ON t (x); DROP INDEX idx")
    r.last shouldBe a[DropIndexResult]
  }

  "BeginResult and CommitResult" in {
    val r = results("BEGIN")
    r.last shouldBe BeginResult
  }

  "RollbackResult" in {
    val r = results("BEGIN; ROLLBACK")
    r.last shouldBe RollbackResult
  }

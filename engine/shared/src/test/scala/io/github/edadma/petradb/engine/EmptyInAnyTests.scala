package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class EmptyInAnyTests extends AnyFreeSpec with Matchers with Testing {

  val setup =
    """CREATE TABLE t (id INT, name TEXT);
      |INSERT INTO t VALUES (1, 'alice'), (2, 'bob'), (3, 'charlie');
      |""".stripMargin

  // ── ANY with empty arrays ─────────────────────────────────────────

  "ANY with empty array" - {
    "id = ANY(ARRAY[]::integer[]) returns no rows" in {
      val table = query(s"$setup SELECT * FROM t WHERE id = ANY(ARRAY[]::integer[]);")
      table.data shouldBe empty
    }

    "id = ANY('{}'::integer[]) returns no rows" in {
      val table = query(s"$setup SELECT * FROM t WHERE id = ANY('{}'::integer[]);")
      table.data shouldBe empty
    }

    "name = ANY(ARRAY[]::text[]) returns no rows" in {
      val table = query(s"$setup SELECT * FROM t WHERE name = ANY(ARRAY[]::text[]);")
      table.data shouldBe empty
    }

    "name = ANY('{}'::text[]) returns no rows" in {
      val table = query(s"$setup SELECT * FROM t WHERE name = ANY('{}'::text[]);")
      table.data shouldBe empty
    }

    "ANY with non-empty array still works" in {
      val table = query(s"$setup SELECT name FROM t WHERE id = ANY(ARRAY[1, 3]) ORDER BY id;")
      table.data.map(_.data(0).string) shouldBe IndexedSeq("alice", "charlie")
    }

    "ANY with single-element array" in {
      val table = query(s"$setup SELECT name FROM t WHERE id = ANY(ARRAY[2]);")
      table.data.map(_.data(0).string) shouldBe IndexedSeq("bob")
    }
  }

  // ── ALL with empty arrays ─────────────────────────────────────────

  "ALL with empty array" - {
    "id = ALL(ARRAY[]::integer[]) returns all rows (vacuous truth)" in {
      val table = query(s"$setup SELECT name FROM t WHERE id = ALL(ARRAY[]::integer[]) ORDER BY id;")
      table.data should have length 3
    }

    "id <> ALL(ARRAY[]::integer[]) returns all rows" in {
      val table = query(s"$setup SELECT name FROM t WHERE id <> ALL(ARRAY[]::integer[]) ORDER BY id;")
      table.data should have length 3
    }

    "id > ALL(ARRAY[]::integer[]) returns all rows" in {
      val table = query(s"$setup SELECT name FROM t WHERE id > ALL(ARRAY[]::integer[]) ORDER BY id;")
      table.data should have length 3
    }
  }

  // ── SOME with empty arrays ────────────────────────────────────────

  "SOME with empty array" - {
    "id = SOME(ARRAY[]::integer[]) returns no rows" in {
      val table = query(s"$setup SELECT * FROM t WHERE id = SOME(ARRAY[]::integer[]);")
      table.data shouldBe empty
    }
  }

  // ── IN with empty list ────────────────────────────────────────────

  "IN with values" - {
    "id IN (1, 3) returns matching rows" in {
      val table = query(s"$setup SELECT name FROM t WHERE id IN (1, 3) ORDER BY id;")
      table.data.map(_.data(0).string) shouldBe IndexedSeq("alice", "charlie")
    }

    "id NOT IN (1, 3) returns non-matching rows" in {
      val table = query(s"$setup SELECT name FROM t WHERE id NOT IN (1, 3);")
      table.data.map(_.data(0).string) shouldBe IndexedSeq("bob")
    }
  }

  // ── ANY/ALL with NULL elements ────────────────────────────────────

  "ANY/ALL with NULLs" - {
    "ANY with array containing NULL and matching element" in {
      val table = query(s"$setup SELECT name FROM t WHERE id = ANY(ARRAY[1, NULL]) ORDER BY id;")
      table.data.map(_.data(0).string) shouldBe IndexedSeq("alice")
    }

    "ANY with array containing only NULL returns no rows" in {
      val table = query(s"$setup SELECT * FROM t WHERE id = ANY(ARRAY[NULL]::integer[]);")
      table.data shouldBe empty
    }
  }

  // ── Comparison operators with ANY ─────────────────────────────────

  "comparison operators with ANY" - {
    "id > ANY(ARRAY[1, 2])" in {
      val table = query(s"$setup SELECT name FROM t WHERE id > ANY(ARRAY[1, 2]) ORDER BY id;")
      table.data.map(_.data(0).string) shouldBe IndexedSeq("bob", "charlie")
    }

    "id < ANY(ARRAY[2, 3])" in {
      val table = query(s"$setup SELECT name FROM t WHERE id < ANY(ARRAY[2, 3]) ORDER BY id;")
      table.data.map(_.data(0).string) shouldBe IndexedSeq("alice", "bob")
    }

    "id >= ANY(ARRAY[3])" in {
      val table = query(s"$setup SELECT name FROM t WHERE id >= ANY(ARRAY[3]);")
      table.data.map(_.data(0).string) shouldBe IndexedSeq("charlie")
    }

    "id <> ANY(ARRAY[1])" in {
      val table = query(s"$setup SELECT name FROM t WHERE id <> ANY(ARRAY[1]) ORDER BY id;")
      table.data.map(_.data(0).string) shouldBe IndexedSeq("bob", "charlie")
    }
  }

  // ── Comparison operators with ALL ─────────────────────────────────

  "comparison operators with ALL" - {
    "id > ALL(ARRAY[1, 2])" in {
      val table = query(s"$setup SELECT name FROM t WHERE id > ALL(ARRAY[1, 2]);")
      table.data.map(_.data(0).string) shouldBe IndexedSeq("charlie")
    }

    "id <> ALL(ARRAY[1, 3])" in {
      val table = query(s"$setup SELECT name FROM t WHERE id <> ALL(ARRAY[1, 3]);")
      table.data.map(_.data(0).string) shouldBe IndexedSeq("bob")
    }
  }
}

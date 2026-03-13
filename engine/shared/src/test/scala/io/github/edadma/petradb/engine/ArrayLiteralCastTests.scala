package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ArrayLiteralCastTests extends AnyFreeSpec with Matchers with Testing {

  // ── Integer arrays ────────────────────────────────────────────────

  "integer array casts" - {
    "'{1,2,3}'::integer[]" in {
      val table = query("SELECT '{1,2,3}'::integer[]")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.map(_.intValue) shouldBe IndexedSeq(1, 2, 3)
    }

    "'{1,2,3}'::int[]" in {
      val table = query("SELECT '{1,2,3}'::int[]")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.map(_.intValue) shouldBe IndexedSeq(1, 2, 3)
    }

    "single element '{42}'::integer[]" in {
      val table = query("SELECT '{42}'::integer[]")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.map(_.intValue) shouldBe IndexedSeq(42)
    }

    "empty array '{}'::integer[]" in {
      val table = query("SELECT '{}'::integer[]")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data shouldBe empty
    }

    "negative numbers '{-1,-2,3}'::integer[]" in {
      val table = query("SELECT '{-1,-2,3}'::integer[]")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.map(_.intValue) shouldBe IndexedSeq(-1, -2, 3)
    }

    "spaces around elements '{ 1 , 2 , 3 }'::integer[]" in {
      val table = query("SELECT '{ 1 , 2 , 3 }'::integer[]")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.map(_.intValue) shouldBe IndexedSeq(1, 2, 3)
    }

    "CAST syntax: CAST('{1,2}' AS integer[])" in {
      val table = query("SELECT CAST('{1,2}' AS integer[])")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.map(_.intValue) shouldBe IndexedSeq(1, 2)
    }
  }

  // ── Bigint arrays ────────────────────────────────────────────────

  "bigint array casts" - {
    "'{1,2,3}'::bigint[]" in {
      val table = query("SELECT '{1,2,3}'::bigint[]")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.map(_.longValue) shouldBe IndexedSeq(1L, 2L, 3L)
    }
  }

  // ── Text arrays ───────────────────────────────────────────────────

  "text array casts" - {
    "'{hello,world}'::text[]" in {
      val table = query("SELECT '{hello,world}'::text[]")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.map(_.string) shouldBe IndexedSeq("hello", "world")
    }

    "single element '{foo}'::text[]" in {
      val table = query("SELECT '{foo}'::text[]")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.map(_.string) shouldBe IndexedSeq("foo")
    }

    "empty array '{}'::text[]" in {
      val table = query("SELECT '{}'::text[]")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data shouldBe empty
    }

    "quoted elements with spaces '{\"hello world\",\"foo bar\"}'::text[]" in {
      val table = query("""SELECT '{"hello world","foo bar"}'::text[]""")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.map(_.string) shouldBe IndexedSeq("hello world", "foo bar")
    }

    "quoted elements with commas '{\"a,b\",\"c,d\"}'::text[]" in {
      val table = query("""SELECT '{"a,b","c,d"}'::text[]""")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.map(_.string) shouldBe IndexedSeq("a,b", "c,d")
    }

    "NULL element '{hello,NULL,world}'::text[]" in {
      val table = query("SELECT '{hello,NULL,world}'::text[]")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data(0).string shouldBe "hello"
      arr.data(1).isNull shouldBe true
      arr.data(2).string shouldBe "world"
    }

    "mixed case null '{a,Null,NULL,b}'::text[]" in {
      val table = query("SELECT '{a,Null,NULL,b}'::text[]")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data(0).string shouldBe "a"
      arr.data(1).isNull shouldBe true
      arr.data(2).isNull shouldBe true
      arr.data(3).string shouldBe "b"
    }

    "quoted NULL is not null '{\"NULL\"}'::text[]" in {
      val table = query("""SELECT '{"NULL"}'::text[]""")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data(0).string shouldBe "NULL"
      arr.data(0).isNull shouldBe false
    }
  }

  // ── Boolean arrays ────────────────────────────────────────────────

  "boolean array casts" - {
    "'{true,false,true}'::boolean[]" in {
      val table = query("SELECT '{true,false,true}'::boolean[]")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.map(_.asInstanceOf[BooleanValue].b) shouldBe IndexedSeq(true, false, true)
    }

    "'{t,f}'::boolean[]" in {
      val table = query("SELECT '{t,f}'::boolean[]")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.map(_.asInstanceOf[BooleanValue].b) shouldBe IndexedSeq(true, false)
    }
  }

  // ── Float/double arrays ───────────────────────────────────────────

  "float array casts" - {
    "'{1.5,2.7,3.14}'::float[]" in {
      val table = query("SELECT '{1.5,2.7,3.14}'::float[]")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.map(_.doubleValue) shouldBe IndexedSeq(1.5, 2.7, 3.14)
    }

    "'{1.5,2.7}'::double precision[]" in {
      val table = query("SELECT '{1.5,2.7}'::double precision[]")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.map(_.doubleValue) shouldBe IndexedSeq(1.5, 2.7)
    }
  }

  // ── Varchar arrays ────────────────────────────────────────────────

  "varchar array casts" - {
    "'{a,b,c}'::varchar[]" in {
      val table = query("SELECT '{a,b,c}'::varchar[]")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.map(_.string) shouldBe IndexedSeq("a", "b", "c")
    }
  }

  // ── Usage in queries ──────────────────────────────────────────────

  "array literal casts in context" - {
    "in WHERE with ANY" in {
      val table = query(
        """CREATE TABLE t (id INT, name TEXT);
          |INSERT INTO t VALUES (1, 'alice'), (2, 'bob'), (3, 'charlie');
          |SELECT name FROM t WHERE name = ANY('{alice,charlie}'::text[]) ORDER BY name;
          |""".stripMargin
      )
      table.data.map(_.data(0).string) shouldBe IndexedSeq("alice", "charlie")
    }

    "in WHERE with integer ANY" in {
      val table = query(
        """CREATE TABLE t (id INT, val INT);
          |INSERT INTO t VALUES (1, 10), (2, 20), (3, 30);
          |SELECT val FROM t WHERE val = ANY('{10,30}'::integer[]) ORDER BY val;
          |""".stripMargin
      )
      table.data.map(_.data(0).intValue) shouldBe IndexedSeq(10, 30)
    }

    "ARRAY constructor still works" in {
      val table = query("SELECT ARRAY[1, 2, 3]")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data.map(_.intValue) shouldBe IndexedSeq(1, 2, 3)
    }
  }
}

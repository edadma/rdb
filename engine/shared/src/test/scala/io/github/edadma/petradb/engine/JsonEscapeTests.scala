package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class JsonEscapeTests extends AnyFreeSpec with Matchers with Testing {

  // ── JSON string escapes (the original bug) ────────────────────────

  "JSON string with escaped double quotes" - {
    "insert and read JSON with backslash-escaped double quotes" in {
      val table = query(
        """CREATE TABLE json_write (id SERIAL PRIMARY KEY, label TEXT NOT NULL DEFAULT '', data JSON);
          |INSERT INTO json_write (label, data) VALUES ('test', '{"quote":"he said \"hello\""}');
          |SELECT data FROM json_write WHERE label = 'test';
          |""".stripMargin
      )
      table.data should have length 1
    }

    "insert JSON with escaped quotes via E-string" in {
      val table = query(
        """CREATE TABLE json_write (id SERIAL PRIMARY KEY, label TEXT NOT NULL DEFAULT '', data JSON);
          |INSERT INTO json_write (label, data) VALUES ('test', E'{"quote":"he said \\"hello\\""}');
          |SELECT data FROM json_write WHERE label = 'test';
          |""".stripMargin
      )
      table.data should have length 1
    }

    "string literal containing backslash-double-quote" in {
      val table = query(
        """SELECT '{"key":"value with \"quotes\""}';
          |""".stripMargin
      )
      table.data(0).data(0).string shouldBe """{"key":"value with \"quotes\""}"""
    }

    "string literal with backslash before double quote is literal" in {
      val table = query(
        """SELECT 'a\"b';
          |""".stripMargin
      )
      table.data(0).data(0).string shouldBe """a\"b"""
    }

    "string literal with just backslashes" in {
      val table = query(
        """SELECT 'a\\b';
          |""".stripMargin
      )
      table.data(0).data(0).string shouldBe """a\\b"""
    }
  }

  // ── JSON parser: all escape sequences ─────────────────────────────

  "JSON parser escape sequences" - {
    "\\n in JSON string" in {
      val table = query("""SELECT '{"text":"line1\nline2"}'::json ->> 'text'""")
      table.data(0).data(0).string shouldBe "line1\nline2"
    }

    "\\t in JSON string" in {
      val table = query("""SELECT '{"text":"col1\tcol2"}'::json ->> 'text'""")
      table.data(0).data(0).string shouldBe "col1\tcol2"
    }

    "\\r in JSON string" in {
      val table = query("""SELECT '{"text":"a\rb"}'::json ->> 'text'""")
      table.data(0).data(0).string shouldBe "a\rb"
    }

    "\\\\ in JSON string" in {
      val table = query("""SELECT '{"path":"c:\\dir"}'::json ->> 'path'""")
      table.data(0).data(0).string shouldBe "c:\\dir"
    }

    "\\/ in JSON string" in {
      val table = query("""SELECT '{"url":"http:\/\/example.com"}'::json ->> 'url'""")
      table.data(0).data(0).string shouldBe "http://example.com"
    }

    "\\b in JSON string" in {
      val table = query("""SELECT '{"text":"a\bb"}'::json ->> 'text'""")
      table.data(0).data(0).string shouldBe "a\bb"
    }

    "\\f in JSON string" in {
      val table = query("""SELECT '{"text":"a\fb"}'::json ->> 'text'""")
      table.data(0).data(0).string shouldBe "a\fb"
    }

    "multiple escapes in one string" in {
      val table = query("""SELECT '{"text":"a\"b\\c\td"}'::json ->> 'text'""")
      table.data(0).data(0).string shouldBe "a\"b\\c\td"
    }
  }

  // ── JSON parser: structure ────────────────────────────────────────

  "JSON parser structures" - {
    "empty object" in {
      val table = query("SELECT '{}'::json")
      table.data(0).data(0) shouldBe a[ObjectValue]
    }

    "empty array" in {
      val table = query("SELECT '[]'::json")
      table.data(0).data(0) shouldBe a[ArrayValue]
    }

    "nested objects" in {
      val table = query("""SELECT '{"a":{"b":{"c":1}}}'::json #>> ARRAY['a','b','c']""")
      table.data(0).data(0).string shouldBe "1"
    }

    "array of objects" in {
      val table = query("""SELECT '[{"id":1},{"id":2}]'::json""")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data should have length 2
    }

    "mixed types in array" in {
      val table = query("""SELECT '[1,"two",true,null,3.14]'::json""")
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data should have length 5
    }

    "null value" in {
      val table = query("""SELECT '{"key":null}'::json -> 'key'""")
      table.data(0).data(0).isNull shouldBe true
    }

    "boolean values" in {
      val table = query("""SELECT '{"a":true,"b":false}'::json""")
      val obj = table.data(0).data(0).asInstanceOf[ObjectValue]
      obj.properties should have length 2
    }

    "integer number" in {
      val table = query("""SELECT '{"n":42}'::json -> 'n'""")
      table.data(0).data(0).intValue shouldBe 42
    }

    "negative number" in {
      val table = query("""SELECT '{"n":-7}'::json -> 'n'""")
      table.data(0).data(0).intValue shouldBe -7
    }

    "decimal number" in {
      val table = query("""SELECT '{"n":3.14}'::json -> 'n'""")
      table.data(0).data(0).doubleValue shouldBe 3.14
    }

    "string with spaces" in {
      val table = query("""SELECT '{"msg":"hello world"}'::json ->> 'msg'""")
      table.data(0).data(0).string shouldBe "hello world"
    }

    "empty string value" in {
      val table = query("""SELECT '{"msg":""}'::json ->> 'msg'""")
      table.data(0).data(0).string shouldBe ""
    }

    "deeply nested array" in {
      val table = query("""SELECT '[[1,[2,[3]]]]'::json""")
      table.data(0).data(0) shouldBe a[ArrayValue]
    }
  }

  // ── JSON roundtrip via table ──────────────────────────────────────

  "JSON roundtrip" - {
    "insert and retrieve complex JSON" in {
      val table = query(
        """CREATE TABLE j (id SERIAL PRIMARY KEY, data JSON);
          |INSERT INTO j (data) VALUES ('{"users":[{"name":"alice","age":30},{"name":"bob","age":25}]}');
          |SELECT data -> 'users' FROM j;
          |""".stripMargin
      )
      val arr = table.data(0).data(0).asInstanceOf[ArrayValue]
      arr.data should have length 2
    }

    "insert JSON with all escape types" in {
      val table = query(
        """CREATE TABLE j (id SERIAL PRIMARY KEY, data JSON);
          |INSERT INTO j (data) VALUES ('{"esc":"tab\there\nnewline\\backslash\"quote"}');
          |SELECT data ->> 'esc' FROM j;
          |""".stripMargin
      )
      table.data(0).data(0).string shouldBe "tab\there\nnewline\\backslash\"quote"
    }
  }
}

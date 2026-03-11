package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class JSONTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE t (id INT, data JSONB);
      |INSERT INTO t (id, data) VALUES (1, {name: 'Alice', age: 30, tags: ['admin', 'user']});
      |INSERT INTO t (id, data) VALUES (2, {name: 'Bob', age: 25, address: {city: 'NYC', zip: '10001'}});
      |INSERT INTO t (id, data) VALUES (3, {name: 'Carol', age: 35, tags: ['user']});
      |""".trim.stripMargin

  // ── Access operators -> and ->> ────────────────────────────────────

  "Access operators" - {
    "-> extracts JSON object field" in {
      val table = query(s"$setup SELECT data -> 'name' FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe TextValue("Alice")
    }

    "->> extracts JSON object field as text" in {
      val table = query(s"$setup SELECT data ->> 'name' FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe TextValue("Alice")
    }

    "-> extracts array element by index" in {
      val table = query(s"$setup SELECT data -> 'tags' -> 0 FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe TextValue("admin")
    }

    "->> extracts array element as text" in {
      val table = query(s"$setup SELECT data -> 'tags' ->> 0 FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe TextValue("admin")
    }

    "-> with negative index" in {
      val table = query(s"$setup SELECT data -> 'tags' -> -1 FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe TextValue("user")
    }

    "-> missing key returns null" in {
      val table = query(s"$setup SELECT data -> 'missing' FROM t WHERE id = 1;")
      table.data(0).data(0).isNull shouldBe true
    }

    "->> missing key returns null" in {
      val table = query(s"$setup SELECT data ->> 'missing' FROM t WHERE id = 1;")
      table.data(0).data(0).isNull shouldBe true
    }

    "chained -> for nested access" in {
      val table = query(s"$setup SELECT data -> 'address' -> 'city' FROM t WHERE id = 2;")
      table.data(0).data(0) shouldBe TextValue("NYC")
    }

    "chained -> then ->> for text result" in {
      val table = query(s"$setup SELECT data -> 'address' ->> 'zip' FROM t WHERE id = 2;")
      table.data(0).data(0) shouldBe TextValue("10001")
    }
  }

  // ── Path operators #> and #>> ──────────────────────────────────────

  "Path operators" - {
    "#> navigates nested path" in {
      val table = query(s"$setup SELECT data #> ARRAY['address', 'city'] FROM t WHERE id = 2;")
      table.data(0).data(0) shouldBe TextValue("NYC")
    }

    "#>> navigates nested path and returns text" in {
      val table = query(s"$setup SELECT data #>> ARRAY['address', 'zip'] FROM t WHERE id = 2;")
      table.data(0).data(0) shouldBe TextValue("10001")
    }

    "#> with missing path returns null" in {
      val table = query(s"$setup SELECT data #> ARRAY['address', 'street'] FROM t WHERE id = 2;")
      table.data(0).data(0).isNull shouldBe true
    }

    "#> into array by index" in {
      val table = query(s"$setup SELECT data #> ARRAY['tags', '0'] FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe TextValue("admin")
    }
  }

  // ── Containment operators @> and <@ ────────────────────────────────

  "Containment operators" - {
    "@> object contains" in {
      val table = query(s"$setup SELECT id FROM t WHERE data @> {name: 'Alice'};")
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(1)
    }

    "@> object does not contain" in {
      val table = query(s"$setup SELECT id FROM t WHERE data @> {name: 'Zoe'};")
      table.data.length shouldBe 0
    }

    "<@ contained by" in {
      val table = query(s"$setup SELECT id FROM t WHERE {name: 'Bob'} <@ data;")
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(2)
    }

    "@> nested containment" in {
      val table = query(s"$setup SELECT id FROM t WHERE data @> {address: {city: 'NYC'}};")
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(2)
    }

    "@> array containment" in {
      val table = query(
        """
          |CREATE TABLE a (id INT, arr JSONB);
          |INSERT INTO a (id, arr) VALUES (1, [1, 2, 3, 4, 5]);
          |SELECT id FROM a WHERE arr @> [2, 4];
          |""".trim.stripMargin
      )
      table.data.length shouldBe 1
    }

    "&& array overlap" in {
      val table = query(
        """
          |CREATE TABLE a (id INT, tags TEXT[]);
          |INSERT INTO a (id, tags) VALUES (1, ARRAY['a', 'b', 'c']);
          |INSERT INTO a (id, tags) VALUES (2, ARRAY['d', 'e']);
          |INSERT INTO a (id, tags) VALUES (3, ARRAY['c', 'f']);
          |SELECT id FROM a WHERE tags && ARRAY['b', 'c'];
          |""".trim.stripMargin
      )
      table.data.length shouldBe 2
      table.data.map(_.data(0).intValue) shouldBe Seq(1, 3)
    }

    "&& no overlap returns empty" in {
      val table = query(
        """
          |CREATE TABLE a (id INT, tags TEXT[]);
          |INSERT INTO a (id, tags) VALUES (1, ARRAY['a', 'b']);
          |SELECT id FROM a WHERE tags && ARRAY['x', 'y'];
          |""".trim.stripMargin
      )
      table.data.length shouldBe 0
    }
  }

  // ── Key existence operators ?, ?|, ?& ──────────────────────────────

  "Key existence operators" - {
    "? checks key existence in object" in {
      val table = query(s"$setup SELECT id FROM t WHERE data ? 'name';")
      table.data.length shouldBe 3
    }

    "? returns false for missing key" in {
      val table = query(s"$setup SELECT id FROM t WHERE data ? 'email';")
      table.data.length shouldBe 0
    }

    "?| checks if any key exists" in {
      val table = query(s"$setup SELECT id FROM t WHERE data ?| ARRAY['email', 'name'];")
      table.data.length shouldBe 3
    }

    "?| returns false when no keys exist" in {
      val table = query(s"$setup SELECT id FROM t WHERE data ?| ARRAY['email', 'phone'];")
      table.data.length shouldBe 0
    }

    "?& checks if all keys exist" in {
      val table = query(s"$setup SELECT id FROM t WHERE data ?& ARRAY['name', 'age'];")
      table.data.length shouldBe 3
    }

    "?& returns false when not all keys exist" in {
      val table = query(s"$setup SELECT id FROM t WHERE data ?& ARRAY['name', 'email'];")
      table.data.length shouldBe 0
    }
  }

  // ── JSON concat via || ─────────────────────────────────────────────

  "JSON concat ||" - {
    "merges two objects" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT {a: 1} || {b: 2} FROM t;
          |""".trim.stripMargin
      )
      val result = table.data(0).data(0).asInstanceOf[ObjectValue]
      result.properties.length shouldBe 2
    }

    "right object overrides left" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT {a: 1, b: 2} || {b: 99} FROM t;
          |""".trim.stripMargin
      )
      val result = table.data(0).data(0).asInstanceOf[ObjectValue]
      result.get("b").get shouldBe NumberValue(99)
    }

    "concatenates two arrays" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT [1, 2] || [3, 4] FROM t;
          |""".trim.stripMargin
      )
      val result = table.data(0).data(0).asInstanceOf[ArrayValue]
      result.data.length shouldBe 4
    }

    "text concat still works" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, name TEXT);
          |INSERT INTO t (id, name) VALUES (1, 'Hello');
          |SELECT name || ' World' FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("Hello World")
    }
  }

  // ── Scalar functions ───────────────────────────────────────────────

  "Scalar functions" - {
    "jsonb_typeof" in {
      val table = query(s"$setup SELECT jsonb_typeof(data) FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe TextValue("object")
    }

    "jsonb_typeof on array" in {
      val table = query(s"$setup SELECT jsonb_typeof(data -> 'tags') FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe TextValue("array")
    }

    "jsonb_typeof on string" in {
      val table = query(s"$setup SELECT jsonb_typeof(data -> 'name') FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe TextValue("string")
    }

    "jsonb_typeof on number" in {
      val table = query(s"$setup SELECT jsonb_typeof(data -> 'age') FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe TextValue("number")
    }

    "jsonb_array_length" in {
      val table = query(s"$setup SELECT jsonb_array_length(data -> 'tags') FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe NumberValue(2)
    }

    "jsonb_object_keys" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, data JSONB);
          |INSERT INTO t (id, data) VALUES (1, {a: 1, b: 2});
          |SELECT jsonb_object_keys(data) FROM t WHERE id = 1;
          |""".trim.stripMargin
      )
      val result = table.data(0).data(0).asInstanceOf[ArrayValue]
      result.data.map(_.string) shouldBe IndexedSeq("a", "b")
    }

    "jsonb_extract_path" in {
      val table = query(s"$setup SELECT jsonb_extract_path(data, 'address', 'city') FROM t WHERE id = 2;")
      table.data(0).data(0) shouldBe TextValue("NYC")
    }

    "jsonb_extract_path_text" in {
      val table = query(s"$setup SELECT jsonb_extract_path_text(data, 'address', 'zip') FROM t WHERE id = 2;")
      table.data(0).data(0) shouldBe TextValue("10001")
    }

    "jsonb_set replaces existing key" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, data JSONB);
          |INSERT INTO t (id, data) VALUES (1, {a: 1, b: 2});
          |SELECT jsonb_set(data, ARRAY['b'], 99) FROM t WHERE id = 1;
          |""".trim.stripMargin
      )
      val result = table.data(0).data(0).asInstanceOf[ObjectValue]
      result.get("b").get shouldBe NumberValue(99)
    }

    "jsonb_set creates missing key" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, data JSONB);
          |INSERT INTO t (id, data) VALUES (1, {a: 1});
          |SELECT jsonb_set(data, ARRAY['c'], 42) FROM t WHERE id = 1;
          |""".trim.stripMargin
      )
      val result = table.data(0).data(0).asInstanceOf[ObjectValue]
      result.get("c").get shouldBe NumberValue(42)
    }

    "jsonb_set with create_missing false" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, data JSONB);
          |INSERT INTO t (id, data) VALUES (1, {a: 1});
          |SELECT jsonb_set(data, ARRAY['c'], 42, false) FROM t WHERE id = 1;
          |""".trim.stripMargin
      )
      val result = table.data(0).data(0).asInstanceOf[ObjectValue]
      result.get("c") shouldBe None
    }

    "jsonb_strip_nulls" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, data JSONB);
          |INSERT INTO t (id, data) VALUES (1, {a: 1, b: null, c: 3});
          |SELECT jsonb_strip_nulls(data) FROM t WHERE id = 1;
          |""".trim.stripMargin
      )
      val result = table.data(0).data(0).asInstanceOf[ObjectValue]
      result.properties.length shouldBe 2
      result.get("b") shouldBe None
    }

    "jsonb_pretty" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, data JSONB);
          |INSERT INTO t (id, data) VALUES (1, {a: 1});
          |SELECT jsonb_pretty(data) FROM t WHERE id = 1;
          |""".trim.stripMargin
      )
      val result = table.data(0).data(0).asInstanceOf[TextValue].s
      result should include("\"a\"")
      result should include("\n")
    }

    "jsonb_build_object" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT jsonb_build_object('a', 1, 'b', 'hello') FROM t;
          |""".trim.stripMargin
      )
      val result = table.data(0).data(0).asInstanceOf[ObjectValue]
      result.get("a").get shouldBe NumberValue(1)
      result.get("b").get shouldBe TextValue("hello")
    }

    "jsonb_build_array" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (1);
          |SELECT jsonb_build_array(1, 'two', true) FROM t;
          |""".trim.stripMargin
      )
      val result = table.data(0).data(0).asInstanceOf[ArrayValue]
      result.data.length shouldBe 3
    }

    "to_jsonb on number" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t (id) VALUES (42);
          |SELECT to_jsonb(id) FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(42)
    }

    "jsonb_insert into array" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, data JSONB);
          |INSERT INTO t (id, data) VALUES (1, [1, 2, 4, 5]);
          |SELECT jsonb_insert(data, ARRAY['2'], 3) FROM t WHERE id = 1;
          |""".trim.stripMargin
      )
      val result = table.data(0).data(0).asInstanceOf[ArrayValue]
      result.data.map(_.intValue) shouldBe IndexedSeq(1, 2, 3, 4, 5)
    }
  }

  // ── Aggregate functions ────────────────────────────────────────────

  "Aggregate functions" - {
    "json_agg collects values into array" in {
      val table = query(s"$setup SELECT json_agg(data -> 'name') FROM t;")
      val result = table.data(0).data(0).asInstanceOf[ArrayValue]
      result.data.length shouldBe 3
      result.data.map(_.string) shouldBe IndexedSeq("Alice", "Bob", "Carol")
    }

    "jsonb_agg collects values into array" in {
      val table = query(s"$setup SELECT jsonb_agg(data ->> 'name') FROM t;")
      val result = table.data(0).data(0).asInstanceOf[ArrayValue]
      result.data.length shouldBe 3
    }

    "json_object_agg collects key-value pairs" in {
      val table = query(s"$setup SELECT json_object_agg(data ->> 'name', data -> 'age') FROM t;")
      val result = table.data(0).data(0).asInstanceOf[ObjectValue]
      result.get("Alice").get shouldBe NumberValue(30)
      result.get("Bob").get shouldBe NumberValue(25)
      result.get("Carol").get shouldBe NumberValue(35)
    }

    "jsonb_object_agg collects key-value pairs" in {
      val table = query(s"$setup SELECT jsonb_object_agg(data ->> 'name', data -> 'age') FROM t;")
      val result = table.data(0).data(0).asInstanceOf[ObjectValue]
      result.properties.length shouldBe 3
    }
  }
}

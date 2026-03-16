package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class JsonContainmentTests extends AnyFreeSpec with Matchers with Testing {

  val setup =
    """CREATE TABLE jsonb_ops (id SERIAL PRIMARY KEY, label TEXT NOT NULL DEFAULT '', data JSON);
      |INSERT INTO jsonb_ops (label, data) VALUES ('object', '{"role":"admin","prefs":{"theme":"dark"}}');
      |INSERT INTO jsonb_ops (label, data) VALUES ('array', '[1,2,3,4,5]');
      |INSERT INTO jsonb_ops (label, data) VALUES ('simple', '{"name":"alice"}');
      |""".stripMargin

  "@> containment" - {
    "object contains matching key-value" in {
      val table = query(s"""$setup SELECT label FROM jsonb_ops WHERE data @> '{"role": "admin"}'""")
      table.data should have length 1
      table.data(0).data(0).string shouldBe "object"
    }

    "object contains nested object" in {
      val table = query(s"""$setup SELECT label FROM jsonb_ops WHERE data @> '{"prefs":{"theme":"dark"}}'""")
      table.data should have length 1
      table.data(0).data(0).string shouldBe "object"
    }

    "object does not contain non-matching value" in {
      val table = query(s"""$setup SELECT label FROM jsonb_ops WHERE data @> '{"role": "user"}'""")
      table.data shouldBe empty
    }

    "object does not contain missing key" in {
      val table = query(s"""$setup SELECT label FROM jsonb_ops WHERE data @> '{"nonexistent": "value"}'""")
      table.data shouldBe empty
    }

    "array contains subset" in {
      val table = query(s"""$setup SELECT label FROM jsonb_ops WHERE data @> '[1,3]'""")
      table.data should have length 1
      table.data(0).data(0).string shouldBe "array"
    }

    "array does not contain missing element" in {
      val table = query(s"""$setup SELECT label FROM jsonb_ops WHERE data @> '[6]'""")
      table.data shouldBe empty
    }
  }

  "<@ contained by" - {
    "smaller object is contained by larger" in {
      val table = query(s"""$setup SELECT label FROM jsonb_ops WHERE '{"role":"admin"}' <@ data""")
      table.data should have length 1
      table.data(0).data(0).string shouldBe "object"
    }
  }

  "@> with inline JSON (no table)" - {
    "literal containment check" in {
      val table = query("""SELECT '{"a":1,"b":2}'::json @> '{"a":1}'::json""")
      table.data(0).data(0).asInstanceOf[BooleanValue].b shouldBe true
    }

    "literal non-containment" in {
      val table = query("""SELECT '{"a":1}'::json @> '{"a":2}'::json""")
      table.data(0).data(0).asInstanceOf[BooleanValue].b shouldBe false
    }
  }
}

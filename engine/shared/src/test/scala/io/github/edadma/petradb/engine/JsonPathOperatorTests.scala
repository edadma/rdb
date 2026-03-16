package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class JsonPathOperatorTests extends AnyFreeSpec with Matchers with Testing {

  val setup =
    """CREATE TABLE jsonb_ops (id SERIAL PRIMARY KEY, label TEXT NOT NULL DEFAULT '', data JSON);
      |INSERT INTO jsonb_ops (label, data) VALUES ('nested', '{"a":{"b":{"c":1}}}');
      |INSERT INTO jsonb_ops (label, data) VALUES ('arr', '{"x":[10,20,30]}');
      |""".stripMargin

  "#> with text array literal" - {
    "#> '{a,b}' navigates nested path" in {
      val table = query(s"$setup SELECT data #> '{a,b}' FROM jsonb_ops WHERE label = 'nested'")
      val obj = table.data(0).data(0).asInstanceOf[ObjectValue]
      obj.properties.head._1 shouldBe "c"
    }

    "#> '{a,b,c}' navigates to leaf" in {
      val table = query(s"$setup SELECT data #> '{a,b,c}' FROM jsonb_ops WHERE label = 'nested'")
      table.data(0).data(0).intValue shouldBe 1
    }

    "#> '{a}' single element path" in {
      val table = query(s"$setup SELECT data #> '{a}' FROM jsonb_ops WHERE label = 'nested'")
      table.data(0).data(0) shouldBe a[ObjectValue]
    }

    "#> with array index" in {
      val table = query(s"$setup SELECT data #> '{x,1}' FROM jsonb_ops WHERE label = 'arr'")
      table.data(0).data(0).intValue shouldBe 20
    }

    "#> nonexistent key returns null" in {
      val table = query(s"$setup SELECT data #> '{z}' FROM jsonb_ops WHERE label = 'nested'")
      table.data(0).data(0).isNull shouldBe true
    }
  }

  "#>> with text array literal" - {
    "#>> '{a,b,c}' returns text" in {
      val table = query(s"$setup SELECT data #>> '{a,b,c}' FROM jsonb_ops WHERE label = 'nested'")
      table.data(0).data(0).string shouldBe "1"
    }

    "#>> '{x,0}' returns text from array" in {
      val table = query(s"$setup SELECT data #>> '{x,0}' FROM jsonb_ops WHERE label = 'arr'")
      table.data(0).data(0).string shouldBe "10"
    }
  }

  "#> with ARRAY constructor still works" - {
    "ARRAY['a','b'] syntax" in {
      val table = query(s"$setup SELECT data #> ARRAY['a','b'] FROM jsonb_ops WHERE label = 'nested'")
      val obj = table.data(0).data(0).asInstanceOf[ObjectValue]
      obj.properties.head._1 shouldBe "c"
    }
  }

  "#> with explicit cast" - {
    "parenthesized ('{a,b}'::text[]) syntax" in {
      val table = query(s"$setup SELECT data #> ('{a,b}'::text[]) FROM jsonb_ops WHERE label = 'nested'")
      val obj = table.data(0).data(0).asInstanceOf[ObjectValue]
      obj.properties.head._1 shouldBe "c"
    }
  }
}

package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class JsonTopLevelValueTests extends AnyFreeSpec with Matchers with Testing {

  "JSON top-level scalar values" - {
    "null is valid JSON" in {
      val table = query("SELECT 'null'::json")
      table.data(0).data(0).isNull shouldBe true
    }

    "true is valid JSON" in {
      val table = query("SELECT 'true'::json")
      table.data(0).data(0).asInstanceOf[BooleanValue].b shouldBe true
    }

    "false is valid JSON" in {
      val table = query("SELECT 'false'::json")
      table.data(0).data(0).asInstanceOf[BooleanValue].b shouldBe false
    }

    "integer is valid JSON" in {
      val table = query("SELECT '42'::json")
      table.data(0).data(0).intValue shouldBe 42
    }

    "negative integer is valid JSON" in {
      val table = query("SELECT '-7'::json")
      table.data(0).data(0).intValue shouldBe -7
    }

    "decimal is valid JSON" in {
      val table = query("SELECT '3.14'::json")
      table.data(0).data(0).doubleValue shouldBe 3.14
    }

    "string is valid JSON" in {
      val table = query("""SELECT '"hello"'::json""")
      table.data(0).data(0).string shouldBe "hello"
    }

    "empty string is valid JSON" in {
      val table = query("""SELECT '""'::json""")
      table.data(0).data(0).string shouldBe ""
    }
  }

  "UPDATE JSON column with scalar values" - {
    val setup =
      """CREATE TABLE j (id SERIAL PRIMARY KEY, label TEXT NOT NULL DEFAULT '', data JSON);
        |INSERT INTO j (label, data) VALUES ('test', '{"a":1}');
        |""".stripMargin

    "update to null" in {
      val table = query(
        s"""$setup UPDATE j SET data = 'null' WHERE label = 'test';
           |SELECT data FROM j WHERE label = 'test';
           |""".stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }

    "update to integer" in {
      val table = query(
        s"""$setup UPDATE j SET data = '42' WHERE label = 'test';
           |SELECT data FROM j WHERE label = 'test';
           |""".stripMargin
      )
      table.data(0).data(0).intValue shouldBe 42
    }

    "update to string" in {
      val table = query(
        s"""$setup UPDATE j SET data = '"hello"' WHERE label = 'test';
           |SELECT data FROM j WHERE label = 'test';
           |""".stripMargin
      )
      table.data(0).data(0).string shouldBe "hello"
    }

    "update to boolean" in {
      val table = query(
        s"""$setup UPDATE j SET data = 'true' WHERE label = 'test';
           |SELECT data FROM j WHERE label = 'test';
           |""".stripMargin
      )
      table.data(0).data(0).asInstanceOf[BooleanValue].b shouldBe true
    }

    "SQL NULL (no quotes) still works" in {
      val table = query(
        s"""$setup UPDATE j SET data = NULL WHERE label = 'test';
           |SELECT data FROM j WHERE label = 'test';
           |""".stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }
  }
}

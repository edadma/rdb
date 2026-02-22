package io.github.edadma.petradb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ColumnAliasTests extends AnyFreeSpec with Matchers with Testing {

  "column aliases on VALUES" - {
    "columns named by alias list" in {
      val table = query("SELECT * FROM (VALUES (1, 'a'), (2, 'b')) AS t (id, name);")

      table.meta.columns(0).name shouldBe "id"
      table.meta.columns(1).name shouldBe "name"
      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(0).data(1) shouldBe TextValue("a")
      table.data(1).data(0) shouldBe NumberValue(2)
      table.data(1).data(1) shouldBe TextValue("b")
    }

    "reference by aliased name" in {
      val table = query("SELECT id, name FROM (VALUES (1, 'a')) AS t (id, name);")

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(0).data(1) shouldBe TextValue("a")
    }

    "qualified reference" in {
      val table = query("SELECT t.id, t.name FROM (VALUES (1, 'a')) AS t (id, name);")

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(0).data(1) shouldBe TextValue("a")
    }

    "wrong column count throws error" in {
      an[Exception] should be thrownBy query("SELECT * FROM (VALUES (1, 'a')) AS t (id);")
    }

    "table qualifier is set" in {
      val table = query("SELECT * FROM (VALUES (1, 'a')) AS t (id, name);")

      table.meta.columns(0).table shouldBe Some("t")
      table.meta.columns(1).table shouldBe Some("t")
    }
  }

  "column aliases with JOIN" - {
    "JOIN with aliased VALUES source" in {
      val table = query(
        """
          |CREATE TABLE items (id INT, label TEXT);
          |INSERT INTO items (id, label) VALUES (1, 'one'), (2, 'two'), (3, 'three');
          |SELECT items.label, d.value
          |FROM items
          |JOIN (VALUES (1, 100), (3, 300)) AS d (id, value) ON items.id = d.id
          |ORDER BY items.id;
          |""".trim.stripMargin
      )

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe TextValue("one")
      table.data(0).data(1) shouldBe NumberValue(100)
      table.data(1).data(0) shouldBe TextValue("three")
      table.data(1).data(1) shouldBe NumberValue(300)
    }
  }

  "direct VALUES in FROM with column aliases" - {
    "direct VALUES source with column aliases" in {
      val table = query("SELECT id, name FROM VALUES (1, 'a'), (2, 'b') AS t (id, name);")

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(0).data(1) shouldBe TextValue("a")
    }
  }
}

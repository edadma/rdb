package io.github.edadma.petradb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ValuesTests extends AnyFreeSpec with Matchers with Testing {

  "standalone VALUES" - {
    "basic two-row result" in {
      val table = query("VALUES (1, 'hello'), (2, 'world');")

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(0).data(1) shouldBe TextValue("hello")
      table.data(1).data(0) shouldBe NumberValue(2)
      table.data(1).data(1) shouldBe TextValue("world")
    }

    "column names are column1, column2, etc." in {
      val table = query("VALUES (1, 'a');")

      table.meta.columns(0).name shouldBe "column1"
      table.meta.columns(1).name shouldBe "column2"
    }

    "single value" in {
      val table = query("VALUES (42);")

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(42)
    }
  }

  "VALUES with ORDER BY and LIMIT" - {
    "ORDER BY ordinal" in {
      val table = query("VALUES (3, 'c'), (1, 'a'), (2, 'b') ORDER BY 1;")

      table.data.length shouldBe 3
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(1).data(0) shouldBe NumberValue(2)
      table.data(2).data(0) shouldBe NumberValue(3)
    }

    "ORDER BY ordinal with LIMIT" in {
      val table = query("VALUES (3, 'c'), (1, 'a'), (2, 'b') ORDER BY 1 LIMIT 2;")

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(1).data(0) shouldBe NumberValue(2)
    }
  }

  "VALUES as FROM source" - {
    "subquery in FROM with alias" in {
      val table = query("SELECT * FROM (VALUES (1, 'a'), (2, 'b')) AS t;")

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(1)
      table.data(0).data(1) shouldBe TextValue("a")
    }

    "direct VALUES in FROM with alias" in {
      val table = query("SELECT * FROM VALUES (10, 20), (30, 40) AS t;")

      table.data.length shouldBe 2
      table.data(0).data(0) shouldBe NumberValue(10)
      table.data(0).data(1) shouldBe NumberValue(20)
    }
  }

  "VALUES with UNION" - {
    "UNION with SELECT" in {
      val table = query("SELECT 1, 'x' UNION VALUES (2, 'y');")

      table.data.length shouldBe 2
    }
  }

  "VALUES with expressions" - {
    "arithmetic and concatenation" in {
      val table = query("VALUES (1 + 1, 'x' || 'y');")

      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(2)
      table.data(0).data(1) shouldBe TextValue("xy")
    }
  }

  "VALUES with NULLs" - {
    "NULL values in rows" in {
      val table = query("VALUES (NULL, 1), (2, NULL);")

      table.data.length shouldBe 2
      table.data(0).data(0).isNull shouldBe true
      table.data(0).data(1) shouldBe NumberValue(1)
      table.data(1).data(0) shouldBe NumberValue(2)
      table.data(1).data(1).isNull shouldBe true
    }
  }

  "VALUES row width mismatch" - {
    "throws error for mismatched widths" in {
      an[Exception] should be thrownBy query("VALUES (1, 2), (3);")
    }
  }
}

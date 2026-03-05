package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import io.github.edadma.dal.{IntType => DIntType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class UpsertTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE kv (id INT PRIMARY KEY, v INT);
      |INSERT INTO kv (id, v) VALUES (1, 10);
      |""".trim.stripMargin

  "ON CONFLICT DO UPDATE" - {

    "basic DO UPDATE — conflict on PK updates non-key column" in {
      val table = query(
        s"""
           |$setup
           |INSERT INTO kv (id, v) VALUES (1, 99) ON CONFLICT (id) DO UPDATE SET v = 99;
           |SELECT id, v FROM kv WHERE id = 1;
           |""".trim.stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(DIntType, 1)
      table.data(0).data(1) shouldBe NumberValue(DIntType, 99)
    }

    "EXCLUDED references proposed insert value" in {
      val table = query(
        s"""
           |$setup
           |INSERT INTO kv (id, v) VALUES (1, 42) ON CONFLICT (id) DO UPDATE SET v = EXCLUDED.v;
           |SELECT v FROM kv WHERE id = 1;
           |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 42)
    }

    "mixed expression using EXCLUDED" in {
      val table = query(
        s"""
           |$setup
           |INSERT INTO kv (id, v) VALUES (1, 5) ON CONFLICT (id) DO UPDATE SET v = EXCLUDED.v + 1;
           |SELECT v FROM kv WHERE id = 1;
           |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 6)
    }

    "bare column ref resolves to existing row value" in {
      val table = query(
        s"""
           |$setup
           |INSERT INTO kv (id, v) VALUES (1, 5) ON CONFLICT (id) DO UPDATE SET v = v + EXCLUDED.v;
           |SELECT v FROM kv WHERE id = 1;
           |""".trim.stripMargin
      )
      // existing v=10, excluded v=5 → 10+5=15
      table.data(0).data(0) shouldBe NumberValue(DIntType, 15)
    }

    "no conflict — new row inserts normally" in {
      val table = query(
        s"""
           |$setup
           |INSERT INTO kv (id, v) VALUES (2, 20) ON CONFLICT (id) DO UPDATE SET v = EXCLUDED.v;
           |SELECT id, v FROM kv ORDER BY id;
           |""".trim.stripMargin
      )
      table.data.length shouldBe 2
      table.data(1).data(0) shouldBe NumberValue(DIntType, 2)
      table.data(1).data(1) shouldBe NumberValue(DIntType, 20)
    }

    "multi-row upsert — one conflicts one doesn't" in {
      val table = query(
        s"""
           |$setup
           |INSERT INTO kv (id, v) VALUES (1, 100), (2, 200) ON CONFLICT (id) DO UPDATE SET v = EXCLUDED.v;
           |SELECT id, v FROM kv ORDER BY id;
           |""".trim.stripMargin
      )
      table.data.length shouldBe 2
      table.data(0).data(1) shouldBe NumberValue(DIntType, 100)
      table.data(1).data(1) shouldBe NumberValue(DIntType, 200)
    }

    "multi-column conflict target" in {
      val table = query(
        """
          |CREATE TABLE ab (a INT, b INT, v INT, PRIMARY KEY (a, b));
          |INSERT INTO ab (a, b, v) VALUES (1, 2, 10);
          |INSERT INTO ab (a, b, v) VALUES (1, 2, 99) ON CONFLICT (a, b) DO UPDATE SET v = EXCLUDED.v;
          |SELECT v FROM ab WHERE a = 1 AND b = 2;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 99)
    }

    "multi-column SET updates several columns at once" in {
      val table = query(
        """
          |CREATE TABLE mc (id INT PRIMARY KEY, x INT, y INT);
          |INSERT INTO mc (id, x, y) VALUES (1, 10, 20);
          |INSERT INTO mc (id, x, y) VALUES (1, 30, 40) ON CONFLICT (id) DO UPDATE SET x = EXCLUDED.x, y = EXCLUDED.y;
          |SELECT x, y FROM mc WHERE id = 1;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(DIntType, 30)
      table.data(0).data(1) shouldBe NumberValue(DIntType, 40)
    }

    "RETURNING reflects post-update values" in {
      val res = results(
        s"""
           |$setup
           |INSERT INTO kv (id, v) VALUES (1, 55) ON CONFLICT (id) DO UPDATE SET v = EXCLUDED.v RETURNING v;
           |""".trim.stripMargin
      )
      val ins = res.last.asInstanceOf[InsertResult]
      ins.obj("v") shouldBe NumberValue(DIntType, 55)
    }

    "DO NOTHING regression — existing behaviour unchanged" in {
      val table = query(
        s"""
           |$setup
           |INSERT INTO kv (id, v) VALUES (1, 999) ON CONFLICT DO NOTHING;
           |SELECT v FROM kv WHERE id = 1;
           |""".trim.stripMargin
      )
      // original value preserved
      table.data(0).data(0) shouldBe NumberValue(DIntType, 10)
    }

  }
}

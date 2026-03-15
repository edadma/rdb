package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ByteaTests extends AnyFreeSpec with Matchers with Testing {

  // ── Storage and retrieval ───────────────────────────────────────

  "bytea storage" - {
    "ARRAY literal into BYTEA column produces ByteaValue" in {
      val table = query(
        """
          |CREATE TABLE t (data BYTEA);
          |INSERT INTO t VALUES (ARRAY[72, 101, 108, 108, 111]);
          |SELECT data FROM t;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe a[ByteaValue]
      table.data(0).data(0).asInstanceOf[ByteaValue].data shouldBe Array[Byte](72, 101, 108, 108, 111)
    }

    "hex string into BYTEA column produces ByteaValue" in {
      val table = query(
        """
          |CREATE TABLE t (data BYTEA);
          |INSERT INTO t VALUES (E'\\x48656c6c6f');
          |SELECT data FROM t;
          |""".stripMargin
      )
      val result = table.data(0).data(0).asInstanceOf[ByteaValue]
      result.data shouldBe Array[Byte](0x48, 0x65, 0x6c, 0x6c, 0x6f) // "Hello"
    }

    "NULL BYTEA returns NullValue" in {
      val table = query(
        """
          |CREATE TABLE t (data BYTEA);
          |INSERT INTO t VALUES (NULL);
          |SELECT data FROM t;
          |""".stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }

    "text string into BYTEA column produces ByteaValue with UTF-8 bytes" in {
      val table = query(
        """
          |CREATE TABLE t (data BYTEA);
          |INSERT INTO t VALUES ('Hello');
          |SELECT data FROM t;
          |""".stripMargin
      )
      val result = table.data(0).data(0).asInstanceOf[ByteaValue]
      new String(result.data, "UTF-8") shouldBe "Hello"
    }
  }

  // ── Functions ───────────────────────────────────────────────────

  "bytea functions" - {
    "octet_length returns byte count" in {
      val table = query(
        """
          |CREATE TABLE t (data BYTEA);
          |INSERT INTO t VALUES (ARRAY[1, 2, 3, 4, 5]);
          |SELECT octet_length(data) FROM t;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(5)
    }

    "get_byte extracts a single byte" in {
      val table = query(
        """
          |CREATE TABLE t (data BYTEA);
          |INSERT INTO t VALUES (ARRAY[10, 20, 30]);
          |SELECT get_byte(data, 1) FROM t;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(20)
    }

    "set_byte modifies a byte" in {
      val table = query(
        """
          |CREATE TABLE t (data BYTEA);
          |INSERT INTO t VALUES (ARRAY[10, 20, 30]);
          |SELECT get_byte(set_byte(data, 1, 99), 1) FROM t;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(99)
    }

    "encode to hex" in {
      val table = query(
        """
          |CREATE TABLE t (data BYTEA);
          |INSERT INTO t VALUES (ARRAY[255, 0, 171]);
          |SELECT encode(data, 'hex') FROM t;
          |""".stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("ff00ab")
    }
  }

  // ── CAST ────────────────────────────────────────────────────────

  "bytea casting" - {
    "CAST ARRAY to BYTEA" in {
      val table = query(
        """
          |CREATE TABLE t (id INT);
          |INSERT INTO t VALUES (1);
          |SELECT CAST(ARRAY[65, 66, 67] AS BYTEA) FROM t;
          |""".stripMargin
      )
      val result = table.data(0).data(0).asInstanceOf[ByteaValue]
      new String(result.data, "UTF-8") shouldBe "ABC"
    }
  }

  // ── Comparison ──────────────────────────────────────────────────

  "bytea comparison" - {
    "equal bytea values match in WHERE" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, data BYTEA);
          |INSERT INTO t VALUES (1, ARRAY[1, 2, 3]);
          |INSERT INTO t VALUES (2, ARRAY[4, 5, 6]);
          |SELECT id FROM t WHERE data = ARRAY[1, 2, 3]::BYTEA;
          |""".stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(1)
    }

    "IS NULL works on bytea columns" in {
      val table = query(
        """
          |CREATE TABLE t (id INT, data BYTEA);
          |INSERT INTO t VALUES (1, ARRAY[1, 2]);
          |INSERT INTO t VALUES (2, NULL);
          |SELECT id FROM t WHERE data IS NULL;
          |""".stripMargin
      )
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(2)
    }
  }
}

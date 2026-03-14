package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class EscapeProbeTest extends AnyFreeSpec with Matchers with Testing {
  "escape probes" - {
    "E-string backslash-n" in {
      val table = query("SELECT E'hello\\nworld'")
      table.data(0).data(0).string shouldBe "hello\nworld"
    }
    "E-string backslash-t" in {
      val table = query("SELECT E'hello\\tworld'")
      table.data(0).data(0).string shouldBe "hello\tworld"
    }
    "E-string backslash-r" in {
      val table = query("SELECT E'hello\\rworld'")
      table.data(0).data(0).string shouldBe "hello\rworld"
    }
    "E-string backslash-backslash" in {
      val table = query("SELECT E'hello\\\\world'")
      table.data(0).data(0).string shouldBe "hello\\world"
    }
    "E-string single quote via backslash" in {
      val table = query("SELECT E'it\\'s'")
      table.data(0).data(0).string shouldBe "it's"
    }
    "E-string backslash-b (backspace)" in {
      val table = query("SELECT E'ab\\bc'")
      table.data(0).data(0).string shouldBe "ab\bc"
    }
    "E-string backslash-f (form feed)" in {
      val table = query("SELECT E'ab\\fc'")
      table.data(0).data(0).string shouldBe "ab\fc"
    }
    "single quote escape (doubled)" in {
      val table = query("SELECT 'it''s'")
      table.data(0).data(0).string shouldBe "it's"
    }
    "regular string treats backslash literally" in {
      val table = query("SELECT 'hello\\nworld'")
      table.data(0).data(0).string shouldBe "hello\\nworld"
    }
    "E-string unicode escape \\uXXXX" in {
      val table = query("SELECT E'\\u0041'")
      table.data(0).data(0).string shouldBe "A"
    }
    "E-string hex escape \\xNN" in {
      val table = query("SELECT E'\\x41'")
      table.data(0).data(0).string shouldBe "A"
    }
    "insert and retrieve with newline" in {
      val table = query(
        """CREATE TABLE t (id INT, val TEXT);
          |INSERT INTO t VALUES (1, E'line1\nline2');
          |SELECT val FROM t WHERE id = 1;
          |""".stripMargin
      )
      table.data(0).data(0).string shouldBe "line1\nline2"
    }
    "insert and retrieve with tab" in {
      val table = query(
        """CREATE TABLE t (id INT, val TEXT);
          |INSERT INTO t VALUES (1, E'col1\tcol2');
          |SELECT val FROM t WHERE id = 1;
          |""".stripMargin
      )
      table.data(0).data(0).string shouldBe "col1\tcol2"
    }
    "empty E-string" in {
      val table = query("SELECT E''")
      table.data(0).data(0).string shouldBe ""
    }
    "E-string with multiple escapes" in {
      val table = query("SELECT E'a\\tb\\nc'")
      table.data(0).data(0).string shouldBe "a\tb\nc"
    }
  }
}

package io.github.edadma.rdb

import io.github.edadma.dal.{IntType => DIntType, DoubleType => DDoubleType}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ScalarFunctionTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE t (id INT, name TEXT, val INT);
      |INSERT INTO t (id, name, val) VALUES
      |  (1, 'Hello World', 42),
      |  (2, '  spaces  ', -7),
      |  (3, 'ABC', 0);
      |""".trim.stripMargin

  "String functions" - {
    "lower" in {
      val table = query(s"$setup SELECT lower(name) FROM t WHERE id = 3;")
      table.data(0).data(0) shouldBe TextValue("abc")
    }

    "upper" in {
      val table = query(s"$setup SELECT upper(name) FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe TextValue("HELLO WORLD")
    }

    "length" in {
      val table = query(s"$setup SELECT length(name) FROM t WHERE id = 3;")
      table.data(0).data(0) shouldBe NumberValue(3)
    }

    "trim" in {
      val table = query(s"$setup SELECT trim(name) FROM t WHERE id = 2;")
      table.data(0).data(0) shouldBe TextValue("spaces")
    }

    "ltrim" in {
      val table = query(s"$setup SELECT ltrim(name) FROM t WHERE id = 2;")
      table.data(0).data(0) shouldBe TextValue("spaces  ")
    }

    "rtrim" in {
      val table = query(s"$setup SELECT rtrim(name) FROM t WHERE id = 2;")
      table.data(0).data(0) shouldBe TextValue("  spaces")
    }

    "substring with start" in {
      val table = query(s"$setup SELECT substring(name, 7) FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe TextValue("World")
    }

    "substring with start and length" in {
      val table = query(s"$setup SELECT substring(name, 1, 5) FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe TextValue("Hello")
    }

    "substr alias" in {
      val table = query(s"$setup SELECT substr(name, 1, 3) FROM t WHERE id = 3;")
      table.data(0).data(0) shouldBe TextValue("ABC")
    }

    "left function" in {
      val table = query(s"""$setup SELECT substring(name, 1, 5) FROM t WHERE id = 1;""")
      table.data(0).data(0) shouldBe TextValue("Hello")
    }

    "right function via substring" in {
      val table = query(s"""$setup SELECT substring(name, 7) FROM t WHERE id = 1;""")
      table.data(0).data(0) shouldBe TextValue("World")
    }

    "replace" in {
      val table = query(s"$setup SELECT replace(name, 'World', 'Earth') FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe TextValue("Hello Earth")
    }

    "concat" in {
      val table = query(s"$setup SELECT concat(name, '!') FROM t WHERE id = 3;")
      table.data(0).data(0) shouldBe TextValue("ABC!")
    }

    "repeat" in {
      val table = query(s"$setup SELECT repeat(name, 3) FROM t WHERE id = 3;")
      table.data(0).data(0) shouldBe TextValue("ABCABCABC")
    }

    "position found" in {
      val table = query(s"$setup SELECT position('World', name) FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe NumberValue(7)
    }

    "position not found" in {
      val table = query(s"$setup SELECT position('xyz', name) FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe NumberValue(0)
    }

    "lpad with default pad" in {
      val table = query(s"$setup SELECT lpad(name, 6) FROM t WHERE id = 3;")
      table.data(0).data(0) shouldBe TextValue("   ABC")
    }

    "lpad with custom pad" in {
      val table = query(s"$setup SELECT lpad(name, 6, '0') FROM t WHERE id = 3;")
      table.data(0).data(0) shouldBe TextValue("000ABC")
    }

    "rpad with default pad" in {
      val table = query(s"$setup SELECT rpad(name, 6) FROM t WHERE id = 3;")
      table.data(0).data(0) shouldBe TextValue("ABC   ")
    }

    "rpad with custom pad" in {
      val table = query(s"$setup SELECT rpad(name, 6, '.') FROM t WHERE id = 3;")
      table.data(0).data(0) shouldBe TextValue("ABC...")
    }
  }

  "Numeric functions" - {
    "abs" in {
      val table = query(s"$setup SELECT abs(val) FROM t WHERE id = 2;")
      table.data(0).data(0) shouldBe NumberValue(7.0)
    }

    "ceil" in {
      val table = query(
        """
          |CREATE TABLE n (val DOUBLE);
          |INSERT INTO n (val) VALUES (3.2);
          |SELECT ceil(val) FROM n;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(4.0)
    }

    "floor" in {
      val table = query(
        """
          |CREATE TABLE n (val DOUBLE);
          |INSERT INTO n (val) VALUES (3.8);
          |SELECT floor(val) FROM n;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(3.0)
    }

    "round without digits" in {
      val table = query(
        """
          |CREATE TABLE n (val DOUBLE);
          |INSERT INTO n (val) VALUES (3.5);
          |SELECT round(val) FROM n;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(4.0)
    }

    "round with digits" in {
      val table = query(
        """
          |CREATE TABLE n (val DOUBLE);
          |INSERT INTO n (val) VALUES (3.14159);
          |SELECT round(val, 2) FROM n;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(3.14)
    }

    "trunc" in {
      val table = query(
        """
          |CREATE TABLE n (val DOUBLE);
          |INSERT INTO n (val) VALUES (3.9);
          |SELECT trunc(val) FROM n;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(3.0)
    }

    "sign positive" in {
      val table = query(s"$setup SELECT sign(val) FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe NumberValue(1.0)
    }

    "sign negative" in {
      val table = query(s"$setup SELECT sign(val) FROM t WHERE id = 2;")
      table.data(0).data(0) shouldBe NumberValue(-1.0)
    }

    "sign zero" in {
      val table = query(s"$setup SELECT sign(val) FROM t WHERE id = 3;")
      table.data(0).data(0) shouldBe NumberValue(0.0)
    }

    "mod" in {
      val table = query(s"$setup SELECT mod(val, 5) FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe NumberValue(2.0)
    }

    "power" in {
      val table = query(
        """
          |CREATE TABLE n (val INT);
          |INSERT INTO n (val) VALUES (3);
          |SELECT power(val, 2) FROM n;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(9.0)
    }

    "sqrt" in {
      val table = query(
        """
          |CREATE TABLE n (val INT);
          |INSERT INTO n (val) VALUES (16);
          |SELECT sqrt(val) FROM n;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(4.0)
    }

    "exp" in {
      val table = query(
        """
          |CREATE TABLE n (val INT);
          |INSERT INTO n (val) VALUES (0);
          |SELECT exp(val) FROM n;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(1.0)
    }

    "ln" in {
      val table = query(
        """
          |CREATE TABLE n (val INT);
          |INSERT INTO n (val) VALUES (1);
          |SELECT ln(val) FROM n;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(0.0)
    }

    "log10" in {
      val table = query(
        """
          |CREATE TABLE n (val INT);
          |INSERT INTO n (val) VALUES (100);
          |SELECT log10(val) FROM n;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe NumberValue(2.0)
    }
  }

  "typeof function" - {
    "returns type name" in {
      val table = query(s"$setup SELECT typeof(val) FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe TextValue("number")
    }

    "returns text for text column" in {
      val table = query(s"$setup SELECT typeof(name) FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe TextValue("text")
    }
  }

  "String concatenation operator (||)" - {
    "concatenates two strings" in {
      val table = query(s"$setup SELECT name || '!' FROM t WHERE id = 3;")
      table.data(0).data(0) shouldBe TextValue("ABC!")
    }

    "concatenates column with column" in {
      val table = query(
        """
          |CREATE TABLE t (fname TEXT, lname TEXT);
          |INSERT INTO t (fname, lname) VALUES ('John', 'Doe');
          |SELECT fname || ' ' || lname FROM t;
          |""".trim.stripMargin
      )
      table.data(0).data(0) shouldBe TextValue("John Doe")
    }
  }

  "Unary minus" - {
    "negates a positive number" in {
      val table = query(s"$setup SELECT -val FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe NumberValue(-42)
    }

    "negates a negative number" in {
      val table = query(s"$setup SELECT -val FROM t WHERE id = 2;")
      table.data(0).data(0) shouldBe NumberValue(7)
    }
  }

  "Arithmetic" - {
    "addition" in {
      val table = query(s"$setup SELECT val + 10 FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe NumberValue(52)
    }

    "subtraction" in {
      val table = query(s"$setup SELECT val - 2 FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe NumberValue(40)
    }

    "multiplication" in {
      val table = query(s"$setup SELECT val * 2 FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe NumberValue(84)
    }

    "division" in {
      val table = query(s"$setup SELECT val / 2 FROM t WHERE id = 1;")
      table.data(0).data(0) shouldBe NumberValue(21)
    }
  }

  "RETURNING clause" - {
    "returns inserted values" in {
      val res = results(
        """
          |CREATE TABLE t (id SERIAL, name TEXT);
          |INSERT INTO t (name) VALUES ('Alice') RETURNING id;
          |""".trim.stripMargin
      )

      // The last result should be the insert result
      res.length shouldBe 2
    }
  }
}

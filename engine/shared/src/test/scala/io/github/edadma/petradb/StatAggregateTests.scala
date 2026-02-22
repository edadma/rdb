package io.github.edadma.petradb

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class StatAggregateTests extends AnyFreeSpec with Matchers with Testing {

  val setup: String =
    """
      |CREATE TABLE data (v INT);
      |INSERT INTO data (v) VALUES (2), (4), (4), (4), (5), (5), (7), (9);
      |""".trim.stripMargin

  private def approx(v: Value, expected: Double, tol: Double = 1e-9): Unit =
    v match
      case NumberValue(_, n) => n.doubleValue shouldBe expected +- tol
      case other             => fail(s"expected NumberValue, got $other")

  "var_pop" - {
    "population variance" in {
      val table = query(s"$setup\nSELECT var_pop(v) FROM data;")
      // mean = 5, var_pop = sum((xi-5)^2)/8 = (9+1+1+1+0+0+4+16)/8 = 32/8 = 4
      approx(table.data(0).data(0), 4.0)
    }
  }

  "var_samp" - {
    "sample variance" in {
      val table = query(s"$setup\nSELECT var_samp(v) FROM data;")
      // var_samp = sum((xi-5)^2)/7 = 32/7
      approx(table.data(0).data(0), 32.0 / 7.0)
    }
  }

  "variance" - {
    "is alias for var_samp" in {
      val table = query(s"$setup\nSELECT variance(v) FROM data;")
      approx(table.data(0).data(0), 32.0 / 7.0)
    }
  }

  "stddev_pop" - {
    "population standard deviation" in {
      val table = query(s"$setup\nSELECT stddev_pop(v) FROM data;")
      approx(table.data(0).data(0), 2.0) // sqrt(4) = 2
    }
  }

  "stddev_samp" - {
    "sample standard deviation" in {
      val table = query(s"$setup\nSELECT stddev_samp(v) FROM data;")
      approx(table.data(0).data(0), math.sqrt(32.0 / 7.0))
    }
  }

  "stddev" - {
    "is alias for stddev_samp" in {
      val table = query(s"$setup\nSELECT stddev(v) FROM data;")
      approx(table.data(0).data(0), math.sqrt(32.0 / 7.0))
    }
  }

  "edge cases" - {
    "single row returns NULL for sample variance" in {
      val table = query(
        """
          |CREATE TABLE one (v INT);
          |INSERT INTO one (v) VALUES (42);
          |SELECT variance(v) FROM one;
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }

    "single row returns NULL for sample stddev" in {
      val table = query(
        """
          |CREATE TABLE one (v INT);
          |INSERT INTO one (v) VALUES (42);
          |SELECT stddev(v) FROM one;
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }

    "single row returns 0 for pop variance" in {
      val table = query(
        """
          |CREATE TABLE one (v INT);
          |INSERT INTO one (v) VALUES (42);
          |SELECT var_pop(v) FROM one;
          |""".trim.stripMargin
      )
      approx(table.data(0).data(0), 0.0)
    }

    "single row returns 0 for pop stddev" in {
      val table = query(
        """
          |CREATE TABLE one (v INT);
          |INSERT INTO one (v) VALUES (42);
          |SELECT stddev_pop(v) FROM one;
          |""".trim.stripMargin
      )
      approx(table.data(0).data(0), 0.0)
    }

    "NULLs are ignored" in {
      val table = query(
        """
          |CREATE TABLE nulldata (v INT);
          |INSERT INTO nulldata (v) VALUES (2), (NULL), (4), (NULL), (6);
          |SELECT var_pop(v) FROM nulldata;
          |""".trim.stripMargin
      )
      // values: 2,4,6 mean=4 var_pop = (4+0+4)/3 = 8/3
      approx(table.data(0).data(0), 8.0 / 3.0)
    }

    "empty table returns NULL" in {
      val table = query(
        """
          |CREATE TABLE empty (v INT);
          |SELECT var_pop(v) FROM empty;
          |""".trim.stripMargin
      )
      table.data(0).data(0).isNull shouldBe true
    }

    "with GROUP BY" in {
      val table = query(
        """
          |CREATE TABLE grouped (cat TEXT, v INT);
          |INSERT INTO grouped (cat, v) VALUES ('a', 1), ('a', 3), ('b', 10), ('b', 20);
          |SELECT cat, var_pop(v) FROM grouped GROUP BY cat ORDER BY cat;
          |""".trim.stripMargin
      )
      // a: mean=2, var_pop=(1+1)/2=1
      approx(table.data(0).data(1), 1.0)
      // b: mean=15, var_pop=(25+25)/2=25
      approx(table.data(1).data(1), 25.0)
    }
  }
}

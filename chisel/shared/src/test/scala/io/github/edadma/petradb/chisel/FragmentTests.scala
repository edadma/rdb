package io.github.edadma.petradb.chisel

import io.github.edadma.petradb.*
import io.github.edadma.dal

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class FragmentTests extends AnyFreeSpec with Matchers:

  "sql interpolator" - {
    "renders $n placeholders and collects params in order" in {
      val id   = 7
      val name = "alice"
      val f    = sql"select * from t where id = $id and name = $name"
      f.sql shouldBe "select * from t where id = $1 and name = $2"
      f.params shouldBe Seq(NumberValue(dal.IntType, 7), TextValue("alice"))
    }

    "handles a fragment with no parameters" in {
      val f = sql"select 1"
      f.sql shouldBe "select 1"
      f.params shouldBe empty
    }

    "binds an Option as a value or NULL" in {
      sql"x = ${Some(3)}".params shouldBe Seq(NumberValue(dal.IntType, 3))
      sql"x = ${Option.empty[Int]}".params shouldBe Seq(NullValue())
    }

    "passes a pre-built Value through unchanged" in {
      sql"x = ${DateValue(java.time.LocalDate.parse("2024-01-02"))}".params shouldBe
        Seq(DateValue(java.time.LocalDate.parse("2024-01-02")))
    }
  }

  "Fragment composition" - {
    "++ joins text and renumbers placeholders globally" in {
      val min = 18
      val lim = 10
      val f   = sql"select * from t where age > $min" ++ sql" limit $lim"
      f.sql shouldBe "select * from t where age > $1 limit $2"
      f.params shouldBe Seq(NumberValue(dal.IntType, 18), NumberValue(dal.IntType, 10))
    }

    "const contributes raw text with no params" in {
      val name = "bob"
      val f    = sql"select * from " ++ Fragment.const("users") ++ sql" where name = $name"
      f.sql shouldBe "select * from users where name = $1"
      f.params shouldBe Seq(TextValue("bob"))
    }

    "empty is the identity for ++" in {
      val f = sql"select 1"
      (Fragment.empty ++ f).sql shouldBe "select 1"
      (f ++ Fragment.empty).sql shouldBe "select 1"
    }
  }

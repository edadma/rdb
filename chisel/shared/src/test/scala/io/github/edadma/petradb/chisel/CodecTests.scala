package io.github.edadma.petradb.chisel

import io.github.edadma.petradb.*
import io.github.edadma.dal

import java.time.{Duration, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime}
import java.util.UUID

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CodecTests extends AnyFreeSpec with Matchers:

  /** Build a Row from name→value pairs, mirroring how a query result row is shaped. */
  private def rowOf(pairs: (String, Value)*): Row =
    val cols = pairs.map((n, v) => ColumnMetadata(None, n, v.vtyp)).toIndexedSeq
    Row(pairs.map(_._2).toIndexedSeq, Metadata(cols), None, None)

  "Get leaf instances" - {
    "decode each scalar type" in {
      Get[Int].get(NumberValue(42)) shouldBe 42
      Get[Long].get(NumberValue(42)) shouldBe 42L
      Get[Double].get(NumberValue(1.5)) shouldBe 1.5
      Get[BigDecimal].get(NumberValue(BigDecimal("12.34"))) shouldBe BigDecimal("12.34")
      Get[String].get(TextValue("hi")) shouldBe "hi"
      Get[Boolean].get(BooleanValue(true)) shouldBe true
      Get[LocalDate].get(DateValue(LocalDate.parse("2024-01-02"))) shouldBe LocalDate.parse("2024-01-02")
      Get[LocalTime].get(TimeValue(LocalTime.parse("10:30"))) shouldBe LocalTime.parse("10:30")
      Get[LocalDateTime].get(TimestampValue(LocalDateTime.parse("2024-01-02T10:30"))) shouldBe
        LocalDateTime.parse("2024-01-02T10:30")
      Get[OffsetDateTime].get(TimestampTZValue(OffsetDateTime.parse("2024-01-02T10:30+05:00"))) shouldBe
        OffsetDateTime.parse("2024-01-02T10:30+05:00")
      Get[OffsetTime].get(TimeTZValue(OffsetTime.parse("10:30+05:00"))) shouldBe OffsetTime.parse("10:30+05:00")
      Get[Duration].get(IntervalValue(Duration.ofHours(2))) shouldBe Duration.ofHours(2)
      Get[Array[Byte]].get(ByteaValue(Array[Byte](1, 2, 3))) shouldBe Array[Byte](1, 2, 3)
      val u = UUID.fromString("123e4567-e89b-12d3-a456-426614174000")
      Get[UUID].get(UUIDValue(u.toString)) shouldBe u
    }

    "a type mismatch raises DecodeException" in {
      a[DecodeException] should be thrownBy Get[Boolean].get(TextValue("nope"))
    }

    "map adapts a decoder" in {
      Get[Int].map(_ * 2).get(NumberValue(21)) shouldBe 42
    }
  }

  "Put leaf instances" - {
    "encode each scalar type" in {
      Put[Int].put(42) shouldBe NumberValue(dal.IntType, 42)
      Put[Long].put(42L) shouldBe NumberValue(dal.LongType, 42L)
      Put[String].put("hi") shouldBe TextValue("hi")
      Put[Boolean].put(true) shouldBe BooleanValue(true)
      Put[LocalDate].put(LocalDate.parse("2024-01-02")) shouldBe DateValue(LocalDate.parse("2024-01-02"))
      Put[Array[Byte]].put(Array[Byte](1, 2)) shouldBe ByteaValue(Array[Byte](1, 2))
      val u = UUID.fromString("123e4567-e89b-12d3-a456-426614174000")
      Put[UUID].put(u) shouldBe UUIDValue(u.toString)
    }

    "contramap adapts an encoder" in {
      Put[String].contramap[Int](_.toString).put(7) shouldBe TextValue("7")
    }
  }

  "Option codecs" - {
    "Get maps NULL to None and a value to Some" in {
      Get[Option[Int]].get(NullValue()) shouldBe None
      Get[Option[Int]].get(NumberValue(5)) shouldBe Some(5)
    }
    "Put maps None to NULL and Some to the value" in {
      Put[Option[Int]].put(None) shouldBe NullValue()
      Put[Option[Int]].put(Some(5)) shouldBe NumberValue(dal.IntType, 5)
    }
  }

  "Read derivation (by name)" - {
    case class User(id: Long, name: String, age: Int) derives Read, Write

    "reads a case class from a row" in {
      val u = Read[User].read(rowOf("id" -> NumberValue(1), "name" -> TextValue("alice"), "age" -> NumberValue(30)))
      u shouldBe User(1L, "alice", 30)
    }

    "is robust to column reordering (by name, not position)" in {
      val u = Read[User].read(rowOf("age" -> NumberValue(30), "name" -> TextValue("alice"), "id" -> NumberValue(1)))
      u shouldBe User(1L, "alice", 30)
    }

    "decodes a nullable field via Option" in {
      case class Maybe(id: Long, nick: Option[String]) derives Read
      Read[Maybe].read(rowOf("id" -> NumberValue(1), "nick" -> NullValue())) shouldBe Maybe(1L, None)
      Read[Maybe].read(rowOf("id" -> NumberValue(1), "nick" -> TextValue("al"))) shouldBe Maybe(1L, Some("al"))
    }
  }

  "Read.positional" - {
    "reads a plain tuple by column position, ignoring names" in {
      val r       = Read.positional[(Long, String)]
      val decoded = r.read(rowOf("anything" -> NumberValue(7), "whatever" -> TextValue("x")))
      decoded shouldBe (7L, "x")
    }
  }

  "Write derivation (by name)" - {
    case class User(id: Long, name: String, age: Int) derives Write

    "encodes a case class to named columns in declaration order" in {
      Write[User].writeNamed(User(1L, "alice", 30)) shouldBe Seq(
        "id"   -> NumberValue(dal.LongType, 1L),
        "name" -> TextValue("alice"),
        "age"  -> NumberValue(dal.IntType, 30),
      )
    }
  }

  "Named tuple derivation" - {
    "reads a named tuple by field label" in {
      val r       = Read.derived[(id: Long, name: String)]
      val decoded = r.read(rowOf("name" -> TextValue("bob"), "id" -> NumberValue(9)))
      decoded.id shouldBe 9L
      decoded.name shouldBe "bob"
    }
  }

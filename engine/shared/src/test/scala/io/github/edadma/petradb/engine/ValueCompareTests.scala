package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
class ValueCompareTests extends AnyFreeSpec with Matchers {

  "NullValue comparison" - {
    "null equals null" in {
      NullValue().compare(NullValue()) shouldBe 0
    }

    "null is less than a non-null value" in {
      (NullValue() < NumberValue(1)) shouldBe true
    }

    "non-null value is greater than null" in {
      (NumberValue(1) > NullValue()) shouldBe true
    }
  }

  "BooleanValue comparison" - {
    "false is less than true" in {
      (BooleanValue(false) < BooleanValue(true)) shouldBe true
    }

    "true is greater than false" in {
      (BooleanValue(true) > BooleanValue(false)) shouldBe true
    }

    "true equals true" in {
      BooleanValue(true).compare(BooleanValue(true)) shouldBe 0
    }

    "false equals false" in {
      BooleanValue(false).compare(BooleanValue(false)) shouldBe 0
    }
  }

  "ByteaValue comparison" - {
    "equal arrays compare to 0" in {
      ByteaValue(Array[Byte](1, 2, 3)).compare(ByteaValue(Array[Byte](1, 2, 3))) shouldBe 0
    }

    "lexicographically less" in {
      (ByteaValue(Array[Byte](1, 2, 3)) < ByteaValue(Array[Byte](1, 2, 4))) shouldBe true
    }

    "lexicographically greater" in {
      (ByteaValue(Array[Byte](1, 2, 4)) > ByteaValue(Array[Byte](1, 2, 3))) shouldBe true
    }

    "shorter prefix is less" in {
      (ByteaValue(Array[Byte](1, 2)) < ByteaValue(Array[Byte](1, 2, 3))) shouldBe true
    }

    "empty array is less than non-empty" in {
      (ByteaValue(Array.empty[Byte]) < ByteaValue(Array[Byte](1))) shouldBe true
    }
  }

  "Cross-type numeric comparison" - {
    "Int vs Long" in {
      NumberValue(42).compare(NumberValue(io.github.edadma.dal.LongType, 42L: java.lang.Long)) shouldBe 0
      (NumberValue(1) < NumberValue(io.github.edadma.dal.LongType, 2L: java.lang.Long)) shouldBe true
    }

    "Int vs Double" in {
      NumberValue(42).compare(NumberValue(42.0)) shouldBe 0
      (NumberValue(1) < NumberValue(1.5)) shouldBe true
    }

    "Int vs BigDecimal" in {
      NumberValue(42).compare(NumberValue(BigDecimal(42))) shouldBe 0
      (NumberValue(1) < NumberValue(BigDecimal(2))) shouldBe true
    }
  }

  "ValueSeqOrdering" - {
    "equal sequences" in {
      ValueSeqOrdering.compare(
        IndexedSeq(NumberValue(1), TextValue("a")),
        IndexedSeq(NumberValue(1), TextValue("a")),
      ) shouldBe 0
    }

    "differs on first element" in {
      ValueSeqOrdering.compare(
        IndexedSeq(NumberValue(1), TextValue("a")),
        IndexedSeq(NumberValue(2), TextValue("a")),
      ) should be < 0
    }

    "differs on second element" in {
      ValueSeqOrdering.compare(
        IndexedSeq(NumberValue(1), TextValue("b")),
        IndexedSeq(NumberValue(1), TextValue("a")),
      ) should be > 0
    }

    "shorter prefix is less than longer" in {
      ValueSeqOrdering.compare(
        IndexedSeq(TextValue("a")),
        IndexedSeq(TextValue("a"), NumberValue(1)),
      ) should be < 0
    }

    "NULL sorts before non-null" in {
      ValueSeqOrdering.compare(
        IndexedSeq(NullValue()),
        IndexedSeq(NumberValue(1)),
      ) should be < 0
    }

    "NULL equals NULL" in {
      ValueSeqOrdering.compare(
        IndexedSeq(NullValue()),
        IndexedSeq(NullValue()),
      ) shouldBe 0
    }

    "mixed types in sequence — null then value" in {
      ValueSeqOrdering.compare(
        IndexedSeq(NullValue(), TextValue("a")),
        IndexedSeq(NullValue(), TextValue("b")),
      ) should be < 0
    }
  }

  "non-orderable types should throw" - {
    "ArrayValue" in {
      an[RuntimeException] should be thrownBy {
        ArrayValue(IndexedSeq(NumberValue(1))).compare(ArrayValue(IndexedSeq(NumberValue(2))))
      }
    }

    "ObjectValue" in {
      an[RuntimeException] should be thrownBy {
        ObjectValue(Seq("a" -> NumberValue(1))).compare(ObjectValue(Seq("b" -> NumberValue(2))))
      }
    }
  }
}

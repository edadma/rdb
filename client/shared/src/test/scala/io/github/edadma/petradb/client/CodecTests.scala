package io.github.edadma.petradb.client

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import io.github.edadma.petradb.*
import io.github.edadma.petradb.Codecs.given
import io.github.edadma.dal.{IntType as DIntType, LongType as DLongType, DoubleType as DDoubleType, BigDecType}
import upickle.default.*

import java.time.{LocalDate, LocalDateTime, LocalTime}

class CodecTests extends AnyFreeSpec with Matchers:

  private def rt[A: ReadWriter](a: A): A = readBinary[A](writeBinary(a))

  // ── Result subtypes ──────────────────────────────────────────────────

  "Result round-trips" - {

    "CreateTableResult" in {
      rt[Seq[Result]](Seq(CreateTableResult("users"))) shouldBe Seq(CreateTableResult("users"))
    }

    "DropTableResult" in {
      rt[Seq[Result]](Seq(DropTableResult("users"))) shouldBe Seq(DropTableResult("users"))
    }

    "UpdateResult" in {
      rt[Seq[Result]](Seq(UpdateResult(3))) shouldBe Seq(UpdateResult(3))
    }

    "DeleteResult" in {
      rt[Seq[Result]](Seq(DeleteResult(7))) shouldBe Seq(DeleteResult(7))
    }

    "TruncateResult" in {
      rt[Seq[Result]](Seq(TruncateResult("t"))) shouldBe Seq(TruncateResult("t"))
    }

    "CreateIndexResult" in {
      rt[Seq[Result]](Seq(CreateIndexResult("idx_name"))) shouldBe Seq(CreateIndexResult("idx_name"))
    }

    "DropIndexResult" in {
      rt[Seq[Result]](Seq(DropIndexResult("idx_name"))) shouldBe Seq(DropIndexResult("idx_name"))
    }

    "CreateTypeResult" in {
      rt[Seq[Result]](Seq(CreateTypeResult("mood"))) shouldBe Seq(CreateTypeResult("mood"))
    }

    "DropTypeResult" in {
      rt[Seq[Result]](Seq(DropTypeResult("mood"))) shouldBe Seq(DropTypeResult("mood"))
    }

    "AlterTableResult" in {
      rt[Seq[Result]](Seq(AlterTableResult())) shouldBe Seq(AlterTableResult())
    }

    "PrepareResult" in {
      rt[Seq[Result]](Seq(PrepareResult("my_stmt"))) shouldBe Seq(PrepareResult("my_stmt"))
    }

    "DeallocateResult" in {
      rt[Seq[Result]](Seq(DeallocateResult("my_stmt"))) shouldBe Seq(DeallocateResult("my_stmt"))
    }

    "BeginResult" in {
      rt[Seq[Result]](Seq(BeginResult)) shouldBe Seq(BeginResult)
    }

    "CommitResult" in {
      rt[Seq[Result]](Seq(CommitResult)) shouldBe Seq(CommitResult)
    }

    "RollbackResult" in {
      rt[Seq[Result]](Seq(RollbackResult)) shouldBe Seq(RollbackResult)
    }

    "multiple results in one payload" in {
      val results: Seq[Result] = Seq(
        CreateTableResult("t"),
        UpdateResult(5),
        DeleteResult(2),
        BeginResult,
        CommitResult,
        RollbackResult,
      )
      rt[Seq[Result]](results) shouldBe results
    }

    "QueryResult with int and text columns" in {
      val meta  = Metadata(IndexedSeq(
        ColumnMetadata(Some("t"), "id",   IntegerType),
        ColumnMetadata(Some("t"), "name", TextType),
      ))
      val row   = Row(IndexedSeq(NumberValue(DIntType, 1), TextValue("Alice")), meta, None, None)
      val table = TableValue(IndexedSeq(row), meta)
      val decoded = rt[Seq[Result]](Seq(QueryResult(table)))
      val QueryResult(t) = decoded.head
      t.data.length shouldBe 1
      t.data(0)("id")   shouldBe NumberValue(DIntType, 1)
      t.data(0)("name") shouldBe TextValue("Alice")
    }

    "QueryResult with null value" in {
      val meta  = Metadata(IndexedSeq(ColumnMetadata(Some("t"), "x", TextType)))
      val row   = Row(IndexedSeq(NullValue()), meta, None, None)
      val table = TableValue(IndexedSeq(row), meta)
      val QueryResult(t) = rt[Seq[Result]](Seq(QueryResult(table))).head
      t.data(0)("x") shouldBe NullValue()
    }

    "QueryResult with multiple rows" in {
      val meta  = Metadata(IndexedSeq(ColumnMetadata(Some("t"), "n", IntegerType)))
      val rows  = (1 to 5).map(i => Row(IndexedSeq(NumberValue(DIntType, i)), meta, None, None))
      val table = TableValue(rows.toIndexedSeq, meta)
      val QueryResult(t) = rt[Seq[Result]](Seq(QueryResult(table))).head
      t.data.length shouldBe 5
      t.data(4)("n") shouldBe NumberValue(DIntType, 5)
    }

    "InsertResult" in {
      val meta  = Metadata(IndexedSeq(ColumnMetadata(Some("t"), "id", SerialType)))
      val row   = Row(IndexedSeq(NumberValue(DIntType, 1)), meta, None, None)
      val table = TableValue(IndexedSeq(row), meta)
      val obj   = Map("id" -> NumberValue(DIntType, 1))
      val InsertResult(decodedObj, decodedTable) = rt[Seq[Result]](Seq(InsertResult(obj, table))).head
      decodedObj("id") shouldBe NumberValue(DIntType, 1)
      decodedTable.data.length shouldBe 1
    }
  }

  // ── Value types ──────────────────────────────────────────────────────

  "Value round-trips" - {

    "NullValue" in {
      rt[Value](NullValue()) shouldBe NullValue()
    }

    "BooleanValue true" in {
      rt[Value](BooleanValue(true)) shouldBe BooleanValue(true)
    }

    "BooleanValue false" in {
      rt[Value](BooleanValue(false)) shouldBe BooleanValue(false)
    }

    "NumberValue int" in {
      rt[Value](NumberValue(DIntType, 42)) shouldBe NumberValue(DIntType, 42)
    }

    "NumberValue long" in {
      rt[Value](NumberValue(DLongType, 9999999999L)) shouldBe NumberValue(DLongType, 9999999999L)
    }

    "NumberValue double" in {
      rt[Value](NumberValue(DDoubleType, 3.14)) shouldBe NumberValue(DDoubleType, 3.14)
    }

    "NumberValue BigDecimal" in {
      rt[Value](NumberValue(BigDecType, BigDecimal("123.456"))) shouldBe NumberValue(BigDecType, BigDecimal("123.456"))
    }

    "TextValue" in {
      rt[Value](TextValue("hello world")) shouldBe TextValue("hello world")
    }

    "TextValue empty string" in {
      rt[Value](TextValue("")) shouldBe TextValue("")
    }

    "TextValue with special characters" in {
      rt[Value](TextValue("héllo\nnewline")) shouldBe TextValue("héllo\nnewline")
    }

    "UUIDValue" in {
      val uuid = "550e8400-e29b-41d4-a716-446655440000"
      rt[Value](UUIDValue(uuid)) shouldBe UUIDValue(uuid)
    }

    "DateValue" in {
      val d = LocalDate.of(2024, 6, 15)
      rt[Value](DateValue(d)) shouldBe DateValue(d)
    }

    "TimeValue" in {
      val t = LocalTime.of(14, 30, 45)
      rt[Value](TimeValue(t)) shouldBe TimeValue(t)
    }

    "TimestampValue" in {
      val ts = LocalDateTime.of(2024, 6, 15, 14, 30, 45)
      rt[Value](TimestampValue(ts)) shouldBe TimestampValue(ts)
    }

    "ByteaValue" in {
      val data    = Array[Byte](0x00, 0x01, 0x7f, 0xff.toByte, 0xfe.toByte)
      val decoded = rt[Value](ByteaValue(data)).asInstanceOf[ByteaValue]
      decoded.data shouldBe data
    }

    "ArrayValue of ints" in {
      val v = ArrayValue(IndexedSeq(NumberValue(DIntType, 1), NumberValue(DIntType, 2), NumberValue(DIntType, 3)))
      rt[Value](v) shouldBe v
    }

    "ArrayValue mixed types" in {
      val v = ArrayValue(IndexedSeq(NumberValue(DIntType, 1), TextValue("a"), NullValue()))
      rt[Value](v) shouldBe v
    }

    "ArrayValue empty" in {
      rt[Value](ArrayValue(IndexedSeq.empty)) shouldBe ArrayValue(IndexedSeq.empty)
    }

    "nested ArrayValue" in {
      val inner = ArrayValue(IndexedSeq(NumberValue(DIntType, 1), NumberValue(DIntType, 2)))
      val outer = ArrayValue(IndexedSeq(inner, TextValue("x")))
      rt[Value](outer) shouldBe outer
    }

    "EnumValue" in {
      val typ     = EnumType("mood", IndexedSeq("happy", "sad", "neutral"))
      val v       = EnumValue(1, typ)
      val decoded = rt[Value](v).asInstanceOf[EnumValue]
      decoded.string shouldBe "sad"
    }

    "ObjectValue" in {
      val v       = ObjectValue(Seq("name" -> TextValue("Alice"), "age" -> NumberValue(DIntType, 30)))
      val decoded = rt[Value](v).asInstanceOf[ObjectValue]
      decoded.get("name") shouldBe Some(TextValue("Alice"))
      decoded.get("age")  shouldBe Some(NumberValue(DIntType, 30))
    }
  }

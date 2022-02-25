package io.github.edadma.rdb

import io.github.edadma.dal.{TypedNumber, DoubleType as DDoubleType, IntType as DIntType, Type as DType}

import scala.util.parsing.input.{Position, Positional}

trait Value(val vtyp: Type) extends Positional with Ordered[Value]:
  def asText: StringValue = problem(pos, "cannot be converted to text")

case class NumberValue(typ: DType, value: Number) extends Value(NumberType) with TypedNumber:
  override def asText: StringValue = StringValue(value.toString)

object NumberValue:
  def apply(n: Int): NumberValue = NumberValue(DIntType, n)

  def apply(n: Double): NumberValue = NumberValue(DDoubleType, n)

  def from(n: (DType, Number)): NumberValue = NumberValue(n._1, n._2)

case object NullValue extends Value(NullType)

case object StarValue extends Value(StarType)

case class StringValue(s: String) extends Value(StringType):
  override def asText: StringValue = this

case class BooleanValue(b: Boolean) extends Value(BooleanType)

trait ArrayLikeValue extends Value:
  infix def contains(v: Value): Boolean
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty

case class TableValue(data: IndexedSeq[Row], meta: Metadata) extends Value(TableType) with ArrayLikeValue:
  infix def contains(v: Value): Boolean =
    require(meta.width == 1, s"contains: expected one column: $meta")
    data.exists(_.data.head == v)
  def isEmpty: Boolean = data.isEmpty

case class ArrayValue(data: IndexedSeq[Value]) extends Value(ArrayType) with ArrayLikeValue:
  infix def contains(v: Value): Boolean = data.contains(v)
  def isEmpty: Boolean = data.isEmpty

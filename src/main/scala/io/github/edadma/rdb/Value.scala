package io.github.edadma.rdb

import io.github.edadma.dal.{IntType => DIntType, TypedNumber, Type => DType}

trait Value(val vtyp: Type):
  var pos: Option[Pos] = None

  def pos(p: Option[Pos]): Value =
    pos = p
    this

case class NumberValue(typ: DType, value: Number) extends Value(NumberType) with TypedNumber

object NumberValue {
  def apply(n: Int): NumberValue = NumberValue(DIntType, n)

  def from(n: (DType, Number)): NumberValue = NumberValue(n._1, n._2)
}

case object NullValue extends Value(NullType)

case class StringValue(s: String) extends Value(StringType)

case class BooleanValue(b: Boolean) extends Value(BooleanType)

trait ArrayLikeValue extends Value:
  infix def contains(v: Value): Boolean
  def isEmpty: Boolean
  def nonEmpty: Boolean = !isEmpty

case class TableValue(data: Seq[Row], meta: Metadata) extends Value(TableType) with ArrayLikeValue:
  infix def contains(v: Value): Boolean =
    require(meta.cols == 1, s"contains: expected one column: $meta")
    data.exists(_.data.head == v)
  def isEmpty: Boolean = data.isEmpty

case class ArrayValue(data: IndexedSeq[Value]) extends Value(TableType) with ArrayLikeValue:
  infix def contains(v: Value): Boolean = data.contains(v)
  def isEmpty: Boolean = data.isEmpty
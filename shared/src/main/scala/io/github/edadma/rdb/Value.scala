package io.github.edadma.rdb

import io.github.edadma.dal.{BasicDAL, TypedNumber, DoubleType as DDoubleType, IntType as DIntType, Type as DType}
import io.github.edadma.datetime.Datetime

import scala.util.parsing.input.{Position, Positional}

trait Value(val vtyp: Type) extends Positional with Ordered[Value]:
  def toText: TextValue = problem(pos, "cannot be converted to text")

  def render: String

  infix def compare(that: Value): Int = problem(pos, s"'$this' can't be compared to '$that''")

  def isNull: Boolean = isInstanceOf[NullValue]

case class NumberValue(typ: DType, value: Number) extends Value(NumberType) with TypedNumber:
  override def toText: TextValue = TextValue(value.toString)

  def render: String = value.toString

  override def compare(that: Value): Int =
    that match
      case n: NumberValue => BasicDAL.compare[TypedNumber](this, n)
      case _              => super.compare(that)

object NumberValue:
  def apply(n: Int): NumberValue = NumberValue(DIntType, n)

  def apply(n: Double): NumberValue = NumberValue(DDoubleType, n)

  def from(n: (DType, Number)): NumberValue = NumberValue(n._1, n._2)

case class NullValue() extends Value(NullType):
  override def toText: TextValue = TextValue("NULL")

  def render: String = "null"

case class StarValue() extends Value(StarType):
  def render: String = "*"

case class TimestampValue(t: Datetime) extends Value(TimestampType):
  t.timestamp

  override def toText: TextValue = TextValue(t.toString)

  override def compare(that: Value): Int =
    that match
      case TimestampValue(u) => t compare u
      case _                 => super.compare(that)

  def render: String = s"'$t'"

case class TextValue(s: String) extends Value(TextType):
  override def toText: TextValue = this

  def render: String = s"\"$s\""

  override def compare(that: Value): Int =
    that match
      case TextValue(t) => s compare t
      case _            => super.compare(that)

case class BooleanValue(b: Boolean) extends Value(BooleanType):
  override def toText: TextValue = TextValue(if b then "TURE" else "FALSE")

  def render: String = if b then "true" else "false"

trait ArrayLikeValue extends Value:
  infix def contains(v: Value): Boolean

  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

case class TableValue(data: IndexedSeq[Row], meta: Metadata) extends Value(TableType) with ArrayLikeValue:
  infix def contains(v: Value): Boolean =
    require(meta.width == 1, s"contains: expected one column: $meta")
    data.exists(_.data.head == v)

  def isEmpty: Boolean = data.isEmpty

  def render: String = data.mkString("[", ", ", "]")

case class ArrayValue(data: IndexedSeq[Value]) extends Value(ArrayType) with ArrayLikeValue:
  override def toText: TextValue = TextValue(render)

  def render: String = data.map(_.render).mkString("[", ", ", "]")

  infix def contains(v: Value): Boolean = data.contains(v)
  def isEmpty: Boolean = data.isEmpty

case class ObjectValue(properties: Seq[(String, Value)]) extends Value(ObjectType):
  override def toText: TextValue = TextValue(render)

  def render: String = properties.map({ case (k, v) => s"\"$k\": ${v.render}" }).mkString("{", ", ", "}")
package io.github.edadma.rdb

import io.github.edadma.dal.{BasicDAL, TypedNumber, DoubleType as DDoubleType, IntType as DIntType, Type as DType}
import io.github.edadma.datetime.Datetime

import scala.util.parsing.input.{Position, Positional}

trait Value(val vtyp: Type) extends Positional with Ordered[Value]:
  def toText: TextValue = problem(pos, "cannot be converted to text")

  def render: String = string

  def string: String

  infix def compare(that: Value): Int = problem(pos, s"'$this' can't be compared to '$that''")

  def isNull: Boolean = isInstanceOf[NullValue]

case class NumberValue(typ: DType, value: Number) extends Value(NumberType) with TypedNumber:
  override def toText: TextValue = TextValue(value.toString)

  def string: String = value.toString

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

  def string: String = "null"

case class StarValue() extends Value(StarType):
  def string: String = "*"

case class TimestampValue(t: Datetime) extends Value(TimestampType):
  t.timestamp

  override def toText: TextValue = TextValue(t.toString)

  override def compare(that: Value): Int =
    that match
      case TimestampValue(u) => t compare u
      case _                 => super.compare(that)

  override def render: String = s"'$t'"

  def string: String = t.toString

case class UUIDValue(id: String) extends Value(UUIDType):
  override def toText: TextValue = TextValue(id)

  override def render: String = s"'$id'"

  def string: String = id

case class TextValue(s: String) extends Value(TextType):
  override def toText: TextValue = this

  override def render: String = s"\"$s\""

  def string: String = s

  override def compare(that: Value): Int =
    that match
      case TextValue(t) => s compare t
      case _            => super.compare(that)

case class BooleanValue(b: Boolean) extends Value(BooleanType):
  override def toText: TextValue = TextValue(if b then "TURE" else "FALSE")

  def string: String = if b then "true" else "false"

trait ArrayLikeValue extends Value:
  infix def contains(v: Value): Boolean

  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

  def length: Int

case class TableValue(data: IndexedSeq[Row], meta: Metadata) extends Value(TableType) with ArrayLikeValue:
  infix def contains(v: Value): Boolean =
    require(meta.width == 1, s"contains: expected one column: $meta")
    data.exists(_.data.head == v)

  def isEmpty: Boolean = data.isEmpty

  def length: Int = data.length

  def string: String = data.mkString("[", ", ", "]")

case class ArrayValue(data: IndexedSeq[Value]) extends Value(ArrayType) with ArrayLikeValue:
  override def toText: TextValue = TextValue(render)

  def string: String = data.map(_.render).mkString("[", ", ", "]")

  infix def contains(v: Value): Boolean = data.contains(v)

  def isEmpty: Boolean = data.isEmpty

  def length: Int = data.length

case class ObjectValue(properties: Seq[(String, Value)]) extends Value(ObjectType):
  override def toText: TextValue = TextValue(render)

  def string: String = properties.map({ case (k, v) => s"\"$k\": ${v.render}" }).mkString("{", ", ", "}")

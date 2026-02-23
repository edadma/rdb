package io.github.edadma.petradb

import io.github.edadma.dal.{BasicDAL, BigDecType, TypedNumber}
import io.github.edadma.dal
import java.time.{Duration, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime, ZoneOffset}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.parsing.input.{Position, Positional}

trait Value(val vtyp: Type) extends Positional with Ordered[Value]:
  def toText: TextValue = throw TypeException(pos, "cannot be converted to text")

  def render: String = string

  def string: String

  infix def compare(that: Value): Int =
    if this.isNull && that.isNull then 0
    else if this.isNull then -1
    else if that.isNull then 1
    else if vtyp != that.vtyp then throw TypeException(pos, s"'$this' can't be compared to '$that''")
    else if this == that then 0
    else throw TypeException(pos, s"'$this' can't be compared to '$that''")

  def isNull: Boolean = isInstanceOf[NullValue]

  def next: Value = throw ExecutionException(pos, "can't generate next value")

case class NumberValue(typ: dal.Type, value: Number) extends Value(NumberType) with TypedNumber:
  override def toText: TextValue = TextValue(value.toString)

  def string: String = value.toString

  override def compare(that: Value): Int =
    that match
      case n: NumberValue => BasicDAL.compare[TypedNumber](this, n)
      case _              => super.compare(that)

  override def next: Value = BasicDAL.compute(PLUS, this, ONE, NumberValue.from)

object NumberValue:
  def apply(n: Int): NumberValue = NumberValue(dal.IntType, n)

  def apply(n: Double): NumberValue = NumberValue(dal.DoubleType, n)

  def apply(n: BigDecimal): NumberValue = NumberValue(BigDecType, n)

  def from(n: (dal.Type, Number)): NumberValue = NumberValue(n._1, n._2)

case class NullValue() extends Value(NullType):
  override def toText: TextValue = TextValue("NULL")

  def string: String = "null"

case class StarValue() extends Value(StarType):
  def string: String = "*"

case class TimestampValue(t: LocalDateTime) extends Value(TimestampType):
  override def toText: TextValue = TextValue(t.toString)

  override def compare(that: Value): Int =
    that match
      case TimestampValue(u) => t.compareTo(u)
      case _                 => super.compare(that)

  override def render: String = s"'$t'"

  def string: String = t.toString

case class DateValue(d: LocalDate) extends Value(DateType):
  override def toText: TextValue = TextValue(d.toString)

  override def compare(that: Value): Int =
    that match
      case DateValue(u) => d.compareTo(u)
      case _            => super.compare(that)

  override def render: String = s"'$d'"

  def string: String = d.toString

case class TimeValue(t: LocalTime) extends Value(TimeType):
  override def toText: TextValue = TextValue(t.toString)

  override def compare(that: Value): Int =
    that match
      case TimeValue(u) => t.compareTo(u)
      case _            => super.compare(that)

  override def render: String = s"'$t'"

  def string: String = t.toString

case class TimeTZValue(t: OffsetTime) extends Value(TimeTZType):
  override def toText: TextValue = TextValue(t.toString)

  override def compare(that: Value): Int =
    that match
      case TimeTZValue(u) => t.compareTo(u)
      case _              => super.compare(that)

  override def render: String = s"'$t'"

  def string: String = t.toString

case class IntervalValue(d: Duration) extends Value(IntervalType):
  override def toText: TextValue = TextValue(string)

  override def compare(that: Value): Int =
    that match
      case IntervalValue(u) => d.compareTo(u)
      case _                => super.compare(that)

  override def render: String = s"'$string'"

  def string: String =
    val totalSeconds = d.getSeconds
    val days         = totalSeconds / 86400
    val hours        = (totalSeconds % 86400) / 3600
    val minutes      = (totalSeconds % 3600) / 60
    val seconds      = totalSeconds % 60
    val parts = Seq(
      if days != 0 then Some(s"$days day${if days.abs != 1 then "s" else ""}") else None,
      if hours != 0 then Some(s"$hours hour${if hours.abs != 1 then "s" else ""}") else None,
      if minutes != 0 then Some(s"$minutes minute${if minutes.abs != 1 then "s" else ""}") else None,
      if seconds != 0 then Some(s"$seconds second${if seconds.abs != 1 then "s" else ""}") else None,
    ).flatten
    if parts.isEmpty then "0 seconds" else parts.mkString(" ")

case class TimestampTZValue(t: OffsetDateTime) extends Value(TimestampTZType):
  override def toText: TextValue = TextValue(t.toString)

  override def compare(that: Value): Int =
    that match
      case TimestampTZValue(u) => t.compareTo(u)
      case _                   => super.compare(that)

  override def render: String = s"'$t'"

  def string: String = t.toString

case class ByteaValue(data: Array[Byte]) extends Value(ByteaType):
  override def toText: TextValue = TextValue(string)

  override def render: String = s"'\\x${data.map(b => f"${b & 0xff}%02x").mkString}'"

  def string: String = s"\\x${data.map(b => f"${b & 0xff}%02x").mkString}"

  override def compare(that: Value): Int =
    that match
      case ByteaValue(other) =>
        val len = math.min(data.length, other.length)
        var i = 0
        while i < len do
          val cmp = (data(i) & 0xff) - (other(i) & 0xff)
          if cmp != 0 then return cmp
          i += 1
        data.length - other.length
      case _ => super.compare(that)

  override def equals(other: Any): Boolean =
    other match
      case ByteaValue(otherData) => java.util.Arrays.equals(data, otherData)
      case _                     => false

  override def hashCode(): Int = java.util.Arrays.hashCode(data)

object UUIDValue:
  val generated = new mutable.HashSet[String]

  @tailrec
  def generate: UUIDValue =
    val uuid = Platform.randomUUID

    if generated(uuid) then generate
    else
      generated += uuid
      UUIDValue(uuid)

case class UUIDValue(id: String) extends Value(UUIDType):
  override def toText: TextValue = TextValue(id)

  override def render: String = s"'$id'"

  def string: String = id

  override def next: Value = UUIDValue.generate

  override def compare(that: Value): Int =
    that match
      case UUIDValue(otherId) => id compare otherId
      case TextValue(text)    => id compare text
      case _                  => super.compare(that)

case class TextValue(s: String) extends Value(TextType):
  override def toText: TextValue = this

  override def render: String = s"\"$s\""

  def string: String = s

  override def compare(that: Value): Int =
    that match
      case TextValue(t)    => s compare t
      case EnumValue(v, t) =>
        t.labelsMap get s match
          case None    => throw TypeException(pos, s"'$s' is not a label of enum '${t.name}'")
          case Some(l) => l compare v
      case _ => super.compare(that)

case class BooleanValue(b: Boolean) extends Value(BooleanType):
  override def toText: TextValue = TextValue(if b then "TRUE" else "FALSE")

  def string: String = if b then "true" else "false"

  override def compare(that: Value): Int =
    that match
      case BooleanValue(o) => b compare o
      case _               => super.compare(that)

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

  def get(key: String): Option[Value] = properties.collectFirst { case (k, v) if k == key => v }
  def keys: Seq[String] = properties.map(_._1)

  def string: String = properties.map({ case (k, v) => s"\"$k\": ${v.render}" }).mkString("{", ", ", "}")

def jsonContains(left: Value, right: Value): Boolean =
  (left, right) match
    case (ObjectValue(lp), ObjectValue(rp)) =>
      rp.forall { case (k, rv) => lp.exists { case (lk, lv) => lk == k && jsonContains(lv, rv) } }
    case (ArrayValue(ld), ArrayValue(rd)) =>
      rd.forall(rv => ld.exists(lv => jsonContains(lv, rv)))
    case (l, r) => l == r

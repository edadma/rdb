package io.github.edadma.rdb

import io.github.edadma.datetime.Datetime
import io.github.edadma.dal.{BigDecType, DoubleType as DDoubleType, IntType as DIntType, LongType as DLongType}

import java.math.MathContext
import scala.util.matching.Regex

trait Type(val name: String):
  def convert(v: Value): Value =
    if v.vtyp != this then problem(v, s"can't auto-convert '$v' to type '$name'")
    else v

  def init: Value = sys.error(s"type '$name' doesn't support auto")

case object NumberType extends Type("number")

case object IntegerType extends Type("integer"):
  override def convert(v: Value): Value =
    v match
      case n @ NumberValue(DIntType, _) => n
      case _                            => super.convert(v)

  override def init: Value = ONE

case object BigintType extends Type("bigint"):
  override def convert(v: Value): Value =
    v match
      case n @ NumberValue(DLongType | DIntType, _) => n
      case _                                        => super.convert(v)

  override def init: Value = ONE

case object DoubleType extends Type("double"):
  override def convert(v: Value): Value =
    v match
      case n @ NumberValue(DDoubleType | DIntType, _) => n
      case _                                          => super.convert(v)

case class NumericType(precision: Int, scale: Int) extends Type("numeric"):
  private val mc = MathContext(precision)
  private val scaler = BigInt(10).pow(scale).toLong

  override def convert(v: Value): Value =
    v match
      case n @ NumberValue(BigDecType, _)       => n
      case NumberValue(DIntType | DLongType, n) => NumberValue(BigDecimal(n.longValue * scaler, scale, mc))
      case NumberValue(DDoubleType, n) =>
        NumberValue(BigDecimal(n.doubleValue, mc).setScale(scale, BigDecimal.RoundingMode.DOWN))
      case _ => super.convert(v)

case object UUIDType extends Type("uuid"):
  private val UUIDv4: Regex = "(?i)^[0-9A-F]{8}-[0-9A-F]{4}-[4][0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}$".r

  def valid(id: String): Boolean = UUIDv4 matches id

  override def convert(v: Value): Value =
    v match
      case id: UUIDValue => id
      case t @ TextValue(id) =>
        if !valid(id) then problem(t, "invalid version 4 UUID")

        UUIDValue(id)
      case _ => super.convert(v)

  override def init: Value = UUIDValue.generate

case object TextType extends Type("text")

case object TimestampType extends Type("timestamp"):
  override def convert(v: Value): Value =
    v match
      case t: TimestampValue => t
      case TextValue(t)      => TimestampValue(Datetime.fromString(t))
      case _                 => super.convert(v)

case object JSONType extends Type("JSON"):
  override def convert(v: Value): Value =
    v match
      case _: (ArrayValue | ObjectValue) => v
      case TextValue(json)               => JSONParser.parseJSON(json)
      case _                             => super.convert(v)

case object ObjectType extends Type("object")

case object NullType extends Type("null")

case object StarType extends Type("star")

case object BooleanType extends Type("boolean")

case object TableType extends Type("table")

case object ArrayType extends Type("array")

case object UnknownType extends Type("unknown")

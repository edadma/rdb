package io.github.edadma.rdb

import java.time.{LocalDate, LocalDateTime}
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import io.github.edadma.dal.{BigDecType, DoubleType as DDoubleType, IntType as DIntType, LongType as DLongType}

import java.math.MathContext
import scala.util.matching.Regex

trait Type(val name: String):
  def convert(v: Value): Value =
    if v.isNull then v
    else if v.vtyp != this then problem(v, s"can't auto-convert '$v' to type '$name'")
    else v

  def isNumber: Boolean = false

  def init: Value = sys.error(s"type '$name' doesn't support auto")

case object NumberType extends Type("number"):
  override val isNumber = true

case object IntegerType extends Type("integer"):
  override val isNumber = true

  override def convert(v: Value): Value =
    v match
      case n @ NumberValue(DIntType, _) => n
      case NumberValue(_, n)            => NumberValue(n.intValue)
      case TextValue(s) =>
        try NumberValue(s.trim.toInt)
        catch case _: NumberFormatException => problem(v, s"cannot cast '$s' to integer")
      case _ => super.convert(v)

  override def init: Value = ONE

case object BigintType extends Type("bigint"):
  override val isNumber = true

  override def convert(v: Value): Value =
    v match
      case n @ NumberValue(DLongType | DIntType, _) => n
      case NumberValue(_, n)                        => NumberValue(DLongType, n.longValue)
      case TextValue(s) =>
        try NumberValue(DLongType, s.trim.toLong)
        catch case _: NumberFormatException => problem(v, s"cannot cast '$s' to bigint")
      case _ => super.convert(v)

  override def init: Value = ONE

case object SerialType extends Type("serial"):
  override val isNumber = true

  override def convert(v: Value): Value =
    v match
      case n @ NumberValue(DIntType, _) => n
      case _                            => super.convert(v)

  override def init: Value = ONE

case object BigSerialType extends Type("bigserial"):
  override val isNumber = true

  override def convert(v: Value): Value =
    v match
      case n @ NumberValue(DLongType | DIntType, _) => n
      case _                                        => super.convert(v)

  override def init: Value = ONE

case object DoubleType extends Type("double"):
  override val isNumber = true

  override def convert(v: Value): Value =
    v match
      case n @ NumberValue(DDoubleType | DIntType, _) => n
      case NumberValue(_, n)                          => NumberValue(n.doubleValue)
      case TextValue(s) =>
        try NumberValue(s.trim.toDouble)
        catch case _: NumberFormatException => problem(v, s"cannot cast '$s' to double")
      case _ => super.convert(v)

case class NumericType(precision: Int, scale: Int) extends Type("numeric"):
  override val isNumber = true

  private val mc     = MathContext(precision)
  private val scaler = BigInt(10).pow(scale).toLong

  override def convert(v: Value): Value =
    v match
      case n @ NumberValue(BigDecType, _)       => n
      case NumberValue(DIntType | DLongType, n) => NumberValue(BigDecimal(n.longValue * scaler, scale, mc))
      case NumberValue(DDoubleType, n)          =>
        NumberValue(BigDecimal(n.doubleValue, mc).setScale(scale, BigDecimal.RoundingMode.DOWN))
      case _ => super.convert(v)

case object UUIDType extends Type("uuid"):
  private val UUIDv4: Regex = "(?i)^[0-9A-F]{8}-[0-9A-F]{4}-4[0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}$".r

  def valid(id: String): Boolean = UUIDv4 matches id

  override def convert(v: Value): Value =
    v match
      case id: UUIDValue => id
      case _             =>
        val textVal = v.toText
        if !valid(textVal.s) then problem(v, "invalid version 4 UUID")
        UUIDValue(textVal.s)

  override def init: Value = UUIDValue.generate

case object TextType extends Type("text"):
  override def convert(v: Value): Value =
    v match
      case t: TextValue => t
      case _            => v.toText

case object TimestampType extends Type("timestamp"):
  override def convert(v: Value): Value =
    v match
      case t: TimestampValue => t
      case _                 =>
        val textVal = v.toText

        TimestampValue(parseTimestamp(textVal.s))

case object JSONType extends Type("JSON"):
  override def convert(v: Value): Value =
    v match
      case _: (ArrayValue | ObjectValue) => v
      case _                             =>
        val textVal = v.toText
        JSONParser.parseJSON(textVal.s)

case object ObjectType extends Type("object")

case object NullType extends Type("null")

case object StarType extends Type("star")

case object BooleanType extends Type("boolean")

case object TableType extends Type("table")

case object ArrayType extends Type("array")

//case object UnknownType extends Type("unknown")

private val spaceTimestampFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

private def parseTimestamp(s: String): LocalDateTime =
  try LocalDateTime.parse(s)
  catch
    case _: DateTimeParseException =>
      try LocalDateTime.parse(s, spaceTimestampFormat)
      catch
        case _: DateTimeParseException =>
          LocalDate.parse(s).atStartOfDay

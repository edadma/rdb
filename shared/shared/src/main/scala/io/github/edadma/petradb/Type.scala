package io.github.edadma.petradb

import java.time.{Duration, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime, ZoneOffset}
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import io.github.edadma.dal.{BigDecType, DoubleType as DDoubleType, IntType as DIntType, LongType as DLongType}

import java.math.MathContext
import scala.util.matching.Regex

trait Type(val name: String):
  def convert(v: Value): Value =
    if v.isNull then v
    else if v.vtyp != this then throw TypeException(v.pos, s"can't auto-convert '$v' to type '$name'")
    else v

  def isNumber: Boolean = false

  def init: Value = sys.error(s"type '$name' doesn't support auto")

case object NumberType extends Type("number"):
  override val isNumber = true

case object SmallintType extends Type("smallint"):
  override val isNumber = true

  override def convert(v: Value): Value =
    v match
      case n @ NumberValue(DIntType, v) =>
        val i = v.intValue
        if i < -32768 || i > 32767 then throw TypeException(n.pos, s"value $i out of smallint range (-32768..32767)")
        n
      case NumberValue(_, n) =>
        val i = n.intValue
        if i < -32768 || i > 32767 then throw TypeException(v.pos, s"value $i out of smallint range (-32768..32767)")
        NumberValue(i)
      case TextValue(s) =>
        try
          val i = s.trim.toInt
          if i < -32768 || i > 32767 then throw TypeException(v.pos, s"value $i out of smallint range (-32768..32767)")
          NumberValue(i)
        catch case _: NumberFormatException => throw TypeException(v.pos, s"cannot cast '$s' to smallint")
      case _ => super.convert(v)

  override def init: Value = ONE

case object IntegerType extends Type("integer"):
  override val isNumber = true

  override def convert(v: Value): Value =
    v match
      case n @ NumberValue(DIntType, _) => n
      case NumberValue(_, n)            => NumberValue(n.intValue)
      case BooleanValue(b)             => NumberValue(if b then 1 else 0)
      case TextValue(s) =>
        try NumberValue(s.trim.toInt)
        catch case _: NumberFormatException => throw TypeException(v.pos, s"cannot cast '$s' to integer")
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
        catch case _: NumberFormatException => throw TypeException(v.pos, s"cannot cast '$s' to bigint")
      case _ => super.convert(v)

  override def init: Value = ONE

case object SmallSerialType extends Type("smallserial"):
  override val isNumber = true

  override def convert(v: Value): Value =
    v match
      case n @ NumberValue(DIntType, v) =>
        val i = v.intValue
        if i < -32768 || i > 32767 then throw TypeException(n.pos, s"value $i out of smallint range (-32768..32767)")
        n
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
      case BooleanValue(b)                            => NumberValue(if b then 1.0 else 0.0)
      case TextValue(s) =>
        try NumberValue(s.trim.toDouble)
        catch case _: NumberFormatException => throw TypeException(v.pos, s"cannot cast '$s' to double")
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
      case TextValue(s) =>
        try NumberValue(BigDecimal(s.trim, mc).setScale(scale, BigDecimal.RoundingMode.DOWN))
        catch case _: NumberFormatException => throw TypeException(v.pos, s"cannot cast '$s' to numeric")
      case _ => super.convert(v)

case class VarcharType(length: Int) extends Type("varchar"):
  override def convert(v: Value): Value =
    v match
      case TextValue(s) =>
        if s.length > length then TextValue(s.substring(0, length))
        else TextValue(s)
      case _ =>
        val s = v.toText.s
        if s.length > length then TextValue(s.substring(0, length))
        else TextValue(s)

case class CharType(length: Int) extends Type("char"):
  override def convert(v: Value): Value =
    v match
      case TextValue(s) =>
        if s.length >= length then TextValue(s.substring(0, length))
        else TextValue(s.padTo(length, ' '))
      case _ =>
        val s = v.toText.s
        if s.length >= length then TextValue(s.substring(0, length))
        else TextValue(s.padTo(length, ' '))

case object UUIDType extends Type("uuid"):
  private val UUIDv4: Regex = "(?i)^[0-9A-F]{8}-[0-9A-F]{4}-4[0-9A-F]{3}-[89AB][0-9A-F]{3}-[0-9A-F]{12}$".r

  def valid(id: String): Boolean = UUIDv4 matches id

  override def convert(v: Value): Value =
    v match
      case id: UUIDValue => id
      case _             =>
        val textVal = v.toText
        if !valid(textVal.s) then throw TypeException(v.pos, "invalid version 4 UUID")
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
      case t: TimestampValue   => t
      case DateValue(d)        => TimestampValue(d.atStartOfDay)
      case TimestampTZValue(t) => TimestampValue(t.toLocalDateTime)
      case _ =>
        val textVal = v.toText
        TimestampValue(parseTimestamp(textVal.s))

case object DateType extends Type("date"):
  override def convert(v: Value): Value =
    v match
      case d: DateValue        => d
      case TimestampValue(t)   => DateValue(t.toLocalDate)
      case TimestampTZValue(t) => DateValue(t.toLocalDate)
      case _ =>
        val s = v.toText.s
        try DateValue(LocalDate.parse(s))
        catch
          case _: DateTimeParseException =>
            try DateValue(LocalDate.parse(s, DateTimeFormatter.ofPattern("yyyy-MM-dd")))
            catch case _: DateTimeParseException => throw TypeException(v.pos, s"cannot parse '$s' as date")

case object TimeType extends Type("time"):
  override def convert(v: Value): Value =
    v match
      case t: TimeValue        => t
      case TimestampValue(t)   => TimeValue(t.toLocalTime)
      case TimestampTZValue(t) => TimeValue(t.toLocalTime)
      case _ =>
        val s = v.toText.s
        try TimeValue(LocalTime.parse(s))
        catch
          case _: DateTimeParseException =>
            try TimeValue(LocalTime.parse(s, DateTimeFormatter.ofPattern("HH:mm")))
            catch case _: DateTimeParseException => throw TypeException(v.pos, s"cannot parse '$s' as time")

case object TimeTZType extends Type("timetz"):
  override def convert(v: Value): Value =
    v match
      case t: TimeTZValue        => t
      case TimeValue(t)          => TimeTZValue(t.atOffset(ZoneOffset.UTC))
      case TimestampTZValue(t)   => TimeTZValue(t.toOffsetTime)
      case _ =>
        val s = v.toText.s
        try TimeTZValue(OffsetTime.parse(s))
        catch
          case _: DateTimeParseException =>
            try TimeTZValue(OffsetTime.parse(s, DateTimeFormatter.ofPattern("HH:mm:ssXXX")))
            catch case _: DateTimeParseException => throw TypeException(v.pos, s"cannot parse '$s' as timetz")

case object IntervalType extends Type("interval"):
  private val simplePattern = """(?i)(?:(\d+)\s*days?)?[,\s]*(?:(\d+)\s*hours?)?[,\s]*(?:(\d+)\s*minutes?)?[,\s]*(?:(\d+)\s*seconds?)?""".r

  override def convert(v: Value): Value =
    v match
      case i: IntervalValue => i
      case _ =>
        val s = v.toText.s.trim
        try IntervalValue(Duration.parse(s))
        catch
          case _: Exception =>
            simplePattern.findFirstMatchIn(s) match
              case Some(m) =>
                val days    = Option(m.group(1)).map(_.toLong).getOrElse(0L)
                val hours   = Option(m.group(2)).map(_.toLong).getOrElse(0L)
                val minutes = Option(m.group(3)).map(_.toLong).getOrElse(0L)
                val seconds = Option(m.group(4)).map(_.toLong).getOrElse(0L)
                if days == 0 && hours == 0 && minutes == 0 && seconds == 0 then
                  throw TypeException(v.pos, s"cannot parse '$s' as interval")
                IntervalValue(Duration.ofDays(days).plusHours(hours).plusMinutes(minutes).plusSeconds(seconds))
              case None => throw TypeException(v.pos, s"cannot parse '$s' as interval")

case object TimestampTZType extends Type("timestamptz"):
  override def convert(v: Value): Value =
    v match
      case t: TimestampTZValue => t
      case TimestampValue(t)   => TimestampTZValue(t.atOffset(ZoneOffset.UTC))
      case DateValue(d)        => TimestampTZValue(d.atStartOfDay.atOffset(ZoneOffset.UTC))
      case _ =>
        val s = v.toText.s
        try TimestampTZValue(OffsetDateTime.parse(s))
        catch case _: DateTimeParseException => throw TypeException(v.pos, s"cannot parse '$s' as timestamp with time zone")

case object ByteaType extends Type("bytea"):
  override def convert(v: Value): Value =
    v match
      case b: ByteaValue => b
      case _ =>
        val s = v.toText.s
        if s.startsWith("\\x") || s.startsWith("\\X") then
          val hex = s.drop(2)
          if hex.length % 2 != 0 then throw TypeException(v.pos, "invalid hex string: odd length")
          try
            val bytes = hex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
            ByteaValue(bytes)
          catch case _: NumberFormatException => throw TypeException(v.pos, s"invalid hex string: '$hex'")
        else
          ByteaValue(s.getBytes("UTF-8"))

case class ArrayColumnType(elementType: Type) extends Type(s"${elementType.name}[]"):
  override def convert(v: Value): Value =
    v match
      case ArrayValue(data) => ArrayValue(data.map(elementType.convert))
      case _                => super.convert(v)

case object JSONType extends Type("JSON"):
  override def convert(v: Value): Value =
    v match
      case _: (ArrayValue | ObjectValue) => v
      case _                             =>
        val textVal = v.toText
        JSONParser.parseJSON(textVal.s)

case object ObjectType extends Type("object")

case object NullType extends Type("null")

case object AnyType extends Type("any")

case object StarType extends Type("star")

case object BooleanType extends Type("boolean"):
  override def convert(v: Value): Value =
    v match
      case b: BooleanValue   => b
      case NumberValue(_, n) => BooleanValue(n.intValue != 0)
      case TextValue(s) =>
        s.trim.toLowerCase match
          case "true" | "t" | "yes" | "y" | "1" | "on"    => BooleanValue(true)
          case "false" | "f" | "no" | "n" | "0" | "off"   => BooleanValue(false)
          case _ => throw TypeException(v.pos, s"cannot cast '$s' to boolean")
      case _ => super.convert(v)

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
          try OffsetDateTime.parse(s).toLocalDateTime
          catch
            case _: DateTimeParseException =>
              try OffsetDateTime.parse(s.replaceFirst(" ", "T")).toLocalDateTime
              catch
                case _: DateTimeParseException =>
                  LocalDate.parse(s).atStartOfDay

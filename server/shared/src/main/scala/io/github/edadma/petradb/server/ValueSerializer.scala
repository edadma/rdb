package io.github.edadma.petradb.server

import io.github.edadma.petradb.*
import zio.json.ast.Json

object ValueSerializer:
  def valueToJson(v: Value): Json = v match
    case NullValue()          => Json.Null
    case TextValue(s)         => Json.Str(s)
    case NumberValue(_, n)    => Json.Num(new java.math.BigDecimal(n.toString))
    case BooleanValue(b)      => Json.Bool(b)
    case DateValue(d)         => Json.Str(d.toString)
    case TimeValue(t)         => Json.Str(t.toString)
    case TimestampValue(ts)   => Json.Str(ts.toString)
    case TimestampTZValue(ts) => Json.Str(ts.toString)
    case TimeTZValue(t)       => Json.Str(t.toString)
    case IntervalValue(d)     => Json.Str(v.string)
    case ByteaValue(bytes)    => Json.Str(base64Encode(bytes))
    case UUIDValue(id)        => Json.Str(id)
    case ArrayValue(data)     => Json.Arr(data.map(valueToJson)*)
    case ObjectValue(props)   => Json.Obj(props.map((k, v) => k -> valueToJson(v))*)
    case other                => Json.Str(other.string)

  private val base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  private def base64Encode(bytes: Array[Byte]): String =
    val sb = new StringBuilder
    var i = 0
    while i < bytes.length do
      val b0 = bytes(i) & 0xff
      if i + 2 < bytes.length then
        val b1 = bytes(i + 1) & 0xff
        val b2 = bytes(i + 2) & 0xff
        sb += base64Chars(b0 >> 2)
        sb += base64Chars(((b0 & 3) << 4) | (b1 >> 4))
        sb += base64Chars(((b1 & 0xf) << 2) | (b2 >> 6))
        sb += base64Chars(b2 & 0x3f)
      else if i + 1 < bytes.length then
        val b1 = bytes(i + 1) & 0xff
        sb += base64Chars(b0 >> 2)
        sb += base64Chars(((b0 & 3) << 4) | (b1 >> 4))
        sb += base64Chars((b1 & 0xf) << 2)
        sb += '='
      else
        sb += base64Chars(b0 >> 2)
        sb += base64Chars((b0 & 3) << 4)
        sb ++= "=="
      i += 3
    sb.result()

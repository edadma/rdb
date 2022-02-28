package io.github.edadma.rdb

import io.github.edadma.datetime.Datetime

trait Type(val name: String):
  def convert(v: Value): Value =
    if v.vtyp != this then problem(v, s"can't auto-convert '$v' to type '$name'")
    else v

case object NumberType extends Type("number")
case object IntegerType extends Type("integer")
case object DoubleType extends Type("double")
case object TextType extends Type("text")
case object TimestampType extends Type("timestamp"):
  override def convert(v: Value): Value =
    v match
      case t: TimestampValue => t
      case TextValue(t)      => TimestampValue(Datetime.fromString(t))

case object JSONType extends Type("JSON")
case object NullType extends Type("null")
case object StarType extends Type("star")
case object BooleanType extends Type("boolean")
case object TableType extends Type("table")
case object ArrayType extends Type("array")
case object UnknownType extends Type("unknown")

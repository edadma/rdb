package io.github.edadma.rdb

trait Type(val name: String)

case object NumberType extends Type("number")
case object StringType extends Type("string")
case object NullType extends Type("any")
case object BooleanType extends Type("boolean")
case object TableType extends Type("table")
case object ArrayType extends Type("array")

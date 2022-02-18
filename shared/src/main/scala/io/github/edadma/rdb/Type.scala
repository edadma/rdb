package io.github.edadma.rdb

trait Type(val name: String)

case object NumberType extends Type("number")
case object StringType extends Type("string")
case object BooleanType extends Type("boolean")
case object TableType extends Type("table")

package io.github.edadma.rdb

trait Type(val name: String)

case object IntType extends Type("integer")
case object StringType extends Type("string")
case object BooleanType extends Type("boolean")
case object TableType extends Type("table")

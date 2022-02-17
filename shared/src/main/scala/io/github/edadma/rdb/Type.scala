package io.github.edadma.rdb

trait Type(val name: String)

case object IntType extends Type("integer")

case object TableType extends Type("table")

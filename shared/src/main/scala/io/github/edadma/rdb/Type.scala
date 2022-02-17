package io.github.edadma.rdb

abstract class Type {
  val name: String
}

case class IntType(name: String) extends Type

package io.github.edadma.petradb.chisel

import io.github.edadma.petradb.Value

/** Raised when a `Value` cannot be decoded into the requested Scala type. */
case class DecodeException(message: String) extends RuntimeException(message)

private[chisel] def decodeFail(v: Value, expected: String): Nothing =
  throw DecodeException(s"expected $expected, got ${v.vtyp.name} ($v)")

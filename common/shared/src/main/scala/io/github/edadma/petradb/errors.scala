package io.github.edadma.petradb

import scala.util.parsing.input.Position

sealed abstract class PetraException(msg: String) extends RuntimeException(msg):
  def pos: Position

case class ParseException(pos: Position, msg: String)              extends PetraException(msg)
case class UndefinedReferenceException(pos: Position, msg: String) extends PetraException(msg)
case class SchemaException(pos: Position, msg: String)             extends PetraException(msg)
case class TypeException(pos: Position, msg: String)               extends PetraException(msg)
case class ConstraintException(pos: Position, msg: String)         extends PetraException(msg)
case class ExecutionException(pos: Position, msg: String)          extends PetraException(msg)

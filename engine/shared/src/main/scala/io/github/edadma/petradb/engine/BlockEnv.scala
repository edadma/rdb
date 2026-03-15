package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import scala.collection.mutable

class BlockEnv(private val parent: Option[BlockEnv] = None):
  private val vars = mutable.LinkedHashMap[String, Value]()
  private val types = mutable.HashMap[String, Type]()

  def declare(name: String, typ: Type, default: Value): Unit =
    val converted = typ.convert(default)
    vars(name) = converted
    types(name) = typ

  def get(name: String): Option[Value] =
    vars.get(name).orElse(parent.flatMap(_.get(name)))

  def set(name: String, value: Value): Unit =
    if vars.contains(name) then
      val typ = types(name)
      vars(name) = typ.convert(value)
    else parent match
      case Some(p) => p.set(name, value)
      case None    => sys.error(s"variable '$name' is not declared")

  def isDeclared(name: String): Boolean =
    vars.contains(name) || parent.exists(_.isDeclared(name))

  def getType(name: String): Option[Type] =
    types.get(name).orElse(parent.flatMap(_.getType(name)))

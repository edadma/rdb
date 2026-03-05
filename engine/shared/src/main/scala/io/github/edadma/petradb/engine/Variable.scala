package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import java.time.{LocalDateTime, ZoneOffset}

abstract class Variable(var name: String):
  def instance: VariableInstance

abstract class VariableInstance:
  def value: Value

class CurrentTimestamp extends Variable("CURRENT_TIMESTAMP"):
  def instance: VariableInstance = new CurrentTimestampInstance

class CurrentTimestampInstance extends VariableInstance:
  lazy val value: TimestampValue = TimestampValue(LocalDateTime.now(ZoneOffset.UTC))

val scalarVariable: Map[String, Variable] =
  List(
    new CurrentTimestamp
  ) map (v => v.name -> v) toMap

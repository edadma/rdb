package io.github.edadma.rdb

import io.github.edadma.datetime.Datetime

abstract class Variable(var name: String):
  def instance: VariableInstance

abstract class VariableInstance:
  def value: Value

class CurrentTimestamp extends Variable("CURRENT_TIMESTAMP"):
  def instance: VariableInstance = new CurrentTimestampInstance

class CurrentTimestampInstance extends VariableInstance:
  lazy val value: TimestampValue = TimestampValue(Datetime.now())

val scalarVariable: Map[String, Variable] =
  List(
    new CurrentTimestamp
  ) map (v => v.name -> v) toMap

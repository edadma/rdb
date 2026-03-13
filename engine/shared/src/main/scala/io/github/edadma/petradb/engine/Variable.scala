package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneOffset}

abstract class Variable(var name: String):
  def instance: VariableInstance

abstract class VariableInstance:
  def value: Value

class CurrentTimestamp extends Variable("CURRENT_TIMESTAMP"):
  def instance: VariableInstance = new CurrentTimestampInstance

class CurrentTimestampInstance extends VariableInstance:
  lazy val value: TimestampValue = TimestampValue(LocalDateTime.now(ZoneOffset.UTC))

class CurrentDateVar extends Variable("CURRENT_DATE"):
  def instance: VariableInstance = new CurrentDateInstance

class CurrentDateInstance extends VariableInstance:
  lazy val value: DateValue = DateValue(LocalDate.now(ZoneOffset.UTC))

class CurrentTimeVar extends Variable("CURRENT_TIME"):
  def instance: VariableInstance = new CurrentTimeInstance

class CurrentTimeInstance extends VariableInstance:
  lazy val value: TimeValue = TimeValue(LocalTime.now(ZoneOffset.UTC))

val scalarVariable: Map[String, Variable] =
  List(
    new CurrentTimestamp,
    new CurrentDateVar,
    new CurrentTimeVar,
  ) map (v => v.name -> v) toMap

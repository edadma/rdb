package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import java.time.{LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime}

abstract class Variable(var name: String):
  def typ: Type
  def instance: VariableInstance

abstract class VariableInstance:
  def value: Value

// CURRENT_TIMESTAMP is `timestamp with time zone` in PostgreSQL: it captures an instant, not a
// zone-naive wall clock. Representing it as a TimestampTZValue carrying the system-zone offset keeps
// the instant intact across the JS/JVM/Native value bridges, where a naive timestamp would be
// reinterpreted in the local zone and silently shifted.
class CurrentTimestamp extends Variable("CURRENT_TIMESTAMP"):
  def typ: Type = TimestampTZType
  def instance: VariableInstance = new CurrentTimestampInstance

class CurrentTimestampInstance extends VariableInstance:
  lazy val value: TimestampTZValue = TimestampTZValue(OffsetDateTime.now())

// LOCALTIMESTAMP is the zone-naive counterpart of CURRENT_TIMESTAMP: the current wall clock in the
// session zone, with no offset attached.
class LocalTimestampVar extends Variable("LOCALTIMESTAMP"):
  def typ: Type = TimestampType
  def instance: VariableInstance = new LocalTimestampInstance

class LocalTimestampInstance extends VariableInstance:
  lazy val value: TimestampValue = TimestampValue(LocalDateTime.now())

class CurrentDateVar extends Variable("CURRENT_DATE"):
  def typ: Type = DateType
  def instance: VariableInstance = new CurrentDateInstance

class CurrentDateInstance extends VariableInstance:
  lazy val value: DateValue = DateValue(LocalDate.now())

// CURRENT_TIME is `time with time zone` in PostgreSQL; LOCALTIME is its zone-naive counterpart.
class CurrentTimeVar extends Variable("CURRENT_TIME"):
  def typ: Type = TimeTZType
  def instance: VariableInstance = new CurrentTimeInstance

class CurrentTimeInstance extends VariableInstance:
  lazy val value: TimeTZValue = TimeTZValue(OffsetTime.now())

class LocalTimeVar extends Variable("LOCALTIME"):
  def typ: Type = TimeType
  def instance: VariableInstance = new LocalTimeInstance

class LocalTimeInstance extends VariableInstance:
  lazy val value: TimeValue = TimeValue(LocalTime.now())

val scalarVariable: Map[String, Variable] =
  List(
    new CurrentTimestamp,
    new LocalTimestampVar,
    new CurrentDateVar,
    new CurrentTimeVar,
    new LocalTimeVar,
  ) map (v => v.name -> v) toMap

package io.github.edadma.petradb.jdbc

import io.github.edadma.petradb.*

def valueToObject(v: Value): AnyRef = v match
  case NullValue()          => null
  case TextValue(s)         => s
  case NumberValue(_, n)    => n.asInstanceOf[AnyRef]
  case BooleanValue(b)      => java.lang.Boolean.valueOf(b)
  case DateValue(d)         => java.sql.Date.valueOf(d)
  case TimestampValue(ts)   => java.sql.Timestamp.valueOf(ts)
  case TimestampTZValue(ts) => java.sql.Timestamp.from(ts.toInstant)
  case UUIDValue(u)         => u
  case e: EnumValue         => e.string
  case _                    => v.string

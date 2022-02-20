package io.github.edadma.rdb

import math._

val scalarFunction: (String, Seq[Value]) => Value =
  case ("sin", Seq(NumberValue(t, n)))  => NumberValue(sin(n.doubleValue))
  case ("cos", Seq(NumberValue(t, n)))  => NumberValue(cos(n.doubleValue))
  case ("tan", Seq(NumberValue(t, n)))  => NumberValue(tan(n.doubleValue))
  case ("ln", Seq(NumberValue(t, n)))   => NumberValue(log(n.doubleValue))
  case ("exp", Seq(NumberValue(t, n)))  => NumberValue(exp(n.doubleValue))
  case ("table", Seq(TableValue(d, _))) => ArrayValue(d.map(r => ArrayValue(r.data)))

val scalarFunctionType =
  Map(
    "sin" -> NumberType,
    "cos" -> NumberType,
    "tan" -> NumberType,
    "ln" -> NumberType,
    "exp" -> NumberType,
    "table" -> ArrayType
  )

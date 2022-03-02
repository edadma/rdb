package io.github.edadma.rdb

import scala.math.*

case class ScalarFunction(name: String, func: PartialFunction[Seq[Value], Value], typ: Type)

val scalarFunction: Map[String, ScalarFunction] =
  List(
    ScalarFunction("ABS", { case Seq(NumberValue(t, n)) => NumberValue(abs(n.doubleValue)) }, NumberType),
    ScalarFunction("TABLE", { case Seq(TableValue(d, _)) => ArrayValue(d map (r => ArrayValue(r.data))) }, ArrayType)
  ) map (f => f.name -> f) toMap

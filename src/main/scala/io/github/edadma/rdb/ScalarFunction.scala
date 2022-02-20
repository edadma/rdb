package io.github.edadma.rdb

import math._

case class ScalarFunction(name: String, apply: PartialFunction[Seq[Value], Value], typ: Type)

val scalarFunction: Map[String, ScalarFunction] =
  List(
    ScalarFunction("abs", { case Seq(NumberValue(t, n)) => NumberValue(abs(n.doubleValue)) }, NumberType),
    ScalarFunction("table", { case Seq(TableValue(d, _)) => ArrayValue(d map (r => ArrayValue(r.data))) }, ArrayType)
  ) map (f => f.name -> f) toMap

//val aggregateFunction: Map[String, (PartialFunction[Seq[Value], Value], Type)] =
//  Map(
//    "abs" -> ({ case Seq(NumberValue(t, n)) => NumberValue(abs(n.doubleValue)) }, NumberType),
//    "table" -> ({ case Seq(TableValue(d, _)) => ArrayValue(d.map(r => ArrayValue(r.data))) }, ArrayType)
//  )
//
//abstract class AggregateFunction:
//

package io.github.edadma.rdb

abstract class AggregateFunction(val name: String, val typ: Type) {
  def acc(v: Value): Value

  def result: Value
}

val aggregateFunction: Map[String, AggregateFunction] =
  List(
    new AggregateFunction("count", NumberType) {
      var count: Int = 0

      def acc(v: Value): Value =
        count += 1
        NumberValue(count)

      def result: NumberValue =
        val res = NumberValue(count)

        count = 0
        res
    }
  ) map (f => f.name -> f) toMap

package io.github.edadma.rdb

import io.github.edadma.dal.BasicDAL

// todo: input type checking
abstract class AggregateFunction(val name: String, val typ: Type) {
  def acc(v: Value): Value

  def result: Value

  def init(): Unit
}

val aggregateFunction: Map[String, AggregateFunction] =
  List(
    new AggregateFunction("count", NumberType) {
      var count: Int = 0

      def acc(v: Value): Value =
        count += 1
        NumberValue(count)

      def result: NumberValue = NumberValue(count)

      def init(): Unit = count = 0
    },
    new AggregateFunction("sum", NumberType) {
      var sum: NumberValue = NumberValue(0)

      def acc(v: Value): Value =
        sum = BasicDAL.compute(PLUS, sum, v.asInstanceOf[NumberValue], NumberValue.from)
        sum

      def result: NumberValue = sum

      def init(): Unit = sum = NumberValue(0)
    }
  ) map (f => f.name -> f) toMap

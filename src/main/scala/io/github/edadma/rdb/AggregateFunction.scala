package io.github.edadma.rdb

import io.github.edadma.dal.BasicDAL

// todo: input type checking
abstract class AggregateFunction(val name: String, val typ: Type) {
  val acc: PartialFunction[Value, Value]

  def result: Value

  def init(): Unit
}

val aggregateFunction: Map[String, AggregateFunction] =
  List(
    new AggregateFunction("count", NumberType) {
      var count: Int = 0

      val acc: PartialFunction[Value, Value] =
        case v =>
          if v != NullValue then count += 1

          NumberValue(count)

      def result: NumberValue = NumberValue(count)

      def init(): Unit = count = 0
    },
    new AggregateFunction("sum", NumberType) {
      var sum: NumberValue = NumberValue(0)

      val acc: PartialFunction[Value, Value] =
        case v: NumberValue =>
          sum = BasicDAL.compute(PLUS, sum, v.asInstanceOf[NumberValue], NumberValue.from)
          sum
        case v => problem(v.pos, "only numbers can be summed")

      def result: NumberValue = sum

      def init(): Unit = sum = NumberValue(0)
    }
  ) map (f => f.name -> f) toMap

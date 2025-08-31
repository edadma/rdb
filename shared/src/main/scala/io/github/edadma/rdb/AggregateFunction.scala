package io.github.edadma.rdb

import io.github.edadma.dal.BasicDAL

import scala.language.postfixOps

// todo: input type checking

abstract class AggregateFunction(val name: String):
  def instantiate: (AggregateFunctionInstance, Type)

abstract class AggregateFunctionInstance(val name: String) {
  val acc: PartialFunction[Value, Value]

  def result: Value

  def init(): Unit
}

val aggregateFunction: Map[String, AggregateFunction] =
  List(
    new AggregateFunction("count") {
      def instantiate: (AggregateFunctionInstance, Type) =
        (
          new AggregateFunctionInstance("count"):
            var count: Int = 0

            val acc: PartialFunction[Value, Value] =
              case v =>
                if !v.isNull then count += 1

                NumberValue(count)

            def result: NumberValue = NumberValue(count)

            def init(): Unit = count = 0
          ,
          NumberType,
        )
    },
    new AggregateFunction("sum") {
      def instantiate: (AggregateFunctionInstance, Type) =
        (
          new AggregateFunctionInstance("sum"):
            var sum: NumberValue = NumberValue(0)

            val acc: PartialFunction[Value, Value] =
              case v: NumberValue =>
                sum = BasicDAL.compute(PLUS, sum, v, NumberValue.from)
                sum
              case v if v.isNull => sum // Ignore NULL values
              case v => problem(v, "only numbers can be summed")

            def result: NumberValue = sum

            def init(): Unit = sum = NumberValue(0)
          ,
          NumberType,
        )
    },
    new AggregateFunction("min") {
      def instantiate: (AggregateFunctionInstance, Type) =
        (
          new AggregateFunctionInstance("min"):
            var minValue: Value = NullValue()
            var hasValue: Boolean = false

            val acc: PartialFunction[Value, Value] =
              case v if !v.isNull =>
                if !hasValue then
                  minValue = v
                  hasValue = true
                else
                  v match
                    case n1: NumberValue =>
                      minValue match
                        case n2: NumberValue => 
                          if n1.value.doubleValue < n2.value.doubleValue then minValue = n1
                        case _ => problem(v, "inconsistent types in min")
                    case t1: TextValue =>
                      minValue match
                        case t2: TextValue => 
                          if t1.s < t2.s then minValue = t1
                        case _ => problem(v, "inconsistent types in min")
                    case _ => 
                      if v.toString < minValue.toString then minValue = v
                minValue
              case _ => minValue

            def result: Value = if hasValue then minValue else NullValue()

            def init(): Unit = 
              minValue = NullValue()
              hasValue = false
          ,
          NumberType, // This will be adjusted based on actual data type
        )
    },
    new AggregateFunction("max") {
      def instantiate: (AggregateFunctionInstance, Type) =
        (
          new AggregateFunctionInstance("max"):
            var maxValue: Value = NullValue()
            var hasValue: Boolean = false

            val acc: PartialFunction[Value, Value] =
              case v if !v.isNull =>
                if !hasValue then
                  maxValue = v
                  hasValue = true
                else
                  v match
                    case n1: NumberValue =>
                      maxValue match
                        case n2: NumberValue => 
                          if n1.value.doubleValue > n2.value.doubleValue then maxValue = n1
                        case _ => problem(v, "inconsistent types in max")
                    case t1: TextValue =>
                      maxValue match
                        case t2: TextValue => 
                          if t1.s > t2.s then maxValue = t1
                        case _ => problem(v, "inconsistent types in max")
                    case _ => 
                      if v.toString > maxValue.toString then maxValue = v
                maxValue
              case _ => maxValue

            def result: Value = if hasValue then maxValue else NullValue()

            def init(): Unit = 
              maxValue = NullValue()
              hasValue = false
          ,
          NumberType, // This will be adjusted based on actual data type
        )
    },
    new AggregateFunction("avg") {
      def instantiate: (AggregateFunctionInstance, Type) =
        (
          new AggregateFunctionInstance("avg"):
            var sum: NumberValue = NumberValue(0)
            var count: Int = 0

            val acc: PartialFunction[Value, Value] =
              case v: NumberValue =>
                sum = BasicDAL.compute(PLUS, sum, v, NumberValue.from)
                count += 1
                sum
              case v if v.isNull => sum // Ignore NULL values
              case v => problem(v, "only numbers can be averaged")

            def result: Value = 
              if count > 0 then NumberValue(sum.value.doubleValue / count.toDouble)
              else NullValue()

            def init(): Unit = 
              sum = NumberValue(0)
              count = 0
          ,
          NumberType,
        )
    },
  ) map (f => f.name -> f) toMap

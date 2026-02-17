package io.github.edadma.rdb

import io.github.edadma.dal.BasicDAL

import scala.language.postfixOps

// todo: input type checking

abstract class AggregateFunction(val name: String):
  def instantiate: (AggregateFunctionInstance, Type)

abstract class AggregateFunctionInstance(val name: String) {
  val acc: PartialFunction[Seq[Value], Value]

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

            val acc: PartialFunction[Seq[Value], Value] =
              case Seq(v) =>
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

            val acc: PartialFunction[Seq[Value], Value] =
              case Seq(v: NumberValue) =>
                sum = BasicDAL.compute(PLUS, sum, v, NumberValue.from)
                sum
              case Seq(v) if v.isNull => sum
              case Seq(v) => problem(v, "only numbers can be summed")

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

            val acc: PartialFunction[Seq[Value], Value] =
              case Seq(v) if !v.isNull =>
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
          NumberType,
        )
    },
    new AggregateFunction("max") {
      def instantiate: (AggregateFunctionInstance, Type) =
        (
          new AggregateFunctionInstance("max"):
            var maxValue: Value = NullValue()
            var hasValue: Boolean = false

            val acc: PartialFunction[Seq[Value], Value] =
              case Seq(v) if !v.isNull =>
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
          NumberType,
        )
    },
    new AggregateFunction("avg") {
      def instantiate: (AggregateFunctionInstance, Type) =
        (
          new AggregateFunctionInstance("avg"):
            var sum: NumberValue = NumberValue(0)
            var count: Int = 0

            val acc: PartialFunction[Seq[Value], Value] =
              case Seq(v: NumberValue) =>
                sum = BasicDAL.compute(PLUS, sum, v, NumberValue.from)
                count += 1
                sum
              case Seq(v) if v.isNull => sum
              case Seq(v) => problem(v, "only numbers can be averaged")

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
    new AggregateFunction("string_agg") {
      def instantiate: (AggregateFunctionInstance, Type) =
        (
          new AggregateFunctionInstance("string_agg"):
            val parts = new scala.collection.mutable.ArrayBuffer[String]
            var separator: String = ","

            val acc: PartialFunction[Seq[Value], Value] =
              case Seq(v, _) if v.isNull =>
                if parts.isEmpty then NullValue() else TextValue(parts.mkString(separator))
              case Seq(v, TextValue(sep)) =>
                separator = sep
                parts += v.string
                TextValue(parts.mkString(sep))

            def result: Value =
              if parts.isEmpty then NullValue()
              else TextValue(parts.mkString(separator))

            def init(): Unit =
              parts.clear()
              separator = ","
          ,
          TextType,
        )
    },
    new AggregateFunction("array_agg") {
      def instantiate: (AggregateFunctionInstance, Type) =
        (
          new AggregateFunctionInstance("array_agg"):
            val elems = new scala.collection.mutable.ArrayBuffer[Value]

            val acc: PartialFunction[Seq[Value], Value] =
              case Seq(v) =>
                elems += v
                ArrayValue(elems.toIndexedSeq)

            def result: Value =
              if elems.isEmpty then NullValue()
              else ArrayValue(elems.toIndexedSeq)

            def init(): Unit = elems.clear()
          ,
          ArrayType,
        )
    },
    new AggregateFunction("bool_and") {
      def instantiate: (AggregateFunctionInstance, Type) =
        (
          new AggregateFunctionInstance("bool_and"):
            var value: Boolean = true
            var hasValue: Boolean = false

            val acc: PartialFunction[Seq[Value], Value] =
              case Seq(BooleanValue(b)) =>
                hasValue = true
                value = value && b
                BooleanValue(value)
              case Seq(v) if v.isNull => BooleanValue(value)

            def result: Value =
              if hasValue then BooleanValue(value) else NullValue()

            def init(): Unit =
              value = true
              hasValue = false
          ,
          BooleanType,
        )
    },
    new AggregateFunction("bool_or") {
      def instantiate: (AggregateFunctionInstance, Type) =
        (
          new AggregateFunctionInstance("bool_or"):
            var value: Boolean = false
            var hasValue: Boolean = false

            val acc: PartialFunction[Seq[Value], Value] =
              case Seq(BooleanValue(b)) =>
                hasValue = true
                value = value || b
                BooleanValue(value)
              case Seq(v) if v.isNull => BooleanValue(value)

            def result: Value =
              if hasValue then BooleanValue(value) else NullValue()

            def init(): Unit =
              value = false
              hasValue = false
          ,
          BooleanType,
        )
    },
  ) map (f => f.name -> f) toMap

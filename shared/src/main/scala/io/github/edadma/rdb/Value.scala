package io.github.edadma.rdb

import io.github.edadma.dal.{IntType => DIntType, TypedNumber, Type => DType}

trait Value(val vtyp: Type):
  var pos: Option[Pos] = None

  def pos(p: Pos): Value =
    pos = Some(p)
    this

case class NumberValue(typ: DType, value: Number) extends Value(NumberType) with TypedNumber

object NumberValue {
  def apply(n: Int): NumberValue = NumberValue(DIntType, n)

  def from(n: (DType, Number)): NumberValue = NumberValue(n._1, n._2)
}

case class StringValue(s: String) extends Value(StringType)

case class BooleanValue(b: Boolean) extends Value(BooleanType)

//case class StringValue(s: String) extends Value
case class TableValue(data: Seq[Row], meta: RowMeta) extends Value(TableType)

/*

object SLNumber {
  def from(n: Int): SLNumber = SLNumber(IntType, n)

  def from(n: (Type, Number)): SLNumber = SLNumber(n._1, n._2)
}

case class SLNumber(typ: Type, value: Number) extends SLValue with TypedNumber {
  val clas: SLClass = PrimitiveClass.NumberClass

  override def toString: String = value.toString
}
 */

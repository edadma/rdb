package io.github.edadma.rdb

import scala.language.postfixOps

case class EnumType(enumName: String, labels: IndexedSeq[String]) extends Type(enumName):
  val labelsMap: Map[String, Int] = labels.zipWithIndex toMap

  override def convert(v: Value): Value =
    v match
      case TextValue(l) => EnumValue(labelsMap getOrElse (l, problem(v, s"unknown label '$l'")), this)
      case _            => super.convert(v)

case class EnumValue(value: Int, typ: EnumType) extends Value(typ):
  override def toText: TextValue = TextValue(string)

  override def render: String = s"'$string'"

  def string: String = typ.labels(value)

  override def compare(that: Value): Int =
    that match
      case EnumValue(v, `typ`) => value compare v
      case t @ TextValue(s) =>
        typ.labelsMap get s match
          case None    => problem(t, s"'$s' is not a label of enum '${typ.name}'")
          case Some(l) => value compare l
      case _ => super.compare(that)

package io.github.edadma.rdb

case class EnumType(enumName: String, labels: IndexedSeq[String]) extends Type(enumName):
  private val labelsMap = labels.zipWithIndex toMap

  override def convert(v: Value): Value =
    v match
      case TextValue(l) => EnumValue(labelsMap getOrElse (l, problem(v, s"unknown label '$l'")), this)
      case _            => super.convert(v)

case class EnumValue(value: Int, typ: EnumType) extends Value(typ):
  override def toText: TextValue = TextValue(string)

  override def render: String = s"'$string'"

  def string: String = typ.labels(value)

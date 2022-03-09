package io.github.edadma.rdb

case class EnumType(name: String, labels: IndexedSeq[String]) extends Type("enum"):
  private val labelsMap = labels.zipWithIndex toMap

  override def convert(v: Value): Value =
    v match
      case TextValue(l) => EnumValue(labelsMap getOrElse (l, problem(v, s"unknown label '$l'")), this)
      case _            => super.convert(v)

case class EnumValue(label: Int, typ: EnumType) extends Value(typ):
  override def toText: TextValue = TextValue()

  override def render: String = s"'$string'"

  def string: String = typ.labels(label)

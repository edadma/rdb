package io.github.edadma.rdb

trait Value(val typ: Type):
  var pos: Option[Pos] = None

  def pos(p: Pos): Value =
    pos = Some(p)
    this

case class IntValue(n: Int) extends Value(IntType)

case class StringValue(s: String) extends Value(StringType)

case class BooleanValue(b: Boolean) extends Value(BooleanType)

//case class StringValue(s: String) extends Value
case class TableValue(data: Seq[Row], meta: RowMeta) extends Value(TableType)

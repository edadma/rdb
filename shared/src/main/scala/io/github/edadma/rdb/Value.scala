package io.github.edadma.rdb

trait Value(val typ: Type)

case class IntValue(n: Int) extends Value(IntType)

//case class StringValue(s: String) extends Value
case class TableValue(data: Seq[Row], meta: RowMeta) extends Value(TableType)

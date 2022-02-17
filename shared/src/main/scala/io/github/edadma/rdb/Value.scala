package io.github.edadma.rdb

trait Value

case class IntValue(n: Int) extends Value
case class StringValue(s: String) extends Value
case class TableValue(data: Seq[Row], meta: TableMeta) extends Value

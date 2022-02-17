package io.github.edadma.rdb

trait Value

case class IntValue(n: Int) extends Value
case class StringValue(s: String) extends Value
case class TableValue(data: Seq[Seq[Value]], meta: TableMeta) extends Value

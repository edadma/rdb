package io.github.edadma.rdb

case class Row(data: Seq[Value], meta: RowMeta):
  var group = false

case class ColumnMeta(name: String, typ: Type)

case class RowMeta(columns: Seq[ColumnMeta]):
  val cols: Int = columns.length
  lazy val columnIndices: Map[String, Int] = columns.map(_.name).zipWithIndex.toMap

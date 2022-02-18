package io.github.edadma.rdb

case class Row(data: IndexedSeq[Value], meta: Metadata):
  var group = false

case class ColumnMetadata(name: String, typ: Type)

case class Metadata(columns: Seq[ColumnMetadata]):
  val cols: Int = columns.length
  lazy val columnIndices: Map[String, Int] = columns.map(_.name).zipWithIndex.toMap

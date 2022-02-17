package io.github.edadma.rdb

case class ColumnMeta(name: String, typ: Type)

case class TableMeta(columns: Seq[ColumnMeta]):
  val cols: Int = columns.length
  val columnIndices: Map[String, Int] = columns.map(_.name).zipWithIndex.toMap

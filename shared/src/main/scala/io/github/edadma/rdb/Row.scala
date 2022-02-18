package io.github.edadma.rdb

import scala.language.postfixOps

case class Row(data: IndexedSeq[Value], meta: Metadata):
  var group = false

case class ColumnMetadata(table: String, name: String, typ: Type)

case class Metadata(columns: Seq[ColumnMetadata]):
  val cols: Int = columns.length
  lazy val columnIndices: Map[String, Int] =
    (columns.map(_.name).zipWithIndex ++ columns.map(c => s"${c.table}.${c.name}").zipWithIndex) toMap

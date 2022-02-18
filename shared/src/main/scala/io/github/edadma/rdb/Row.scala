package io.github.edadma.rdb

import scala.language.postfixOps

case class Row(data: IndexedSeq[Value], meta: Metadata):
  var group = false

case class ColumnMetadata(table: Option[String], name: String, typ: Type)

case class Metadata(columns: Seq[ColumnMetadata]):
  val cols: Int = columns.length
  lazy val columnIndices: Map[String, Int] =
    (columns
      .map(_.name)
      .zipWithIndex ++ columns.filter(_.table.isDefined).map(c => s"${c.table.get}.${c.name}").zipWithIndex) toMap
  lazy val singleTable: Boolean = columns.map(_.table).distinct.length == 1

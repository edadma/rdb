package io.github.edadma.rdb

import scala.language.postfixOps

case class Row(data: IndexedSeq[Value], meta: Metadata):
  var group = false

case class ColumnMetadata(table: Option[String], name: String, typ: Type)

case class Metadata(columns: Seq[ColumnMetadata]):
  val width: Int = columns.length
  lazy val columnMap: Map[String, (Int, Type, Option[String])] =
    columns.zipWithIndex.flatMap { case (ColumnMetadata(table, name, typ), idx) =>
      val v = (idx, typ, table)

      table match
        case None    => List(name -> v)
        case Some(t) => List(s"$t.$name" -> v, name -> v)
    } toMap
  lazy val singleTable: Boolean = columns.map(_.table).distinct.length == 1

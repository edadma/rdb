package io.github.edadma.rdb

import scala.language.postfixOps

enum AggregateMode:
  case Return, Accumulate, AccumulateReturn, Disallow

case class Row(data: IndexedSeq[Value], meta: Metadata, mode: AggregateMode = AggregateMode.Return)

case class ColumnMetadata(table: Option[String], name: String, typ: Type)

case class Metadata(columns: IndexedSeq[ColumnMetadata]):
  lazy val width: Int = columns.length
  lazy val columnMap: Map[String, (Int, Type, Option[String])] =
    val ambiguous = columns groupBy (_.name) map ((k, v) => k -> (v.length > 1))

    columns.zipWithIndex.flatMap { case (ColumnMetadata(table, name, typ), idx) =>
      val v = (idx, typ, table)

      table match
        case None                       => List(name -> v)
        case Some(t) if ambiguous(name) => List(s"$t.$name" -> v)
        case Some(t)                    => List(s"$t.$name" -> v, name -> v)
    } toMap
  lazy val singleTable: Boolean = columns.map(_.table).distinct.length == 1

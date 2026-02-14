package io.github.edadma.rdb

import scala.language.postfixOps

case class Row(
    data: IndexedSeq[Value],
    meta: Metadata,
    updater: Option[Seq[(String, Value)] => Unit],
    deleter: Option[() => Unit],
) {
  // Get value by column name
  def apply(name: String): Value = {
    val (idx, _, _) = meta.columnMap.getOrElse(
      name,
      throw new NoSuchElementException(s"Column '$name' not found"),
    )
    data(idx)
  }

  // String extraction
  def getString(name: String): String               = apply(name).string
  def getStringOption(name: String): Option[String] = {
    val value = apply(name)
    if (value.isNull || value.string.isEmpty) None else Some(value.string)
  }

  // Long extraction
  def getLong(name: String): Long               = apply(name).asInstanceOf[NumberValue].value.longValue
  def getLongOption(name: String): Option[Long] = {
    val value = apply(name)
    if (value.isNull) None else Some(value.asInstanceOf[NumberValue].value.longValue)
  }

  // Int extraction
  def getInt(name: String): Int               = apply(name).asInstanceOf[NumberValue].value.intValue
  def getIntOption(name: String): Option[Int] = {
    val value = apply(name)
    if (value.isNull) None else Some(value.asInstanceOf[NumberValue].value.intValue)
  }

  // Boolean extraction
  def getBoolean(name: String): Boolean               = apply(name).asInstanceOf[BooleanValue].b
  def getBooleanOption(name: String): Option[Boolean] = {
    val value = apply(name)
    if (value.isNull) None else Some(value.asInstanceOf[BooleanValue].b)
  }
}

case class ColumnMetadata(table: Option[String], name: String, typ: Type)

case class Metadata(columns: IndexedSeq[ColumnMetadata]):
  lazy val width: Int                                          = columns.length
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

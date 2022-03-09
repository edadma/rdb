package io.github.edadma.rdb

import scala.collection.mutable

abstract class DB:

  def name: String

  def hasTable(name: String): Boolean

  def hasType(name: String): Boolean

  infix def getTable(name: String): Option[Table]

  infix def getType(name: String): Option[Type]

  def createTable(name: String, specs: Seq[Spec]): Table

  def createEnum(name: String, labels: Seq[String]): Unit

abstract class Table extends Process:

  def name: String

  def hasColumn(name: String): Boolean

  def addColumn(spec: ColumnSpec): Unit

  def rows: Int

  def auto(col: String): Value

  def insert(row: Map[String, Value]): Map[String, Value]

  def bulkInsert(header: Seq[String], rows: Seq[Seq[Value]]): Map[String, Value]

trait Spec
case class ColumnSpec(
    name: String,
    typ: Type,
    auto: Boolean = false,
    required: Boolean = false,
    pk: Boolean = false,
    indexed: Boolean = false,
    unique: Boolean = false,
    fk: Option[String] = None,
    default: Option[Value] = None,
) extends Spec

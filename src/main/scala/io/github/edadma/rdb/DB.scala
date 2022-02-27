package io.github.edadma.rdb

import scala.collection.mutable

abstract class DB:

  def name: String

  def hasTable(name: String): Boolean

  def table(name: String): Option[Table]

  def create(name: String, spec: Seq[Spec]): Table

abstract class Table extends Process:

  def name: String

  def hasColumn(name: String): Boolean

  def addColumn(spec: ColumnSpec): Unit

  def rows: Int

  def row(idx: Int): Row

  def increment(col: String): Value

  def insert(row: Map[String, Value]): Map[String, Value]

  def bulkInsert(header: Seq[String], rows: Seq[Seq[Value]]): Map[String, Value]

trait Spec
case class ColumnSpec(
    name: String,
    typ: Type,
    pk: Boolean = false,
    auto: Boolean = false,
    required: Boolean = false,
    indexed: Boolean = false,
    unique: Boolean = false,
    fk: Option[String] = None,
    default: Option[Value] = None
) extends Spec

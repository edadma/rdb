package io.github.edadma.rdb

import scala.collection.mutable

abstract class DB:

  def name: String

  def exists(name: String): Boolean

  def table(name: String): Option[Table]

  def create(name: String, spec: Seq[Spec]): Table

abstract class Table extends RowIterable:

  def name: String

  def addColumn(spec: ColumnSpec): Unit

  def rows: Int

  def row(idx: Int): Row

  def insert(row: Map[String, Value]): Map[String, Value]

  def bulkInsert(header: Seq[String], rows: Seq[Seq[Value]]): Unit

trait Spec
case class ColumnSpec(
    name: String,
    typ: Type,
    pk: Boolean = false,
    auto: Boolean = false,
    required: Boolean = false,
    indexed: Boolean = false,
    unique: Boolean = false,
    fk: Option[String] = None
) extends Spec

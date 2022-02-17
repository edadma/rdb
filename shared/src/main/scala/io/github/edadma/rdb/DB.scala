package io.github.edadma.rdb

import scala.collection.mutable

abstract class DB:

  def name: String

  def exists(name: String): Boolean

  def table(name: String): Option[Table]

  def create(name: String, meta: RowMeta): Table

abstract class Table:

  def name: String
  def meta: RowMeta

  def size: Int

  def row(idx: Int): Row

  def insert(row: Map[String, Value]): Map[String, Value]

  def bulkInsert(header: Seq[String], rows: Seq[Seq[Value]]): Unit

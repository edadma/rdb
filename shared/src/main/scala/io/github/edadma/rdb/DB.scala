package io.github.edadma.rdb

import scala.collection.mutable

abstract class DB:

  def name: String

  def exists(name: String): Boolean

  def table(name: String): Option[Table]

  def create(name: String, meta: TableMeta): Table

abstract class Table:

  def name: String
  def meta: TableMeta

  def size: Int

  def row(idx: Int): Seq[Value]

  def insert(row: Map[String, Value]): Map[String, Value]

  def bulkInsert(header: Seq[String], rows: Seq[Row]): Unit

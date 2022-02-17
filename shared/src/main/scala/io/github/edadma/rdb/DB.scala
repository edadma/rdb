package io.github.edadma.rdb

import scala.collection.mutable

abstract class DB:

  def name: String

  def exists(name: String): Boolean

  def table(name: String): Option[Table]

  def create(name: String, meta: TableMeta): Unit

abstract class Table:

  def name: String
  def meta: TableMeta

  def rows: Int

  def row(idx: Int): Seq[Value]

  def insert(row: Map[String, Value]): Map[String, Value]

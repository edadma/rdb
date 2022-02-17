package io.github.edadma.rdb

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer

class MemoryDB(val name: String) extends DB:

  private val tables = new mutable.HashMap[String, MemoryTable]

  def exists(name: String): Boolean = tables contains name

  def table(name: String): Option[Table] = tables get name

  def create(name: String, meta: TableMeta): Unit =
    if (tables contains name) sys.error(s"table '$name' already exists")
    else tables(name) = new MemoryTable(name, meta)

class MemoryTable(val name: String, val meta: TableMeta) extends Table:

  private val data = new ArrayBuffer[Array[Value]]

  def rows: Int = data.length

  def row(idx: Int): Seq[Value] = data(idx).toIndexedSeq

  def insert(row: Map[String, Value]): Map[String, Value] =
    val arr = new Array[Value](meta.cols)

    row.foreach {
      case (k, v) => arr(meta.columnIndices(k)) = v
    }

    data += arr
    Map.empty

package io.github.edadma.rdb

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class MemoryDB(val name: String) extends DB:

  private val tables = new mutable.HashMap[String, MemoryTable]

  def exists(name: String): Boolean = tables contains name

  def table(name: String): Option[Table] = tables get name

  def create(name: String, meta: TableMeta): Table =
    if (tables contains name) sys.error(s"table '$name' already exists")
    else
      val res = new MemoryTable(name, meta)

      tables(name) = res
      res

  override def toString: String = s"[Database '$name': ${tables map ((_, t) => t) mkString ", "}]"

class MemoryTable(val name: String, val meta: TableMeta) extends Table:

  private val data = new ArrayBuffer[Array[Value]]

  def size: Int = data.length

  def row(idx: Int): Seq[Value] = data(idx).toIndexedSeq

  def insert(row: Map[String, Value]): Map[String, Value] =
    val arr = new Array[Value](meta.cols)

    row foreach ((k, v) => arr(meta.columnIndices(k)) = v)
    data += arr
    Map.empty

  def bulkInsert(header: Seq[String], rows: Seq[Row]): Unit =
    val mapping = header map meta.columnIndices

    for (r <- rows)
      val arr = new Array[Value](meta.cols)

      for ((i, v) <- mapping zip r)
        arr(i) = v

      data += arr

  override def toString: String = s"[MemoryTable '$name': $meta; ${data map (_.toSeq)}]"

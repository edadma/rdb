package io.github.edadma.rdb

import io.github.edadma.bittydb.Connection

import scala.collection.immutable

class BMemDB extends DB:
  val name = "in-memory DB"
  val db: Connection = Connection.mem()

  protected def addTable(name: String, specs: Seq[Spec]) = new BMemTable(name, specs)

  protected def addEnum(name: String, labels: Seq[String]): Unit = {}

class BMemTable(name: String, specs: Seq[Spec]) extends Table(name, specs):
  private val data = new DLList[Array[Value]]

  protected def addColumn(spec: ColumnSpec): Unit = {}

  def iterator(ctx: Seq[Row]): RowIterator =
    data.nodeIterator map (n => Row(n.element to immutable.ArraySeq, meta, Some(updater(n.element)), Some(deleter(n))))

  protected def addRow(row: Seq[Value]): Unit = data += row.toArray

  class Updater private[MemoryTable] (row: Array[Value]) extends (Seq[(String, Value)] => Unit):
    def apply(update: Seq[(String, Value)]): Unit =
      for ((k, v) <- update)
        val col = columnMap.getOrElse(k, sys.error(s"table '$name' has no column '$k'"))
        val spec = columns(col)

        row(col) = spec.typ.convert(v)

    override def toString: String = "[MemoryDB Updater]"

  private def updater(row: Array[Value]) = new Updater(row)

  class Deleter private[MemoryTable] (node: data.Node) extends (() => Unit):
    def apply(): Unit = node.unlink

    override def toString: String = "[MemoryDB Deleter]"

  private def deleter(node: data.Node) = new Deleter(node)

  override def toString: String = s"[MemoryTable '$name': $meta; ${data map (_.toSeq)}]"

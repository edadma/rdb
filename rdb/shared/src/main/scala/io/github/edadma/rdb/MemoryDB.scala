package io.github.edadma.rdb

import io.github.edadma.dllist.{DLListNode, DLList}

import scala.collection.immutable

class MemoryDB extends DB:
  val name = "in-memory DB"

  protected def addTable(name: String, specs: Seq[Spec]) = new MemoryTable(name, specs)

  protected def addEnum(name: String, labels: Seq[String]): EnumType = EnumType(name, labels.toIndexedSeq)

class MemoryTable(name: String, specs: Seq[Spec]) extends Table(name, specs):
  private val data = new DLList[Array[Value]]

  protected def addColumn(spec: ColumnSpec): Unit = {}

  protected def addColumnData(defaultValue: Value): Unit =
    for (node <- data.nodeIterator)
      val old = node.element
      val arr = new Array[Value](old.length + 1)
      System.arraycopy(old, 0, arr, 0, old.length)
      arr(old.length) = defaultValue
      node.element = arr

  protected def dropColumnData(index: Int): Unit =
    for (node <- data.nodeIterator)
      val old = node.element
      val arr = new Array[Value](old.length - 1)
      System.arraycopy(old, 0, arr, 0, index)
      System.arraycopy(old, index + 1, arr, index, old.length - index - 1)
      node.element = arr

  protected def convertColumnData(index: Int, newType: Type): Unit =
    for (node <- data.nodeIterator)
      node.element(index) = newType.convert(node.element(index))

  protected def hasNullInColumn(index: Int): Boolean =
    data.nodeIterator.exists(_.element(index).isNull)

  def iterator(ctx: Seq[Row]): RowIterator =
    data.nodeIterator map (n => Row(n.element to immutable.ArraySeq, meta, Some(updater(n.element)), Some(deleter(n))))

  protected def addRow(row: Seq[Value]): Unit = data += row.toArray

  class Updater private[MemoryTable] (row: Array[Value]) extends (Seq[(String, Value)] => Unit):
    def apply(update: Seq[(String, Value)]): Unit =
      for ((k, v) <- update)
        val col  = columnMap.getOrElse(k, sys.error(s"table '$name' has no column '$k'"))
        val spec = columns(col)

        row(col) = spec.typ.convert(v)

    override def toString: String = "[MemoryDB Updater]"

  private def updater(row: Array[Value]) = new Updater(row)

  class Deleter private[MemoryTable] (node: DLListNode[Array[Value]]) extends (() => Unit):
    def apply(): Unit = node.unlink

    override def toString: String = "[MemoryDB Deleter]"

  private def deleter(node: DLListNode[Array[Value]]) = new Deleter(node)

  override def toString: String = s"[MemoryTable '$name': $meta; ${data map (_.toSeq)}]"

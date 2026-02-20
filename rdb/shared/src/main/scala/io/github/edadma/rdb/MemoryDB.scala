package io.github.edadma.rdb

import io.github.edadma.dllist.{DLListNode, DLList}
import io.github.edadma.bptree.MemoryBPlusTree

import scala.collection.immutable

class MemoryDB extends DB:
  val name = "in-memory DB"

  protected def addTable(name: String, specs: Seq[Spec]) = new MemoryTable(name, specs)

  protected def addEnum(name: String, labels: Seq[String]): EnumType = EnumType(name, labels.toIndexedSeq)

  override def createTable(name: String, specs: Seq[Spec]): Table =
    val table = super.createTable(name, specs)
    table.primaryKey.foreach { pk =>
      createIndex(s"${name}_pkey", name, pk.columns, unique = true)
    }
    table

  override def createIndex(indexName: String, tableName: String, columnNames: Seq[String], unique: Boolean): Unit =
    val table = tables(tableName).asInstanceOf[MemoryTable]
    val colIndices = columnNames.map(c => table.meta.columnMap(c)._1).toIndexedSeq

    given Ordering[IndexedSeq[Value]] = ValueSeqOrdering
    val tree = new MemoryBPlusTree[IndexedSeq[Value], DLListNode[Array[Value]]](50)

    var rowId = 0L
    for node <- table.data.nodeIterator do
      val baseKey = colIndices.map(i => node.element(i): Value)
      val key = if unique then baseKey else baseKey :+ NumberValue(rowId.toInt)
      if unique then
        if tree.insertIfNotFound(key, node) then
          sys.error(s"could not create unique index '$indexName': duplicate key found")
      else
        tree.insert(key, node)
      rowId += 1

    val meta = IndexMeta(indexName, tableName, columnNames, unique, nextRowId = rowId)
    val idx = MemoryTableIndex(meta, colIndices, tree, rowId)
    indexes(indexName) = meta
    table.tableIndexes(indexName) = idx

class MemoryTable(name: String, specs: Seq[Spec]) extends Table(name, specs):
  private[rdb] val data = new DLList[Array[Value]]

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
    data.nodeIterator map (n => Row(n.element to immutable.ArraySeq, meta, Some(updater(n)), Some(deleter(n))))

  protected def addRow(row: Seq[Value]): Unit =
    val arr = row.toArray
    val node = data.appendElement(arr)

    // Insert into all indexes, tracking what we've inserted for rollback on failure
    val inserted = new scala.collection.mutable.ArrayBuffer[(MemoryTableIndex, IndexedSeq[Value])]
    try
      for (idxName, idx) <- tableIndexes do
        val midx = idx.asInstanceOf[MemoryTableIndex]
        val baseKey = midx.columnIndices.map(i => arr(i): Value)
        if midx.meta.unique then
          if midx.tree.insertIfNotFound(baseKey, node) then
            sys.error(s"duplicate key value violates unique constraint \"${midx.meta.name}\"")
          else
            inserted += ((midx, baseKey))
        else
          val key = baseKey :+ NumberValue(midx.nextRowId.toInt)
          midx.nextRowId += 1
          midx.tree.insert(key, node)
          inserted += ((midx, key))
    catch
      case e: Exception =>
        // Roll back: remove entries from indexes we already inserted into, then unlink the row
        for (midx, key) <- inserted do
          midx.tree.delete(key)
        node.unlink
        throw e

  class Updater private[MemoryTable] (node: DLListNode[Array[Value]]) extends (Seq[(String, Value)] => Unit):
    def apply(update: Seq[(String, Value)]): Unit =
      val row = node.element

      // Save old values for rollback
      val oldValues = row.clone()

      // Remove old index entries
      val removedEntries = new scala.collection.mutable.ArrayBuffer[(MemoryTableIndex, IndexedSeq[Value], DLListNode[Array[Value]])]
      for (idxName, idx) <- tableIndexes do
        val midx = idx.asInstanceOf[MemoryTableIndex]
        val baseKey = midx.columnIndices.map(i => row(i): Value)
        if midx.meta.unique then
          midx.tree.delete(baseKey)
          removedEntries += ((midx, baseKey, node))
        else
          removeNonUniqueEntry(midx, baseKey, node)
          removedEntries += ((midx, baseKey, node))

      // Apply updates
      for ((k, v) <- update)
        val col  = columnMap.getOrElse(k, sys.error(s"table '$name' has no column '$k'"))
        val spec = columns(col)
        row(col) = spec.typ.convert(v)

      // Insert new index entries, rolling back on failure
      val inserted = new scala.collection.mutable.ArrayBuffer[(MemoryTableIndex, IndexedSeq[Value])]
      try
        for (idxName, idx) <- tableIndexes do
          val midx = idx.asInstanceOf[MemoryTableIndex]
          val newKey = midx.columnIndices.map(i => row(i): Value)
          if midx.meta.unique then
            if midx.tree.insertIfNotFound(newKey, node) then
              sys.error(s"duplicate key value violates unique constraint \"${midx.meta.name}\"")
            else
              inserted += ((midx, newKey))
          else
            val key = newKey :+ NumberValue(midx.nextRowId.toInt)
            midx.nextRowId += 1
            midx.tree.insert(key, node)
            inserted += ((midx, key))
      catch
        case e: Exception =>
          // Roll back new index entries
          for (midx, key) <- inserted do
            midx.tree.delete(key)
          // Restore old row values
          System.arraycopy(oldValues, 0, row, 0, oldValues.length)
          // Re-insert old index entries
          for (midx, oldKey, n) <- removedEntries do
            if midx.meta.unique then
              midx.tree.insert(oldKey, n)
            else
              val key = oldKey :+ NumberValue(midx.nextRowId.toInt)
              midx.nextRowId += 1
              midx.tree.insert(key, n)
          throw e

    override def toString: String = "[MemoryDB Updater]"

  private def updater(node: DLListNode[Array[Value]]) = new Updater(node)

  class Deleter private[MemoryTable] (node: DLListNode[Array[Value]]) extends (() => Unit):
    def apply(): Unit =
      val row = node.element

      // Remove from all indexes
      for (idxName, idx) <- tableIndexes do
        val midx = idx.asInstanceOf[MemoryTableIndex]
        val baseKey = midx.columnIndices.map(i => row(i): Value)
        if midx.meta.unique then
          midx.tree.delete(baseKey)
        else
          removeNonUniqueEntry(midx, baseKey, node)

      node.unlink

    override def toString: String = "[MemoryDB Deleter]"

  private def deleter(node: DLListNode[Array[Value]]) = new Deleter(node)

  private def removeNonUniqueEntry(midx: MemoryTableIndex, baseKey: IndexedSeq[Value], node: DLListNode[Array[Value]]): Unit =
    // For non-unique indexes, scan from the base key prefix to find the entry with this node
    import io.github.edadma.bptree.Bound
    val iter = midx.tree.boundedIterator((Bound.Gte, baseKey))
    var found = false
    while iter.hasNext && !found do
      val (k, v) = iter.next()
      // Check that the key prefix matches
      val prefix = k.take(baseKey.length)
      if ValueSeqOrdering.compare(prefix, baseKey) != 0 then
        found = true // went past the prefix range, entry not found (shouldn't happen)
      else if v eq node then
        midx.tree.delete(k)
        found = true

  override def toString: String = s"[MemoryTable '$name': $meta; ${data map (_.toSeq)}]"

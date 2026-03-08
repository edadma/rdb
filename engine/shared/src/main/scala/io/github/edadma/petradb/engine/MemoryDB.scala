package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import io.github.edadma.dllist.{DLListNode, DLList}
import io.github.edadma.bptree.{Bound, MemoryBPlusTree}

import scala.collection.immutable
import scala.collection.mutable

class MemoryDB extends DB:
  val name = "in-memory DB"

  // Undo log types
  private sealed trait UndoEntry
  private case class UndoInsert(table: MemoryTable, node: DLListNode[Array[Value]]) extends UndoEntry
  private case class UndoDelete(table: MemoryTable, data: Array[Value]) extends UndoEntry
  private case class UndoUpdate(table: MemoryTable, node: DLListNode[Array[Value]], oldData: Array[Value]) extends UndoEntry

  // The currently active undo log (set during DML execution within a transaction)
  private var currentUndoLog: Option[mutable.ArrayBuffer[UndoEntry]] = None
  private var activationDepth: Int = 0

  private class MemoryTransactionHandle(
      val undoLog: mutable.ArrayBuffer[UndoEntry],
  ) extends TransactionHandle

  override def snapshot(): TransactionHandle =
    new MemoryTransactionHandle(new mutable.ArrayBuffer[UndoEntry])

  override def commitSnapshot(handle: TransactionHandle): Unit = ()

  override def rollbackSnapshot(handle: TransactionHandle): Unit =
    val h = handle.asInstanceOf[MemoryTransactionHandle]
    val savedUndoLog = currentUndoLog
    currentUndoLog = None // Prevent recording undo entries during rollback

    // Replay undo log in reverse to undo this transaction's changes
    for entry <- h.undoLog.reverseIterator do
      entry match
        case UndoInsert(table, node) => table.undoInsert(node)
        case UndoDelete(table, data) => table.undoDelete(data)
        case UndoUpdate(table, node, oldData) => table.undoUpdate(node, oldData)

    currentUndoLog = savedUndoLog

  override def activateHandle(handle: TransactionHandle): Unit =
    activationDepth += 1
    if activationDepth == 1 then
      currentUndoLog = Some(handle.asInstanceOf[MemoryTransactionHandle].undoLog)

  override def deactivateHandle(): Unit =
    activationDepth -= 1
    if activationDepth == 0 then
      currentUndoLog = None

  // Called by MemoryTable after a successful insert
  private[engine] def recordInsert(table: MemoryTable, node: DLListNode[Array[Value]]): Unit =
    currentUndoLog.foreach(_ += UndoInsert(table, node))

  // Called by MemoryTable before a delete
  private[engine] def recordDelete(table: MemoryTable, data: Array[Value]): Unit =
    currentUndoLog.foreach(_ += UndoDelete(table, data))

  // Called by MemoryTable before an update
  private[engine] def recordUpdate(table: MemoryTable, node: DLListNode[Array[Value]], oldData: Array[Value]): Unit =
    currentUndoLog.foreach(_ += UndoUpdate(table, node, oldData))

  protected def addTable(name: String, specs: Seq[Spec]) = new MemoryTable(name, specs, this)

  protected def addEnum(name: String, labels: Seq[String]): EnumType = EnumType(name, labels.toIndexedSeq)

  override def createTable(name: String, specs: Seq[Spec]): Table =
    val table = super.createTable(name, specs)
    table.primaryKey.foreach { pk =>
      createIndex(s"${name}_pkey", name, pk.columns, unique = true)
    }
    for spec <- specs do
      spec match
        case UniqueSpec(cols, cname) =>
          val indexName = cname.getOrElse(s"${name}_${cols.mkString("_")}_key")
          createIndex(indexName, name, cols, unique = true)
        case cs: ColumnSpec if cs.unique =>
          createIndex(s"${name}_${cs.name}_key", name, Seq(cs.name), unique = true)
        case _ =>
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
    onMutation()

class MemoryTable(name: String, specs: Seq[Spec], private[engine] val db: MemoryDB) extends Table(name, specs):
  private[engine] val data = new DLList[Array[Value]]

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

  def truncate(): Unit =
    for node <- data.nodeIterator.toList do node.unlink
    autoMap.clear()
    for (idxName, idx) <- tableIndexes do
      given Ordering[IndexedSeq[Value]] = ValueSeqOrdering
      val midx = idx.asInstanceOf[MemoryTableIndex]
      val newTree = new MemoryBPlusTree[IndexedSeq[Value], DLListNode[Array[Value]]](50)
      tableIndexes(idxName) = MemoryTableIndex(midx.meta, midx.columnIndices, newTree, 0L)

  def iterator(ctx: Seq[Row]): RowIterator =
    data.nodeIterator map (n => Row(n.element to immutable.ArraySeq, meta, Some(updater(n)), Some(deleter(n))))

  private def nodeToRow(n: DLListNode[Array[Value]]): Row =
    Row(n.element to immutable.ArraySeq, meta, Some(updater(n)), Some(deleter(n)))

  override def indexPointScan(index: TableIndex, key: IndexedSeq[Value]): Option[RowIterator] =
    index match
      case midx: MemoryTableIndex =>
        if midx.meta.unique then
          if key.length == midx.meta.columns.length then
            // Full key: exact lookup
            Some(midx.tree.search(key).map(node => Iterator(nodeToRow(node))).getOrElse(Iterator.empty))
          else
            // Prefix key: bounded scan with prefix match
            val iter = midx.tree.boundedIterator((Bound.Gte, key))
            val matching = iter.takeWhile { case (k, _) =>
              val prefix = k.take(key.length)
              ValueSeqOrdering.compare(prefix, key) == 0
            }.map { case (_, node) => nodeToRow(node) }
            Some(matching)
        else
          val iter = midx.tree.boundedIterator((Bound.Gte, key))
          val matching = iter.takeWhile { case (k, _) =>
            val prefix = k.take(key.length)
            ValueSeqOrdering.compare(prefix, key) == 0
          }.map { case (_, node) => nodeToRow(node) }
          Some(matching)
      case _ => None

  override def indexRangeScan(index: TableIndex, lower: IndexedSeq[Value], upper: IndexedSeq[Value]): Option[RowIterator] =
    index match
      case midx: MemoryTableIndex =>
        if midx.meta.unique then
          val iter = midx.tree.boundedIterator((Bound.Gte, lower), (Bound.Lte, upper))
          Some(iter.map { case (_, node) => nodeToRow(node) })
        else
          val iter = midx.tree.boundedIterator((Bound.Gte, lower))
          val matching = iter.takeWhile { case (k, _) =>
            val prefix = k.take(upper.length)
            ValueSeqOrdering.compare(prefix, upper) <= 0
          }.map { case (_, node) => nodeToRow(node) }
          Some(matching)
      case _ => None

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
          if baseKey.exists(_.isNull) then
            // NULLs are always distinct for UNIQUE constraint purposes
            val key = baseKey :+ NumberValue(midx.nextRowId.toInt)
            midx.nextRowId += 1
            midx.tree.insert(key, node)
            inserted += ((midx, key))
          else if midx.tree.insertIfNotFound(baseKey, node) then
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

    // Record successful insert for transaction undo log
    db.recordInsert(this, node)
    db.onMutation()

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

      // Record successful update for transaction undo log
      db.recordUpdate(MemoryTable.this, node, oldValues)
      db.onMutation()

    override def toString: String = "[MemoryDB Updater]"

  private def updater(node: DLListNode[Array[Value]]) = new Updater(node)

  class Deleter private[MemoryTable] (node: DLListNode[Array[Value]]) extends (() => Unit):
    def apply(): Unit =
      val row = node.element
      val savedData = row.clone()

      // Remove from all indexes
      for (idxName, idx) <- tableIndexes do
        val midx = idx.asInstanceOf[MemoryTableIndex]
        val baseKey = midx.columnIndices.map(i => row(i): Value)
        if midx.meta.unique then
          midx.tree.delete(baseKey)
        else
          removeNonUniqueEntry(midx, baseKey, node)

      node.unlink

      // Record successful delete for transaction undo log
      db.recordDelete(MemoryTable.this, savedData)
      db.onMutation()

    override def toString: String = "[MemoryDB Deleter]"

  private def deleter(node: DLListNode[Array[Value]]) = new Deleter(node)

  // Undo methods for transaction rollback
  private[engine] def undoInsert(node: DLListNode[Array[Value]]): Unit =
    val row = node.element
    for (idxName, idx) <- tableIndexes do
      val midx = idx.asInstanceOf[MemoryTableIndex]
      val baseKey = midx.columnIndices.map(i => row(i): Value)
      if midx.meta.unique then
        midx.tree.delete(baseKey)
      else
        removeNonUniqueEntry(midx, baseKey, node)
    node.unlink

  private[engine] def undoDelete(rowData: Array[Value]): Unit =
    val node = data.appendElement(rowData)
    for (idxName, idx) <- tableIndexes do
      val midx = idx.asInstanceOf[MemoryTableIndex]
      val baseKey = midx.columnIndices.map(i => rowData(i): Value)
      if midx.meta.unique then
        midx.tree.insert(baseKey, node)
      else
        val key = baseKey :+ NumberValue(midx.nextRowId.toInt)
        midx.nextRowId += 1
        midx.tree.insert(key, node)

  private[engine] def undoUpdate(node: DLListNode[Array[Value]], oldData: Array[Value]): Unit =
    val currentRow = node.element
    // Remove current index entries
    for (idxName, idx) <- tableIndexes do
      val midx = idx.asInstanceOf[MemoryTableIndex]
      val baseKey = midx.columnIndices.map(i => currentRow(i): Value)
      if midx.meta.unique then
        midx.tree.delete(baseKey)
      else
        removeNonUniqueEntry(midx, baseKey, node)
    // Restore old data
    System.arraycopy(oldData, 0, node.element, 0, oldData.length)
    // Re-insert old index entries
    for (idxName, idx) <- tableIndexes do
      val midx = idx.asInstanceOf[MemoryTableIndex]
      val baseKey = midx.columnIndices.map(i => oldData(i): Value)
      if midx.meta.unique then
        midx.tree.insert(baseKey, node)
      else
        val key = baseKey :+ NumberValue(midx.nextRowId.toInt)
        midx.nextRowId += 1
        midx.tree.insert(key, node)

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

package io.github.edadma.rdb

import io.github.edadma.dllist.{DLListNode, DLList}
import io.github.edadma.bptree.{Bound, MemoryBPlusTree}

import scala.collection.immutable
import scala.collection.mutable

class MemoryDB extends DB:
  val name = "in-memory DB"

  // Transaction state
  private var _inTransaction = false
  private var _aborted = false
  private var snapshot: Option[MemorySnapshot] = None

  private case class TableSnapshot(
      rows: Seq[Array[Value]],        // deep-copied row arrays
      autoMap: Map[String, Value],
      indexNextRowIds: Map[String, Long],
  )

  private case class MemorySnapshot(
      tables: Map[String, TableSnapshot],
  )

  override def inTransaction: Boolean = _inTransaction
  override def isTransactionAborted: Boolean = _aborted
  override def markTransactionAborted(): Unit = _aborted = true

  override def beginTransaction(): Unit =
    if _inTransaction then sys.error("already in a transaction")
    _inTransaction = true
    _aborted = false

    // Snapshot all tables
    val tableSnapshots = tables.map { case (tname, t) =>
      val mt = t.asInstanceOf[MemoryTable]
      val rows = mt.data.nodeIterator.map(_.element.clone()).toSeq
      val autoState = mt.autoMap.toMap
      val indexRowIds = mt.tableIndexes.map { case (iname, idx) =>
        iname -> idx.asInstanceOf[MemoryTableIndex].nextRowId
      }.toMap
      tname -> TableSnapshot(rows, autoState, indexRowIds)
    }.toMap
    snapshot = Some(MemorySnapshot(tableSnapshots))

  override def commitTransaction(): Unit =
    if !_inTransaction then sys.error("no active transaction")
    _inTransaction = false
    _aborted = false
    snapshot = None

  override def rollbackTransaction(): Unit =
    if !_inTransaction then sys.error("no active transaction")
    _inTransaction = false
    _aborted = false

    for snap <- snapshot; (tname, ts) <- snap.tables; t <- tables.get(tname) do
      val mt = t.asInstanceOf[MemoryTable]

      // Restore data
      mt.data.clear()
      for row <- ts.rows do mt.data.appendElement(row.clone())

      // Restore auto-increment state
      mt.autoMap.clear()
      mt.autoMap ++= ts.autoMap

      // Rebuild all indexes from restored data with fresh trees
      given Ordering[IndexedSeq[Value]] = ValueSeqOrdering
      val idxEntries = mt.tableIndexes.toSeq
      for (idxName, idx) <- idxEntries do
        val midx = idx.asInstanceOf[MemoryTableIndex]
        val newTree = new MemoryBPlusTree[IndexedSeq[Value], DLListNode[Array[Value]]](50)

        var rowId = 0L
        for node <- mt.data.nodeIterator do
          val baseKey = midx.columnIndices.map(i => node.element(i): Value)
          if midx.meta.unique then
            newTree.insert(baseKey, node)
          else
            val key = baseKey :+ NumberValue(rowId.toInt)
            newTree.insert(key, node)
            rowId += 1

        val restoredRowId = ts.indexNextRowIds.getOrElse(idxName, 0L)
        val newIdx = MemoryTableIndex(midx.meta, midx.columnIndices, newTree, rowId.max(restoredRowId))
        mt.tableIndexes(idxName) = newIdx

    snapshot = None

  protected def addTable(name: String, specs: Seq[Spec]) = new MemoryTable(name, specs)

  protected def addEnum(name: String, labels: Seq[String]): EnumType = EnumType(name, labels.toIndexedSeq)

  override def createTable(name: String, specs: Seq[Spec]): Table =
    val table = super.createTable(name, specs)
    table.primaryKey.foreach { pk =>
      createIndex(s"${name}_pkey", name, pk.columns, unique = true)
    }
    table

  override def createIndex(indexName: String, tableName: String, columnNames: Seq[String], unique: Boolean): Unit =
    guardDDL()
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

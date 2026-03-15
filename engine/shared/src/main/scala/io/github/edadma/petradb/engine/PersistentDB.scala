package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import io.github.edadma.stow.{FilePageStore, PageId, NoPage, WriteBatch, Transaction}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

private case class PersistentSnapshot(
    catalog: CatalogSnapshot,
    tableState: Map[String, (PageId, PageId, Map[String, Value], Map[String, (TableIndex, Long)])],
    sequenceStateSnap: Map[String, (Long, Boolean)],
)

class PersistentDB private (val store: FilePageStore) extends DB:
  val name = "persistent DB"

  private var pendingTxnHandle: Option[PersistentTransactionHandle] = None
  private var activeTxn: Option[Transaction] = None

  private class PersistentTransactionHandle(val snap: PersistentSnapshot) extends TransactionHandle

  override def snapshot(): TransactionHandle =
    if pendingTxnHandle.isDefined then sys.error("PersistentDB supports only one active transaction at a time")
    val tableStatSnap = tables.map { (n, t) =>
      val pt = t.asInstanceOf[PersistentTable]
      val idxSnap = t.tableIndexes.map { (iName, idx) =>
        val pidx = idx.asInstanceOf[PersistentTableIndex]
        iName -> (idx, pidx.nextRowId)
      }.toMap
      n -> (pt.firstDataPage, pt.headerPage, pt.autoMap.toMap, idxSnap)
    }.toMap
    val seqSnap = sequences.map { (n, s) => n -> s.stateSnapshot() }.toMap
    val snap = PersistentSnapshot(
      catalog = takeCatalogSnapshot(),
      tableState = tableStatSnap,
      sequenceStateSnap = seqSnap,
    )
    val handle = new PersistentTransactionHandle(snap)
    pendingTxnHandle = Some(handle)
    handle

  private def ensureStowTransaction(): Transaction =
    activeTxn match
      case Some(txn) => txn
      case None =>
        val txn = store.beginTransaction()
        activeTxn = Some(txn)
        txn

  override def commitSnapshot(handle: TransactionHandle): Unit =
    activeTxn.foreach(_.commit())
    activeTxn = None
    pendingTxnHandle = None

  override def rollbackSnapshot(handle: TransactionHandle): Unit =
    val h = handle.asInstanceOf[PersistentTransactionHandle]
    activeTxn.foreach(_.rollback())
    activeTxn = None

    val snap = h.snap
    restoreCatalog(snap.catalog)

    // Restore per-table mutable state
    for (n, (fdp, hp, autoState, idxSnap)) <- snap.tableState do
      tables.get(n).foreach { t =>
        val pt = t.asInstanceOf[PersistentTable]
        pt.firstDataPage = fdp
        pt.headerPage = hp
        pt.autoMap.clear()
        pt.autoMap ++= autoState
        t.tableIndexes.clear()
        for (iName, (idx, nextRowId)) <- idxSnap do
          val pidx = idx.asInstanceOf[PersistentTableIndex]
          pidx.nextRowId = nextRowId
          t.tableIndexes(iName) = idx
      }

    // Restore sequence state
    for (n, statSnap) <- snap.sequenceStateSnap do
      sequences.get(n).foreach(_.restoreState(statSnap))

    // Restore backing sequence references for tables
    for (_, t) <- tables do
      t.backingSequences.clear()
    for (seqName, seq) <- sequences do
      for
        tableName <- seq.ownedByTable
        colName <- seq.ownedByColumn
        table <- tables.get(resolveKey(tableName))
      do table.backingSequences(colName) = seq

    pendingTxnHandle = None

  private[engine] def withBatch(fn: WriteBatch => Unit): Unit =
    pendingTxnHandle match
      case Some(_) => fn(ensureStowTransaction())
      case None    => store.modify(fn)

  private[engine] def readPage(id: PageId): Array[Byte] =
    activeTxn match
      case Some(txn) => txn.read(id)
      case None      => store.read(id)

  protected def addTable(name: String, specs: Seq[Spec]): Table =
    new PersistentTable(name, specs, store, this)

  override def createTable(name: String, specs: Seq[Spec]): Table =
    val table = super.createTable(name, specs)
    withBatch { batch =>
      val pt = table.asInstanceOf[PersistentTable]
      pt.headerPage = batch.allocate()
      pt.writeHeaderPage(batch)
      table.primaryKey.foreach { pk =>
        createPersistentIndex(s"${name}_pkey", name, pk.columns, unique = true, batch)
      }
      for spec <- specs do
        spec match
          case UniqueSpec(cols, cname) =>
            val indexName = cname.getOrElse(s"${table.name}_${cols.mkString("_")}_key")
            createPersistentIndex(indexName, table.name, cols, unique = true, batch)
          case cs: ColumnSpec if cs.unique =>
            createPersistentIndex(s"${table.name}_${cs.name}_key", table.name, Seq(cs.name), unique = true, batch)
          case _ =>
      writeCatalogInBatch(batch)
    }
    table

  protected def addEnum(name: String, labels: Seq[String]): EnumType =
    val e = EnumType(name, labels.toIndexedSeq)
    e

  override def createEnum(name: String, labels: Seq[String]): Unit =
    super.createEnum(name, labels)
    persistCatalog()

  override def dropTable(name: String): Unit =
    // Free all data pages and header page for this table
    tables.get(resolveKey(name)).foreach { table =>
      val pt = table.asInstanceOf[PersistentTable]
      pt.freeAllPages()
    }
    super.dropTable(name)
    persistCatalog()

  override def renameTable(oldName: String, newName: String): Unit =
    super.renameTable(oldName, newName)
    persistCatalog()

  override def dropType(name: String): Unit =
    super.dropType(name)
    persistCatalog()

  override def createIndex(indexName: String, tableName: String, columnNames: Seq[String], unique: Boolean, whereExpr: Option[Expr] = None, exprKeys: Option[Seq[Expr]] = None): Unit =
    withBatch { batch =>
      createPersistentIndex(indexName, tableName, columnNames, unique, batch)
      writeCatalogInBatch(batch)
    }

  override def dropIndex(indexName: String): Unit =
    super.dropIndex(indexName)
    persistCatalog()

  override def alterTable(name: String, alteration: TableAlteration)(using Session): Unit =
    super.alterTable(name, alteration)
    alteration match
      case _: RenameTableAlteration => () // renameTable already called persistCatalog
      case _                        => persistCatalog()

  override def createView(name: String, sql: String, orReplace: Boolean): Unit =
    super.createView(name, sql, orReplace)
    persistCatalog()

  override def dropView(name: String): Unit =
    super.dropView(name)
    persistCatalog()

  override def createSequence(
      name: String,
      increment: Long,
      minValue: Long,
      maxValue: Long,
      startValue: Option[Long],
      cycle: Boolean,
      ownedByTable: Option[String] = None,
      ownedByColumn: Option[String] = None,
  ): Sequence =
    val seq = super.createSequence(name, increment, minValue, maxValue, startValue, cycle, ownedByTable, ownedByColumn)
    persistCatalog()
    seq

  override def dropSequence(name: String): Unit =
    super.dropSequence(name)
    persistCatalog()

  private def createPersistentIndex(indexName: String, tableName: String, columnNames: Seq[String], unique: Boolean, batch: WriteBatch): Unit =
    val table = tables(resolveKey(tableName))
    val colIndices = columnNames.map(c => table.meta.columnMap(c)._1).toIndexedSeq

    given Ordering[IndexedSeq[Value]] = ValueSeqOrdering
    val keyCodec = StowCodec.valueSeq(using batch, store.pageSize)
    val tree = StowBPlusTree.create[IndexedSeq[Value], (PageId, Int)](
      50, store, batch, keyCodec, StowCodec.pageIdPair,
    )

    // Populate from existing data
    val pt = table.asInstanceOf[PersistentTable]
    val enumTypes = types.collect { case (n, e: EnumType) => (n, e) }.toMap
    var rowId = 0L
    var currentPageId = pt.firstDataPage

    while currentPageId != NoPage do
      val pageData = batch.read(currentPageId)
      val page = SlottedPage.wrap(pageData)
      for i <- 0 until page.slotCount do
        page.getSlot(i).foreach { slotData =>
          val (values, _) = deserializeRow(slotData, table.columns.length, store, enumTypes)
          val baseKey = colIndices.map(values(_))
          val key = if unique then baseKey else baseKey :+ NumberValue(rowId.toInt)
          if unique then
            if tree.insertIfNotFound(key, (currentPageId, i)) then
              sys.error(s"could not create unique index '$indexName': duplicate key found")
          else
            tree.insert(key, (currentPageId, i))
          rowId += 1
        }
      currentPageId = page.nextPage

    val meta = IndexMeta(indexName, resolveKey(tableName), columnNames, unique, tree.treeRecordPage, rowId)
    val idx = PersistentTableIndex(meta, colIndices, rowId)
    indexes(indexName) = meta
    table.tableIndexes(indexName) = idx

  private[engine] def persistCatalog(): Unit =
    withBatch { batch =>
      writeCatalogInBatch(batch)
    }

  private[engine] def writeCatalogInBatch(batch: WriteBatch): Unit =
    // Free old catalog chain
    val oldMeta = store.metaRoot
    if oldMeta != NoPage then
      freeChain(oldMeta, batch, store.pageSize)

    // Build catalog entries
    val entries = tables.map { (tName, table) =>
      val pt = table.asInstanceOf[PersistentTable]
      CatalogTableEntry(
        name = tName,
        headerPage = pt.headerPage,
        columns = pt.columns.toSeq,
        primaryKey = pt.primaryKey,
        constraints = pt.constraints.toSeq,
      )
    }

    val indexEntries = indexes.map { (_, meta) =>
      CatalogIndexEntry(meta.name, meta.tableName, meta.columns, meta.unique, meta.treeRecordPage, meta.nextRowId)
    }

    val seqEntries = sequences.values.map { s =>
      CatalogSequenceEntry(s.name, s.currentValue, s.increment, s.minValue, s.maxValue, s.startValue, s.cycle, s.called, s.ownedByTable, s.ownedByColumn)
    }
    val catalogBytes = serializeCatalog(types.toMap, entries, indexEntries, batch, store.pageSize, views.toSeq, seqEntries,
      storedFunctions.map((n, f) => (n, f.source)).toSeq, storedProcedures.map((n, p) => (n, p.source)).toSeq)
    val newRoot      = writeChain(catalogBytes, batch, store.pageSize)
    batch.setMetaRoot(newRoot)

  override def close(): Unit = store.close()

  // Restore from existing catalog on open
  private def restoreFromCatalog(): Unit =
    val metaRoot = store.metaRoot
    if metaRoot == NoPage then return

    // Read catalog chain — we need to figure out the total length
    // Read the catalog data using a page-walking approach
    val catalogBytes = readCatalogChain(metaRoot)
    val (enums, tableEntries, indexEntries, viewEntries, seqEntries, funcEntries, procEntries) = deserializeCatalog(catalogBytes, store)

    // Restore enum types
    for (eName, eType) <- enums do types(eName) = eType

    // Restore views
    for (vName, vSql) <- viewEntries do views(vName) = vSql

    // Restore sequences
    for entry <- seqEntries do
      val seq = new Sequence(entry.name, entry.currentValue, entry.increment, entry.minValue, entry.maxValue, entry.startValue, entry.cycle, entry.called, entry.ownedByTable, entry.ownedByColumn)
      sequences(entry.name) = seq

    // Restore tables
    for entry <- tableEntries do
      val allSpecs: Seq[Spec] = entry.columns ++ entry.constraints
      val table = new PersistentTable(entry.name, allSpecs, store, this)
      table.headerPage = entry.headerPage
      val (fdp, autoState) = deserializeTableHeader(store.read(entry.headerPage), store)
      table.firstDataPage = fdp
      table.restoreAutoState(autoState)
      tables(entry.name) = table
      // Restore backing sequence references for SERIAL columns
      for seq <- sequences.values if seq.ownedByTable.map(resolveKey).contains(entry.name) do
        seq.ownedByColumn.foreach(col => table.backingSequences(col) = seq)

    // Restore indexes
    for entry <- indexEntries do
      val meta = IndexMeta(entry.name, entry.tableName, entry.columns, entry.unique, entry.treeRecordPage, entry.nextRowId)
      indexes(entry.name) = meta
      tables.get(entry.tableName).foreach { table =>
        val colIndices = entry.columns.map(c => table.meta.columnMap(c)._1).toIndexedSeq
        table.tableIndexes(entry.name) = PersistentTableIndex(meta, colIndices, entry.nextRowId)
      }

    // Restore stored routines by re-executing their source SQL
    given session: Session = connect()
    for (_, source) <- funcEntries do
      try executeSQL(source + ";")(using session)
      catch case _: Exception => ()
    for (_, source) <- procEntries do
      try executeSQL(source + ";")(using session)
      catch case _: Exception => ()

  private def readCatalogChain(firstPage: PageId): Array[Byte] =
    // First pass: count total bytes
    val payloadPerPage = store.pageSize - 4
    var currentPage    = firstPage
    var totalLength    = 0
    val pageList       = new ArrayBuffer[PageId]

    while currentPage != NoPage do
      pageList += currentPage
      val buf = store.read(currentPage)
      currentPage = ((buf(0) & 0xff) << 24) | ((buf(1) & 0xff) << 16) | ((buf(2) & 0xff) << 8) | (buf(3) & 0xff)

    totalLength = (pageList.length - 1) * payloadPerPage + payloadPerPage // max estimate

    // Second pass: read all data
    val result = new Array[Byte](pageList.length * payloadPerPage)
    var offset = 0
    for pid <- pageList do
      val buf       = store.read(pid)
      val chunkSize = payloadPerPage
      System.arraycopy(buf, 4, result, offset, chunkSize)
      offset += chunkSize

    result

object PersistentDB:
  def create(path: String, pageSize: Int): PersistentDB =
    val store = FilePageStore.create(path, pageSize)
    new PersistentDB(store)

  def open(path: String): PersistentDB =
    val store = FilePageStore.open(path)
    val db    = new PersistentDB(store)
    db.restoreFromCatalog()
    db

class PersistentTable(
    name: String,
    specs: Seq[Spec],
    private val store: FilePageStore,
    private val db: PersistentDB,
) extends Table(name, specs):

  var firstDataPage: PageId = NoPage
  var headerPage: PageId    = NoPage

  def writeHeaderPage(batch: WriteBatch): Unit =
    batch.write(headerPage, serializeTableHeader(firstDataPage, autoMap.toMap, batch, store.pageSize))

  protected def addColumn(spec: ColumnSpec): Unit = {} // handled by catalog persistence

  private def openIndexTree(pidx: PersistentTableIndex, batch: WriteBatch): StowBPlusTree[IndexedSeq[Value], (PageId, Int)] =
    given Ordering[IndexedSeq[Value]] = ValueSeqOrdering
    val keyCodec = StowCodec.valueSeq(using batch, store.pageSize)
    StowBPlusTree.open[IndexedSeq[Value], (PageId, Int)](
      50, store, batch, pidx.meta.treeRecordPage, keyCodec, StowCodec.pageIdPair,
    )

  private type TreeCache = Map[String, StowBPlusTree[IndexedSeq[Value], (PageId, Int)]]

  private def openIndexTrees(batch: WriteBatch): TreeCache =
    tableIndexes.map { (idxName, idx) =>
      idxName -> openIndexTree(idx.asInstanceOf[PersistentTableIndex], batch)
    }.toMap

  private def addRowInBatch(row: Seq[Value], batch: WriteBatch, trees: TreeCache): Unit =
    val rowBytes = serializeRow(row, batch, store.pageSize)
    val (insertedPageId, insertedSlotIndex) = insertRowBytes(rowBytes, batch)

    for (idxName, idx) <- tableIndexes do
      val pidx = idx.asInstanceOf[PersistentTableIndex]
      val tree = trees(idxName)
      val baseKey = pidx.columnIndices.map(i => row(i))
      if pidx.meta.unique then
        if tree.insertIfNotFound(baseKey, (insertedPageId, insertedSlotIndex)) then
          sys.error(s"duplicate key value violates unique constraint \"${pidx.meta.name}\"")
      else
        val key = baseKey :+ NumberValue(pidx.nextRowId.toInt)
        pidx.nextRowId += 1
        tree.insert(key, (insertedPageId, insertedSlotIndex))

  protected def addRow(row: Seq[Value]): Unit =
    bulkBatch match
      case Some((batch, trees)) =>
        addRowInBatch(row, batch, trees)
      case None =>
        db.withBatch { batch =>
          val oldFirstDataPage = firstDataPage
          addRowInBatch(row, batch, openIndexTrees(batch))
          if autoMap.nonEmpty || firstDataPage != oldFirstDataPage then
            writeHeaderPage(batch)
          if backingSequences.nonEmpty then
            db.writeCatalogInBatch(batch)
        }

  override def bulkInsert(header: Seq[String], rows: Seq[Seq[Value]], returning: Option[Seq[String]], fkCheck: Option[IndexedSeq[Value] => Unit] = None): Map[String, Value] =
    if rows.size <= 1 then return super.bulkInsert(header, rows, returning, fkCheck)
    var result: Map[String, Value] = Map.empty
    db.withBatch { batch =>
      val oldFirstDataPage = firstDataPage
      val trees = openIndexTrees(batch)
      // Delegate row preparation to super, but intercept addRow via bulkBatch state
      bulkBatch = Some((batch, trees))
      try result = super.bulkInsert(header, rows, returning, fkCheck)
      finally bulkBatch = None
      if autoMap.nonEmpty || firstDataPage != oldFirstDataPage then
        writeHeaderPage(batch)
      if backingSequences.nonEmpty then
        db.writeCatalogInBatch(batch)
    }
    result

  // When set, addRow reuses this batch+trees instead of opening its own
  private var bulkBatch: Option[(WriteBatch, TreeCache)] = None

  private def insertRowBytes(rowBytes: Array[Byte], batch: WriteBatch): (PageId, Int) =
    // Try to find a data page with room
    var prevPageId: PageId = NoPage
    var currentPageId      = firstDataPage

    while currentPageId != NoPage do
      val pageData = batch.read(currentPageId)
      val page     = SlottedPage.wrap(pageData)
      if page.addSlot(rowBytes) then
        val slotIdx = page.slotCount - 1
        batch.write(currentPageId, page.data)
        return (currentPageId, slotIdx)
      prevPageId = currentPageId
      currentPageId = page.nextPage

    // No room — allocate a new data page
    val newPageId = batch.allocate()
    val newPage   = SlottedPage.create(store.pageSize)
    require(newPage.addSlot(rowBytes), s"row too large for page (${rowBytes.length} bytes, page size ${store.pageSize})")

    if firstDataPage == NoPage then
      firstDataPage = newPageId
    else
      // Link from previous last page
      val prevData = batch.read(prevPageId)
      val prevPage = SlottedPage.wrap(prevData)
      prevPage.setNextPage(newPageId)
      batch.write(prevPageId, prevPage.data)

    batch.write(newPageId, newPage.data)
    (newPageId, 0)

  def iterator(ctx: Seq[Row]): RowIterator =
    // Collect all rows from all data pages
    val rows = new ArrayBuffer[Row]
    var currentPageId = firstDataPage

    while currentPageId != NoPage do
      val pageData = db.readPage(currentPageId)
      val page     = SlottedPage.wrap(pageData)
      val pid      = currentPageId

      for i <- 0 until page.slotCount do
        page.getSlot(i) match
          case Some(slotData) =>
            val enumTypes = db.types.collect { case (n, e: EnumType) => (n, e) }.toMap
            val (values, chains) = deserializeRow(slotData, columns.length, store, enumTypes)
            val slotIndex = i
            val capturedPageId = pid
            rows += Row(
              values,
              meta,
              Some(makeUpdater(capturedPageId, slotIndex, chains)),
              Some(makeDeleter(capturedPageId, slotIndex, chains)),
            )
          case None => // tombstone, skip

      currentPageId = page.nextPage

    rows.iterator

  private def makeUpdater(pageId: PageId, slotIndex: Int, oldChains: IndexedSeq[Option[ChainRef]]): Seq[(String, Value)] => Unit =
    (updates: Seq[(String, Value)]) =>
      db.withBatch { batch =>
        // Read current row data
        val pageData = batch.read(pageId)
        val page     = SlottedPage.wrap(pageData)
        val enumTypes = db.types.collect { case (n, e: EnumType) => (n, e) }.toMap
        val currentSlotData = page.getSlot(slotIndex).getOrElse(sys.error("slot is tombstone during update"))
        val (currentValues, currentChains) = deserializeRow(currentSlotData, columns.length, store, enumTypes)

        // Remove old index entries
        for (idxName, idx) <- tableIndexes do
          val pidx = idx.asInstanceOf[PersistentTableIndex]
          val tree = openIndexTree(pidx, batch)
          if pidx.meta.unique then
            val oldKey = pidx.columnIndices.map(currentValues(_))
            tree.delete(oldKey)
          else
            removeNonUniqueEntry(pidx, batch, currentValues, pageId, slotIndex)

        val updatedValues = currentValues.toArray

        // Apply updates
        for (k, v) <- updates do
          val col  = columnMap.getOrElse(k, sys.error(s"table '$name' has no column '$k'"))
          val spec = columns(col)
          updatedValues(col) = spec.typ.convert(v)

        // Free old chains for columns being updated
        for (k, _) <- updates do
          val col = columnMap(k)
          currentChains(col).foreach { ref =>
            freeChain(ref.firstPage, batch, store.pageSize)
          }

        // Serialize new row
        val newRowBytes = serializeRow(updatedValues.toIndexedSeq, batch, store.pageSize)

        // Write row (may move to new location)
        val (newPageId, newSlotIndex) =
          if page.updateSlot(slotIndex, newRowBytes) then
            batch.write(pageId, page.data)
            (pageId, slotIndex)
          else
            page.removeSlot(slotIndex)
            batch.write(pageId, page.data)
            insertRowBytes(newRowBytes, batch)

        // Insert new index entries
        for (idxName, idx) <- tableIndexes do
          val pidx = idx.asInstanceOf[PersistentTableIndex]
          val tree = openIndexTree(pidx, batch)
          val newKey = pidx.columnIndices.map(i => updatedValues(i): Value)
          if pidx.meta.unique then
            if tree.insertIfNotFound(newKey, (newPageId, newSlotIndex)) then
              sys.error(s"duplicate key value violates unique constraint \"${pidx.meta.name}\"")
          else
            val key = newKey :+ NumberValue(pidx.nextRowId.toInt)
            pidx.nextRowId += 1
            tree.insert(key, (newPageId, newSlotIndex))
      }

  private def makeDeleter(pageId: PageId, slotIndex: Int, chains: IndexedSeq[Option[ChainRef]]): () => Unit =
    () =>
      db.withBatch { batch =>
        // Remove from all indexes before deleting the row
        if tableIndexes.nonEmpty then
          val enumTypes = db.types.collect { case (n, e: EnumType) => (n, e) }.toMap
          val pageData = batch.read(pageId)
          val page     = SlottedPage.wrap(pageData)
          page.getSlot(slotIndex).foreach { slotData =>
            val (values, _) = deserializeRow(slotData, columns.length, store, enumTypes)
            for (idxName, idx) <- tableIndexes do
              val pidx = idx.asInstanceOf[PersistentTableIndex]
              val tree = openIndexTree(pidx, batch)
              if pidx.meta.unique then
                val key = pidx.columnIndices.map(values(_))
                tree.delete(key)
              else
                removeNonUniqueEntry(pidx, batch, values, pageId, slotIndex)
          }

        // Free chain pages
        for chainOpt <- chains; ref <- chainOpt do
          freeChain(ref.firstPage, batch, store.pageSize)

        val pageData2 = batch.read(pageId)
        val page2     = SlottedPage.wrap(pageData2)
        page2.removeSlot(slotIndex)
        batch.write(pageId, page2.data)
      }

  private def removeNonUniqueEntry(pidx: PersistentTableIndex, batch: WriteBatch, values: IndexedSeq[Value], targetPageId: PageId, targetSlotIndex: Int): Unit =
    import io.github.edadma.bptree.Bound
    val tree = openIndexTree(pidx, batch)
    val baseKey = pidx.columnIndices.map(values(_))
    val iter = tree.boundedIterator((Bound.Gte, baseKey))
    var found = false
    while iter.hasNext && !found do
      val (k, v) = iter.next()
      val prefix = k.take(baseKey.length)
      if ValueSeqOrdering.compare(prefix, baseKey) != 0 then
        found = true // went past the prefix range
      else if v == (targetPageId, targetSlotIndex) then
        tree.delete(k)
        found = true

  protected def addColumnData(defaultValue: Value): Unit =
    // columns.length is now N+1 (createColumn already called), but old rows have N columns
    rewriteAllRows(columns.length - 1) { values =>
      values :+ defaultValue
    }

  protected def dropColumnData(index: Int): Unit =
    // columns.length is still N, rows have N columns
    rewriteAllRows(columns.length) { values =>
      values.patch(index, Nil, 1)
    }

  protected def convertColumnData(index: Int, newType: Type): Unit =
    rewriteAllRows(columns.length) { values =>
      values.updated(index, newType.convert(values(index)))
    }

  protected def hasNullInColumn(index: Int): Boolean =
    import scala.util.boundary, boundary.break
    val enumTypes = db.types.collect { case (n, e: EnumType) => (n, e) }.toMap

    boundary:
      var currentPageId = firstDataPage
      while currentPageId != NoPage do
        val pageData = store.read(currentPageId)
        val page     = SlottedPage.wrap(pageData)

        for i <- 0 until page.slotCount do
          page.getSlot(i).foreach { slotData =>
            val (values, _) = deserializeRow(slotData, columns.length, store, enumTypes)
            if values(index).isNull then break(true)
          }

        currentPageId = page.nextPage

      false

  private def rewriteAllRows(oldColCount: Int)(transform: IndexedSeq[Value] => IndexedSeq[Value]): Unit =
    // Read all rows
    val allRows = new ArrayBuffer[IndexedSeq[Value]]
    val enumTypes = db.types.collect { case (n, e: EnumType) => (n, e) }.toMap
    var currentPageId = firstDataPage

    while currentPageId != NoPage do
      val pageData = store.read(currentPageId)
      val page     = SlottedPage.wrap(pageData)
      for i <- 0 until page.slotCount do
        page.getSlot(i).foreach { slotData =>
          val (values, _) = deserializeRow(slotData, oldColCount, store, enumTypes)
          allRows += values
        }
      currentPageId = page.nextPage

    db.withBatch { batch =>
      // Free all old data pages and their chains
      freeAllDataPages(batch)

      // Rewrite all rows with transformation
      for values <- allRows do
        val transformed = transform(values)
        val rowBytes    = serializeRow(transformed, batch, store.pageSize)
        insertRowBytes(rowBytes, batch)

      writeHeaderPage(batch)
    }

  private[engine] def freeAllPages(): Unit =
    db.withBatch { batch =>
      freeAllDataPages(batch)
      if headerPage != NoPage then
        batch.free(headerPage)
        headerPage = NoPage
    }

  private def freeAllDataPages(batch: WriteBatch): Unit =
    val enumTypes = db.types.collect { case (n, e: EnumType) => (n, e) }.toMap
    var currentPageId = firstDataPage

    while currentPageId != NoPage do
      val pageData = batch.read(currentPageId)
      val page     = SlottedPage.wrap(pageData)

      // Free chain pages for all non-tombstone slots
      for i <- 0 until page.slotCount do
        page.getSlot(i).foreach { slotData =>
          try
            val (_, chains) = deserializeRow(slotData, columns.length, store, enumTypes)
            for chainOpt <- chains; ref <- chainOpt do
              freeChain(ref.firstPage, batch, store.pageSize)
          catch case _: Exception => () // ignore deserialization errors during cleanup
        }

      val nextPageId = page.nextPage
      batch.free(currentPageId)
      currentPageId = nextPageId

    firstDataPage = NoPage

  def truncate(): Unit =
    db.withBatch { batch =>
      freeAllDataPages(batch)
      // Clear and rebuild index trees
      for (idxName, idx) <- tableIndexes do
        val pidx = idx.asInstanceOf[PersistentTableIndex]
        given Ordering[IndexedSeq[Value]] = ValueSeqOrdering
        val keyCodec = StowCodec.valueSeq(using batch, store.pageSize)
        val tree = openIndexTree(pidx, batch)
        // Delete all entries by iterating
        val allKeys = tree.iterator.map(_._1).toVector
        for key <- allKeys do tree.delete(key)
        tableIndexes(idxName) = PersistentTableIndex(pidx.meta.copy(nextRowId = 0), pidx.columnIndices, 0L)
      autoMap.clear()
      // Reset backing sequences to their start values
      for (_, seq) <- backingSequences do
        seq.currentValue = 0
        seq.called = false
      writeHeaderPage(batch)
      if backingSequences.nonEmpty then
        db.writeCatalogInBatch(batch)
    }


  override def toString: String = s"[PersistentTable '$name': $meta]"

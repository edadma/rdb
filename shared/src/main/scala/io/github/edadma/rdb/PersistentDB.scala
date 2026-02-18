package io.github.edadma.rdb

import io.github.edadma.stow.{FilePageStore, PageId, NoPage, PageStore, WriteBatch}

import scala.collection.immutable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class PersistentDB private (val store: FilePageStore) extends DB:
  val name = "persistent DB"

  // Table first-page tracking (needed for catalog)
  private[rdb] val tableFirstPages = new mutable.HashMap[String, PageId]

  protected def addTable(name: String, specs: Seq[Spec]): Table =
    val table = new PersistentTable(name, specs, store, this)
    tableFirstPages(name) = NoPage
    table

  override def createTable(name: String, specs: Seq[Spec]): Table =
    val table = super.createTable(name, specs)
    persistCatalog()
    table

  protected def addEnum(name: String, labels: Seq[String]): EnumType =
    val e = EnumType(name, labels.toIndexedSeq)
    e

  override def createEnum(name: String, labels: Seq[String]): Unit =
    super.createEnum(name, labels)
    persistCatalog()

  override def dropTable(name: String): Unit =
    // Free all data pages for this table
    tables.get(name).foreach { table =>
      val pt = table.asInstanceOf[PersistentTable]
      pt.freeAllPages()
    }
    tableFirstPages.remove(name)
    super.dropTable(name)
    persistCatalog()

  override def dropType(name: String): Unit =
    super.dropType(name)
    persistCatalog()

  private[rdb] def persistCatalog(): Unit =
    store.modify { batch =>
      writeCatalogInBatch(batch)
    }

  private[rdb] def writeCatalogInBatch(batch: WriteBatch): Unit =
    // Free old catalog chain
    val oldMeta = store.metaRoot
    if oldMeta != NoPage then
      freeChain(oldMeta, batch, store.pageSize)

    // Build catalog entries
    val entries = tables.map { (tName, table) =>
      val pt = table.asInstanceOf[PersistentTable]
      CatalogTableEntry(
        name = tName,
        firstDataPage = pt.firstDataPage,
        columns = pt.columns.toSeq,
        primaryKey = pt.primaryKey,
        constraints = pt.constraints.toSeq,
        autoState = pt.autoMap.toMap,
      )
    }

    val catalogBytes = serializeCatalog(types.toMap, entries, batch, store.pageSize)
    val newRoot      = writeChain(catalogBytes, batch, store.pageSize)
    batch.setMetaRoot(newRoot)

  def close(): Unit = store.close()

  // Restore from existing catalog on open
  private def restoreFromCatalog(): Unit =
    val metaRoot = store.metaRoot
    if metaRoot == NoPage then return

    // Read catalog chain — we need to figure out the total length
    // Read the catalog data using a page-walking approach
    val catalogBytes = readCatalogChain(metaRoot)
    val (enums, tableEntries) = deserializeCatalog(catalogBytes, store)

    // Restore enum types
    for (eName, eType) <- enums do types(eName) = eType

    // Restore tables
    for entry <- tableEntries do
      val allSpecs: Seq[Spec] = entry.columns ++ entry.constraints
      val table = new PersistentTable(entry.name, allSpecs, store, this)
      table.firstDataPage = entry.firstDataPage
      table.restoreAutoState(entry.autoState)
      tables(entry.name) = table
      tableFirstPages(entry.name) = entry.firstDataPage

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

  protected def addColumn(spec: ColumnSpec): Unit = {} // handled by catalog persistence

  protected def addRow(row: Seq[Value]): Unit =
    store.modify { batch =>
      val rowBytes = serializeRow(row, batch, store.pageSize)
      insertRowBytes(rowBytes, batch)
      db.writeCatalogInBatch(batch)
    }

  private def insertRowBytes(rowBytes: Array[Byte], batch: WriteBatch): Unit =
    // Try to find a data page with room
    var prevPageId: PageId = NoPage
    var currentPageId      = firstDataPage

    while currentPageId != NoPage do
      val pageData = batch.read(currentPageId)
      val page     = SlottedPage.wrap(pageData)
      if page.addSlot(rowBytes) then
        batch.write(currentPageId, page.data)
        return
      prevPageId = currentPageId
      currentPageId = page.nextPage

    // No room — allocate a new data page
    val newPageId = batch.allocate()
    val newPage   = SlottedPage.create(store.pageSize)
    require(newPage.addSlot(rowBytes), s"row too large for page (${rowBytes.length} bytes, page size ${store.pageSize})")

    if firstDataPage == NoPage then
      firstDataPage = newPageId
      db.tableFirstPages(this.name) = newPageId
    else
      // Link from previous last page
      val prevData = batch.read(prevPageId)
      val prevPage = SlottedPage.wrap(prevData)
      prevPage.setNextPage(newPageId)
      batch.write(prevPageId, prevPage.data)

    batch.write(newPageId, newPage.data)

  def iterator(ctx: Seq[Row]): RowIterator =
    // Collect all rows from all data pages
    val rows = new ArrayBuffer[Row]
    var currentPageId = firstDataPage

    while currentPageId != NoPage do
      val pageData = store.read(currentPageId)
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
      store.modify { batch =>
        // Read current row data
        val pageData = batch.read(pageId)
        val page     = SlottedPage.wrap(pageData)
        val enumTypes = db.types.collect { case (n, e: EnumType) => (n, e) }.toMap
        val currentSlotData = page.getSlot(slotIndex).getOrElse(sys.error("slot is tombstone during update"))
        val (currentValues, currentChains) = deserializeRow(currentSlotData, columns.length, store, enumTypes)
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

        // Try to update in place
        if !page.updateSlot(slotIndex, newRowBytes) then
          // Doesn't fit — tombstone old slot, insert in a new slot/page
          page.removeSlot(slotIndex)
          batch.write(pageId, page.data)
          insertRowBytes(newRowBytes, batch)
        else
          batch.write(pageId, page.data)

        db.writeCatalogInBatch(batch)
      }

  private def makeDeleter(pageId: PageId, slotIndex: Int, chains: IndexedSeq[Option[ChainRef]]): () => Unit =
    () =>
      store.modify { batch =>
        // Free chain pages
        for chainOpt <- chains; ref <- chainOpt do
          freeChain(ref.firstPage, batch, store.pageSize)

        val pageData = batch.read(pageId)
        val page     = SlottedPage.wrap(pageData)
        page.removeSlot(slotIndex)
        batch.write(pageId, page.data)
        db.writeCatalogInBatch(batch)
      }

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

    store.modify { batch =>
      // Free all old data pages and their chains
      freeAllDataPages(batch)

      // Rewrite all rows with transformation
      for values <- allRows do
        val transformed = transform(values)
        val rowBytes    = serializeRow(transformed, batch, store.pageSize)
        insertRowBytes(rowBytes, batch)
    }

  private[rdb] def freeAllPages(): Unit =
    store.modify { batch =>
      freeAllDataPages(batch)
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
    db.tableFirstPages(this.name) = NoPage

  // Override DDL methods to persist catalog after schema changes
  override def addColumnToTable(spec: ColumnSpec, defaultValue: Value): Unit =
    super.addColumnToTable(spec, defaultValue)
    db.persistCatalog()

  override def dropColumnFromTable(colName: String): Unit =
    super.dropColumnFromTable(colName)
    db.persistCatalog()

  override def alterColumnType(colName: String, newType: Type): Unit =
    super.alterColumnType(colName, newType)
    db.persistCatalog()

  override def alterColumnSetDefault(colName: String, default: Value): Unit =
    super.alterColumnSetDefault(colName, default)
    db.persistCatalog()

  override def alterColumnDropDefault(colName: String): Unit =
    super.alterColumnDropDefault(colName)
    db.persistCatalog()

  override def alterColumnSetNotNull(colName: String): Unit =
    super.alterColumnSetNotNull(colName)
    db.persistCatalog()

  override def alterColumnDropNotNull(colName: String): Unit =
    super.alterColumnDropNotNull(colName)
    db.persistCatalog()

  override def addConstraintToTable(spec: Spec): Unit =
    super.addConstraintToTable(spec)
    db.persistCatalog()

  override def dropConstraintFromTable(constraintName: String): Unit =
    super.dropConstraintFromTable(constraintName)
    db.persistCatalog()

  override def toString: String = s"[PersistentTable '$name': $meta]"

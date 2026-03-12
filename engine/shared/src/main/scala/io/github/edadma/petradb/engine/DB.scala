package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import io.github.edadma.dllist.DLList
import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.concurrent.{Future, ExecutionContext}

trait TransactionHandle
object NoOpTransactionHandle extends TransactionHandle

class Sequence(
    val name: String,
    var currentValue: Long,
    val increment: Long = 1,
    val minValue: Long = 1,
    val maxValue: Long = Long.MaxValue / 2,
    val startValue: Long = 1,
    val cycle: Boolean = false,
    var called: Boolean = false,
    val ownedByTable: Option[String] = None,
    val ownedByColumn: Option[String] = None,
):
  def nextval(): Long =
    if !called then
      called = true
      if currentValue == 0 then currentValue = startValue
      // else: setval(v, false) was called, keep currentValue as-is
    else
      currentValue += increment
      if increment > 0 && currentValue > maxValue then
        if cycle then currentValue = minValue
        else sys.error(s"nextval: reached maximum value of sequence \"$name\" ($maxValue)")
      else if increment < 0 && currentValue < minValue then
        if cycle then currentValue = maxValue
        else sys.error(s"nextval: reached minimum value of sequence \"$name\" ($minValue)")
    currentValue

  def setval(value: Long, isCalled: Boolean = true): Long =
    currentValue = value
    called = isCalled
    value

  def stateSnapshot(): (Long, Boolean) = (currentValue, called)

  def restoreState(snap: (Long, Boolean)): Unit =
    currentValue = snap._1
    called = snap._2

case class CatalogSnapshot(
    tablesSnap: Map[String, Table],
    indexesSnap: Map[String, IndexMeta],
    viewsSnap: Map[String, String],
    typesSnap: Map[String, Type],
    schemasSnap: Set[String],
    sequencesSnap: Map[String, Sequence] = Map.empty,
)

abstract class DB:

  val name: String

  protected val tables = new mutable.HashMap[String, Table]
  protected[petradb] val types = new mutable.HashMap[String, Type]
  protected[petradb] val indexes = new mutable.HashMap[String, IndexMeta]
  protected val views = new mutable.HashMap[String, String] // name -> SQL
  protected[petradb] val sequences = new mutable.HashMap[String, Sequence]
  protected[petradb] val schemas = new mutable.LinkedHashSet[String]
  schemas += "public"

  // ── Schema management ──────────────────────────────────────────────

  def hasSchema(name: String): Boolean = schemas.contains(name)

  def createSchema(name: String): Unit =
    schemas += name
    onMutation()

  def dropSchema(name: String, cascade: Boolean): Unit =
    if cascade then
      val prefix = s"$name."
      val toRemove = tables.keys.filter(_.startsWith(prefix)).toSeq
      for key <- toRemove do
        val idx2Remove = indexes.filter(_._2.tableName == key).keys.toSeq
        for idx <- idx2Remove do indexes.remove(idx)
        tables.get(key).foreach(_.tableIndexes.clear())
        tables.remove(key)
      val viewsToRemove = views.keys.filter(_.startsWith(prefix)).toSeq
      for key <- viewsToRemove do views.remove(key)
    else
      val prefix = s"$name."
      if tables.keys.exists(_.startsWith(prefix)) || views.keys.exists(_.startsWith(prefix)) then
        sys.error(s"cannot drop schema '$name' because it is not empty")
    schemas -= name
    onMutation()

  def schemaNames: Iterable[String] = schemas

  // ── Internal key helpers ───────────────────────────────────────────

  private[petradb] def qualifiedKey(schema: String, name: String): String = s"$schema.$name"
  private[petradb] def qualifiedKey(schema: Option[String], name: String): String = s"${schema.getOrElse("public")}.$name"

  // ── Table accessors (schema-aware) ─────────────────────────────────

  def tableNames: Iterable[String] = tables.keys.map(_.split('.').last)

  def tableNamesInSchema(schema: String): Iterable[String] =
    val prefix = s"$schema."
    tables.keys.filter(_.startsWith(prefix)).map(_.stripPrefix(prefix))

  def allQualifiedTableNames: Iterable[(String, String)] =
    tables.keys.map { key =>
      val dot = key.indexOf('.')
      (key.substring(0, dot), key.substring(dot + 1))
    }

  // Resolve a possibly-qualified name ("schema.table" or just "table")
  private[petradb] def resolveKey(name: String): String =
    if name.contains('.') then name else qualifiedKey("public", name)

  infix def hasTable(name: String): Boolean = tables contains resolveKey(name)
  def hasTable(schema: String, name: String): Boolean = tables contains qualifiedKey(schema, name)

  infix def getTable(name: String): Option[Table] = tables get resolveKey(name)
  def getTable(schema: String, name: String): Option[Table] = tables get qualifiedKey(schema, name)

  protected def addTable(name: String, specs: Seq[Spec]): Table

  protected[engine] def onMutation(): Unit = ()

  protected def registerTable(name: String, table: Table): Unit = tables(name) = table

  // Extract the short (unqualified) table name from a possibly-qualified name
  private[petradb] def shortName(name: String): String =
    val dot = name.indexOf('.')
    if dot >= 0 then name.substring(dot + 1) else name

  def createTable(name: String, specs: Seq[Spec]): Table =
    val key = resolveKey(name)
    require(!(tables contains key), s"table '${shortName(name)}' already exists")

    val table = addTable(shortName(name), specs)

    registerTable(key, table)
    onMutation()
    table

  def dropTable(name: String): Unit =
    val key = resolveKey(name)
    // Remove indexes for this table
    val toRemove = indexes.filter(_._2.tableName == key).keys.toSeq
    for idx <- toRemove do indexes.remove(idx)
    tables.get(key).foreach { t =>
      t.tableIndexes.clear()
    }
    tables.remove(key)
    onMutation()

  def renameTable(oldName: String, newName: String): Unit =
    val oldKey = resolveKey(oldName)
    val newKey = resolveKey(newName)
    val table = tables.remove(oldKey).getOrElse(sys.error(s"table '$oldName' not found"))
    table.name = shortName(newName)
    tables(newKey) = table
    onMutation()

  protected def addEnum(name: String, labels: Seq[String]): EnumType

  def createEnum(name: String, labels: Seq[String]): Unit =
    require(!types.contains(name), s"type $name already exists")

    types(name) = addEnum(name, labels)
    onMutation()

  def dropType(name: String): Unit =
    types.remove(name)
    onMutation()

  infix def hasType(name: String): Boolean = types contains name

  infix def getType(name: String): Option[Type] = types get name

  def createView(name: String, sql: String, orReplace: Boolean): Unit =
    val key = resolveKey(name)
    if !orReplace then require(!views.contains(key), s"view '$name' already exists")
    views(key) = sql
    onMutation()

  def dropView(name: String): Unit =
    views.remove(resolveKey(name))
    onMutation()

  def hasView(name: String): Boolean = views contains resolveKey(name)

  def getView(name: String): Option[String] = views get resolveKey(name)

  def viewNames: Iterable[String] = views.keys.map(_.split('.').last)

  // ── Sequence management ──────────────────────────────────────────

  def createSequence(
      name: String,
      increment: Long = 1,
      minValue: Long = 1,
      maxValue: Long = Long.MaxValue / 2,
      startValue: Option[Long] = None,
      cycle: Boolean = false,
      ownedByTable: Option[String] = None,
      ownedByColumn: Option[String] = None,
  ): Sequence =
    require(!sequences.contains(name), s"sequence '$name' already exists")
    val start = startValue.getOrElse(if increment > 0 then minValue else maxValue)
    val seq = new Sequence(name, 0, increment, minValue, maxValue, start, cycle, false, ownedByTable, ownedByColumn)
    sequences(name) = seq
    onMutation()
    seq

  def dropSequence(name: String): Unit =
    sequences.remove(name)
    onMutation()

  def hasSequence(name: String): Boolean = sequences.contains(name)

  def getSequence(name: String): Option[Sequence] = sequences.get(name)

  def createIndex(indexName: String, tableName: String, columnNames: Seq[String], unique: Boolean): Unit

  def alterTable(name: String, alteration: TableAlteration)(using Session): Unit =
    val t = tables(resolveKey(name))
    alteration match
      case AddColumnTableAlteration(ColumnDesc(cid @ Ident(colName), typeDesc, required, unique, default, references, _, _, _)) =>
        if t.hasColumn(colName) then throw SchemaException(cid.pos, s"column '$colName' already exists")
        val typ = typeDesc match
          case Left(primitive)             => primitive
          case Right(tid @ Ident(defined)) => getType(defined).getOrElse(throw UndefinedReferenceException(tid.pos, s"type '$defined' is undefined"))
        val fk = references.map { case (tbl, col, onDel, onUpd) => (tbl.name, col.name, onDel, onUpd) }
        val defaultValue = default.map(expr => eval(rewrite(expr), Nil)).getOrElse(NullValue())
        val spec = ColumnSpec(colName, typ, required, false, unique, fk, default.map(expr => eval(rewrite(expr), Nil)))
        t.addColumnToTable(spec, defaultValue)
      case DropColumnTableAlteration(cid @ Ident(colName)) =>
        if !t.hasColumn(colName) then throw UndefinedReferenceException(cid.pos, s"column '$colName' not found")
        t.dropColumnFromTable(colName)
      case AlterColumnTableAlteration(cid @ Ident(colName), mod) =>
        if !t.hasColumn(colName) then throw UndefinedReferenceException(cid.pos, s"column '$colName' not found")
        mod match
          case SetDataTypeColumnModification(typeDesc) =>
            val typ = typeDesc match
              case Left(primitive)             => primitive
              case Right(tid @ Ident(defined)) => getType(defined).getOrElse(throw UndefinedReferenceException(tid.pos, s"type '$defined' is undefined"))
            t.alterColumnType(colName, typ)
          case SetDefaultColumnModification(expr) =>
            t.alterColumnSetDefault(colName, eval(rewrite(expr), Nil))
          case DropDefaultColumnModification() =>
            t.alterColumnDropDefault(colName)
          case SetNotNullColumnModification() =>
            t.alterColumnSetNotNull(colName)
          case DropNotNullColumnModification() =>
            t.alterColumnDropNotNull(colName)
      case AddConstraintTableAlteration(constraint) =>
        val spec = constraint match
          case UniqueConstraint(cname, cols)                                    => UniqueSpec(cols.map(_.name), cname)
          case PrimaryKeyConstraint(cname, cols)                                => PrimaryKeySpec(cols.map(_.name), cname)
          case ForeignKeyConstraint(cname, cols, refTable, refCols, onDel, onUpd) =>
            ForeignKeySpec(cols.map(_.name), refTable.name, refCols.map(_.name), cname, onDel, onUpd)
          case CheckConstraint(cname, expr)                                     => CheckSpec(exprToSQL(expr), expr, cname)
        t.addConstraintToTable(spec)
      case DropConstraintTableAlteration(Ident(constraintName)) =>
        t.dropConstraintFromTable(constraintName)
      case RenameTableAlteration(Ident(newName)) =>
        renameTable(name, newName)
      case RenameColumnTableAlteration(cid @ Ident(oldName), Ident(newName)) =>
        if !t.hasColumn(oldName) then throw UndefinedReferenceException(cid.pos, s"column '$oldName' not found")
        t.renameColumnInTable(oldName, newName)
      case AddForeignKeyTableAlteration(fk, ref) =>
        t.addConstraintToTable(ForeignKeySpec(Seq(fk.name), ref.name, Seq(fk.name), None))
      case AddForeignKeyConstraintTableAlteration(constraint) =>
        t.addConstraintToTable(ForeignKeySpec(constraint.columns.map(_.name), constraint.referencedTable.name, constraint.referencedColumns.map(_.name), constraint.name, constraint.onDelete, constraint.onUpdate))
    onMutation()

  def dropIndex(indexName: String): Unit =
    indexes.get(indexName) match
      case Some(meta) =>
        tables.get(meta.tableName).foreach(_.tableIndexes.remove(indexName))
        indexes.remove(indexName)
      case None => sys.error(s"index '$indexName' not found")
    onMutation()

  def hasIndex(name: String): Boolean = indexes contains name

  // ── Catalog snapshot for transactional DDL rollback ────────────────

  protected def takeCatalogSnapshot(): CatalogSnapshot =
    CatalogSnapshot(
      tablesSnap = tables.toMap,
      indexesSnap = indexes.toMap,
      viewsSnap = views.toMap,
      typesSnap = types.toMap,
      schemasSnap = schemas.toSet,
      sequencesSnap = sequences.toMap,
    )

  protected def restoreCatalog(snap: CatalogSnapshot): Unit =
    tables.clear(); tables ++= snap.tablesSnap
    indexes.clear(); indexes ++= snap.indexesSnap
    views.clear(); views ++= snap.viewsSnap
    types.clear(); types ++= snap.typesSnap
    schemas.clear(); schemas ++= snap.schemasSnap
    sequences.clear(); sequences ++= snap.sequencesSnap

  def snapshot(): TransactionHandle = NoOpTransactionHandle
  def commitSnapshot(handle: TransactionHandle): Unit = ()
  def rollbackSnapshot(handle: TransactionHandle): Unit = ()
  def activateHandle(handle: TransactionHandle): Unit = ()
  def deactivateHandle(): Unit = ()

  def connect(): Session = new Session(this)

  def close(): Unit = ()

  override def toString: String = s"[Database '$name': ${tables map ((_, t) => t) mkString ", "}]"

  // ── Foreign Key Helpers ──────────────────────────────────────────

  private[petradb] def foreignKeys(table: Table): Seq[ForeignKeySpec] =
    val fromConstraints = table.constraints.collect { case fk: ForeignKeySpec => fk }
    val fromColumns = table.columns.collect {
      case cs if cs.fk.isDefined =>
        val (refTable, refCol, onDel, onUpd) = cs.fk.get
        ForeignKeySpec(Seq(cs.name), refTable, Seq(refCol), None, onDel, onUpd)
    }
    // Deduplicate: if a column-level FK is also in constraints (same cols + refTable), skip it
    val constraintKeys = fromConstraints.map(fk => (fk.columns, fk.referencedTable)).toSet
    fromConstraints.toSeq ++ fromColumns.filterNot(fk => constraintKeys.contains((fk.columns, fk.referencedTable)))

  private[engine] def childForeignKeys(parentTableName: String): Seq[(Table, ForeignKeySpec)] =
    tables.values.flatMap { t =>
      foreignKeys(t).filter(_.referencedTable == parentTableName).map(fk => (t, fk))
    }.toSeq

  private[engine] def checkParentExists(childTableName: String, fk: ForeignKeySpec, row: IndexedSeq[Value], colMap: mutable.HashMap[String, Int]): Unit =
    val fkValues = fk.columns.map(c => row(colMap(c)))
    if fkValues.exists(_.isNull) then return

    val parentTable = tables.getOrElse(resolveKey(fk.referencedTable),
      sys.error(s"foreign key references non-existent table '${fk.referencedTable}'"))

    val parentKey = fkValues.toIndexedSeq
    if !findRowByColumns(parentTable, fk.referencedColumns, parentKey) then
      val constraintName = fk.name.getOrElse(s"${childTableName}_${fk.columns.mkString("_")}_fkey")
      sys.error(
        s"""insert or update on table "$childTableName" violates foreign key constraint "$constraintName"""" + "\n" +
        s"""Key (${fk.columns.mkString(", ")})=(${fkValues.map(_.string).mkString(", ")}) is not present in table "${fk.referencedTable}"."""
      )

  private[engine] def enforceChildConstraints(
      parentTableName: String,
      oldRow: Row,
      operation: String,
      updatedCols: Option[Set[String]] = None,
      newValues: Option[Seq[(String, Value)]] = None,
  ): Unit =
    for (childTable, fk) <- childForeignKeys(parentTableName) do
      if operation == "delete" || updatedCols.exists(_.intersect(fk.referencedColumns.toSet).nonEmpty) then
        val action = if operation == "delete" then fk.onDelete else fk.onUpdate
        val parentKeyValues = fk.referencedColumns.map(c => oldRow(c)).toIndexedSeq
        if !parentKeyValues.exists(_.isNull) then
          val childRows = findRowsByColumns(childTable, fk.columns, parentKeyValues)
          if childRows.nonEmpty then
            action match
              case ReferentialAction.NoAction | ReferentialAction.Restrict =>
                val constraintName = fk.name.getOrElse(s"${childTable.name}_${fk.columns.mkString("_")}_fkey")
                sys.error(
                  s"""update or delete on table "$parentTableName" violates foreign key constraint "$constraintName" on table "${childTable.name}"""" + "\n" +
                  s"""Key (${fk.referencedColumns.mkString(", ")})=(${parentKeyValues.map(_.string).mkString(", ")}) is still referenced from table "${childTable.name}"."""
                )
              case ReferentialAction.Cascade =>
                if operation == "delete" then
                  for r <- childRows do
                    enforceChildConstraints(childTable.name, r, "delete")
                    r.deleter.getOrElse(sys.error("child row not deletable during CASCADE"))()
                else
                  val newValueMap = newValues.getOrElse(Nil).toMap
                  for r <- childRows do
                    val updates = fk.columns.zip(fk.referencedColumns).collect {
                      case (childCol, parentCol) if newValueMap.contains(parentCol) =>
                        childCol -> newValueMap(parentCol)
                    }
                    if updates.nonEmpty then
                      r.updater.getOrElse(sys.error("child row not updatable during CASCADE"))(updates)
              case ReferentialAction.SetNull =>
                for r <- childRows do
                  val updates = fk.columns.map(c => c -> NullValue())
                  r.updater.getOrElse(sys.error("child row not updatable during SET NULL"))(updates)

  private def findRowByColumns(table: Table, columnNames: Seq[String], values: IndexedSeq[Value]): Boolean =
    val matchingIndex = table.tableIndexes.values.find(idx => idx.meta.columns == columnNames)
    matchingIndex match
      case Some(idx) =>
        table.indexPointScan(idx, values) match
          case Some(iter) => iter.hasNext
          case None       => scanForMatch(table, columnNames, values)
      case None => scanForMatch(table, columnNames, values)

  private def scanForMatch(table: Table, columnNames: Seq[String], values: IndexedSeq[Value]): Boolean =
    val colIndices = columnNames.map(c => table.columnMap(c))
    table.iterator(Nil).exists { row =>
      colIndices.zip(values).forall { (idx, v) =>
        val rv = row.data(idx)
        !rv.isNull && rv.compare(v) == 0
      }
    }

  private[engine] def findRowsByColumns(table: Table, columnNames: Seq[String], values: IndexedSeq[Value]): Seq[Row] =
    val matchingIndex = table.tableIndexes.values.find(idx => idx.meta.columns == columnNames)
    val iter = matchingIndex match
      case Some(idx) =>
        table.indexPointScan(idx, values).getOrElse(scanIterator(table, columnNames, values))
      case None => scanIterator(table, columnNames, values)
    iter.toSeq

  private def scanIterator(table: Table, columnNames: Seq[String], values: IndexedSeq[Value]): RowIterator =
    val colIndices = columnNames.map(c => table.columnMap(c))
    table.iterator(Nil).filter { row =>
      colIndices.zip(values).forall { (idx, v) =>
        val rv = row.data(idx)
        !rv.isNull && rv.compare(v) == 0
      }
    }

abstract class Table(var name: String, specs: Seq[Spec]) extends Process:

  protected[petradb] val columns   = new ArrayBuffer[ColumnSpec]
  protected[petradb] val columnMap  = new mutable.HashMap[String, Int]
  protected[petradb] val autoMap   = new mutable.HashMap[String, Value]
  private[engine] val backingSequences = new mutable.HashMap[String, Sequence]
  private var _meta: Metadata = Metadata(Vector.empty)
  protected[petradb] var primaryKey: Option[PrimaryKeySpec] = None
  protected[petradb] val constraints                       = new ArrayBuffer[Spec]
  protected[petradb] val tableIndexes                      = new mutable.HashMap[String, TableIndex]

  specs foreach {
    case s: ColumnSpec => createColumn(s)
    case pk: PrimaryKeySpec => 
      primaryKey = Some(pk)
      constraints += pk
    case constraint => constraints += constraint
  }

  private val autoSet = columns filter (c => c.typ == SmallSerialType || c.typ == SerialType || c.typ == BigSerialType || c.typ == UUIDType) map (_.name) toSet

  def meta: Metadata = _meta

  def iterator(ctx: Seq[Row]): RowIterator

  def indexPointScan(index: TableIndex, key: IndexedSeq[Value]): Option[RowIterator] = None
  def indexRangeScan(index: TableIndex, lower: IndexedSeq[Value], upper: IndexedSeq[Value]): Option[RowIterator] = None

  def hasColumn(name: String): Boolean = columnMap contains name

  protected def addColumn(spec: ColumnSpec): Unit

  // (name, typ, pk, auto, required, indexed, unique, fk)
  def createColumn(spec: ColumnSpec): Unit =
    require(!(columnMap contains spec.name), s"duplicate column '${spec.name}'")
    columnMap(spec.name) = columns.length
    columns += spec
    _meta = Metadata(columns to immutable.ArraySeq map (s => ColumnMetadata(Some(name), s.name, s.typ)))
    addColumn(spec)

//
//  def rows: Int = data.length

  // Optional callback for sequence tracking (set by executeSQL to update session state)
  var onSequenceUsed: Option[(String, Long) => Unit] = None

  def auto(col: String): Value =
    backingSequences.get(col) match
      case Some(seq) =>
        val n = seq.nextval()
        onSequenceUsed.foreach(_(seq.name, n))
        val v = columns(columnMap(col)).typ match
          case BigSerialType => NumberValue(io.github.edadma.dal.LongType, n: java.lang.Long)
          case _             => NumberValue(n.toInt)
        autoMap(col) = v
        v
      case None =>
        autoMap get col match
          case None =>
            val first = columns(columnMap(col)).typ.init
            autoMap(col) = first
            first
          case Some(cur) =>
            val next = cur.next
            autoMap(col) = next
            next

  protected[engine] def restoreAutoState(state: Map[String, Value]): Unit = autoMap ++= state

  // Abstract storage methods for subclasses to implement
  protected def addColumnData(defaultValue: Value): Unit
  protected def dropColumnData(index: Int): Unit
  protected def convertColumnData(index: Int, newType: Type): Unit
  protected def hasNullInColumn(index: Int): Boolean
  def truncate(): Unit

  protected def rebuildMeta(): Unit =
    _meta = Metadata(columns.to(immutable.ArraySeq).map(s => ColumnMetadata(Some(name), s.name, s.typ)))

  def addColumnToTable(spec: ColumnSpec, defaultValue: Value): Unit =
    createColumn(spec)
    addColumnData(defaultValue)

  def dropColumnFromTable(colName: String): Unit =
    val idx = columnMap.getOrElse(colName, sys.error(s"column '$colName' not found"))
    // Check if column is part of primary key
    primaryKey match
      case Some(pk) if pk.columns.contains(colName) =>
        sys.error(s"cannot drop column '$colName': part of primary key")
      case _ =>
    dropColumnData(idx)
    columns.remove(idx)
    columnMap.remove(colName)
    // Shift indices for columns after the removed one
    for ((cn, ci) <- columnMap)
      if ci > idx then columnMap(cn) = ci - 1
    rebuildMeta()

  def renameColumnInTable(oldName: String, newName: String): Unit =
    val idx = columnMap.getOrElse(oldName, sys.error(s"column '$oldName' not found"))
    columnMap.remove(oldName)
    columnMap(newName) = idx
    columns(idx) = columns(idx).copy(name = newName)
    // Update constraint references
    for (i <- constraints.indices)
      constraints(i) = constraints(i) match
        case pk: PrimaryKeySpec => pk.copy(columns = pk.columns.map(c => if c == oldName then newName else c))
        case u: UniqueSpec => u.copy(columns = u.columns.map(c => if c == oldName then newName else c))
        case fk: ForeignKeySpec => fk.copy(columns = fk.columns.map(c => if c == oldName then newName else c))
        case other => other
    primaryKey = primaryKey.map(pk => pk.copy(columns = pk.columns.map(c => if c == oldName then newName else c)))
    rebuildMeta()

  def alterColumnType(colName: String, newType: Type): Unit =
    val idx = columnMap.getOrElse(colName, sys.error(s"column '$colName' not found"))
    columns(idx) = columns(idx).copy(typ = newType)
    convertColumnData(idx, newType)
    rebuildMeta()

  def alterColumnSetDefault(colName: String, default: Value): Unit =
    val idx = columnMap.getOrElse(colName, sys.error(s"column '$colName' not found"))
    columns(idx) = columns(idx).copy(default = Some(default))

  def alterColumnDropDefault(colName: String): Unit =
    val idx = columnMap.getOrElse(colName, sys.error(s"column '$colName' not found"))
    columns(idx) = columns(idx).copy(default = None)

  def alterColumnSetNotNull(colName: String): Unit =
    val idx = columnMap.getOrElse(colName, sys.error(s"column '$colName' not found"))
    if hasNullInColumn(idx) then sys.error(s"column '$colName' contains null values")
    columns(idx) = columns(idx).copy(required = true)

  def alterColumnDropNotNull(colName: String): Unit =
    val idx = columnMap.getOrElse(colName, sys.error(s"column '$colName' not found"))
    columns(idx) = columns(idx).copy(required = false)

  def addConstraintToTable(spec: Spec): Unit =
    spec match
      case pk: PrimaryKeySpec =>
        if primaryKey.isDefined then sys.error("table already has a primary key")
        primaryKey = Some(pk)
      case _ =>
    constraints += spec

  def dropConstraintFromTable(constraintName: String): Unit =
    val idx = constraints.indexWhere {
      case pk: PrimaryKeySpec => pk.name.contains(constraintName)
      case u: UniqueSpec => u.name.contains(constraintName)
      case fk: ForeignKeySpec => fk.name.contains(constraintName)
      case c: CheckSpec => c.name.contains(constraintName)
      case _ => false
    }
    if idx < 0 then sys.error(s"constraint '$constraintName' not found")
    constraints(idx) match
      case _: PrimaryKeySpec => primaryKey = None
      case _ =>
    constraints.remove(idx)

  def insert(row: Map[String, Value], returning: Option[Seq[String]]): Map[String, Value] =
    val (keys, values) = row.toSeq.unzip

    bulkInsert(keys, Seq(values), returning)

  protected def addRow(row: Seq[Value]): Unit

  // Precompute generated column info: (index, parsed expression)
  private lazy val generatedColumns: Seq[(Int, Expr)] =
    columns.zipWithIndex.collect { case (spec, idx) if spec.generated.isDefined => (idx, spec.generated.get._2) }.toSeq

  // Set of generated column names (cannot be explicitly inserted or updated)
  private lazy val generatedSet: Set[String] =
    columns.filter(_.generated.isDefined).map(_.name).toSet

  def bulkInsert(header: Seq[String], rows: Seq[Seq[Value]], returning: Option[Seq[String]], fkCheck: Option[IndexedSeq[Value] => Unit] = None): Map[String, Value] =
    val headerSet = header.toSet
    val columnSet = columnMap.keySet

    require(headerSet subsetOf columnSet, s"unknown columns: ${headerSet diff columnSet mkString ", "}")

    // Reject explicit values for generated columns
    val explicitGenerated = headerSet intersect generatedSet
    if explicitGenerated.nonEmpty then
      sys.error(s"cannot insert a value into generated column: ${explicitGenerated.mkString(", ")}")

    val missingSet = columnSet diff headerSet
    val missing    =
      for (m <- (missingSet diff autoSet) diff generatedSet)
        yield
          val idx = columnMap(m)
          val s   = columns(idx)

          if s.required && s.default.isEmpty then sys.error(s"bulkInsert: column '$m' is required and has no default")

          // Check if this column is part of primary key
          primaryKey match
            case Some(pk) if pk.columns.contains(m) => sys.error(s"bulkInsert: column '$m' is part of the primary key and is required")
            case _ =>

          (idx, s.default getOrElse NullValue())
    val autos                      = autoSet intersect missingSet map (c => (c, columnMap(c)))
    val mapping                    = header map (h => meta.columnMap(h)._1)
    val specs                      = header map (h => columns(columnMap(h)))
    var result: Map[String, Value] = Map.empty

    for (r <- rows)
      val arr = new Array[Value](meta.width)

      for (((i, v), s) <- mapping zip r zip specs)
        if v.isNull then
          if s.required then throw ConstraintException(v.pos, s"column '${s.name}' is required")
          else arr(i) = v
        else arr(i) = s.typ.convert(v)

      for ((i, v) <- missing)
        arr(i) = v

      val newAutos =
        for ((c, i) <- autos)
          yield
            val v = auto(c)

            arr(i) = v
            c -> v

      result = newAutos.toMap

      // Compute generated columns using current row values
      if generatedColumns.nonEmpty then
        val row = Row(arr.toIndexedSeq, meta, None, None)
        for (idx, expr) <- generatedColumns do
          val v = eval(expr, Seq(row))
          arr(idx) = columns(idx).typ.convert(v)

      // Enforce NOT NULL for PRIMARY KEY columns
      primaryKey.foreach { pk =>
        for colName <- pk.columns do
          val idx = columnMap(colName)
          if arr(idx).isNull then
            sys.error(s"null value in column \"$colName\" violates not-null constraint")
      }

      // Enforce CHECK constraints
      val checks = constraints.collect { case c: CheckSpec => c }
      if checks.nonEmpty then
        val row = Row(arr.toIndexedSeq, meta, None, None)
        for c <- checks do
          if !beval(c.parsedExpr, Seq(row)) then
            sys.error(s"new row violates check constraint${c.name.map(n => s""" "$n"""").getOrElse("")}")

      for retCols <- returning do
        if retCols.isEmpty then
          // RETURNING * — include all columns
          for col <- columns do
            result += (col.name -> arr(columnMap(col.name)))
        else
          for col <- retCols do
            val idx = columnMap.getOrElse(col, sys.error(s"column '$col' not found"))
            result += (col -> arr(idx))

      fkCheck.foreach(_(arr.toIndexedSeq))
      addRow(arr to immutable.ArraySeq)

    result

case class PreparedStatement(name: String, commands: Seq[Command]):
  def execute(params: Value*)(using session: Session): Seq[Result] =
    val copied = deepCopyCommands(commands, params.toIndexedSeq)
    executeCommands(copied)

  def execute(params: IndexedSeq[Value])(using session: Session, ec: ExecutionContext): Future[Seq[Result]] =
    Future.successful(execute(params*))

  def parameterCount: Int =
    def countInExpr(expr: Expr): Seq[Int] =
      expr match
        case ParameterExpr(index)              => Seq(index)
        case AliasExpr(e, _)                   => countInExpr(e)
        case UnaryExpr(_, e)                   => countInExpr(e)
        case BinaryExpr(l, _, r)               => countInExpr(l) ++ countInExpr(r)
        case BetweenExpr(v, _, lo, hi)         => countInExpr(v) ++ countInExpr(lo) ++ countInExpr(hi)
        case OverlapsExpr(a, b, c, d)          => countInExpr(a) ++ countInExpr(b) ++ countInExpr(c) ++ countInExpr(d)
        case CaseExpr(whens, els) =>
          whens.flatMap { case When(w, e) => countInExpr(w) ++ countInExpr(e) } ++ els.toSeq.flatMap(countInExpr)
        case ApplyExpr(_, args, filter)        => args.flatMap(countInExpr) ++ filter.toSeq.flatMap(countInExpr)
        case InSeqExpr(v, _, es)               => countInExpr(v) ++ es.flatMap(countInExpr)
        case InQueryExpr(v, _, q)              => countInExpr(v) ++ countInExpr(q)
        case SubqueryExpr(q)                   => countInExpr(q)
        case ExistsExpr(q)                     => countInExpr(q)
        case CastExpr(e, _)                    => countInExpr(e)
        case LateralExpr(q)                    => countInExpr(q)
        case SQLSelectExpr(exprs, from, where, _, having, _, _, _, _) =>
          exprs.flatMap(countInExpr) ++
            from.toSeq.flatMap(_.flatMap(countInExpr)) ++
            where.toSeq.flatMap(countInExpr) ++
            having.toSeq.flatMap(countInExpr)
        case _ => Nil
    def countInCommand(cmd: Command): Seq[Int] =
      cmd match
        case QueryCommand(q)                    => countInExpr(q)
        case InsertCommand(_, _, rows, _, _)       => rows.flatMap(_.flatMap(countInExpr))
        case InsertSelectCommand(_, _, q, _, _)    => countInExpr(q)
        case UpdateCommand(_, sets, from, cond, _)  => sets.flatMap(s => countInExpr(s.value)) ++ from.toSeq.flatMap(_.flatMap(countInExpr)) ++ cond.toSeq.flatMap(countInExpr)
        case DeleteCommand(_, cond, _)             => cond.toSeq.flatMap(countInExpr)
        case _                                  => Nil
    commands.flatMap(countInCommand).maxOption.getOrElse(0)

enum ReferentialAction:
  case NoAction, Restrict, Cascade, SetNull

trait Spec
case class ColumnSpec(
    name: String,
    typ: Type,
    required: Boolean = false,
    indexed: Boolean = false,
    unique: Boolean = false,
    fk: Option[(String, String, ReferentialAction, ReferentialAction)] = None,
    default: Option[Value] = None,
    generated: Option[(String, Expr)] = None, // (SQL source, parsed expression) for GENERATED ALWAYS AS columns
) extends Spec

// Table-level constraint specifications
case class PrimaryKeySpec(columns: Seq[String], name: Option[String] = None) extends Spec
case class UniqueSpec(columns: Seq[String], name: Option[String] = None) extends Spec
case class ForeignKeySpec(
    columns: Seq[String],
    referencedTable: String,
    referencedColumns: Seq[String],
    name: Option[String] = None,
    onDelete: ReferentialAction = ReferentialAction.NoAction,
    onUpdate: ReferentialAction = ReferentialAction.NoAction,
) extends Spec
case class CheckSpec(exprSource: String, parsedExpr: Expr, name: Option[String] = None) extends Spec

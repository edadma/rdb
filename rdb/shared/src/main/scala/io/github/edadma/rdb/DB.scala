package io.github.edadma.rdb

import io.github.edadma.dllist.DLList
import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

abstract class DB:

  val name: String

  protected val tables = new mutable.HashMap[String, Table]
  protected[rdb] val types = new mutable.HashMap[String, Type]
  protected[rdb] val indexes = new mutable.HashMap[String, IndexMeta]

  def tableNames: Iterable[String] = tables.keys

  infix def hasTable(name: String): Boolean = tables contains name

  infix def getTable(name: String): Option[Table] = tables get name

  protected def addTable(name: String, specs: Seq[Spec]): Table

  protected def registerTable(name: String, table: Table): Unit = tables(name) = table

  protected def guardDDL(): Unit =
    if inTransaction then sys.error("DDL not allowed inside a transaction")

  def createTable(name: String, specs: Seq[Spec]): Table =
    guardDDL()
    require(!(tables contains name), s"table '$name' already exists")

    val table = addTable(name, specs)

    registerTable(name, table)
    table

  def dropTable(name: String): Unit =
    guardDDL()
    // Remove indexes for this table
    val toRemove = indexes.filter(_._2.tableName == name).keys.toSeq
    for idx <- toRemove do indexes.remove(idx)
    tables.get(name).foreach { t =>
      t.tableIndexes.clear()
    }
    tables.remove(name)

  def renameTable(oldName: String, newName: String): Unit =
    guardDDL()
    val table = tables.remove(oldName).getOrElse(sys.error(s"table '$oldName' not found"))
    table.name = newName
    tables(newName) = table

  protected def addEnum(name: String, labels: Seq[String]): EnumType

  def createEnum(name: String, labels: Seq[String]): Unit =
    guardDDL()
    require(!types.contains(name), s"type $name already exists")

    types(name) = addEnum(name, labels)

  def dropType(name: String): Unit =
    guardDDL()
    types.remove(name)

  infix def hasType(name: String): Boolean = types contains name

  infix def getType(name: String): Option[Type] = types get name

  def createIndex(indexName: String, tableName: String, columnNames: Seq[String], unique: Boolean): Unit

  def dropIndex(indexName: String): Unit =
    guardDDL()
    indexes.get(indexName) match
      case Some(meta) =>
        tables.get(meta.tableName).foreach(_.tableIndexes.remove(indexName))
        indexes.remove(indexName)
      case None => sys.error(s"index '$indexName' not found")

  def hasIndex(name: String): Boolean = indexes contains name

  def beginTransaction(): Unit = ()
  def commitTransaction(): Unit = ()
  def rollbackTransaction(): Unit = ()
  def inTransaction: Boolean = false
  def isTransactionAborted: Boolean = false
  def markTransactionAborted(): Unit = ()

  override def toString: String = s"[Database '$name': ${tables map ((_, t) => t) mkString ", "}]"

abstract class Table(var name: String, specs: Seq[Spec]) extends Process:

  protected[rdb] val columns   = new ArrayBuffer[ColumnSpec]
  protected val columnMap      = new mutable.HashMap[String, Int]
  protected[rdb] val autoMap   = new mutable.HashMap[String, Value]
  private var _meta: Metadata = Metadata(Vector.empty)
  protected[rdb] var primaryKey: Option[PrimaryKeySpec] = None
  protected[rdb] val constraints                       = new ArrayBuffer[Spec]
  protected[rdb] val tableIndexes                      = new mutable.HashMap[String, TableIndex]

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

  def auto(col: String): Value =
    autoMap get col match
      case None =>
        val first = columns(columnMap(col)).typ.init

        autoMap(col) = first
        first
      case Some(cur) =>
        val next = cur.next

        autoMap(col) = next
        next

  protected[rdb] def restoreAutoState(state: Map[String, Value]): Unit = autoMap ++= state

  // Abstract storage methods for subclasses to implement
  protected def addColumnData(defaultValue: Value): Unit
  protected def dropColumnData(index: Int): Unit
  protected def convertColumnData(index: Int, newType: Type): Unit
  protected def hasNullInColumn(index: Int): Boolean

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
      case _ => false
    }
    if idx < 0 then sys.error(s"constraint '$constraintName' not found")
    constraints(idx) match
      case _: PrimaryKeySpec => primaryKey = None
      case _ =>
    constraints.remove(idx)

  def insert(row: Map[String, Value], returning: Option[Ident]): Map[String, Value] =
    val (keys, values) = row.toSeq.unzip

    bulkInsert(keys, Seq(values), returning)

  protected def addRow(row: Seq[Value]): Unit

  def bulkInsert(header: Seq[String], rows: Seq[Seq[Value]], returning: Option[Ident]): Map[String, Value] =
    val headerSet = header.toSet
    val columnSet = columnMap.keySet

    require(headerSet subsetOf columnSet, s"unknown columns: ${headerSet diff columnSet mkString ", "}")

    val missingSet = columnSet diff headerSet
    val missing    =
      for (m <- missingSet diff autoSet)
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
          if s.required then problem(v, s"column '${s.name}' is required")
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

      // Enforce NOT NULL for PRIMARY KEY columns
      primaryKey.foreach { pk =>
        for colName <- pk.columns do
          val idx = columnMap(colName)
          if arr(idx).isNull then
            sys.error(s"null value in column \"$colName\" violates not-null constraint")
      }

      if returning.isDefined then
        val idx =
          columnMap getOrElse (returning.get.name, problem(returning.get, s"column '${returning.get.name}' not found"))

        result += (returning.get.name -> arr(idx))

      addRow(arr to immutable.ArraySeq)

    result

trait Spec
case class ColumnSpec(
    name: String,
    typ: Type,
    required: Boolean = false,
    indexed: Boolean = false,
    unique: Boolean = false,
    fk: Option[(String, String)] = None,
    default: Option[Value] = None,
) extends Spec

// Table-level constraint specifications
case class PrimaryKeySpec(columns: Seq[String], name: Option[String] = None) extends Spec
case class UniqueSpec(columns: Seq[String], name: Option[String] = None) extends Spec  
case class ForeignKeySpec(columns: Seq[String], referencedTable: String, referencedColumns: Seq[String], name: Option[String] = None) extends Spec

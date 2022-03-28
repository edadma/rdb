package io.github.edadma.rdb

import io.github.edadma.dllist.DLList
import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

abstract class DB:

  val name: String

  private val tables = new mutable.HashMap[String, Table]
  private val types = new mutable.HashMap[String, Type]

  def hasTable(name: String): Boolean = tables contains name

  def getTable(name: String): Option[Table] = tables get name

  def addTable(name: String, specs: Seq[Spec]): Table

  def createTable(name: String, specs: Seq[Spec]): Table =
    require(!(tables contains name), s"table '$name' already exists")

    val res = addTable(name, specs)

    tables(name) = res
    res

  def addEnum(name: String, labels: Seq[String]): Unit

  def createEnum(name: String, labels: Seq[String]): Unit =
    require(!types.contains(name), s"type $name already exists")

    types(name) = EnumType(name, labels.toIndexedSeq)
    addEnum(name, labels)

  def hasType(name: String): Boolean = types contains name

  def getType(name: String): Option[Type] = types get name

  override def toString: String = s"[Database '$name': ${tables map ((_, t) => t) mkString ", "}]"

abstract class Table(val name: String, specs: Seq[Spec]) extends Process:

//  private class TableRow(var deleted: Boolean, val data: Array[Value])

  protected val columns = new ArrayBuffer[ColumnSpec]
  protected val columnMap = new mutable.HashMap[String, Int]
  private val autoMap = new mutable.HashMap[String, Value]
  private var _meta: Metadata = Metadata(Vector.empty)

  specs foreach {
    case s: ColumnSpec => createColumn(s)
    case _             =>
  }

  private val autoSet = columns filter (_.auto) map (_.name) toSet

  def meta: Metadata = _meta

  def iterator(ctx: Seq[Row]): RowIterator

  def hasColumn(name: String): Boolean = columnMap contains name

  def addColumn(spec: ColumnSpec): Unit

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

  def insert(row: Map[String, Value]): Map[String, Value] =
    val (keys, values) = row.toSeq.unzip

    bulkInsert(keys, Seq(values))

  def addRow(row: Seq[Value]): Unit

  def bulkInsert(header: Seq[String], rows: Seq[Seq[Value]]): Map[String, Value] =
    val headerSet = header.toSet
    val columnSet = columnMap.keySet

    require(headerSet subsetOf columnSet, s"unknown columns: ${headerSet diff columnSet mkString ", "}")

    val missingSet = columnSet diff headerSet
    val missing =
      for (m <- missingSet diff autoSet)
        yield
          val idx = columnMap(m)
          val s = columns(idx)

          if s.required && s.default.isEmpty then sys.error(s"bulkInsert: column '$m' is required and has no default")

          if s.pk then sys.error(s"bulkInsert: column '$m' is a required primary key")

          (idx, s.default getOrElse NullValue())
    val autos = autoSet intersect missingSet map (c => (c, columnMap(c)))
    val mapping = header map (h => meta.columnMap(h)._1)
    val specs = header map (h => columns(columnMap(h)))
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
      addRow(arr)

    result

trait Spec
case class ColumnSpec(
    name: String,
    typ: Type,
    auto: Boolean = false,
    required: Boolean = false,
    pk: Boolean = false,
    indexed: Boolean = false,
    unique: Boolean = false,
    fk: Option[String] = None,
    default: Option[Value] = None,
) extends Spec

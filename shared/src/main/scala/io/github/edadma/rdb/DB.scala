package io.github.edadma.rdb

import io.github.edadma.dllist.DLList
import scala.collection.immutable.ArraySeq
import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class DB(val name: String = "mem"):

  private val tables = new mutable.HashMap[String, Table]
  private val types = new mutable.HashMap[String, Type]

  def hasTable(name: String): Boolean = tables contains name

  def getTable(name: String): Option[Table] = tables get name

  def createTable(name: String, specs: Seq[Spec]): Table =
    require(!(tables contains name), s"table '$name' already exists")

    val res = Table(name, specs)

    tables(name) = res
    res

  def createEnum(name: String, labels: Seq[String]): Unit =
    require(!types.contains(name), s"type $name already exists")

    types(name) = EnumType(name, labels.toIndexedSeq)

  def hasType(name: String): Boolean = types contains name

  def getType(name: String): Option[Type] = types get name

  override def toString: String = s"[Database '$name': ${tables map ((_, t) => t) mkString ", "}]"

class Table(val name: String, spec: Seq[Spec]) extends Process:

//  private class TableRow(var deleted: Boolean, val data: Array[Value])

  private val columns = new ArrayBuffer[ColumnSpec]
  private val columnMap = new mutable.HashMap[String, Int]
  private val data = new DLList[Array[Value]]
  private val autoMap = new mutable.HashMap[String, Value]
  private var _meta: Metadata = Metadata(Vector.empty)

  spec foreach {
    case s: ColumnSpec => addColumn(s)
    case _             =>
  }

  private val autoSet = columns filter (_.auto) map (_.name) toSet

  def meta: Metadata = _meta

  def iterator(ctx: Seq[Row]): RowIterator =
    data.nodeIterator map (n => Row(n.element to ArraySeq, meta, Some(updater(n.element)), Some(deleter(n))))

  def hasColumn(name: String): Boolean = columnMap contains name

  // (name, typ, pk, auto, required, indexed, unique, fk)
  def addColumn(spec: ColumnSpec): Unit =
    require(!(columnMap contains spec.name), s"duplicate column '${spec.name}'")
    columnMap(spec.name) = columns.length
    columns += spec
    _meta = Metadata(columns to ArraySeq map (s => ColumnMetadata(Some(name), s.name, s.typ)))

  def rows: Int = data.length

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
      data += arr

    result

  class Updater private[Table] (row: Array[Value]) extends (Seq[(String, Value)] => Unit):
    def apply(update: Seq[(String, Value)]): Unit =
      for ((k, v) <- update)
        val col = columnMap.getOrElse(k, sys.error(s"table '$name' has no column '$k'"))
        val spec = columns(col)

        row(col) = spec.typ.convert(v)

    override def toString: String = "[MemoryDB Updater]"

  private def updater(row: Array[Value]) = new Updater(row)

  class Deleter private[Table] (node: data.Node) extends (() => Unit):
    def apply(): Unit = node.unlink

    override def toString: String = "[MemoryDB Deleter]"

  private def deleter(node: data.Node) = new Deleter(node)

  override def toString: String = s"[MemoryTable '$name': $meta; ${data map (_.toSeq)}]"

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

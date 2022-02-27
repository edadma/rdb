package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq
import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class MemoryDB(val name: String) extends DB:

  private val tables = new mutable.HashMap[String, MemoryTable]

  def hasTable(name: String): Boolean = tables contains name

  def table(name: String): Option[Table] = tables get name

  def create(name: String, specs: Seq[Spec]): Table =
    if tables contains name then sys.error(s"table '$name' already exists")
    else
      val res = MemoryTable(name, spec)

      tables(name) = res
      res

  override def toString: String = s"[Database '$name': ${tables map ((_, t) => t) mkString ", "}]"

class MemoryTable(val name: String, spec: Seq[Spec]) extends Table:

  private val columns = new ArrayBuffer[ColumnSpec]
  private val columnMap = new mutable.HashMap[String, Int]
  private val data = new ArrayBuffer[Array[Value]]
  private val auto = new mutable.HashMap[String, Int]
  private var _meta: Metadata = Metadata(Vector.empty)
  private val autoSet = columns filter (_.auto) map (_.name) toSet

  spec foreach {
    case s: ColumnSpec => addColumn(s)
    case _             =>
  }

  def meta: Metadata = _meta

  def iterator(ctx: Seq[Row]): RowIterator = data.iterator map (r => Row(r to ArraySeq, meta))

  def hasColumn(name: String): Boolean = columnMap contains name

  // (name, typ, pk, auto, required, indexed, unique, fk)
  def addColumn(spec: ColumnSpec): Unit =
    require(!(columnMap contains spec.name), s"duplicate column '${spec.name}'")
    columnMap(spec.name) = columns.length
    columns += spec
    _meta = Metadata(columns to ArraySeq map (s => ColumnMetadata(Some(name), s.name, s.typ)))

  def rows: Int = data.length

  def row(idx: Int): Row = Row(data(idx) to ArraySeq, meta)

  def increment(col: String): Value =
    auto get col match
      case None =>
        auto(col) = 1
        ONE
      case Some(cur) =>
        auto(col) = cur + 1
        NumberValue(cur)

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

          (idx, s.default getOrElse NullValue())
    val autos = autoSet diff missingSet map (c => (c, columnMap(c)))
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

      for ((c, i) <- autos)
        arr(i) = increment(c)

      data += arr

    result

  override def toString: String = s"[MemoryTable '$name': $meta; ${data map (_.toSeq)}]"

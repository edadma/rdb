package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq
import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class MemoryDB(val name: String) extends DB:

  private val tables = new mutable.HashMap[String, MemoryTable]

  def exists(name: String): Boolean = tables contains name

  def table(name: String): Option[Table] = tables get name

  def create(name: String, spec: Seq[Spec]): Table =
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
  private var _meta: Metadata = Metadata(Vector.empty)

  spec foreach {
    case s: ColumnSpec => addColumn(s)
    case _             =>
  }

  def meta: Metadata = _meta

  def iterator(ctx: Seq[Row]): RowIterator = data.iterator map (r => Row(r to ArraySeq, meta))

  // (name, typ, pk, auto, required, indexed, unique, fk)
  def addColumn(spec: ColumnSpec): Unit =
    require(!(columnMap contains spec.name), s"duplicate column '${spec.name}'")
    columnMap(spec.name) = columns.length
    columns += spec
    _meta = Metadata(columns to ArraySeq map (s => ColumnMetadata(Some(name), s.name, s.typ)))

  def rows: Int = data.length

  def row(idx: Int): Row = Row(data(idx) to ArraySeq, meta)

  def insert(row: Map[String, Value]): Map[String, Value] =
    val (keys, values) = row.toSeq.unzip

    bulkInsert(keys, Seq(values))

  def bulkInsert(header: Seq[String], rows: Seq[Seq[Value]]): Map[String, Value] =
    require(header.toSet subsetOf columnMap.keySet)
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

      data += arr

    result

  override def toString: String = s"[MemoryTable '$name': $meta; ${data map (_.toSeq)}]"

package io.github.edadma.rdb

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
  private val data = new ArrayBuffer[Array[Value]]
  private var _meta: Metadata = Metadata(Vector.empty)

  spec foreach {
    case s: ColumnSpec => addColumn(s)
    case _             =>
  }

  def meta: Metadata = _meta

  def iterator(ctx: Seq[Row]): RowIterator = data.iterator map (r => Row(r.toIndexedSeq, meta))

  // (name, typ, pk, auto, required, indexed, unique, fk)
  def addColumn(spec: ColumnSpec): Unit =
    columns += spec
    _meta = Metadata(columns.toIndexedSeq map (s => ColumnMetadata(Some(name), s.name, s.typ)))

  def rows: Int = data.length

  def row(idx: Int): Row = Row(data(idx).toIndexedSeq, meta)

  def insert(row: Map[String, Value]): Map[String, Value] =
    val arr = new Array[Value](meta.width)

    row foreach ((k, v) => arr(meta.columnMap(k)._1) = v)
    data += arr
    Map.empty

  def bulkInsert(header: Seq[String], rows: Seq[Seq[Value]]): Unit =
    val mapping = header map (h => meta.columnMap(h)._1)

    for (r <- rows)
      val arr = new Array[Value](meta.width)

      for ((i, v) <- mapping zip r)
        arr(i) = v

      data += arr

  override def toString: String = s"[MemoryTable '$name': $meta; ${data map (_.toSeq)}]"

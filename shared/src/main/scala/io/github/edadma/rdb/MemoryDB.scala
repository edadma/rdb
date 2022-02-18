package io.github.edadma.rdb

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class MemoryDB(val name: String) extends DB:

  private val tables = new mutable.HashMap[String, MemoryTable]

  def exists(name: String): Boolean = tables contains name

  def table(name: String): Option[Table] = tables get name

  def create(name: String, spec: Seq[Spec]): Table =
    if (tables contains name) sys.error(s"table '$name' already exists")
    else
      val res = new MemoryTable(name, spec)

      tables(name) = res
      res

  override def toString: String = s"[Database '$name': ${tables map ((_, t) => t) mkString ", "}]"

class MemoryTable(val name: String, spec: Seq[Spec]) extends Table:

  private val columns = new ArrayBuffer[ColumnSpec]
  private val data = new ArrayBuffer[Array[Value]]
  private var _meta: Metadata = Metadata(Nil)

  spec foreach {
    case s: ColumnSpec => addColumn(s)
    case _             =>
  }

  def meta: Metadata = _meta

  def iterator: RowIterator = data.iterator.map(r => Row(r.toIndexedSeq, meta))

  // (name, typ, pk, auto, required, indexed, unique, fk)
  def addColumn(spec: ColumnSpec): Unit =
    columns += spec
    _meta = Metadata(columns.toSeq map (s => ColumnMetadata(s.name, s.typ)))

  def rows: Int = data.length

  def row(idx: Int): Row = Row(data(idx).toIndexedSeq, meta)

  def insert(row: Map[String, Value]): Map[String, Value] =
    val arr = new Array[Value](meta.cols)

    row foreach ((k, v) => arr(meta.columnIndices(k)) = v)
    data += arr
    Map.empty

  def bulkInsert(header: Seq[String], rows: Seq[Seq[Value]]): Unit =
    val mapping = header map meta.columnIndices

    for (r <- rows)
      val arr = new Array[Value](meta.cols)

      for ((i, v) <- mapping zip r)
        arr(i) = v

      data += arr

  override def toString: String = s"[MemoryTable '$name': $meta; ${data map (_.toSeq)}]"

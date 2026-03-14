package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import scala.collection.immutable.ArraySeq

/** A virtual table module defines how to create virtual table instances.
  * Register modules with `db.registerVirtualTableModule("name", module)`.
  *
  * Example:
  * {{{
  * object CsvModule extends VirtualTableModule:
  *   def create(tableName: String, args: Seq[String]): VirtualTableProvider =
  *     val path = args.head
  *     // ... parse CSV header to get columns ...
  *     VirtualTableProvider(columns, () => rowIterator)
  * }}}
  */
trait VirtualTableModule:
  /** Create a virtual table provider from the arguments in CREATE VIRTUAL TABLE ... USING module(args).
    * @param tableName the name of the virtual table being created
    * @param args the arguments passed in parentheses (as strings)
    * @return a VirtualTableProvider with column definitions and a row factory
    */
  def create(tableName: String, args: Seq[String]): VirtualTableProvider

/** Describes a virtual table's schema and how to produce rows. */
case class VirtualTableProvider(
    columns: Seq[(String, Type)],
    rowFactory: () => Iterator[IndexedSeq[Value]],
)

/** A read-only Table backed by a VirtualTableModule.
  * Appears in SHOW TABLES and can be queried like any other table.
  */
class VirtualTable(
    tableName: String,
    provider: VirtualTableProvider,
) extends Table(
      tableName,
      provider.columns.map { case (name, typ) =>
        ColumnSpec(name, typ)
      },
    ):

  def iterator(ctx: Seq[Row]): RowIterator =
    provider.rowFactory().map { values =>
      Row(values, meta, None, None)
    }

  // Virtual tables are read-only
  protected def addColumn(spec: ColumnSpec): Unit = {}
  protected def addColumnData(defaultValue: Value): Unit = {}
  protected def dropColumnData(index: Int): Unit = {}
  protected def convertColumnData(index: Int, newType: Type): Unit = {}
  protected def hasNullInColumn(index: Int): Boolean = false
  protected def addRow(row: Seq[Value]): Unit =
    throw new UnsupportedOperationException("INSERT into virtual table is not supported")
  def truncate(): Unit =
    throw new UnsupportedOperationException("TRUNCATE on virtual table is not supported")

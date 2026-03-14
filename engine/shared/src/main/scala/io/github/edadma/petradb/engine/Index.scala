package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import io.github.edadma.bptree.MemoryBPlusTree
import io.github.edadma.dllist.DLListNode
import io.github.edadma.stow.PageId

case class IndexMeta(
    name: String,
    tableName: String,
    columns: Seq[String],
    unique: Boolean,
    treeRecordPage: Int = 0,
    nextRowId: Long = 0,
    whereExpr: Option[Expr] = None,
    exprKeys: Option[Seq[Expr]] = None,
)

trait TableIndex:
  def meta: IndexMeta
  def columnIndices: IndexedSeq[Int]
  def extractKey(row: IndexedSeq[Value]): IndexedSeq[Value] =
    columnIndices.map(row(_))

class MemoryTableIndex(
    val meta: IndexMeta,
    val columnIndices: IndexedSeq[Int],
    val tree: MemoryBPlusTree[IndexedSeq[Value], DLListNode[Array[Value]]],
    private[engine] var nextRowId: Long,
) extends TableIndex

class PersistentTableIndex(
    val meta: IndexMeta,
    val columnIndices: IndexedSeq[Int],
    private[engine] var nextRowId: Long,
) extends TableIndex

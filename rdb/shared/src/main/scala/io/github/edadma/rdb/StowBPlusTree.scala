package io.github.edadma.rdb

import io.github.edadma.bptree.BPlusTree
import io.github.edadma.stow.{PageId, NoPage, PageStore, WriteBatch}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}
import scala.collection.mutable.ArrayBuffer

trait StowCodec[T]:
  def encode(out: DataOutputStream, value: T): Unit
  def decode(in: DataInputStream): T
  def encodedSize(value: T): Int

object StowCodec:
  val int: StowCodec[Int] = new StowCodec[Int]:
    def encode(out: DataOutputStream, value: Int): Unit = out.writeInt(value)
    def decode(in: DataInputStream): Int = in.readInt()
    def encodedSize(value: Int): Int = 4

  val pageIdPair: StowCodec[(PageId, Int)] = new StowCodec[(PageId, Int)]:
    def encode(out: DataOutputStream, value: (PageId, Int)): Unit =
      out.writeInt(value._1)
      out.writeInt(value._2)
    def decode(in: DataInputStream): (PageId, Int) = (in.readInt(), in.readInt())
    def encodedSize(value: (PageId, Int)): Int = 8

  def valueSeq(using batch: WriteBatch, pageSize: Int): StowCodec[IndexedSeq[Value]] = new StowCodec[IndexedSeq[Value]]:
    def encode(out: DataOutputStream, value: IndexedSeq[Value]): Unit =
      out.writeShort(value.length)
      for v <- value do serializeValue(v, out, batch, pageSize)

    def decode(in: DataInputStream): IndexedSeq[Value] =
      val count = in.readUnsignedShort()
      val values = new ArrayBuffer[Value](count)
      for _ <- 0 until count do
        val (v, _) = deserializeValue(in, StowCodec.dummyStore, Map.empty)
        values += v
      values.toIndexedSeq

    def encodedSize(value: IndexedSeq[Value]): Int =
      val baos = new ByteArrayOutputStream()
      val out = new DataOutputStream(baos)
      encode(out, value)
      out.flush()
      baos.size

  private object dummyStore extends PageStore:
    def pageSize: Int = 4096
    def read(id: PageId): Array[Byte] = throw new UnsupportedOperationException("chain reading not supported in index keys")
    def modify(fn: WriteBatch => Unit): Unit = throw new UnsupportedOperationException
    def beginTransaction(): io.github.edadma.stow.Transaction = throw new UnsupportedOperationException
    def metaRoot: PageId = NoPage
    def close(): Unit = ()

// Node page layout:
//   [0]        node type: 0 = leaf, 1 = internal
//   [1..4]     parent PageId (4 bytes)
//   [5..8]     prev PageId (4 bytes)
//   [9..12]    next PageId (4 bytes)
//   [13..14]   key count (2 bytes)
//   [15..16]   branch count (2 bytes) - for internal nodes
//   [17..]     serialized key/value/branch data
//
// Tree record page layout:
//   [0..3]   root PageId
//   [4..7]   first PageId
//   [8..11]  last PageId

class StowBPlusTree[K: Ordering, V](
    val order: Int,
    store: PageStore,
    batch: WriteBatch,
    val treeRecordPage: PageId,
    keyCodec: StowCodec[K],
    valueCodec: StowCodec[V],
) extends BPlusTree[K, V]:

  private val LEAF_NODE: Byte = 0
  private val INTERNAL_NODE: Byte = 1

  private val NODE_TYPE = 0
  private val NODE_PARENT = 1
  private val NODE_PREV = 5
  private val NODE_NEXT = 9
  private val NODE_LENGTH = 13
  private val NODE_BRANCHES = 15 // branch count for internal nodes
  private val NODE_DATA = 17

  protected type N = PageId

  private def readTreeRecord(): (PageId, PageId, PageId) =
    val data = batch.read(treeRecordPage)
    val in = new DataInputStream(new ByteArrayInputStream(data))
    (in.readInt(), in.readInt(), in.readInt())

  private def writeTreeRecord(rootId: PageId, firstId: PageId, lastId: PageId): Unit =
    val data = new Array[Byte](store.pageSize)
    val baos = new ByteArrayOutputStream()
    val out = new DataOutputStream(baos)
    out.writeInt(rootId)
    out.writeInt(firstId)
    out.writeInt(lastId)
    out.flush()
    System.arraycopy(baos.toByteArray, 0, data, 0, 12)
    batch.write(treeRecordPage, data)

  private val (initRoot, initFirst, initLast) = readTreeRecord()

  protected var root: PageId = initRoot
  protected var first: PageId = initFirst
  protected var last: PageId = initLast
  protected var lastlen: Int = if initLast != NoPage then nodeLength(initLast) else 0

  // Cached node for overflow handling
  private var savedNode: PageId = NoPage
  private var savedIsLeaf: Boolean = false
  private var savedLength: Int = 0
  private var savedKeys: ArrayBuffer[K] = new ArrayBuffer[K]
  private var savedValues: ArrayBuffer[V] = new ArrayBuffer[V]
  private var savedBranches: ArrayBuffer[PageId] = new ArrayBuffer[PageId]

  // -- Page read/write helpers --

  private def readPageId(data: Array[Byte], offset: Int): PageId =
    ((data(offset) & 0xff) << 24) | ((data(offset + 1) & 0xff) << 16) |
      ((data(offset + 2) & 0xff) << 8) | (data(offset + 3) & 0xff)

  private def writePageId(data: Array[Byte], offset: Int, id: PageId): Unit =
    data(offset) = ((id >> 24) & 0xff).toByte
    data(offset + 1) = ((id >> 16) & 0xff).toByte
    data(offset + 2) = ((id >> 8) & 0xff).toByte
    data(offset + 3) = (id & 0xff).toByte

  private def readShort(data: Array[Byte], offset: Int): Int =
    ((data(offset) & 0xff) << 8) | (data(offset + 1) & 0xff)

  private def writeShort(data: Array[Byte], offset: Int, value: Int): Unit =
    data(offset) = ((value >> 8) & 0xff).toByte
    data(offset + 1) = (value & 0xff).toByte

  private def readNodeKeys(data: Array[Byte], count: Int): ArrayBuffer[K] =
    val in = new DataInputStream(new ByteArrayInputStream(data, NODE_DATA, data.length - NODE_DATA))
    val keys = new ArrayBuffer[K](count)
    for _ <- 0 until count do keys += keyCodec.decode(in)
    keys

  private def readLeafData(data: Array[Byte], count: Int): (ArrayBuffer[K], ArrayBuffer[V]) =
    val in = new DataInputStream(new ByteArrayInputStream(data, NODE_DATA, data.length - NODE_DATA))
    val keys = new ArrayBuffer[K](count)
    val values = new ArrayBuffer[V](count)
    for _ <- 0 until count do keys += keyCodec.decode(in)
    for _ <- 0 until count do values += valueCodec.decode(in)
    (keys, values)

  private def readInternalData(data: Array[Byte], keyCount: Int): (ArrayBuffer[K], ArrayBuffer[PageId]) =
    val branchCount = readShort(data, NODE_BRANCHES)
    val in = new DataInputStream(new ByteArrayInputStream(data, NODE_DATA, data.length - NODE_DATA))
    val keys = new ArrayBuffer[K](keyCount)
    val branches = new ArrayBuffer[PageId](branchCount)
    for _ <- 0 until keyCount do keys += keyCodec.decode(in)
    for _ <- 0 until branchCount do branches += in.readInt()
    (keys, branches)

  private def writeLeafPage(pageId: PageId, parent: PageId, prev: PageId, next: PageId, keys: scala.collection.Seq[K], values: scala.collection.Seq[V]): Unit =
    val page = new Array[Byte](store.pageSize)
    page(NODE_TYPE) = LEAF_NODE
    writePageId(page, NODE_PARENT, parent)
    writePageId(page, NODE_PREV, prev)
    writePageId(page, NODE_NEXT, next)
    writeShort(page, NODE_LENGTH, keys.length)
    val baos = new ByteArrayOutputStream()
    val out = new DataOutputStream(baos)
    for k <- keys do keyCodec.encode(out, k)
    for v <- values do valueCodec.encode(out, v)
    out.flush()
    val serialized = baos.toByteArray
    System.arraycopy(serialized, 0, page, NODE_DATA, serialized.length)
    batch.write(pageId, page)

  private def writeInternalPage(pageId: PageId, parent: PageId, prev: PageId, next: PageId, keys: scala.collection.Seq[K], branches: scala.collection.Seq[PageId]): Unit =
    val page = new Array[Byte](store.pageSize)
    page(NODE_TYPE) = INTERNAL_NODE
    writePageId(page, NODE_PARENT, parent)
    writePageId(page, NODE_PREV, prev)
    writePageId(page, NODE_NEXT, next)
    writeShort(page, NODE_LENGTH, keys.length)
    writeShort(page, NODE_BRANCHES, branches.length)
    val baos = new ByteArrayOutputStream()
    val out = new DataOutputStream(baos)
    for k <- keys do keyCodec.encode(out, k)
    for b <- branches do out.writeInt(b)
    out.flush()
    val serialized = baos.toByteArray
    System.arraycopy(serialized, 0, page, NODE_DATA, serialized.length)
    batch.write(pageId, page)

  // Helper to read parent/prev/next from existing page data
  private def headerOf(data: Array[Byte]): (PageId, PageId, PageId) =
    (readPageId(data, NODE_PARENT), readPageId(data, NODE_PREV), readPageId(data, NODE_NEXT))

  // -- BPlusTree abstract method implementations --

  protected def nul: PageId = NoPage

  protected def isLeaf(node: PageId): Boolean =
    if node == savedNode then savedIsLeaf
    else batch.read(node)(NODE_TYPE) == LEAF_NODE

  protected def nodeLength(node: PageId): Int =
    if node == savedNode then savedLength
    else readShort(batch.read(node), NODE_LENGTH)

  protected def getParent(node: PageId): PageId =
    readPageId(batch.read(node), NODE_PARENT)

  protected def setParent(node: PageId, p: PageId): Unit =
    val data = batch.read(node)
    writePageId(data, NODE_PARENT, p)
    batch.write(node, data)

  protected def getNext(node: PageId): PageId =
    readPageId(batch.read(node), NODE_NEXT)

  protected def setNext(node: PageId, p: PageId): Unit =
    val data = batch.read(node)
    writePageId(data, NODE_NEXT, p)
    batch.write(node, data)

  protected def getPrev(node: PageId): PageId =
    readPageId(batch.read(node), NODE_PREV)

  protected def setPrev(node: PageId, p: PageId): Unit =
    val data = batch.read(node)
    writePageId(data, NODE_PREV, p)
    batch.write(node, data)

  protected def getKey(node: PageId, index: Int): K =
    if node == savedNode then savedKeys(index)
    else
      val data = batch.read(node)
      readNodeKeys(data, readShort(data, NODE_LENGTH))(index)

  protected def getKeys(node: PageId): Seq[K] =
    if node == savedNode then savedKeys.toSeq
    else
      val data = batch.read(node)
      readNodeKeys(data, readShort(data, NODE_LENGTH)).toSeq

  protected def setKey(node: PageId, index: Int, key: K): Unit =
    if node == savedNode then
      savedKeys(index) = key
    else
      val data = batch.read(node)
      val count = readShort(data, NODE_LENGTH)
      val (parent, prev, next) = headerOf(data)
      if data(NODE_TYPE) == LEAF_NODE then
        val (keys, values) = readLeafData(data, count)
        keys(index) = key
        writeLeafPage(node, parent, prev, next, keys, values)
      else
        val (keys, branches) = readInternalData(data, count)
        keys(index) = key
        writeInternalPage(node, parent, prev, next, keys, branches)

  protected def getValue(node: PageId, index: Int): V =
    if node == savedNode then savedValues(index)
    else
      val data = batch.read(node)
      readLeafData(data, readShort(data, NODE_LENGTH))._2(index)

  protected def getValues(node: PageId): Seq[V] =
    if node == savedNode then savedValues.toSeq
    else
      val data = batch.read(node)
      readLeafData(data, readShort(data, NODE_LENGTH))._2.toSeq

  protected def setValue(node: PageId, index: Int, v: V): Unit =
    if node == savedNode then
      savedValues(index) = v
    else
      val data = batch.read(node)
      val count = readShort(data, NODE_LENGTH)
      val (parent, prev, next) = headerOf(data)
      val (keys, values) = readLeafData(data, count)
      values(index) = v
      writeLeafPage(node, parent, prev, next, keys, values)

  protected def getBranch(node: PageId, index: Int): PageId =
    if node == savedNode then savedBranches(index)
    else
      val data = batch.read(node)
      readInternalData(data, readShort(data, NODE_LENGTH))._2(index)

  protected def getBranches(node: PageId): Seq[PageId] =
    if node == savedNode then savedBranches.toSeq
    else
      val data = batch.read(node)
      readInternalData(data, readShort(data, NODE_LENGTH))._2.toSeq

  protected def addKey(node: PageId, key: K): Unit =
    if node == savedNode then
      savedKeys += key
      savedLength += 1
    else
      val data = batch.read(node)
      val count = readShort(data, NODE_LENGTH)
      val (parent, prev, next) = headerOf(data)
      if data(NODE_TYPE) == LEAF_NODE then
        val (keys, values) = readLeafData(data, count)
        keys += key
        writeLeafPage(node, parent, prev, next, keys, values)
      else
        val (keys, branches) = readInternalData(data, count)
        keys += key
        writeInternalPage(node, parent, prev, next, keys, branches)

  protected def addBranch(node: PageId, branch: PageId): Unit =
    if node == savedNode then
      savedBranches += branch
    else
      val data = batch.read(node)
      val count = readShort(data, NODE_LENGTH)
      val (parent, prev, next) = headerOf(data)
      val (keys, branches) = readInternalData(data, count)
      branches += branch
      writeInternalPage(node, parent, prev, next, keys, branches)

  protected def setBranch(node: PageId, index: Int, branch: PageId): Unit =
    if node == savedNode then
      savedBranches(index) = branch
    else
      val data = batch.read(node)
      val count = readShort(data, NODE_LENGTH)
      val (parent, prev, next) = headerOf(data)
      val (keys, branches) = readInternalData(data, count)
      branches(index) = branch
      writeInternalPage(node, parent, prev, next, keys, branches)

  protected def newLeaf(parent: PageId): PageId =
    val pageId = batch.allocate()
    writeLeafPage(pageId, parent, NoPage, NoPage, Nil, Nil)
    pageId

  protected def newInternal(parent: PageId): PageId =
    val pageId = batch.allocate()
    writeInternalPage(pageId, parent, NoPage, NoPage, Nil, Nil)
    pageId

  protected def newRoot(branch: PageId): PageId =
    val pageId = batch.allocate()
    writeInternalPage(pageId, NoPage, NoPage, NoPage, Nil, Seq(branch))
    writeTreeRecord(pageId, first, last)
    pageId

  protected def freeNode(node: PageId): Unit =
    if node != NoPage then batch.free(node)

  protected def freeKey(node: PageId, index: Int): Unit = ()
  protected def freeValue(node: PageId, index: Int): Unit = ()

  protected def setRoot(node: PageId): Unit =
    writeTreeRecord(node, first, last)

  protected def setFirst(leaf: PageId): Unit =
    writeTreeRecord(root, leaf, last)

  protected def setLast(leaf: PageId): Unit =
    writeTreeRecord(root, first, leaf)

  protected def insertLeaf(node: PageId, index: Int, key: K, value: V): Unit =
    val data = batch.read(node)
    val count = readShort(data, NODE_LENGTH)

    if count < order - 1 then
      val (parent, prev, next) = headerOf(data)
      val (keys, values) = readLeafData(data, count)
      keys.insert(index, key)
      values.insert(index, value)
      writeLeafPage(node, parent, prev, next, keys, values)
    else
      if savedNode != NoPage then sys.error("a node is already being saved")
      val (keys, values) = readLeafData(data, count)
      keys.insert(index, key)
      values.insert(index, value)
      savedNode = node
      savedIsLeaf = true
      savedLength = count + 1
      savedKeys = keys
      savedValues = values
      savedBranches = new ArrayBuffer[PageId]

  protected def insertInternal(node: PageId, keyIndex: Int, key: K, branchIndex: Int, branch: PageId): Unit =
    val data = batch.read(node)
    val count = readShort(data, NODE_LENGTH)

    if count < order - 1 then
      val (parent, prev, next) = headerOf(data)
      val (keys, branches) = readInternalData(data, count)
      keys.insert(keyIndex, key)
      branches.insert(branchIndex, branch)
      writeInternalPage(node, parent, prev, next, keys, branches)
    else
      if savedNode != NoPage then sys.error("a node is already being saved")
      val (keys, branches) = readInternalData(data, count)
      keys.insert(keyIndex, key)
      branches.insert(branchIndex, branch)
      savedNode = node
      savedIsLeaf = false
      savedLength = count + 1
      savedKeys = keys
      savedValues = new ArrayBuffer[V]
      savedBranches = branches

  protected def moveLeaf(src: PageId, begin: Int, end: Int, dst: PageId, index: Int): Unit =
    if savedNode == NoPage then
      val srcData = batch.read(src)
      val srcCount = readShort(srcData, NODE_LENGTH)
      val (srcKeys, srcValues) = readLeafData(srcData, srcCount)
      val (srcParent, srcPrev, srcNext) = headerOf(srcData)

      val dstData = batch.read(dst)
      val dstCount = readShort(dstData, NODE_LENGTH)
      val (dstKeys, dstValues) = readLeafData(dstData, dstCount)
      val (dstParent, dstPrev, dstNext) = headerOf(dstData)

      dstKeys.insertAll(index, srcKeys.view.slice(begin, end))
      dstValues.insertAll(index, srcValues.view.slice(begin, end))
      srcKeys.remove(begin, end - begin)
      srcValues.remove(begin, end - begin)

      writeLeafPage(src, srcParent, srcPrev, srcNext, srcKeys, srcValues)
      writeLeafPage(dst, dstParent, dstPrev, dstNext, dstKeys, dstValues)
    else
      val dstKeys = savedKeys.view.slice(begin, end).to(ArrayBuffer)
      val dstValues = savedValues.view.slice(begin, end).to(ArrayBuffer)
      val srcLen = savedLength - (end - begin)
      val srcKeys = savedKeys.view.take(srcLen).to(ArrayBuffer)
      val srcValues = savedValues.view.take(srcLen).to(ArrayBuffer)

      val srcData = batch.read(src)
      val (srcParent, srcPrev, srcNext) = headerOf(srcData)
      writeLeafPage(src, srcParent, srcPrev, srcNext, srcKeys, srcValues)

      val dstData = batch.read(dst)
      val (dstParent, dstPrev, dstNext) = headerOf(dstData)
      writeLeafPage(dst, dstParent, dstPrev, dstNext, dstKeys, dstValues)

      savedNode = NoPage

  protected def moveInternal(src: PageId, begin: Int, end: Int, dst: PageId, index: Int): Unit =
    if savedNode == NoPage then
      val srcData = batch.read(src)
      val srcCount = readShort(srcData, NODE_LENGTH)
      val (srcKeys, srcBranches) = readInternalData(srcData, srcCount)
      val (srcParent, srcPrev, srcNext) = headerOf(srcData)

      val dstData = batch.read(dst)
      val dstCount = readShort(dstData, NODE_LENGTH)
      val (dstKeys, dstBranches) = readInternalData(dstData, dstCount)
      val (dstParent, dstPrev, dstNext) = headerOf(dstData)

      dstKeys.insertAll(index, srcKeys.view.slice(begin, end))
      dstBranches.insertAll(index + 1, srcBranches.view.slice(begin + 1, end + 1))
      srcKeys.remove(begin, end - begin)
      srcBranches.remove(begin + 1, end - begin)

      writeInternalPage(src, srcParent, srcPrev, srcNext, srcKeys, srcBranches)
      writeInternalPage(dst, dstParent, dstPrev, dstNext, dstKeys, dstBranches)
    else
      val dstKeys = savedKeys.view.slice(begin, end).to(ArrayBuffer)
      val dstBranches = savedBranches.view.slice(begin + 1, end + 1).to(ArrayBuffer)
      val srcLen = savedLength - (end - begin)
      val srcKeysRemaining = savedKeys.view.take(srcLen).to(ArrayBuffer)
      val srcBranchesRemaining = savedBranches.view.take(begin + 1).to(ArrayBuffer)

      val srcData = batch.read(src)
      val (srcParent, srcPrev, srcNext) = headerOf(srcData)
      writeInternalPage(src, srcParent, srcPrev, srcNext, srcKeysRemaining, srcBranchesRemaining)

      val dstData = batch.read(dst)
      val dstCount = readShort(dstData, NODE_LENGTH)
      val (dstParent, dstPrev, dstNext) = headerOf(dstData)
      val (existingDstKeys, existingDstBranches) = readInternalData(dstData, dstCount)

      existingDstKeys.insertAll(0, dstKeys)
      if existingDstBranches.isEmpty then
        existingDstBranches ++= dstBranches
      else
        existingDstBranches.insertAll(1, dstBranches)

      writeInternalPage(dst, dstParent, dstPrev, dstNext, existingDstKeys, existingDstBranches)

      savedNode = NoPage

  protected def removeLeaf(node: PageId, index: Int): Int =
    val data = batch.read(node)
    val count = readShort(data, NODE_LENGTH)
    val (parent, prev, next) = headerOf(data)
    val (keys, values) = readLeafData(data, count)
    keys.remove(index)
    values.remove(index)
    writeLeafPage(node, parent, prev, next, keys, values)
    keys.length

  protected def removeInternal(node: PageId, keyIndex: Int, branchIndex: Int): Int =
    if node == savedNode then
      savedKeys.remove(keyIndex)
      savedBranches.remove(branchIndex)
      savedLength -= 1
      savedLength
    else
      val data = batch.read(node)
      val count = readShort(data, NODE_LENGTH)
      val (parent, prev, next) = headerOf(data)
      val (keys, branches) = readInternalData(data, count)
      keys.remove(keyIndex)
      branches.remove(branchIndex)
      writeInternalPage(node, parent, prev, next, keys, branches)
      keys.length

object StowBPlusTree:
  def create[K: Ordering, V](
      order: Int,
      store: PageStore,
      batch: WriteBatch,
      keyCodec: StowCodec[K],
      valueCodec: StowCodec[V],
  ): StowBPlusTree[K, V] =
    val treeRecordPage = batch.allocate()
    val rootLeaf = batch.allocate()

    // Write empty leaf root
    val leafPage = new Array[Byte](store.pageSize)
    leafPage(0) = 0 // LEAF_NODE
    batch.write(rootLeaf, leafPage)

    // Write tree record
    val treeData = new Array[Byte](store.pageSize)
    val baos = new ByteArrayOutputStream()
    val out = new DataOutputStream(baos)
    out.writeInt(rootLeaf)
    out.writeInt(rootLeaf)
    out.writeInt(rootLeaf)
    out.flush()
    System.arraycopy(baos.toByteArray, 0, treeData, 0, 12)
    batch.write(treeRecordPage, treeData)

    new StowBPlusTree[K, V](order, store, batch, treeRecordPage, keyCodec, valueCodec)

  def open[K: Ordering, V](
      order: Int,
      store: PageStore,
      batch: WriteBatch,
      treeRecordPage: PageId,
      keyCodec: StowCodec[K],
      valueCodec: StowCodec[V],
  ): StowBPlusTree[K, V] =
    new StowBPlusTree[K, V](order, store, batch, treeRecordPage, keyCodec, valueCodec)

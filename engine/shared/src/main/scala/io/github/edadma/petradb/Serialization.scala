package io.github.edadma.petradb

import io.github.edadma.dal.{BigDecType, DoubleType as DDoubleType, IntType as DIntType, LongType as DLongType}
import io.github.edadma.stow.{PageId, NoPage, WriteBatch, PageStore}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}
import java.time.{Duration, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime, ZoneOffset}
import scala.collection.mutable.ArrayBuffer

// Inline threshold: TEXT/BYTEA <= this many bytes stored inline; larger uses page chains
private val InlineThreshold = 64

// Value type tags
private val TagNull: Byte        = 0x00
private val TagBoolean: Byte     = 0x01
private val TagSmallint: Byte    = 0x02
private val TagInt: Byte         = 0x03
private val TagLong: Byte        = 0x04
private val TagDouble: Byte      = 0x05
private val TagNumeric: Byte     = 0x06
private val TagText: Byte        = 0x07
private val TagChar: Byte        = 0x08
private val TagDate: Byte        = 0x09
private val TagTime: Byte        = 0x0a
private val TagTimestamp: Byte   = 0x0b
private val TagTimestampTZ: Byte = 0x0c
private val TagInterval: Byte    = 0x0d
private val TagBytea: Byte       = 0x0e
private val TagUUID: Byte        = 0x0f
private val TagArray: Byte       = 0x10
private val TagObject: Byte      = 0x11
private val TagEnum: Byte        = 0x12
private val TagTimeTZ: Byte      = 0x13

// Sub-tags for inline vs chain
private val SubTagInline: Byte = 0x00
private val SubTagChain: Byte  = 0x01

// Type serialization tags
private val TTagSmallint: Byte      = 0x01
private val TTagInteger: Byte       = 0x02
private val TTagBigint: Byte        = 0x03
private val TTagSmallSerial: Byte   = 0x04
private val TTagSerial: Byte        = 0x05
private val TTagBigSerial: Byte     = 0x06
private val TTagDouble: Byte        = 0x07
private val TTagNumeric: Byte       = 0x08
private val TTagText: Byte          = 0x09
private val TTagChar: Byte          = 0x0a
private val TTagBoolean: Byte       = 0x0b
private val TTagDate: Byte          = 0x0c
private val TTagTime: Byte          = 0x0d
private val TTagTimestamp: Byte     = 0x0e
private val TTagTimestampTZ: Byte   = 0x0f
private val TTagInterval: Byte      = 0x10
private val TTagBytea: Byte         = 0x11
private val TTagUUID: Byte          = 0x12
private val TTagArray: Byte         = 0x13
private val TTagJSON: Byte          = 0x14
private val TTagEnum: Byte          = 0x15
private val TTagTimeTZ: Byte        = 0x16

// ---- Page Chains for large values ----

case class ChainRef(firstPage: PageId, totalLength: Int)

def writeChain(data: Array[Byte], batch: WriteBatch, pageSize: Int): PageId =
  val payloadPerPage = pageSize - 4 // first 4 bytes = next page pointer
  val pages          = new ArrayBuffer[PageId]
  var offset         = 0

  while offset < data.length do
    val page = batch.allocate()
    pages += page
    offset += payloadPerPage

  // Write pages in reverse to build the chain
  var nextPage: PageId = NoPage
  var i                = pages.length - 1
  offset = i * payloadPerPage

  while i >= 0 do
    val buf       = new Array[Byte](pageSize)
    val chunkSize = math.min(payloadPerPage, data.length - offset)
    // Write next pointer
    buf(0) = ((nextPage >> 24) & 0xff).toByte
    buf(1) = ((nextPage >> 16) & 0xff).toByte
    buf(2) = ((nextPage >> 8) & 0xff).toByte
    buf(3) = (nextPage & 0xff).toByte
    // Write payload
    System.arraycopy(data, offset, buf, 4, chunkSize)
    batch.write(pages(i), buf)
    nextPage = pages(i)
    i -= 1
    offset -= payloadPerPage

  pages.head

def readChain(firstPage: PageId, totalLength: Int, store: PageStore): Array[Byte] =
  val result         = new Array[Byte](totalLength)
  val payloadPerPage = store.pageSize - 4
  var currentPage    = firstPage
  var offset         = 0

  while currentPage != NoPage && offset < totalLength do
    val buf       = store.read(currentPage)
    val chunkSize = math.min(payloadPerPage, totalLength - offset)
    System.arraycopy(buf, 4, result, offset, chunkSize)
    offset += chunkSize
    currentPage = ((buf(0) & 0xff) << 24) | ((buf(1) & 0xff) << 16) | ((buf(2) & 0xff) << 8) | (buf(3) & 0xff)

  result

def readChainInBatch(firstPage: PageId, totalLength: Int, batch: WriteBatch, pageSize: Int): Array[Byte] =
  val result         = new Array[Byte](totalLength)
  val payloadPerPage = pageSize - 4
  var currentPage    = firstPage
  var offset         = 0

  while currentPage != NoPage && offset < totalLength do
    val buf       = batch.read(currentPage)
    val chunkSize = math.min(payloadPerPage, totalLength - offset)
    System.arraycopy(buf, 4, result, offset, chunkSize)
    offset += chunkSize
    currentPage = ((buf(0) & 0xff) << 24) | ((buf(1) & 0xff) << 16) | ((buf(2) & 0xff) << 8) | (buf(3) & 0xff)

  result

def freeChain(firstPage: PageId, batch: WriteBatch, pageSize: Int): Unit =
  var currentPage = firstPage

  while currentPage != NoPage do
    val buf = batch.read(currentPage)
    val next = ((buf(0) & 0xff) << 24) | ((buf(1) & 0xff) << 16) | ((buf(2) & 0xff) << 8) | (buf(3) & 0xff)
    batch.free(currentPage)
    currentPage = next

// ---- Value Serialization ----

def serializeValue(v: Value, out: DataOutputStream, batch: WriteBatch, pageSize: Int): Unit =
  v match
    case _: NullValue =>
      out.writeByte(TagNull)

    case BooleanValue(b) =>
      out.writeByte(TagBoolean)
      out.writeByte(if b then 1 else 0)

    case NumberValue(DIntType, n) =>
      out.writeByte(TagInt)
      out.writeInt(n.intValue)

    case NumberValue(DLongType, n) =>
      out.writeByte(TagLong)
      out.writeLong(n.longValue)

    case NumberValue(DDoubleType, n) =>
      out.writeByte(TagDouble)
      out.writeDouble(n.doubleValue)

    case NumberValue(BigDecType, n) =>
      out.writeByte(TagNumeric)
      val s = n.toString
      val bytes = s.getBytes("UTF-8")
      out.writeInt(bytes.length)
      out.write(bytes)

    case NumberValue(_, n) =>
      // Fallback: store as int
      out.writeByte(TagInt)
      out.writeInt(n.intValue)

    case TextValue(s) =>
      val bytes = s.getBytes("UTF-8")
      out.writeByte(TagText)
      writeInlineOrChain(bytes, out, batch, pageSize)

    case DateValue(d) =>
      out.writeByte(TagDate)
      out.writeInt(d.toEpochDay.toInt)

    case TimeValue(t) =>
      out.writeByte(TagTime)
      out.writeLong(t.toNanoOfDay)

    case TimestampValue(t) =>
      out.writeByte(TagTimestamp)
      out.writeLong(t.toLocalDate.toEpochDay)
      out.writeLong(t.toLocalTime.toNanoOfDay)

    case TimestampTZValue(t) =>
      out.writeByte(TagTimestampTZ)
      out.writeLong(t.toLocalDate.toEpochDay)
      out.writeLong(t.toLocalTime.toNanoOfDay)
      out.writeInt(t.getOffset.getTotalSeconds)

    case TimeTZValue(t) =>
      out.writeByte(TagTimeTZ)
      out.writeLong(t.toLocalTime.toNanoOfDay)
      out.writeInt(t.getOffset.getTotalSeconds)

    case IntervalValue(d) =>
      out.writeByte(TagInterval)
      out.writeLong(d.getSeconds)
      out.writeInt(d.getNano)

    case ByteaValue(data) =>
      out.writeByte(TagBytea)
      writeInlineOrChain(data, out, batch, pageSize)

    case UUIDValue(id) =>
      out.writeByte(TagUUID)
      val bytes = id.getBytes("UTF-8")
      out.writeShort(bytes.length)
      out.write(bytes)

    case ArrayValue(data) =>
      out.writeByte(TagArray)
      out.writeInt(data.length)
      for elem <- data do serializeValue(elem, out, batch, pageSize)

    case ObjectValue(properties) =>
      out.writeByte(TagObject)
      out.writeInt(properties.length)
      for (k, v) <- properties do
        val keyBytes = k.getBytes("UTF-8")
        out.writeInt(keyBytes.length)
        out.write(keyBytes)
        serializeValue(v, out, batch, pageSize)

    case EnumValue(ordinal, typ) =>
      out.writeByte(TagEnum)
      val nameBytes = typ.enumName.getBytes("UTF-8")
      out.writeShort(nameBytes.length)
      out.write(nameBytes)
      out.writeInt(ordinal)

    case _ =>
      // Fallback: serialize as text
      out.writeByte(TagText)
      val bytes = v.string.getBytes("UTF-8")
      writeInlineOrChain(bytes, out, batch, pageSize)

private def writeInlineOrChain(bytes: Array[Byte], out: DataOutputStream, batch: WriteBatch, pageSize: Int): Unit =
  if bytes.length <= InlineThreshold then
    out.writeByte(SubTagInline)
    out.writeShort(bytes.length)
    out.write(bytes)
  else
    out.writeByte(SubTagChain)
    val firstPage = writeChain(bytes, batch, pageSize)
    out.writeInt(firstPage)
    out.writeInt(bytes.length)

def deserializeValue(in: DataInputStream, store: PageStore, enumTypes: Map[String, EnumType]): (Value, Option[ChainRef]) =
  val tag = in.readByte()
  tag match
    case TagNull =>
      (NullValue(), None)

    case TagBoolean =>
      (BooleanValue(in.readByte() != 0), None)

    case TagSmallint =>
      (NumberValue(DIntType, in.readShort().toInt: java.lang.Integer), None)

    case TagInt =>
      (NumberValue(DIntType, in.readInt(): java.lang.Integer), None)

    case TagLong =>
      (NumberValue(DLongType, in.readLong(): java.lang.Long), None)

    case TagDouble =>
      (NumberValue(DDoubleType, in.readDouble(): java.lang.Double), None)

    case TagNumeric =>
      val len = in.readInt()
      val bytes = new Array[Byte](len)
      in.readFully(bytes)
      (NumberValue(BigDecimal(new String(bytes, "UTF-8"))), None)

    case TagText =>
      readInlineOrChain(in, store) match
        case (bytes, chainRef) => (TextValue(new String(bytes, "UTF-8")), chainRef)

    case TagChar =>
      readInlineOrChain(in, store) match
        case (bytes, chainRef) => (TextValue(new String(bytes, "UTF-8")), chainRef)

    case TagDate =>
      (DateValue(LocalDate.ofEpochDay(in.readInt().toLong)), None)

    case TagTime =>
      (TimeValue(LocalTime.ofNanoOfDay(in.readLong())), None)

    case TagTimestamp =>
      val epochDay = in.readLong()
      val nanoOfDay = in.readLong()
      (TimestampValue(LocalDateTime.of(LocalDate.ofEpochDay(epochDay), LocalTime.ofNanoOfDay(nanoOfDay))), None)

    case TagTimestampTZ =>
      val epochDay = in.readLong()
      val nanoOfDay = in.readLong()
      val offsetSec = in.readInt()
      (TimestampTZValue(OffsetDateTime.of(
        LocalDate.ofEpochDay(epochDay),
        LocalTime.ofNanoOfDay(nanoOfDay),
        ZoneOffset.ofTotalSeconds(offsetSec),
      )), None)

    case TagTimeTZ =>
      val nanoOfDay = in.readLong()
      val offsetSec = in.readInt()
      (TimeTZValue(OffsetTime.of(LocalTime.ofNanoOfDay(nanoOfDay), ZoneOffset.ofTotalSeconds(offsetSec))), None)

    case TagInterval =>
      val seconds = in.readLong()
      val nanos = in.readInt()
      (IntervalValue(Duration.ofSeconds(seconds, nanos.toLong)), None)

    case TagBytea =>
      readInlineOrChain(in, store) match
        case (bytes, chainRef) => (ByteaValue(bytes), chainRef)

    case TagUUID =>
      val len = in.readUnsignedShort()
      val bytes = new Array[Byte](len)
      in.readFully(bytes)
      (UUIDValue(new String(bytes, "UTF-8")), None)

    case TagArray =>
      val count = in.readInt()
      val elems = new ArrayBuffer[Value](count)
      val chains = new ArrayBuffer[Option[ChainRef]]
      for _ <- 0 until count do
        val (v, c) = deserializeValue(in, store, enumTypes)
        elems += v
        chains += c
      // For arrays, we combine chain refs — but for simplicity we don't track individual element chains
      (ArrayValue(elems.toIndexedSeq), None)

    case TagObject =>
      val count = in.readInt()
      val props = new ArrayBuffer[(String, Value)](count)
      for _ <- 0 until count do
        val keyLen = in.readInt()
        val keyBytes = new Array[Byte](keyLen)
        in.readFully(keyBytes)
        val key = new String(keyBytes, "UTF-8")
        val (v, _) = deserializeValue(in, store, enumTypes)
        props += ((key, v))
      (ObjectValue(props.toSeq), None)

    case TagEnum =>
      val nameLen = in.readUnsignedShort()
      val nameBytes = new Array[Byte](nameLen)
      in.readFully(nameBytes)
      val enumName = new String(nameBytes, "UTF-8")
      val ordinal = in.readInt()
      val enumType = enumTypes.getOrElse(enumName, sys.error(s"unknown enum type '$enumName'"))
      (EnumValue(ordinal, enumType), None)

    case other =>
      sys.error(s"unknown value tag: $other")

private def readInlineOrChain(in: DataInputStream, store: PageStore): (Array[Byte], Option[ChainRef]) =
  val subTag = in.readByte()
  subTag match
    case SubTagInline =>
      val len = in.readUnsignedShort()
      val bytes = new Array[Byte](len)
      in.readFully(bytes)
      (bytes, None)
    case SubTagChain =>
      val firstPage = in.readInt()
      val totalLen = in.readInt()
      val bytes = readChain(firstPage, totalLen, store)
      (bytes, Some(ChainRef(firstPage, totalLen)))
    case other =>
      sys.error(s"unknown sub-tag: $other")

// ---- Row Serialization ----

def serializeRow(values: Seq[Value], batch: WriteBatch, pageSize: Int): Array[Byte] =
  val baos = new ByteArrayOutputStream()
  val out  = new DataOutputStream(baos)
  for v <- values do serializeValue(v, out, batch, pageSize)
  out.flush()
  baos.toByteArray

def deserializeRow(data: Array[Byte], colCount: Int, store: PageStore, enumTypes: Map[String, EnumType]): (IndexedSeq[Value], IndexedSeq[Option[ChainRef]]) =
  val in     = new DataInputStream(new ByteArrayInputStream(data))
  val values = new ArrayBuffer[Value](colCount)
  val chains = new ArrayBuffer[Option[ChainRef]](colCount)

  for _ <- 0 until colCount do
    val (v, c) = deserializeValue(in, store, enumTypes)
    values += v
    chains += c

  (values.toIndexedSeq, chains.toIndexedSeq)

// ---- Type Serialization ----

def serializeType(typ: Type, out: DataOutputStream): Unit =
  typ match
    case SmallintType        => out.writeByte(TTagSmallint)
    case IntegerType         => out.writeByte(TTagInteger)
    case BigintType          => out.writeByte(TTagBigint)
    case SmallSerialType     => out.writeByte(TTagSmallSerial)
    case SerialType          => out.writeByte(TTagSerial)
    case BigSerialType       => out.writeByte(TTagBigSerial)
    case DoubleType          => out.writeByte(TTagDouble)
    case NumericType(p, s)   =>
      out.writeByte(TTagNumeric)
      out.writeInt(p)
      out.writeInt(s)
    case TextType            => out.writeByte(TTagText)
    case CharType(len)       =>
      out.writeByte(TTagChar)
      out.writeInt(len)
    case BooleanType         => out.writeByte(TTagBoolean)
    case DateType            => out.writeByte(TTagDate)
    case TimeType            => out.writeByte(TTagTime)
    case TimeTZType          => out.writeByte(TTagTimeTZ)
    case TimestampType       => out.writeByte(TTagTimestamp)
    case TimestampTZType     => out.writeByte(TTagTimestampTZ)
    case IntervalType        => out.writeByte(TTagInterval)
    case ByteaType           => out.writeByte(TTagBytea)
    case UUIDType            => out.writeByte(TTagUUID)
    case ArrayColumnType(et) =>
      out.writeByte(TTagArray)
      serializeType(et, out)
    case JSONType            => out.writeByte(TTagJSON)
    case e: EnumType         =>
      out.writeByte(TTagEnum)
      val nameBytes = e.enumName.getBytes("UTF-8")
      out.writeShort(nameBytes.length)
      out.write(nameBytes)
    case other               => sys.error(s"cannot serialize type: $other")

def deserializeType(in: DataInputStream, enumTypes: Map[String, EnumType]): Type =
  val tag = in.readByte()
  tag match
    case TTagSmallint    => SmallintType
    case TTagInteger     => IntegerType
    case TTagBigint      => BigintType
    case TTagSmallSerial => SmallSerialType
    case TTagSerial      => SerialType
    case TTagBigSerial   => BigSerialType
    case TTagDouble      => DoubleType
    case TTagNumeric     =>
      val p = in.readInt()
      val s = in.readInt()
      NumericType(p, s)
    case TTagText        => TextType
    case TTagChar        =>
      val len = in.readInt()
      CharType(len)
    case TTagBoolean     => BooleanType
    case TTagDate        => DateType
    case TTagTime        => TimeType
    case TTagTimeTZ      => TimeTZType
    case TTagTimestamp   => TimestampType
    case TTagTimestampTZ => TimestampTZType
    case TTagInterval    => IntervalType
    case TTagBytea       => ByteaType
    case TTagUUID        => UUIDType
    case TTagArray       =>
      val elemType = deserializeType(in, enumTypes)
      ArrayColumnType(elemType)
    case TTagJSON        => JSONType
    case TTagEnum        =>
      val nameLen = in.readUnsignedShort()
      val nameBytes = new Array[Byte](nameLen)
      in.readFully(nameBytes)
      val enumName = new String(nameBytes, "UTF-8")
      enumTypes.getOrElse(enumName, sys.error(s"unknown enum type '$enumName'"))
    case other           => sys.error(s"unknown type tag: $other")

// ---- Table Header Page Serialization ----

def serializeTableHeader(firstDataPage: PageId, autoState: Map[String, Value], batch: WriteBatch, pageSize: Int): Array[Byte] =
  val baos = new ByteArrayOutputStream()
  val out  = new DataOutputStream(baos)

  out.writeInt(firstDataPage)
  out.writeShort(autoState.size)
  for (col, v) <- autoState do
    writeString(out, col)
    serializeValue(v, out, batch, pageSize)

  out.flush()
  val content = baos.toByteArray
  val page    = new Array[Byte](pageSize)
  System.arraycopy(content, 0, page, 0, content.length)
  page

def deserializeTableHeader(pageData: Array[Byte], store: PageStore): (PageId, Map[String, Value]) =
  val in            = new DataInputStream(new ByteArrayInputStream(pageData))
  val firstDataPage = in.readInt()
  val autoCount     = in.readUnsignedShort()
  val autoState     = new scala.collection.mutable.HashMap[String, Value]

  for _ <- 0 until autoCount do
    val col    = readString(in)
    val (v, _) = deserializeValue(in, store, Map.empty)
    autoState(col) = v

  (firstDataPage, autoState.toMap)

// ---- Catalog Serialization ----

// Catalog layout:
//   [2 bytes] enum count
//   For each enum:
//     [2 bytes] name len + name UTF-8
//     [2 bytes] label count
//     For each label: [2 bytes] label len + label UTF-8
//   [2 bytes] table count
//   For each table:
//     [2 bytes] name len + name UTF-8
//     [4 bytes] headerPage (per-table header page storing firstDataPage + autoState)
//     [2 bytes] column count
//     For each column:
//       [2 bytes] name len + name UTF-8
//       type (via serializeType)
//       [1 byte] flags: required(bit 0), indexed(bit 1), unique(bit 2), has_fk(bit 3), has_default(bit 4)
//       if has_fk: [2 bytes] ref_table len + ref_table + [2 bytes] ref_col len + ref_col
//       if has_default: serialized value (using serializeValue with no batch needed for defaults — all inline)
//     [1 byte] has_primary_key
//     if has_primary_key:
//       [1 byte] has_name
//       if has_name: [2 bytes] name len + name UTF-8
//       [2 bytes] pk_column count
//       For each: [2 bytes] col_name len + col_name UTF-8
//     [2 bytes] constraint count (excluding PK)
//     For each constraint: type tag + data

def serializeCatalog(
    enumTypes: Iterable[(String, Type)],
    tableEntries: Iterable[CatalogTableEntry],
    indexEntries: Iterable[CatalogIndexEntry],
    batch: WriteBatch,
    pageSize: Int,
): Array[Byte] =
  val baos = new ByteArrayOutputStream()
  val out  = new DataOutputStream(baos)

  // Enum types
  val enums = enumTypes.collect { case (name, e: EnumType) => (name, e) }.toSeq
  out.writeShort(enums.size)
  for (name, e) <- enums do
    writeString(out, e.enumName)
    out.writeShort(e.labels.size)
    for label <- e.labels do writeString(out, label)

  // Tables
  out.writeShort(tableEntries.size)
  for entry <- tableEntries do
    writeString(out, entry.name)
    out.writeInt(entry.headerPage)

    // Columns
    out.writeShort(entry.columns.size)
    for col <- entry.columns do
      writeString(out, col.name)
      serializeType(col.typ, out)
      var flags = 0
      if col.required then flags |= 1
      if col.indexed then flags |= 2
      if col.unique then flags |= 4
      if col.fk.isDefined then flags |= 8
      if col.default.isDefined then flags |= 16
      out.writeByte(flags)
      col.fk.foreach { (refTable, refCol, onDel, onUpd) =>
        writeString(out, refTable)
        writeString(out, refCol)
        out.writeByte(onDel.ordinal)
        out.writeByte(onUpd.ordinal)
      }
      col.default.foreach { v =>
        serializeValue(v, out, batch, pageSize)
      }

    // Primary key
    entry.primaryKey match
      case Some(pk) =>
        out.writeByte(1)
        pk.name match
          case Some(n) =>
            out.writeByte(1)
            writeString(out, n)
          case None =>
            out.writeByte(0)
        out.writeShort(pk.columns.size)
        for col <- pk.columns do writeString(out, col)
      case None =>
        out.writeByte(0)

    // Constraints (excluding PK which is handled above)
    val nonPkConstraints = entry.constraints.filter(!_.isInstanceOf[PrimaryKeySpec])
    out.writeShort(nonPkConstraints.size)
    for c <- nonPkConstraints do
      c match
        case UniqueSpec(cols, name) =>
          out.writeByte(1)
          name match
            case Some(n) => out.writeByte(1); writeString(out, n)
            case None    => out.writeByte(0)
          out.writeShort(cols.size)
          for col <- cols do writeString(out, col)
        case ForeignKeySpec(cols, refTable, refCols, name, onDelete, onUpdate) =>
          out.writeByte(3)
          name match
            case Some(n) => out.writeByte(1); writeString(out, n)
            case None    => out.writeByte(0)
          out.writeShort(cols.size)
          for col <- cols do writeString(out, col)
          writeString(out, refTable)
          out.writeShort(refCols.size)
          for col <- refCols do writeString(out, col)
          out.writeByte(onDelete.ordinal)
          out.writeByte(onUpdate.ordinal)
        case CheckSpec(exprSource, _, name) =>
          out.writeByte(4)
          name match
            case Some(n) => out.writeByte(1); writeString(out, n)
            case None    => out.writeByte(0)
          writeString(out, exprSource)
        case _ => // skip non-serializable constraints

  // Indexes
  out.writeShort(indexEntries.size)
  for entry <- indexEntries do
    writeString(out, entry.name)
    writeString(out, entry.tableName)
    out.writeByte(if entry.unique then 1 else 0)
    out.writeInt(entry.treeRecordPage)
    out.writeLong(entry.nextRowId)
    out.writeShort(entry.columns.size)
    for col <- entry.columns do writeString(out, col)

  out.flush()
  baos.toByteArray

case class CatalogTableEntry(
    name: String,
    headerPage: PageId,
    columns: Seq[ColumnSpec],
    primaryKey: Option[PrimaryKeySpec],
    constraints: Seq[Spec],
)

case class CatalogIndexEntry(
    name: String,
    tableName: String,
    columns: Seq[String],
    unique: Boolean,
    treeRecordPage: PageId,
    nextRowId: Long,
)

def deserializeCatalog(
    data: Array[Byte],
    store: PageStore,
): (Seq[(String, EnumType)], Seq[CatalogTableEntry], Seq[CatalogIndexEntry]) =
  val in = new DataInputStream(new ByteArrayInputStream(data))

  // Enum types
  val enumCount = in.readUnsignedShort()
  val enums     = new ArrayBuffer[(String, EnumType)](enumCount)
  val enumMap   = new scala.collection.mutable.HashMap[String, EnumType]

  for _ <- 0 until enumCount do
    val name = readString(in)
    val labelCount = in.readUnsignedShort()
    val labels = (0 until labelCount).map(_ => readString(in)).toIndexedSeq
    val enumType = EnumType(name, labels)
    enums += ((name, enumType))
    enumMap(name) = enumType

  // Tables
  val tableCount = in.readUnsignedShort()
  val tables     = new ArrayBuffer[CatalogTableEntry](tableCount)

  for _ <- 0 until tableCount do
    val name = readString(in)
    val headerPage = in.readInt()

    // Columns
    val colCount = in.readUnsignedShort()
    val columns  = new ArrayBuffer[ColumnSpec](colCount)
    for _ <- 0 until colCount do
      val colName  = readString(in)
      val colType  = deserializeType(in, enumMap.toMap)
      val flags    = in.readUnsignedByte()
      val required = (flags & 1) != 0
      val indexed  = (flags & 2) != 0
      val unique   = (flags & 4) != 0
      val hasFk    = (flags & 8) != 0
      val hasDef   = (flags & 16) != 0
      val fk = if hasFk then
        val refTable = readString(in)
        val refCol   = readString(in)
        val onDel    = ReferentialAction.fromOrdinal(in.readByte())
        val onUpd    = ReferentialAction.fromOrdinal(in.readByte())
        Some((refTable, refCol, onDel, onUpd))
      else None
      val default = if hasDef then
        val (v, _) = deserializeValue(in, store, enumMap.toMap)
        Some(v)
      else None
      columns += ColumnSpec(colName, colType, required, indexed, unique, fk, default)

    // Primary key
    val hasPk = in.readByte() != 0
    val primaryKey = if hasPk then
      val hasName = in.readByte() != 0
      val pkName  = if hasName then Some(readString(in)) else None
      val pkColCount = in.readUnsignedShort()
      val pkCols = (0 until pkColCount).map(_ => readString(in))
      Some(PrimaryKeySpec(pkCols, pkName))
    else None

    // Constraints
    val constraintCount = in.readUnsignedShort()
    val constraints     = new ArrayBuffer[Spec](constraintCount)
    for _ <- 0 until constraintCount do
      val cType = in.readByte()
      cType match
        case 1 => // UniqueSpec
          val hasName = in.readByte() != 0
          val cName   = if hasName then Some(readString(in)) else None
          val colCnt  = in.readUnsignedShort()
          val cols    = (0 until colCnt).map(_ => readString(in))
          constraints += UniqueSpec(cols, cName)
        case 2 => // ForeignKeySpec (legacy, no actions)
          val hasName = in.readByte() != 0
          val cName   = if hasName then Some(readString(in)) else None
          val colCnt  = in.readUnsignedShort()
          val cols    = (0 until colCnt).map(_ => readString(in))
          val refTable = readString(in)
          val refColCnt = in.readUnsignedShort()
          val refCols   = (0 until refColCnt).map(_ => readString(in))
          constraints += ForeignKeySpec(cols, refTable, refCols, cName)
        case 3 => // ForeignKeySpec with actions
          val hasName = in.readByte() != 0
          val cName   = if hasName then Some(readString(in)) else None
          val colCnt  = in.readUnsignedShort()
          val cols    = (0 until colCnt).map(_ => readString(in))
          val refTable = readString(in)
          val refColCnt = in.readUnsignedShort()
          val refCols   = (0 until refColCnt).map(_ => readString(in))
          val onDelete  = ReferentialAction.fromOrdinal(in.readByte())
          val onUpdate  = ReferentialAction.fromOrdinal(in.readByte())
          constraints += ForeignKeySpec(cols, refTable, refCols, cName, onDelete, onUpdate)
        case 4 => // CheckSpec
          val hasName = in.readByte() != 0
          val cName   = if hasName then Some(readString(in)) else None
          val exprSource = readString(in)
          val parsed = SQLParser.parse(exprSource, SQLParser.booleanExpression)
          constraints += CheckSpec(exprSource, parsed, cName)
        case other => sys.error(s"unknown constraint type tag: $other")

    // Add PK to constraints list for reconstruction
    val allConstraints = primaryKey.toSeq ++ constraints.toSeq

    tables += CatalogTableEntry(name, headerPage, columns.toSeq, primaryKey, allConstraints)

  // Indexes (may not be present in older catalogs)
  val indexEntries = new ArrayBuffer[CatalogIndexEntry]
  if in.available() > 0 then
    val indexCount = in.readUnsignedShort()
    for _ <- 0 until indexCount do
      val idxName = readString(in)
      val idxTableName = readString(in)
      val idxUnique = in.readByte() != 0
      val idxTreeRecordPage = in.readInt()
      val idxNextRowId = in.readLong()
      val idxColCount = in.readUnsignedShort()
      val idxCols = (0 until idxColCount).map(_ => readString(in))
      indexEntries += CatalogIndexEntry(idxName, idxTableName, idxCols, idxUnique, idxTreeRecordPage, idxNextRowId)

  (enums.toSeq, tables.toSeq, indexEntries.toSeq)

private def writeString(out: DataOutputStream, s: String): Unit =
  val bytes = s.getBytes("UTF-8")
  out.writeShort(bytes.length)
  out.write(bytes)

private def readString(in: DataInputStream): String =
  val len = in.readUnsignedShort()
  val bytes = new Array[Byte](len)
  in.readFully(bytes)
  new String(bytes, "UTF-8")

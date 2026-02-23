package io.github.edadma.petradb.client

import io.github.edadma.petradb.*
import io.github.edadma.dal.{BigDecType, DoubleType as DDoubleType, IntType as DIntType, LongType as DLongType}
import java.time.*

object ResponseParser:

  def parseResponse(json: String, rowMode: String = "object"): Seq[Result] =
    ujson.read(json).arr.map(parseResult(_, rowMode)).toSeq

  private def dataTypeToType(dt: String): Type = dt match
    case "smallint" | "smallserial"  => SmallintType
    case "integer" | "serial"        => IntegerType
    case "bigint" | "bigserial"      => BigintType
    case "double"                    => DoubleType
    case "boolean"                   => BooleanType
    case "text" | "varchar" | "char" => TextType
    case "uuid"                      => UUIDType
    case "date"                      => DateType
    case "time"                      => TimeType
    case "timetz"                    => TimeTZType
    case "timestamp"                 => TimestampType
    case "timestamptz"               => TimestampTZType
    case "interval"                  => IntervalType
    case "bytea"                     => ByteaType
    case _                           => AnyType

  private def jsonToValue(j: ujson.Value, typ: Type): Value = j match
    case ujson.Null    => NullValue()
    case ujson.Bool(b) => BooleanValue(b)
    case ujson.Num(n)  => typ match
      case BigintType | BigSerialType => NumberValue(DLongType, n.toLong)
      case DoubleType                 => NumberValue(DDoubleType, n)
      case NumericType(_, _)          => NumberValue(BigDecType, BigDecimal(n.toString))
      case _ =>
        val l = n.toLong
        if l.toDouble == n then NumberValue(DIntType, l.toInt)
        else NumberValue(DDoubleType, n)
    case ujson.Str(s)  => typ match
      case DateType        => DateValue(LocalDate.parse(s))
      case TimeType        => TimeValue(LocalTime.parse(s))
      case TimestampType   => TimestampValue(LocalDateTime.parse(s))
      case TimestampTZType => TimestampTZValue(OffsetDateTime.parse(s))
      case TimeTZType      => TimeTZValue(OffsetTime.parse(s))
      case IntervalType    => IntervalValue(Duration.parse(s))
      case UUIDType        => UUIDValue(s)
      case ByteaType       => ByteaValue(base64Decode(s))
      case _               => TextValue(s)
    case ujson.Arr(a)  => ArrayValue(a.map(jsonToValue(_, AnyType)).toIndexedSeq)
    case ujson.Obj(o)  => ObjectValue(o.toSeq.map { (k, v) => k -> jsonToValue(v, AnyType) })

  private val base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  private def base64Decode(s: String): Array[Byte] =
    val clean  = s.filter(c => base64Chars.contains(c) || c == '=')
    val result = scala.collection.mutable.ArrayBuffer[Byte]()
    var i = 0
    while i + 3 < clean.length do
      val b0 = base64Chars.indexOf(clean(i))
      val b1 = base64Chars.indexOf(clean(i + 1))
      val b2 = if clean(i + 2) == '=' then 0 else base64Chars.indexOf(clean(i + 2))
      val b3 = if clean(i + 3) == '=' then 0 else base64Chars.indexOf(clean(i + 3))
      result += ((b0 << 2) | (b1 >> 4)).toByte
      if clean(i + 2) != '=' then result += (((b1 & 0xf) << 4) | (b2 >> 2)).toByte
      if clean(i + 3) != '=' then result += (((b2 & 0x3) << 6) | b3).toByte
      i += 4
    result.toArray

  private def parseFields(fieldsOpt: Option[ujson.Value]): IndexedSeq[ColumnMetadata] =
    fieldsOpt match
      case None      => IndexedSeq.empty
      case Some(arr) =>
        arr.arr.map(f => ColumnMetadata(None, f("name").str, dataTypeToType(f("dataType").str))).toIndexedSeq

  private def parseRowsObject(rowsOpt: Option[ujson.Value], fields: IndexedSeq[ColumnMetadata], meta: Metadata): IndexedSeq[Row] =
    rowsOpt match
      case None      => IndexedSeq.empty
      case Some(arr) =>
        arr.arr.map { row =>
          val data = fields.map(col => jsonToValue(row(col.name), col.typ))
          Row(data, meta, None, None)
        }.toIndexedSeq

  private def parseRowsArray(rowsOpt: Option[ujson.Value], fields: IndexedSeq[ColumnMetadata], meta: Metadata): IndexedSeq[Row] =
    rowsOpt match
      case None      => IndexedSeq.empty
      case Some(arr) =>
        arr.arr.map { row =>
          val data = row.arr.zipWithIndex.map { case (v, i) =>
            jsonToValue(v, if i < fields.length then fields(i).typ else AnyType)
          }.toIndexedSeq
          Row(data, meta, None, None)
        }.toIndexedSeq

  private def parseResult(j: ujson.Value, rowMode: String): Result =
    val obj    = j.obj
    val fields = parseFields(obj.get("fields"))
    val meta   = Metadata(fields)
    val rows   =
      if rowMode == "array" then parseRowsArray(obj.get("rows"), fields, meta)
      else parseRowsObject(obj.get("rows"), fields, meta)
    obj("command").str match
      case "select"        => QueryResult(TableValue(rows, meta))
      case "insert"        =>
        val resultMap = obj.get("result") match
          case None    => Map.empty[String, Value]
          case Some(r) => r.obj.map { (k, v) => k -> jsonToValue(v, AnyType) }.toMap
        InsertResult(resultMap, TableValue(rows, meta))
      case "update"        => UpdateResult(obj("rowCount").num.toInt)
      case "delete"        => DeleteResult(obj("rowCount").num.toInt)
      case "create table"  => CreateTableResult(obj("table").str)
      case "drop table"    => DropTableResult(obj("table").str)
      case "truncate table"=> TruncateResult(obj("table").str)
      case "create index"  => CreateIndexResult(obj("index").str)
      case "drop index"    => DropIndexResult(obj("index").str)
      case "create type"   => CreateTypeResult(obj("type").str)
      case "drop type"     => DropTypeResult(obj("type").str)
      case "alter table"   => AlterTableResult()
      case "explain"       => ExplainResult(obj("name").str)
      case "prepare"       => PrepareResult(obj("name").str)
      case "deallocate"    => DeallocateResult(obj("name").str)
      case "begin"         => BeginResult
      case "commit"        => CommitResult
      case "rollback"      => RollbackResult
      case cmd             => sys.error(s"Unknown command: $cmd")

package io.github.edadma.petradb

import upickle.default.*
import io.github.edadma.dal.{BigDecType, DoubleType as DDoubleType, IntType as DIntType, LongType as DLongType}
import java.time.*

object Codecs:

  // ── Type tag encoding ────────────────────────────────────────────────

  def typeTag(t: Type): String = t match
    case NumericType(p, s)      => s"numeric:$p:$s"
    case VarcharType(l)         => s"varchar:$l"
    case CharType(l)            => s"char:$l"
    case EnumType(name, labels) => s"enum:$name:${labels.mkString("\u001f")}"
    case ArrayColumnType(et)    => s"acol:${typeTag(et)}"
    case NumberType             => "number"
    case SmallintType           => "smallint"
    case IntegerType            => "integer"
    case BigintType             => "bigint"
    case SmallSerialType        => "smallserial"
    case SerialType             => "serial"
    case BigSerialType          => "bigserial"
    case DoubleType             => "double"
    case UUIDType               => "uuid"
    case TextType               => "text"
    case TimestampType          => "timestamp"
    case DateType               => "date"
    case TimeType               => "time"
    case TimeTZType             => "timetz"
    case IntervalType           => "interval"
    case TimestampTZType        => "timestamptz"
    case ByteaType              => "bytea"
    case JSONType               => "json"
    case ObjectType             => "object"
    case NullType               => "null"
    case AnyType                => "any"
    case StarType               => "star"
    case BooleanType            => "boolean"
    case TableType              => "table"
    case ArrayType              => "array"
    case _                      => "unknown"

  def typeFromTag(s: String): Type =
    if s.startsWith("numeric:") then
      val parts = s.split(":")
      NumericType(parts(1).toInt, parts(2).toInt)
    else if s.startsWith("varchar:") then VarcharType(s.drop(8).toInt)
    else if s.startsWith("char:") then CharType(s.drop(5).toInt)
    else if s.startsWith("enum:") then
      val rest  = s.drop(5)
      val colon = rest.indexOf(':')
      val name  = rest.take(colon)
      val labels =
        val raw = rest.drop(colon + 1)
        if raw.isEmpty then IndexedSeq.empty else raw.split('\u001f').toIndexedSeq
      EnumType(name, labels)
    else if s.startsWith("acol:") then ArrayColumnType(typeFromTag(s.drop(5)))
    else
      s match
        case "number"      => NumberType
        case "smallint"    => SmallintType
        case "integer"     => IntegerType
        case "bigint"      => BigintType
        case "smallserial" => SmallSerialType
        case "serial"      => SerialType
        case "bigserial"   => BigSerialType
        case "double"      => DoubleType
        case "uuid"        => UUIDType
        case "text"        => TextType
        case "timestamp"   => TimestampType
        case "date"        => DateType
        case "time"        => TimeType
        case "timetz"      => TimeTZType
        case "interval"    => IntervalType
        case "timestamptz" => TimestampTZType
        case "bytea"       => ByteaType
        case "json"        => JSONType
        case "object"      => ObjectType
        case "null"        => NullType
        case "any"         => AnyType
        case "star"        => StarType
        case "boolean"     => BooleanType
        case "table"       => TableType
        case "array"       => ArrayType
        case _             => AnyType

  // ── Value encoding ───────────────────────────────────────────────────

  private def encodeValue(v: Value): ujson.Value = v match
    case NumberValue(DIntType, n)    => ujson.Obj("t" -> ujson.Str("int"), "v" -> ujson.Num(n.intValue.toDouble))
    case NumberValue(DLongType, n)   => ujson.Obj("t" -> ujson.Str("long"), "v" -> ujson.Str(n.longValue.toString))
    case NumberValue(DDoubleType, n) => ujson.Obj("t" -> ujson.Str("double"), "v" -> ujson.Num(n.doubleValue))
    case NumberValue(BigDecType, n)  => ujson.Obj("t" -> ujson.Str("decimal"), "v" -> ujson.Str(n.toString))
    case NumberValue(_, n)           => ujson.Obj("t" -> ujson.Str("int"), "v" -> ujson.Num(n.intValue.toDouble))
    case TextValue(s)                => ujson.Obj("t" -> ujson.Str("text"), "v" -> ujson.Str(s))
    case BooleanValue(b)             => ujson.Obj("t" -> ujson.Str("bool"), "v" -> ujson.Bool(b))
    case NullValue()                 => ujson.Obj("t" -> ujson.Str("null"))
    case StarValue()                 => ujson.Obj("t" -> ujson.Str("star"))
    case UUIDValue(id)               => ujson.Obj("t" -> ujson.Str("uuid"), "v" -> ujson.Str(id))
    case TimestampValue(t)           => ujson.Obj("t" -> ujson.Str("ts"), "v" -> ujson.Str(t.toString))
    case DateValue(d)                => ujson.Obj("t" -> ujson.Str("date"), "v" -> ujson.Str(d.toString))
    case TimeValue(t)                => ujson.Obj("t" -> ujson.Str("time"), "v" -> ujson.Str(t.toString))
    case TimeTZValue(t)              => ujson.Obj("t" -> ujson.Str("timetz"), "v" -> ujson.Str(t.toString))
    case IntervalValue(d)            => ujson.Obj("t" -> ujson.Str("interval"), "v" -> ujson.Str(d.toString))
    case TimestampTZValue(t)         => ujson.Obj("t" -> ujson.Str("tstz"), "v" -> ujson.Str(t.toString))
    case ByteaValue(data)            =>
      ujson.Obj("t" -> ujson.Str("bytea"), "v" -> ujson.Str(data.map(b => f"${b & 0xff}%02x").mkString))
    case ArrayValue(elems)           =>
      ujson.Obj("t" -> ujson.Str("array"), "v" -> ujson.Arr(elems.map(encodeValue)*))
    case ObjectValue(props)          =>
      ujson.Obj(
        "t" -> ujson.Str("object"),
        "v" -> ujson.Arr(props.map { case (k, v) => ujson.Arr(ujson.Str(k), encodeValue(v)) }*),
      )
    case EnumValue(_, typ)           =>
      ujson.Obj(
        "t"      -> ujson.Str("enum"),
        "v"      -> ujson.Str(v.string),
        "typ"    -> ujson.Str(typ.enumName),
        "labels" -> ujson.Arr(typ.labels.map(ujson.Str.apply)*),
      )
    case TableValue(data, meta)      =>
      ujson.Obj(
        "t"    -> ujson.Str("table"),
        "meta" -> encodeMeta(meta),
        "data" -> ujson.Arr(data.map(r => encodeRowData(r.data))*),
      )

  private def decodeValue(j: ujson.Value): Value =
    val obj = j.obj
    obj("t").str match
      case "int"      => NumberValue(DIntType, obj("v").num.toInt)
      case "long"     => NumberValue(DLongType, obj("v").str.toLong)
      case "double"   => NumberValue(DDoubleType, obj("v").num)
      case "decimal"  => NumberValue(BigDecType, BigDecimal(obj("v").str))
      case "text"     => TextValue(obj("v").str)
      case "bool"     => BooleanValue(obj("v").bool)
      case "null"     => NullValue()
      case "star"     => StarValue()
      case "uuid"     => UUIDValue(obj("v").str)
      case "ts"       => TimestampValue(LocalDateTime.parse(obj("v").str))
      case "date"     => DateValue(LocalDate.parse(obj("v").str))
      case "time"     => TimeValue(LocalTime.parse(obj("v").str))
      case "timetz"   => TimeTZValue(OffsetTime.parse(obj("v").str))
      case "interval" => IntervalValue(Duration.parse(obj("v").str))
      case "tstz"     => TimestampTZValue(OffsetDateTime.parse(obj("v").str))
      case "bytea"    =>
        val hex = obj("v").str
        ByteaValue(hex.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray)
      case "array"    => ArrayValue(obj("v").arr.map(decodeValue).toIndexedSeq)
      case "object"   =>
        ObjectValue(obj("v").arr.map(pair => pair.arr(0).str -> decodeValue(pair.arr(1))).toSeq)
      case "enum"     =>
        val typName  = obj("typ").str
        val labels   = obj("labels").arr.map(_.str).toIndexedSeq
        val enumType = EnumType(typName, labels)
        EnumValue(enumType.labelsMap(obj("v").str), enumType)
      case "table"    =>
        val meta = decodeMeta(obj("meta"))
        val data = obj("data").arr.zipWithIndex.map { case (row, _) =>
          Row(row.arr.map(decodeValue).toIndexedSeq, meta, None, None)
        }.toIndexedSeq
        TableValue(data, meta)
      case tag => sys.error(s"Unknown value tag: $tag")

  // ── Metadata encoding ────────────────────────────────────────────────

  private def encodeColMeta(cm: ColumnMetadata): ujson.Value =
    ujson.Obj(
      "table" -> cm.table.map(ujson.Str.apply).getOrElse(ujson.Null),
      "name"  -> ujson.Str(cm.name),
      "typ"   -> ujson.Str(typeTag(cm.typ)),
    )

  private def decodeColMeta(j: ujson.Value): ColumnMetadata =
    val obj   = j.obj
    val table = if obj("table") == ujson.Null then None else Some(obj("table").str)
    ColumnMetadata(table, obj("name").str, typeFromTag(obj("typ").str))

  private def encodeMeta(m: Metadata): ujson.Value =
    ujson.Arr(m.columns.map(encodeColMeta)*)

  private def decodeMeta(j: ujson.Value): Metadata =
    Metadata(j.arr.map(decodeColMeta).toIndexedSeq)

  private def encodeRowData(data: IndexedSeq[Value]): ujson.Value =
    ujson.Arr(data.map(encodeValue)*)

  // ── Result encoding ──────────────────────────────────────────────────

  private def encodeResult(r: Result): ujson.Value = r match
    case QueryResult(table)       =>
      ujson.Obj("cmd" -> ujson.Str("select"), "table" -> encodeValue(table))
    case InsertResult(obj, table) =>
      val resultObj = ujson.Obj()
      obj.foreach { case (k, v) => resultObj.value(k) = encodeValue(v) }
      ujson.Obj(
        "cmd"    -> ujson.Str("insert"),
        "result" -> resultObj,
        "table"  -> encodeValue(table),
      )
    case UpdateResult(n)          => ujson.Obj("cmd" -> ujson.Str("update"), "rowCount" -> ujson.Num(n.toDouble))
    case DeleteResult(n)          => ujson.Obj("cmd" -> ujson.Str("delete"), "rowCount" -> ujson.Num(n.toDouble))
    case CreateTableResult(t)     => ujson.Obj("cmd" -> ujson.Str("create table"), "table" -> ujson.Str(t))
    case DropTableResult(t)       => ujson.Obj("cmd" -> ujson.Str("drop table"), "table" -> ujson.Str(t))
    case TruncateResult(t)        => ujson.Obj("cmd" -> ujson.Str("truncate table"), "table" -> ujson.Str(t))
    case CreateIndexResult(n)     => ujson.Obj("cmd" -> ujson.Str("create index"), "index" -> ujson.Str(n))
    case DropIndexResult(n)       => ujson.Obj("cmd" -> ujson.Str("drop index"), "index" -> ujson.Str(n))
    case CreateTypeResult(t)      => ujson.Obj("cmd" -> ujson.Str("create type"), "type" -> ujson.Str(t))
    case DropTypeResult(n)        => ujson.Obj("cmd" -> ujson.Str("drop type"), "type" -> ujson.Str(n))
    case AlterTableResult()       => ujson.Obj("cmd" -> ujson.Str("alter table"))
    case ExplainResult(plan)      => ujson.Obj("cmd" -> ujson.Str("explain"), "plan" -> ujson.Str(plan))
    case CreateViewResult(n)      => ujson.Obj("cmd" -> ujson.Str("create view"), "view" -> ujson.Str(n))
    case DropViewResult(n)        => ujson.Obj("cmd" -> ujson.Str("drop view"), "view" -> ujson.Str(n))
    case CreateSequenceResult(n)  => ujson.Obj("cmd" -> ujson.Str("create sequence"), "sequence" -> ujson.Str(n))
    case DropSequenceResult(n)    => ujson.Obj("cmd" -> ujson.Str("drop sequence"), "sequence" -> ujson.Str(n))
    case PrepareResult(n)         => ujson.Obj("cmd" -> ujson.Str("prepare"), "name" -> ujson.Str(n))
    case DeallocateResult(n)      => ujson.Obj("cmd" -> ujson.Str("deallocate"), "name" -> ujson.Str(n))
    case BeginResult              => ujson.Obj("cmd" -> ujson.Str("begin"))
    case CommitResult             => ujson.Obj("cmd" -> ujson.Str("commit"))
    case RollbackResult           => ujson.Obj("cmd" -> ujson.Str("rollback"))

  private def decodeResult(j: ujson.Value): Result =
    val obj = j.obj
    obj("cmd").str match
      case "select"        => QueryResult(decodeValue(obj("table")).asInstanceOf[TableValue])
      case "insert"        =>
        val resObj = obj("result").obj.map { case (k, v) => k -> decodeValue(v) }.toMap
        InsertResult(resObj, decodeValue(obj("table")).asInstanceOf[TableValue])
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
      case "explain"       => ExplainResult(obj("plan").str)
      case "create view"     => CreateViewResult(obj("view").str)
      case "drop view"       => DropViewResult(obj("view").str)
      case "create sequence" => CreateSequenceResult(obj("sequence").str)
      case "drop sequence"   => DropSequenceResult(obj("sequence").str)
      case "prepare"       => PrepareResult(obj("name").str)
      case "deallocate"    => DeallocateResult(obj("name").str)
      case "begin"         => BeginResult
      case "commit"        => CommitResult
      case "rollback"      => RollbackResult
      case cmd             => sys.error(s"Unknown result command: $cmd")

  // ── Public ReadWriter instances ──────────────────────────────────────

  given ReadWriter[Value] = readwriter[ujson.Value].bimap(encodeValue, decodeValue)

  given ReadWriter[TableValue] = readwriter[ujson.Value].bimap(
    tv => encodeValue(tv),
    j  => decodeValue(j).asInstanceOf[TableValue],
  )

  given ReadWriter[Row] = readwriter[ujson.Value].bimap(
    r => encodeRowData(r.data),
    j => Row(j.arr.map(decodeValue).toIndexedSeq, Metadata(IndexedSeq.empty), None, None),
  )

  given ReadWriter[Result] = readwriter[ujson.Value].bimap(encodeResult, decodeResult)

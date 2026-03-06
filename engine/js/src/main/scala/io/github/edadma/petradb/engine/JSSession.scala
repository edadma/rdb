package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import io.github.edadma.dal.{IntType => DIntType, DoubleType => DDoubleType, BigDecType}

import io.github.edadma.cross_platform.exists

import scala.scalajs.js
import js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.concurrent.ExecutionContext

@JSExportTopLevel("Session")
class JSSession(options: js.UndefOr[js.Dynamic] = js.undefined):

  private val db: DB =
    val storage = options.toOption.flatMap(o => o.selectDynamic("storage").asInstanceOf[js.UndefOr[String]].toOption).getOrElse("memory")
    val path = options.toOption.flatMap(o => o.selectDynamic("path").asInstanceOf[js.UndefOr[String]].toOption)

    storage match
      case "memory" => new MemoryDB()
      case "persistent" =>
        val p = path.getOrElse(throw js.JavaScriptException(js.Error("'path' option is required for persistent storage")))
        val pageSize = options.toOption
          .flatMap(o => o.selectDynamic("pageSize").asInstanceOf[js.UndefOr[Int]].toOption)
          .getOrElse(4096)
        if exists(p) then PersistentDB.open(p) else PersistentDB.create(p, pageSize)
      case "text" =>
        val p = path.getOrElse(throw js.JavaScriptException(js.Error("'path' option is required for text storage")))
        TextDB.open(p)
      case other =>
        throw js.JavaScriptException(js.Error(s"Unknown storage type: '$other'. Use 'memory', 'persistent', or 'text'."))

  given session: Session = db.connect()
  private given ExecutionContext = ExecutionContext.global

  private val defaultRowMode: String =
    options.toOption.flatMap(o => o.selectDynamic("rowMode").asInstanceOf[js.UndefOr[String]].toOption).getOrElse("object")

  private def toJS(v: Value): js.Any =
    v match
      case NumberValue(DIntType, n)    => n.intValue
      case NumberValue(DDoubleType, n) => n.doubleValue
      case NumberValue(BigDecType, n)  => n.doubleValue
      case TextValue(s)                => s
      case BooleanValue(b)             => b
      case UUIDValue(id)               => id
      case EnumValue(_, _)             => v.string
      case NullValue()                 => null
      case ArrayValue(elems)           => elems map toJS toJSArray
      case ObjectValue(properties)     => (properties map { case (k, v) => k -> toJS(v) } toMap) toJSDictionary
      case TimestampValue(t)           => new js.Date(t.toString)
      case DateValue(d)                => new js.Date(d.toString)
      case TimeValue(t)                => t.toString
      case TimestampTZValue(t)         => new js.Date(t.toString)
      case TimeTZValue(t)              => t.toString
      case IntervalValue(d)            => v.string
      case ByteaValue(data)            => data.toJSArray

  private def fromJS(v: js.Any): Value =
    if v == null || js.isUndefined(v) then NullValue()
    else if js.typeOf(v) == "string" then TextValue(v.asInstanceOf[String])
    else if js.typeOf(v) == "boolean" then BooleanValue(v.asInstanceOf[Boolean])
    else if js.typeOf(v) == "number" then
      val n = v.asInstanceOf[Double]
      NumberValue(if n == n.toInt then DIntType else DDoubleType, n)
    else TextValue(v.toString)

  private def typeString(typ: Type): String =
    typ match
      case SerialType        => "serial"
      case BigSerialType     => "bigserial"
      case IntegerType       => "int"
      case BigintType        => "bigint"
      case DoubleType        => "double"
      case NumericType(_, _) => "numeric"
      case TextType          => "text"
      case BooleanType       => "boolean"
      case UUIDType          => "uuid"
      case TimestampType     => "timestamp"
      case DateType          => "date"
      case TimeType          => "time"
      case TimestampTZType   => "timestamptz"
      case TimeTZType        => "timetz"
      case IntervalType      => "interval"
      case ByteaType         => "bytea"
      case JSONType          => "json"
      case ArrayType         => "array"
      case _: EnumType       => "enum"
      case NumberType        => "number"
      case _                 => "unknown"

  private def buildQueryResult(table: TableValue, rowMode: String): js.Any =
    val columns = table.meta.columns
    val fields = columns.map(col =>
      js.Dynamic.literal(name = col.name, dataType = typeString(col.typ))
    ).toJSArray

    val rows = rowMode match
      case "array" =>
        table.data.map(row => (row.data map toJS toJSArray): js.Any).toJSArray
      case _ =>
        table.data.map { row =>
          val obj = js.Dynamic.literal()
          for ((col, i) <- columns.zipWithIndex)
            obj.updateDynamic(col.name)(toJS(row.data(i)))
          obj: js.Any
        }.toJSArray

    js.Dynamic.literal(command = "select", rows = rows, fields = fields)

  private def resultToJS(result: Result, rowMode: String): js.Any =
    result match
      case CreateTableResult(table) =>
        js.Dynamic.literal(command = "create table", table = table)
      case InsertResult(obj, table) =>
        val res = obj.view.mapValues(toJS).toMap.toJSDictionary
        val queryResult = buildQueryResult(table, rowMode)
        val rows = queryResult.asInstanceOf[js.Dynamic].rows
        val fields = queryResult.asInstanceOf[js.Dynamic].fields
        js.Dynamic.literal(command = "insert", result = res, rows = rows, fields = fields)
      case QueryResult(table) =>
        buildQueryResult(table, rowMode)
      case UpdateResult(rows) =>
        js.Dynamic.literal(command = "update", rowCount = rows)
      case DeleteResult(rows) =>
        js.Dynamic.literal(command = "delete", rowCount = rows)
      case DropTableResult(table) =>
        js.Dynamic.literal(command = "drop table", table = table)
      case CreateTypeResult(typ) =>
        js.Dynamic.literal(command = "create type", `type` = typ)
      case DropTypeResult(name) =>
        js.Dynamic.literal(command = "drop type", `type` = name)
      case CreateIndexResult(name) =>
        js.Dynamic.literal(command = "create index", index = name)
      case DropIndexResult(name) =>
        js.Dynamic.literal(command = "drop index", index = name)
      case TruncateResult(table) =>
        js.Dynamic.literal(command = "truncate table", table = table)
      case AlterTableResult() =>
        js.Dynamic.literal(command = "alter table")
      case PrepareResult(name) =>
        js.Dynamic.literal(command = "prepare", name = name)
      case DeallocateResult(name) =>
        js.Dynamic.literal(command = "deallocate", name = name)
      case BeginResult =>
        js.Dynamic.literal(command = "begin")
      case CommitResult =>
        js.Dynamic.literal(command = "commit")
      case RollbackResult =>
        js.Dynamic.literal(command = "rollback")
      case CreateViewResult(name) =>
        js.Dynamic.literal(command = "create view", view = name)
      case DropViewResult(name) =>
        js.Dynamic.literal(command = "drop view", view = name)
      case ExplainResult(plan) =>
        js.Dynamic.literal(command = "explain", plan = plan)
      case CopyResult(rows) =>
        js.Dynamic.literal(command = "copy", rowCount = rows)

  @JSExport
  def execute(sql: String, options: js.UndefOr[js.Dynamic] = js.undefined): js.Promise[js.Array[js.Any]] =
    val rowMode = options.toOption
      .flatMap(o => o.selectDynamic("rowMode").asInstanceOf[js.UndefOr[String]].toOption)
      .getOrElse(defaultRowMode)

    session.execute(sql).map(results => (results map (r => resultToJS(r, rowMode))).toJSArray).toJSPromise

  @JSExport
  def close(): Unit = db.close()

  @JSExport
  def prepare(sql: String): PreparedStatementJS = new PreparedStatementJS(session.prepare(sql))

  class PreparedStatementJS(ps: PreparedStatement):
    @JSExport
    def execute(params: js.Array[js.Any] = js.Array(), options: js.UndefOr[js.Dynamic] = js.undefined): js.Promise[js.Array[js.Any]] =
      val rowMode = options.toOption
        .flatMap(o => o.selectDynamic("rowMode").asInstanceOf[js.UndefOr[String]].toOption)
        .getOrElse(defaultRowMode)
      val paramValues = params.map(fromJS).toIndexedSeq
      ps.execute(paramValues).map(results => (results map (r => resultToJS(r, rowMode))).toJSArray).toJSPromise

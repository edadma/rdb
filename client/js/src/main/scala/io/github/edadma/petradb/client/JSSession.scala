package io.github.edadma.petradb.client

import io.github.edadma.petradb.*
import io.github.edadma.dal.{IntType => DIntType, DoubleType => DDoubleType, BigDecType}

import scala.scalajs.js
import js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.concurrent.ExecutionContext.Implicits.global

@JSExportTopLevel("Session")
class JSSession(options: js.UndefOr[js.Dynamic] = js.undefined):

  private val opts = options.toOption
  private val session = new Session(SessionOptions(
    host = opts.flatMap(o => o.selectDynamic("host").asInstanceOf[js.UndefOr[String]].toOption).getOrElse("localhost"),
    port = opts.flatMap(o => o.selectDynamic("port").asInstanceOf[js.UndefOr[Int]].toOption).getOrElse(DefaultPort),
    username = opts.flatMap(o => o.selectDynamic("username").asInstanceOf[js.UndefOr[String]].toOption),
    password = opts.flatMap(o => o.selectDynamic("password").asInstanceOf[js.UndefOr[String]].toOption),
  ))

  private val defaultRowMode: String =
    opts.flatMap(o => o.selectDynamic("rowMode").asInstanceOf[js.UndefOr[String]].toOption).getOrElse("object")

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
      case ExplainResult(plan) =>
        js.Dynamic.literal(command = "explain", plan = plan)
      case CreateViewResult(name) =>
        js.Dynamic.literal(command = "create view", view = name)
      case DropViewResult(name) =>
        js.Dynamic.literal(command = "drop view", view = name)
      case CopyResult(rows) =>
        js.Dynamic.literal(command = "copy", rowCount = rows)
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

  @JSExport
  def connect(): js.Promise[String] =
    session.connect().toJSPromise

  @JSExport
  def execute(sql: String, options: js.UndefOr[js.Dynamic] = js.undefined): js.Promise[js.Array[js.Any]] =
    val rowMode = options.toOption
      .flatMap(o => o.selectDynamic("rowMode").asInstanceOf[js.UndefOr[String]].toOption)
      .getOrElse(defaultRowMode)

    session.execute(sql).map(results => (results map (r => resultToJS(r, rowMode))).toJSArray).toJSPromise

  @JSExport
  def close(): js.Promise[Unit] =
    session.close().toJSPromise

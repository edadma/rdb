package io.github.edadma.rdb

import io.github.edadma.dal.{IntType => DIntType, DoubleType => DDoubleType, BigDecType}

import scala.scalajs.js
import js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("ConnectSQL")
class ConnectSQL(options: js.UndefOr[js.Dynamic] = js.undefined):

  given db: DB = new MemoryDB()

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

  @JSExport
  def execute(sql: String, options: js.UndefOr[js.Dynamic] = js.undefined): js.Array[js.Any] =
    val rowMode = options.toOption
      .flatMap(o => o.selectDynamic("rowMode").asInstanceOf[js.UndefOr[String]].toOption)
      .getOrElse(defaultRowMode)

    executeSQL(sql) map {
      case CreateTableResult(table) =>
        js.Dynamic.literal(command = "create table", table = table)
      case InsertResult(obj, _) =>
        val res = obj.view.mapValues(toJS).toMap.toJSDictionary

        js.Dynamic.literal(command = "insert", result = res)
      case QueryResult(table) =>
        buildQueryResult(table, rowMode)
      case UpdateResult(rows) =>
        js.Dynamic.literal(command = "update", rows = rows)
      case DeleteResult(rows) =>
        js.Dynamic.literal(command = "delete", rows = rows)
      case DropTableResult(table) =>
        js.Dynamic.literal(command = "drop table", table = table)
      case CreateTypeResult(typ) =>
        js.Dynamic.literal(command = "create type", `type` = typ)
      case DropTypeResult(name) =>
        js.Dynamic.literal(command = "drop type", `type` = name)
      case DropIndexResult(name) =>
        js.Dynamic.literal(command = "drop index", index = name)
      case AlterTableResult() =>
        js.Dynamic.literal(command = "alter table")
    } toJSArray

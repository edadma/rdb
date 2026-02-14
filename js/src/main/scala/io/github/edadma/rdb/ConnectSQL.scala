package io.github.edadma.rdb

import io.github.edadma.dal.{IntType => DIntType, DoubleType => DDoubleType, BigDecType}

import scala.scalajs.js
import js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("ConnectSQL")
class ConnectSQL():

  given db: DB = new MemoryDB()

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

  @JSExport
  def execute(sql: String): js.Array[js.Any] =
    executeSQL(sql) map {
      case CreateTableResult(table) =>
        js.Dynamic.literal(command = "create table", table = table)
      case InsertResult(obj, _) =>
        val res = obj.view.mapValues(toJS).toMap.toJSDictionary

        js.Dynamic.literal(command = "insert", result = res)
      case QueryResult(table) =>
        val res = table.data map (_.data map toJS toJSArray) toJSArray

        js.Dynamic.literal(command = "select", result = res)
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

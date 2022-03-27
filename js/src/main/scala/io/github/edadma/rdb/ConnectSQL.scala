package io.github.edadma.rdb

import io.github.edadma.dal.{IntType => DIntType, DoubleType => DDoubleType}

import scala.scalajs.js
import js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("ConnectSQL")
class ConnectSQL():

  val db = new DB()

  private def toJS(v: Value): js.Any =
    v match
      case NumberValue(DIntType, n)    => n.intValue
      case NumberValue(DDoubleType, n) => n.doubleValue
      case TextValue(s)                => s
      case NullValue()                 => null
      case ArrayValue(elems)           => elems map toJS toJSArray
      case ObjectValue(properties)     => (properties map { case (k, v) => k -> toJS(v) } toMap) toJSDictionary
      case TimestampValue(t)           => new js.Date(t.toString)

  @JSExport
  def execute(sql: String): js.Array[js.Any] =
    executeSQL(sql)(db) map {
      case CreateTableResult(table) =>
        js.Dynamic.literal(command = "create table", table = table)
      case InsertResult(obj, _) =>
        val res = obj.view.mapValues(toJS).toMap.toJSDictionary

        js.Dynamic.literal(command = "insert", result = res)
      case QueryResult(table) =>
        val res = table.data map (_.data map toJS toJSArray) toJSArray

        js.Dynamic.literal(command = "select", result = res)
    } toJSArray

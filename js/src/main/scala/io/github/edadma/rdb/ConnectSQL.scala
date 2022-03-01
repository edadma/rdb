package io.github.edadma.rdb

import scala.scalajs.js
import js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("ConnectSQL")
class ConnectSQL():

  val db = new MemoryDB()

  @JSExport
  def execute(sql: String): Any =
    executeSQL(sql)(db) match
      case CreateTableResult(table) =>
        js.Dynamic.literal(command = "create table", table = table)
      case InsertResult(result) =>
        val res = result.view.mapValues(_.toText.s).toMap.toJSDictionary

        js.Dynamic.literal(command = "insert", result = res)
      case QueryResult(table) =>
        val res = table.data map (_.data map (_.toText.s) toJSArray) toJSArray

        js.Dynamic.literal(command = "select", result = res)

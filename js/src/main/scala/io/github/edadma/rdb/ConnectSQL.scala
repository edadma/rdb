package io.github.edadma.rdb

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("ConnectSQL")
class ConnectSQL(database: String):

  val db = new MemoryDB(database)

  @JSExport
  def execute(sql: String): Result = executeSQL(sql)(db)

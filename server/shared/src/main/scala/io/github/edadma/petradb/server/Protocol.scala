package io.github.edadma.petradb.server

import zio.json.*
import zio.json.ast.Json

case class SqlRequest(
  sql: String,
  rowMode: Option[String] = None,
) derives JsonDecoder

case class FieldInfo(name: String, dataType: String)

case class ResultJson(
  command: String,
  rows: Option[Json] = None,
  fields: Option[Seq[FieldInfo]] = None,
  result: Option[Map[String, Json]] = None,
  rowCount: Option[Int] = None,
  table: Option[String] = None,
  name: Option[String] = None,
  typ: Option[String] = None,
  index: Option[String] = None,
)

object ResultJson:
  given JsonEncoder[ResultJson] = JsonEncoder[Json].contramap { r =>
    val buf = scala.collection.mutable.ArrayBuffer[(String, Json)]()
    buf += "command" -> Json.Str(r.command)
    r.rows.foreach(v => buf += "rows" -> v)
    r.fields.foreach { fs =>
      buf += "fields" -> Json.Arr(fs.map(f => Json.Obj("name" -> Json.Str(f.name), "dataType" -> Json.Str(f.dataType)))*)
    }
    r.result.foreach(m => buf += "result" -> Json.Obj(m.toSeq*))
    r.rowCount.foreach(n => buf += "rowCount" -> Json.Num(new java.math.BigDecimal(n)))
    r.table.foreach(t => buf += "table" -> Json.Str(t))
    r.name.foreach(n => buf += "name" -> Json.Str(n))
    r.typ.foreach(t => buf += "type" -> Json.Str(t))
    r.index.foreach(i => buf += "index" -> Json.Str(i))
    Json.Obj(buf.toSeq*)
  }

case class ErrorResponse(error: String, detail: Option[String] = None)

object ErrorResponse:
  given JsonEncoder[ErrorResponse] = JsonEncoder[Json].contramap { e =>
    val buf = scala.collection.mutable.ArrayBuffer[(String, Json)]()
    buf += "error" -> Json.Str(e.error)
    e.detail.foreach(d => buf += "detail" -> Json.Str(d))
    Json.Obj(buf.toSeq*)
  }

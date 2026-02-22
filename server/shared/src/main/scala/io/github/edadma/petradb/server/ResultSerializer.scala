package io.github.edadma.petradb.server

import io.github.edadma.petradb.*
import zio.json.ast.Json

object ResultSerializer:
  def serializeResults(results: Seq[Result], rowMode: String = "object"): Seq[ResultJson] =
    results.map(serializeResult(_, rowMode))

  def serializeResult(result: Result, rowMode: String): ResultJson = result match
    case QueryResult(table) =>
      val fields = table.meta.columns.map(c => FieldInfo(c.name, c.typ.toString))
      val rows =
        if rowMode == "array" then serializeRowsArray(table)
        else serializeRowsObject(table)
      ResultJson(command = "select", rows = Some(rows), fields = Some(fields))

    case InsertResult(obj, table) =>
      val fields = table.meta.columns.map(c => FieldInfo(c.name, c.typ.toString))
      val rows =
        if rowMode == "array" then serializeRowsArray(table)
        else serializeRowsObject(table)
      val resultObj = obj.map((k, v) => k -> ValueSerializer.valueToJson(v))
      ResultJson(command = "insert", result = Some(resultObj), rows = Some(rows), fields = Some(fields))

    case UpdateResult(n)       => ResultJson(command = "update", rowCount = Some(n))
    case DeleteResult(n)       => ResultJson(command = "delete", rowCount = Some(n))
    case CreateTableResult(t)  => ResultJson(command = "create table", table = Some(t))
    case DropTableResult(t)    => ResultJson(command = "drop table", table = Some(t))
    case TruncateResult(t)     => ResultJson(command = "truncate", table = Some(t))
    case CreateIndexResult(n)  => ResultJson(command = "create index", name = Some(n))
    case DropIndexResult(n)    => ResultJson(command = "drop index", name = Some(n))
    case CreateTypeResult(t)   => ResultJson(command = "create type", typ = Some(t))
    case DropTypeResult(n)     => ResultJson(command = "drop type", name = Some(n))
    case AlterTableResult()    => ResultJson(command = "alter table")
    case PrepareResult(n)      => ResultJson(command = "prepare", name = Some(n))
    case DeallocateResult(n)   => ResultJson(command = "deallocate", name = Some(n))
    case BeginResult           => ResultJson(command = "begin")
    case CommitResult          => ResultJson(command = "commit")
    case RollbackResult        => ResultJson(command = "rollback")

  private def serializeRowsObject(table: TableValue): Json =
    Json.Arr(table.data.map { row =>
      Json.Obj(row.meta.columns.zip(row.data).map { case (col, value) =>
        col.name -> ValueSerializer.valueToJson(value)
      }*)
    }*)

  private def serializeRowsArray(table: TableValue): Json =
    Json.Arr(table.data.map { row =>
      Json.Arr(row.data.map(ValueSerializer.valueToJson)*)
    }*)

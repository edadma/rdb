package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import scala.collection.immutable.ArraySeq
import scala.scalajs.js
import scala.util.parsing.input.NoPosition

object ASTConverter:

  def toCommand(obj: js.Dynamic): Command =
    obj.kind.asInstanceOf[String] match
      case "query"       => QueryCommand(toExpr(obj.query))
      case "insert"      => toInsertCommand(obj)
      case "update"      => toUpdateCommand(obj)
      case "delete"      => toDeleteCommand(obj)
      case "createTable" => toCreateTableCommand(obj)
      case k             => throw js.JavaScriptException(js.Error(s"Unknown command kind: $k"))

  def toExpr(obj: js.Dynamic): Expr =
    val e: Expr = obj.kind.asInstanceOf[String] match
      case "column" =>
        val table =
          if js.isUndefined(obj.table) || obj.table == null then None
          else Some(ident(obj.table.asInstanceOf[String]))
        ColumnExpr(table, ident(obj.name.asInstanceOf[String]))
      case "string"    => StringExpr(obj.value.asInstanceOf[String])
      case "number"    => NumberExpr(obj.value.asInstanceOf[Number])
      case "boolean"   => BooleanExpr(obj.value.asInstanceOf[Boolean])
      case "null"      => NullExpr()
      case "star"      => StarExpr()
      case "tableStar" => TableStarExpr(ident(obj.table.asInstanceOf[String]))
      case "binary"    => BinaryExpr(toExpr(obj.left), obj.op.asInstanceOf[String], toExpr(obj.right))
      case "unary"     => UnaryExpr(obj.op.asInstanceOf[String], toExpr(obj.expr))
      case "alias"     => AliasExpr(toExpr(obj.expr), ident(obj.alias.asInstanceOf[String]))
      case "apply" =>
        ApplyExpr(
          ident(obj.func.asInstanceOf[String]),
          obj.args.asInstanceOf[js.Array[js.Dynamic]].map(toExpr).toSeq,
        )
      case "in" =>
        InSeqExpr(
          toExpr(obj.value),
          obj.op.asInstanceOf[String],
          obj.exprs.asInstanceOf[js.Array[js.Dynamic]].map(toExpr).toSeq,
        )
      case "between" =>
        BetweenExpr(toExpr(obj.value), obj.op.asInstanceOf[String], toExpr(obj.lower), toExpr(obj.upper))
      case "case" =>
        val whens = obj.whens.asInstanceOf[js.Array[js.Dynamic]].map { w =>
          When(toExpr(w.when), toExpr(w.expr))
        }.toSeq
        val els =
          if js.isUndefined(obj.els) || obj.els == null then None
          else Some(toExpr(obj.els))
        CaseExpr(whens, els)
      case "subquery" => SubqueryExpr(toExpr(obj.query))
      case "exists"   => ExistsExpr(toExpr(obj.subquery))
      case "cast"     => CastExpr(toExpr(obj.expr), typeFromString(obj.targetType.asInstanceOf[String]))
      case "parameter" => ParameterExpr(obj.index.asInstanceOf[Int])
      case "select"   => toSelectExpr(obj)
      case "table"    => TableOperator(ident(obj.name.asInstanceOf[String]))
      case "aliasRelation" =>
        AliasOperator(toExpr(obj.relation), ident(obj.alias.asInstanceOf[String]))
      case "joinInner" =>
        InnerJoinOperator(toExpr(obj.left), toExpr(obj.right), toExpr(obj.on))
      case "joinLeft" =>
        LeftJoinOperator(toExpr(obj.left), toExpr(obj.right), toExpr(obj.on))
      case k => throw js.JavaScriptException(js.Error(s"Unknown expression kind: $k"))

    e.pos = NoPosition
    e

  private def toSelectExpr(obj: js.Dynamic): SQLSelectExpr =
    val exprs = ArraySeq.from(obj.exprs.asInstanceOf[js.Array[js.Dynamic]].map(toExpr))

    val from =
      if js.isUndefined(obj.from) || obj.from == null then None
      else Some(obj.from.asInstanceOf[js.Array[js.Dynamic]].map(toExpr).toSeq)

    val where =
      if js.isUndefined(obj.where) || obj.where == null then None
      else Some(toExpr(obj.where))

    val groupBy =
      if js.isUndefined(obj.groupBy) || obj.groupBy == null then None
      else Some(obj.groupBy.asInstanceOf[js.Array[js.Dynamic]].map(toExpr).toSeq)

    val having =
      if js.isUndefined(obj.having) || obj.having == null then None
      else Some(toExpr(obj.having))

    val orderBy =
      if js.isUndefined(obj.orderBy) || obj.orderBy == null then None
      else
        Some(obj.orderBy.asInstanceOf[js.Array[js.Dynamic]].map { o =>
          val dir = o.direction.asInstanceOf[String]
          val nf =
            if js.isUndefined(o.nullsFirst) || o.nullsFirst == null then dir == "asc"
            else o.nullsFirst.asInstanceOf[Boolean]
          OrderBy(toExpr(o.expr), dir == "asc", nf)
        }.toSeq)

    val offset =
      if js.isUndefined(obj.offset) || obj.offset == null then None
      else Some(Count(NoPosition, NumberExpr(obj.offset.asInstanceOf[Number])))

    val limit =
      if js.isUndefined(obj.limit) || obj.limit == null then None
      else Some(Count(NoPosition, NumberExpr(obj.limit.asInstanceOf[Number])))

    val distinct =
      if js.isUndefined(obj.distinct) || obj.distinct == null then false
      else obj.distinct.asInstanceOf[Boolean]

    SQLSelectExpr(exprs, from, where, groupBy, having, orderBy, offset, limit, distinct)

  private def toInsertCommand(obj: js.Dynamic): InsertCommand =
    val table = ident(obj.table.asInstanceOf[String])
    val columns = Some(obj.columns.asInstanceOf[js.Array[String]].map(c => ident(c)).toSeq)
    val rows = obj.rows.asInstanceOf[js.Array[js.Array[js.Dynamic]]].map { row =>
      row.map(toExpr).toSeq
    }.toSeq
    val returning =
      if js.isUndefined(obj.returning) || obj.returning == null then None
      else Some(obj.returning.asInstanceOf[js.Array[js.Dynamic]].map(toExpr).toSeq)
    InsertCommand(table, columns, rows, returning)

  private def toUpdateCommand(obj: js.Dynamic): UpdateCommand =
    val table = ident(obj.table.asInstanceOf[String])
    val sets = obj.sets.asInstanceOf[js.Array[js.Dynamic]].map { s =>
      UpdateSet(ident(s.col.asInstanceOf[String]), toExpr(s.value))
    }.toSeq
    val where =
      if js.isUndefined(obj.where) || obj.where == null then None
      else Some(toExpr(obj.where))
    val returning =
      if js.isUndefined(obj.returning) || obj.returning == null then None
      else Some(obj.returning.asInstanceOf[js.Array[js.Dynamic]].map(toExpr).toSeq)
    UpdateCommand(table, sets, None, where, returning)

  private def toDeleteCommand(obj: js.Dynamic): DeleteCommand =
    val table = ident(obj.table.asInstanceOf[String])
    val where =
      if js.isUndefined(obj.where) || obj.where == null then None
      else Some(toExpr(obj.where))
    val returning =
      if js.isUndefined(obj.returning) || obj.returning == null then None
      else Some(obj.returning.asInstanceOf[js.Array[js.Dynamic]].map(toExpr).toSeq)
    DeleteCommand(table, where, returning)

  private def toCreateTableCommand(obj: js.Dynamic): CreateTableCommand =
    val table = ident(obj.table.asInstanceOf[String])
    val columns = obj.columns.asInstanceOf[js.Array[js.Dynamic]].map { c =>
      val name = ident(c.name.asInstanceOf[String])
      val typ = typeFromString(c.`type`.asInstanceOf[String])
      val notNull = c.notNull.asInstanceOf[Boolean]
      val unique = c.unique.asInstanceOf[Boolean]
      val pk = c.primaryKey.asInstanceOf[Boolean]
      val default =
        if js.isUndefined(c.default) || c.default == null then None
        else Some(toExpr(c.default))
      val references =
        if js.isUndefined(c.references) || c.references == null then None
        else
          val ref = c.references.asInstanceOf[js.Dynamic]
          Some((
            ident(ref.table.asInstanceOf[String]),
            ident(ref.column.asInstanceOf[String]),
            ReferentialAction.NoAction,
            ReferentialAction.NoAction,
          ))
      ColumnDesc(name, Left(typ), notNull || pk, unique, default, references, None, pk)
    }.toSeq

    val ifNotExists =
      if js.isUndefined(obj.ifNotExists) || obj.ifNotExists == null then false
      else obj.ifNotExists.asInstanceOf[Boolean]

    CreateTableCommand(table, columns, Seq.empty, ifNotExists)

  private def ident(name: String): Ident =
    val id = Ident(name)
    id.pos = NoPosition
    id

  private def typeFromString(s: String): Type =
    s.toLowerCase match
      case "serial"    => SerialType
      case "text"      => TextType
      case "integer"   => IntegerType
      case "int"       => IntegerType
      case "boolean"   => BooleanType
      case "bool"      => BooleanType
      case "double"    => DoubleType
      case "bigint"    => BigintType
      case "uuid"      => UUIDType
      case "timestamp" => TimestampType
      case "date"      => DateType
      case "time"      => TimeType
      case "json"      => JSONType
      case "bytea"     => ByteaType
      case "interval"  => IntervalType
      case other       => throw js.JavaScriptException(js.Error(s"Unknown type: $other"))

package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}
import io.github.edadma.{dal}

//import pprint.pprintln

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.language.postfixOps
import io.github.edadma.csv.{CSVRead, CSVWrite}
import io.github.edadma.cross_platform.{readFile, writeFile}

def executeQuery(query: String)(using session: Session): QueryResult = executeSelect(SQLParser.parseQuery(query))

def executeSelect(query: Expr)(using session: Session) =
  QueryResult(eval(rewrite(query), Nil).asInstanceOf[TableValue])

def executeSQL(sql: String)(using session: Session): Seq[Result] =
  val cs = SQLParser.parseCommands(sql)
  executeCommands(cs)

def executeSQL(sql: String, params: IndexedSeq[Any])(using session: Session): Seq[Result] =
  val cs = SQLParser.parseCommands(sql)
  val paramValues = params.map(anyToValue)
  val bound = deepCopyCommands(cs, paramValues)
  executeCommands(bound)

private[engine] def anyToValue(a: Any): Value =
  a match
    case null              => NullValue()
    case v: Value          => v
    case b: Boolean        => BooleanValue(b)
    case i: Int            => NumberValue(dal.IntType, i)
    case l: Long           => NumberValue(dal.LongType, l)
    case d: Double         => NumberValue(dal.DoubleType, d)
    case f: Float          => NumberValue(dal.DoubleType, f.toDouble)
    case s: Short          => NumberValue(dal.IntType, s.toInt)
    case b: Byte           => NumberValue(dal.IntType, b.toInt)
    case bd: BigDecimal    => NumberValue(dal.BigDecType, bd.bigDecimal)
    case bd: java.math.BigDecimal => NumberValue(dal.BigDecType, bd)
    case s: String         => TextValue(s)
    case d: java.time.LocalDate     => DateValue(d)
    case t: java.time.LocalTime     => TimeValue(t)
    case dt: java.time.LocalDateTime => TimestampValue(dt)
    case odt: java.time.OffsetDateTime => TimestampTZValue(odt)
    case ot: java.time.OffsetTime   => TimeTZValue(ot)
    case dur: java.time.Duration    => IntervalValue(dur)
    case bytes: Array[Byte]         => ByteaValue(bytes)
    case seq: Seq[?]       => ArrayValue(seq.map(anyToValue).toIndexedSeq)
    case arr: Array[?]     => ArrayValue(arr.map(anyToValue).toIndexedSeq)
    case iter: Iterable[?] => ArrayValue(iter.map(anyToValue).toIndexedSeq)
    case other             => platformAnyToValue(other)

private[engine] def executeCommands(cs: Seq[Command], blockEnv: Option[BlockEnv] = None)(using session: Session): Seq[Result] =

  val db = session.db

  def guardTransaction[T](fn: => T): T =
    if session.isTransactionAborted then sys.error("current transaction is aborted, use ROLLBACK")
    if session.inTransaction then
      session.activateHandle()
      try fn
      catch
        case e: Throwable =>
          session.markTransactionAborted()
          throw e
      finally
        session.deactivateHandle()
    else fn

  cs map {
    case BeginCommand    => session.beginTransaction(); BeginResult
    case CommitCommand   => session.commitTransaction(); CommitResult
    case RollbackCommand => session.rollbackTransaction(); RollbackResult
    case DoBlockCommand(block) =>
      executeBlock(block)
    case CreateSchemaCommand(Ident(name), ifNotExists) =>
      if ifNotExists && db.hasSchema(name) then CreateSchemaResult(name)
      else
        db.createSchema(name)
        CreateSchemaResult(name)
    case ShowTablesCommand =>
      val names = db.tableNames.toSeq.sorted
      val meta = Metadata(IndexedSeq(ColumnMetadata(None, "table_name", TextType)))
      val rows = names.map { n =>
        Row(IndexedSeq(TextValue(n)), meta, None, None)
      }.toVector
      QueryResult(TableValue(rows, meta))
    case ShowViewsCommand =>
      val meta = Metadata(IndexedSeq(
        ColumnMetadata(None, "view_name", TextType),
        ColumnMetadata(None, "definition", TextType),
      ))
      val rows = db.viewNames.toSeq.sorted.map { name =>
        Row(IndexedSeq(TextValue(name), TextValue(db.getView(name).getOrElse(""))), meta, None, None)
      }.toVector
      QueryResult(TableValue(rows, meta))
    case ShowSequencesCommand =>
      val meta = Metadata(IndexedSeq(
        ColumnMetadata(None, "sequence_name", TextType),
        ColumnMetadata(None, "current_value", NumberType),
        ColumnMetadata(None, "increment", NumberType),
        ColumnMetadata(None, "min_value", NumberType),
        ColumnMetadata(None, "max_value", NumberType),
        ColumnMetadata(None, "cycle", BooleanType),
        ColumnMetadata(None, "owned_by", TextType),
      ))
      val rows = db.sequences.values.toSeq.sortBy(_.name).map { s =>
        val ownedBy = (s.ownedByTable, s.ownedByColumn) match
          case (Some(t), Some(c)) => s"$t.$c"
          case _ => ""
        Row(IndexedSeq(
          TextValue(s.name),
          NumberValue(s.currentValue.toInt),
          NumberValue(s.increment.toInt),
          NumberValue(s.minValue.toInt),
          NumberValue(s.maxValue.toInt),
          BooleanValue(s.cycle),
          TextValue(ownedBy),
        ), meta, None, None)
      }.toVector
      QueryResult(TableValue(rows, meta))
    case ShowAllIndexesCommand =>
      val meta = Metadata(IndexedSeq(
        ColumnMetadata(None, "index_name", TextType),
        ColumnMetadata(None, "table_name", TextType),
        ColumnMetadata(None, "columns", TextType),
        ColumnMetadata(None, "is_unique", BooleanType),
      ))
      val rows = db.indexes.values.toSeq.sortBy(_.name).map { idx =>
        Row(IndexedSeq(
          TextValue(idx.name),
          TextValue(db.shortName(idx.tableName)),
          TextValue(idx.columns.mkString(", ")),
          BooleanValue(idx.unique),
        ), meta, None, None)
      }.toVector
      QueryResult(TableValue(rows, meta))
    case ShowColumnsCommand(Ident(table)) =>
      val t = db.getTable(table).getOrElse(sys.error(s"unknown table: $table"))
      val meta = Metadata(IndexedSeq(
        ColumnMetadata(None, "name", TextType),
        ColumnMetadata(None, "type", TextType),
        ColumnMetadata(None, "required", BooleanType),
        ColumnMetadata(None, "indexed", BooleanType),
        ColumnMetadata(None, "unique", BooleanType),
        ColumnMetadata(None, "fk_table", TextType),
        ColumnMetadata(None, "fk_column", TextType),
        ColumnMetadata(None, "fk_on_delete", TextType),
        ColumnMetadata(None, "fk_on_update", TextType),
        ColumnMetadata(None, "default_value", TextType),
      ))
      val rows = t.columns.map { cs =>
        val fkTable    = cs.fk.map(_._1).getOrElse("")
        val fkColumn   = cs.fk.map(_._2).getOrElse("")
        val fkOnDelete = cs.fk.map(_._3.toString).getOrElse("")
        val fkOnUpdate = cs.fk.map(_._4.toString).getOrElse("")
        val defaultVal = cs.default.map(_.toString).getOrElse("")
        Row(IndexedSeq(
          TextValue(cs.name),
          TextValue(Codecs.typeTag(cs.typ)),
          BooleanValue(cs.required),
          BooleanValue(cs.indexed),
          BooleanValue(cs.unique),
          TextValue(fkTable),
          TextValue(fkColumn),
          TextValue(fkOnDelete),
          TextValue(fkOnUpdate),
          TextValue(defaultVal),
        ), meta, None, None)
      }.toVector
      QueryResult(TableValue(rows, meta))
    case ShowPrimaryKeyCommand(Ident(table)) =>
      val t = db.getTable(table).getOrElse(sys.error(s"unknown table: $table"))
      val meta = Metadata(IndexedSeq(
        ColumnMetadata(None, "column_name", TextType),
        ColumnMetadata(None, "pk_name", TextType),
      ))
      val rows = t.primaryKey match
        case Some(pk) =>
          pk.columns.map { col =>
            Row(IndexedSeq(TextValue(col), TextValue(pk.name.getOrElse(""))), meta, None, None)
          }.toVector
        case None => Vector.empty
      QueryResult(TableValue(rows, meta))
    case ShowForeignKeysCommand(Ident(table)) =>
      val t = db.getTable(table).getOrElse(sys.error(s"unknown table: $table"))
      val fks = db.foreignKeys(t)
      val meta = Metadata(IndexedSeq(
        ColumnMetadata(None, "fk_name", TextType),
        ColumnMetadata(None, "fk_column", TextType),
        ColumnMetadata(None, "ref_table", TextType),
        ColumnMetadata(None, "ref_column", TextType),
        ColumnMetadata(None, "on_delete", TextType),
        ColumnMetadata(None, "on_update", TextType),
        ColumnMetadata(None, "seq", IntegerType),
      ))
      val rows = fks.flatMap { fk =>
        fk.columns.zip(fk.referencedColumns).zipWithIndex.map { case ((col, refCol), idx) =>
          Row(IndexedSeq(
            TextValue(fk.name.getOrElse("")),
            TextValue(col),
            TextValue(fk.referencedTable),
            TextValue(refCol),
            TextValue(fk.onDelete.toString),
            TextValue(fk.onUpdate.toString),
            NumberValue(idx + 1),
          ), meta, None, None)
        }
      }.toVector
      QueryResult(TableValue(rows, meta))
    case ShowIndexesCommand(Ident(table)) =>
      val t = db.getTable(table).getOrElse(sys.error(s"unknown table: $table"))
      val meta = Metadata(IndexedSeq(
        ColumnMetadata(None, "index_name", TextType),
        ColumnMetadata(None, "column_name", TextType),
        ColumnMetadata(None, "is_unique", BooleanType),
        ColumnMetadata(None, "seq", IntegerType),
      ))
      val rows = t.tableIndexes.values.toSeq.sortBy(_.meta.name).flatMap { idx =>
        idx.meta.columns.zipWithIndex.map { case (col, i) =>
          Row(IndexedSeq(
            TextValue(idx.meta.name),
            TextValue(col),
            BooleanValue(idx.meta.unique),
            NumberValue(i + 1),
          ), meta, None, None)
        }
      }.toVector
      QueryResult(TableValue(rows, meta))
    case PrepareCommand(id @ Ident(name), cmds) =>
      if session.preparedStatements.contains(name) then
        throw SchemaException(id.pos, s"prepared statement '$name' already exists")
      session.preparedStatements(name) = PreparedStatement(name, cmds)
      PrepareResult(name)
    case ExecuteCommand(id @ Ident(name), paramExprs) =>
      val ps = session.preparedStatements.getOrElse(name, throw UndefinedReferenceException(id.pos, s"prepared statement '$name' not found"))
      val paramValues = paramExprs.map(e => eval(rewrite(e), Nil)).toIndexedSeq
      val copied = deepCopyCommands(ps.commands, paramValues)
      executeCommands(copied).last
    case DeallocateCommand(id @ Ident(name)) =>
      if !session.preparedStatements.contains(name) then
        throw UndefinedReferenceException(id.pos, s"prepared statement '$name' not found")
      session.preparedStatements.remove(name)
      DeallocateResult(name)
    case ExplainCommand(innerCmd) =>
      innerCmd match
        case QueryCommand(query) =>
          val rewritten = rewrite(query)
          val plan = formatPlan(rewritten, 0)
          ExplainResult(plan)
        case _ => ExplainResult("(non-query command)")
    case cmd             => guardTransaction { cmd match
      case InsertCommand(id @ Ident(table), columns, rows, returning, onConflict) =>

        val t = session.getTable(table).getOrElse(throw UndefinedReferenceException(id.pos, s"unknown table: $table"))
        // Track sequence usage for currval/lastval in this session
        if t.backingSequences.nonEmpty then
          t.onSequenceUsed = Some { (seqName, value) =>
            session.sequenceValues(seqName) = value
            session.lastSequenceUsed = Some(seqName)
          }
        val resolvedColumns0 = columns.getOrElse(t.columns.map(c => Ident(c.name)).toSeq)
        val cols = resolvedColumns0.length

        // Resolve RETURNING exprs to column name list for the DB layer
        val retColNames: Option[Seq[String]] = returning.map { exprs =>
          if exprs.exists(_.isInstanceOf[StarExpr]) then Seq.empty // empty = all columns
          else exprs.map {
            case ColumnExpr(_, Ident(name)) => name
            case Ident(name) => name
            case e => sys.error(s"unsupported RETURNING expression: $e")
          }
        }

        rows find (_.length != cols) match
          case Some(row) => throw ExecutionException(row.head.pos, s"row length (${row.length}) not equal to number of columns ($cols)")
          case None      =>
            // Filter out columns/values where DEFAULT is used
            val defaultPositions = rows.head.zipWithIndex.collect { case (DefaultExpr, i) => i }.toSet
            val resolvedColumns =
              if defaultPositions.isEmpty then resolvedColumns0
              else resolvedColumns0.zipWithIndex.filterNot(p => defaultPositions(p._2)).map(_._1)
            val effectiveRows =
              if defaultPositions.isEmpty then rows
              else rows.map(r => r.zipWithIndex.filterNot(p => defaultPositions(p._2)).map(_._1))

            val data =
              for (r <- effectiveRows)
                yield r map (e => eval(rewrite(e), Nil))

            for (id @ Ident(c) <- resolvedColumns)
              if !t.hasColumn(c) then throw UndefinedReferenceException(id.pos, s"unknown column: $c")

            // Check for duplicate columns in INSERT column list
            val colNames = resolvedColumns.collect { case Ident(name) => name }
            val dupes = colNames.groupBy(identity).collect { case (name, occurrences) if occurrences.size > 1 => name }
            if dupes.nonEmpty then
              throw ExecutionException(resolvedColumns.head.pos, s"column \"${dupes.head}\" specified more than once")

            val fks = db.foreignKeys(t)
            val fkCheck: Option[IndexedSeq[Value] => Unit] =
              if fks.isEmpty then None
              else Some { (row: IndexedSeq[Value]) =>
                for fk <- fks do db.checkParentExists(table, fk, row, t.columnMap)
              }

            def buildInsertResult(lastResult: Map[String, Value]): InsertResult =
              val (row, metadata) =
                returning match
                  case None =>
                    val (cols, seq) = lastResult map { case (k, v) => (ColumnMetadata(Some(table), k, v.vtyp), v) } unzip
                    val metadata    = Metadata(cols.toIndexedSeq)
                    (Row(seq.toIndexedSeq, metadata, None, None), metadata)
                  case Some(retExprs) =>
                    // Determine which columns to include in response
                    val retNames =
                      if retExprs.exists(_.isInstanceOf[StarExpr]) then t.columns.map(_.name).toSeq
                      else retExprs.map {
                        case ColumnExpr(_, Ident(name)) => name
                        case Ident(name) => name
                        case e => sys.error(s"unsupported RETURNING expression: $e")
                      }
                    val filtered = retNames.flatMap { name =>
                      lastResult.get(name).map(v => (ColumnMetadata(Some(table), name, v.vtyp), v))
                    }
                    val (cols, seq) = filtered.unzip
                    val metadata = Metadata(cols.toIndexedSeq)
                    (Row(seq.toIndexedSeq, metadata, None, None), metadata)
              InsertResult(lastResult, TableValue(Vector(row), metadata))

            onConflict match
              case None =>
                val result = t.bulkInsert(resolvedColumns map (_.name), data, retColNames, fkCheck)
                buildInsertResult(result)

              case Some(OnConflictDoNothing) =>
                var lastResult: Map[String, Value] = Map.empty
                for d <- data do
                  try
                    lastResult = t.bulkInsert(resolvedColumns map (_.name), Seq(d), retColNames, fkCheck)
                  catch
                    case e: Exception if e.getMessage != null && e.getMessage.contains("duplicate key value violates unique constraint") => ()
                buildInsertResult(lastResult)

              case Some(OnConflictDoUpdate(conflictCols, updates)) =>
                val conflictColNames = conflictCols.map(_.name)
                val rewrites = updates.map { case UpdateSet(uid @ Ident(col), value) =>
                  if !t.hasColumn(col) then throw UndefinedReferenceException(uid.pos, s"unknown column: $col")
                  col -> rewrite(value)
                }
                val excludedMeta = Metadata(t.columns.map(spec => ColumnMetadata(Some("excluded"), spec.name, spec.typ)).toIndexedSeq)
                var lastResult: Map[String, Value] = Map.empty
                for d <- data do
                  try
                    lastResult = t.bulkInsert(resolvedColumns.map(_.name), Seq(d), retColNames, fkCheck)
                  catch
                    case e: Exception if e.getMessage != null && e.getMessage.contains("duplicate key value violates unique constraint") =>
                      val insertedColMap = resolvedColumns.map(_.name).zip(d).toMap
                      val excludedData   = t.columns.map(spec => insertedColMap.getOrElse(spec.name, NullValue())).toIndexedSeq
                      val excludedRow    = Row(excludedData, excludedMeta, None, None)
                      val conflictValues = conflictColNames.map(insertedColMap.getOrElse(_, NullValue()))
                      val conflictRow = t.iterator(Nil).find { row =>
                        conflictColNames.zip(conflictValues).forall { (col, v) =>
                          t.columnMap.get(col).exists(idx => row.data(idx) == v)
                        }
                      }
                      conflictRow match
                        case None => throw e
                        case Some(existing) =>
                          val evalCtx    = Seq(existing, excludedRow)
                          val evalUpdates = rewrites.map { (col, rwExpr) => col -> eval(rwExpr, evalCtx) }
                          existing.updater.getOrElse(sys.error("conflicting row is not updatable"))(evalUpdates)
                          val updatedData = existing.data.toArray
                          for (col, value) <- evalUpdates do
                            updatedData(t.columnMap(col)) = value
                          lastResult = t.columns.map(_.name).zip(updatedData).toMap
                          // Add RETURNING columns to result for upsert
                          for retNames <- retColNames do
                            val names = if retNames.isEmpty then t.columns.map(_.name).toSeq else retNames
                            for col <- names do
                              t.columnMap.get(col).foreach(idx => lastResult += (col -> updatedData(idx)))
                buildInsertResult(lastResult)
      case InsertSelectCommand(id @ Ident(table), columns, selectQuery, returning, onConflict) =>

        val t = session.getTable(table).getOrElse(throw UndefinedReferenceException(id.pos, s"unknown table: $table"))
        if t.backingSequences.nonEmpty then
          t.onSequenceUsed = Some { (seqName, value) =>
            session.sequenceValues(seqName) = value
            session.lastSequenceUsed = Some(seqName)
          }
        val queryResult = eval(rewrite(selectQuery), Nil).asInstanceOf[TableValue]
        val resolvedColumns = columns.getOrElse(t.columns.map(c => Ident(c.name)).toSeq)
        val cols = resolvedColumns.length

        // Resolve RETURNING exprs to column name list for the DB layer
        val retColNames: Option[Seq[String]] = returning.map { exprs =>
          if exprs.exists(_.isInstanceOf[StarExpr]) then Seq.empty
          else exprs.map {
            case ColumnExpr(_, Ident(name)) => name
            case Ident(name) => name
            case e => sys.error(s"unsupported RETURNING expression: $e")
          }
        }

        for (id @ Ident(c) <- resolvedColumns)
          if !t.hasColumn(c) then throw UndefinedReferenceException(id.pos, s"unknown column: $c")

        val data = queryResult.data.map { row =>
          if row.data.length != cols then
            sys.error(s"query result has ${row.data.length} columns, expected $cols")
          row.data.map(identity)
        }

        val fks = db.foreignKeys(t)
        val fkCheck: Option[IndexedSeq[Value] => Unit] =
          if fks.isEmpty then None
          else Some { (row: IndexedSeq[Value]) =>
            for fk <- fks do db.checkParentExists(table, fk, row, t.columnMap)
          }

        def buildSelectInsertResult(lastResult: Map[String, Value]): InsertResult =
          val (row, metadata) =
            returning match
              case None =>
                val (cols, seq) = lastResult map { case (k, v) => (ColumnMetadata(Some(table), k, v.vtyp), v) } unzip
                val metadata = Metadata(cols.toIndexedSeq)
                (Row(seq.toIndexedSeq, metadata, None, None), metadata)
              case Some(retExprs) =>
                val retNames =
                  if retExprs.exists(_.isInstanceOf[StarExpr]) then t.columns.map(_.name).toSeq
                  else retExprs.map {
                    case ColumnExpr(_, Ident(name)) => name
                    case Ident(name) => name
                    case e => sys.error(s"unsupported RETURNING expression: $e")
                  }
                val filtered = retNames.flatMap { name =>
                  lastResult.get(name).map(v => (ColumnMetadata(Some(table), name, v.vtyp), v))
                }
                val (cols, seq) = filtered.unzip
                val metadata = Metadata(cols.toIndexedSeq)
                (Row(seq.toIndexedSeq, metadata, None, None), metadata)
          InsertResult(lastResult, TableValue(Vector(row), metadata))

        onConflict match
          case None =>
            val result = t.bulkInsert(resolvedColumns map (_.name), data, retColNames, fkCheck)
            buildSelectInsertResult(result)

          case Some(OnConflictDoNothing) =>
            var lastResult: Map[String, Value] = Map.empty
            for d <- data do
              try
                lastResult = t.bulkInsert(resolvedColumns map (_.name), Seq(d), retColNames, fkCheck)
              catch
                case e: Exception if e.getMessage != null && e.getMessage.contains("duplicate key value violates unique constraint") => ()
            buildSelectInsertResult(lastResult)

          case Some(OnConflictDoUpdate(conflictCols, updates)) =>
            val conflictColNames = conflictCols.map(_.name)
            val rewrites = updates.map { case UpdateSet(uid @ Ident(col), value) =>
              if !t.hasColumn(col) then throw UndefinedReferenceException(uid.pos, s"unknown column: $col")
              col -> rewrite(value)
            }
            val excludedMeta = Metadata(t.columns.map(spec => ColumnMetadata(Some("excluded"), spec.name, spec.typ)).toIndexedSeq)
            var lastResult: Map[String, Value] = Map.empty
            for d <- data do
              try
                lastResult = t.bulkInsert(resolvedColumns.map(_.name), Seq(d), retColNames, fkCheck)
              catch
                case e: Exception if e.getMessage != null && e.getMessage.contains("duplicate key value violates unique constraint") =>
                  val insertedColMap = resolvedColumns.map(_.name).zip(d).toMap
                  val excludedData   = t.columns.map(spec => insertedColMap.getOrElse(spec.name, NullValue())).toIndexedSeq
                  val excludedRow    = Row(excludedData, excludedMeta, None, None)
                  val conflictValues = conflictColNames.map(insertedColMap.getOrElse(_, NullValue()))
                  val conflictRow = t.iterator(Nil).find { row =>
                    conflictColNames.zip(conflictValues).forall { (col, v) =>
                      t.columnMap.get(col).exists(idx => row.data(idx) == v)
                    }
                  }
                  conflictRow match
                    case None => throw e
                    case Some(existing) =>
                      val evalCtx    = Seq(existing, excludedRow)
                      val evalUpdates = rewrites.map { (col, rwExpr) => col -> eval(rwExpr, evalCtx) }
                      existing.updater.getOrElse(sys.error("conflicting row is not updatable"))(evalUpdates)
                      val updatedData = existing.data.toArray
                      for (col, value) <- evalUpdates do
                        updatedData(t.columnMap(col)) = value
                      lastResult = t.columns.map(_.name).zip(updatedData).toMap
                      for retNames <- retColNames do
                        val names = if retNames.isEmpty then t.columns.map(_.name).toSeq else retNames
                        for col <- names do
                          t.columnMap.get(col).foreach(idx => lastResult += (col -> updatedData(idx)))
            buildSelectInsertResult(lastResult)
      case QueryCommand(query)                                         => executeSelect(query)
      case CreateTableCommand(id @ Ident(table), columns, constraints, ifNotExists, temporary) =>
        val alreadyExists = if temporary then session.hasTempTable(table) else session.hasTable(table)
        if alreadyExists && ifNotExists then CreateTableResult(table)
        else
          if alreadyExists then throw SchemaException(id.pos, s"duplicate table: $table")

          val names = new mutable.HashSet[String]

          val columnSpecs = columns map { case ColumnDesc(id @ Ident(name), typeDesc, required, unique, default, references, _, pk, generated) =>
            if names contains name then throw SchemaException(id.pos, s"duplicate column name: $name")

            names += name

            val typ =
              typeDesc match
                case Left(primitive)             => primitive
                case Right(tid @ Ident(defined)) =>
                  db getType defined match
                    case None    => throw UndefinedReferenceException(tid.pos, s"type '$defined' is undefined")
                    case Some(t) => t
            val fkTuple = references.map { case (table, col, onDel, onUpd) => (table.name, col.name, onDel, onUpd) }

            ColumnSpec(
              name,
              typ,
              required || pk, // column-level PRIMARY KEY implies NOT NULL
              false, // indexed
              unique,
              fkTuple,
              default.map(expr => eval(rewrite(expr), Nil)),
              generated.map(expr => (exprToSQL(expr), rewrite(expr))),
            )
          }

          val constraintSpecs: Seq[Spec] = constraints.map {
            case PrimaryKeyConstraint(name, cols) =>
              PrimaryKeySpec(cols.map(_.name), name)
            case UniqueConstraint(name, cols) =>
              UniqueSpec(cols.map(_.name), name)
            case ForeignKeyConstraint(name, cols, refTable, refCols, onDel, onUpd) =>
              ForeignKeySpec(cols.map(_.name), refTable.name, refCols.map(_.name), name, onDel, onUpd)
            case CheckConstraint(name, expr) =>
              CheckSpec(exprToSQL(expr), expr, name)
          }

          // Synthesize PrimaryKeySpec from column-level PRIMARY KEY
          val columnPKCols = columns.collect { case ColumnDesc(Ident(name), _, _, _, _, _, _, true, _) => name }
          val hasTableLevelPK = constraintSpecs.exists(_.isInstanceOf[PrimaryKeySpec])
          if columnPKCols.nonEmpty && hasTableLevelPK then
            throw SchemaException(id.pos, s"cannot specify both column-level and table-level PRIMARY KEY")
          val columnPKSpecs: Seq[PrimaryKeySpec] =
            if columnPKCols.nonEmpty then Seq(PrimaryKeySpec(columnPKCols, None))
            else Nil

          // Convert column-level CHECK constraints to CheckSpec
          val columnCheckSpecs: Seq[CheckSpec] = columns.collect {
            case ColumnDesc(_, _, _, _, _, _, Some(expr), _, _) =>
              CheckSpec(exprToSQL(expr), expr, None)
          }

          // Validate FK references (look up via session to include temp tables)
          for spec <- columnSpecs do
            spec match
              case cs: ColumnSpec if cs.fk.isDefined =>
                val (refTableName, refColName, _, _) = cs.fk.get
                val refTable = session.getTable(refTableName).getOrElse(
                  throw UndefinedReferenceException(id.pos, s"referenced table '$refTableName' does not exist"))
                if !refTable.hasColumn(refColName) then
                  throw UndefinedReferenceException(id.pos, s"referenced column '$refColName' not found in table '$refTableName'")
              case _ =>
          for spec <- constraintSpecs do
            spec match
              case fk: ForeignKeySpec =>
                val refTable = session.getTable(fk.referencedTable).getOrElse(
                  throw UndefinedReferenceException(id.pos, s"referenced table '${fk.referencedTable}' does not exist"))
                for col <- fk.referencedColumns do
                  if !refTable.hasColumn(col) then
                    throw UndefinedReferenceException(id.pos, s"referenced column '$col' not found in table '${fk.referencedTable}'")
              case _ =>

          val allSpecs = columnSpecs ++ constraintSpecs ++ columnPKSpecs ++ columnCheckSpecs
          if temporary then
            val t = session.tempDB.createTable(table, allSpecs)
            session.tempTables(table) = t
          else
            val t = db.createTable(table, allSpecs)
            // Create backing sequences for SERIAL columns (PostgreSQL behavior)
            for spec <- columnSpecs if spec.typ == SmallSerialType || spec.typ == SerialType || spec.typ == BigSerialType do
              val seqName = s"${table}_${spec.name}_seq"
              val seq = db.createSequence(seqName, 1, 1, Long.MaxValue / 2, Some(1), false, Some(table), Some(spec.name))
              t.backingSequences(spec.name) = seq
          CreateTableResult(table)
      case CreateVirtualTableCommand(id @ Ident(tableName), moduleName, args) =>
        if session.hasTable(tableName) then
          throw SchemaException(id.pos, s"table '$tableName' already exists")
        db.getVirtualTableModule(moduleName) match
          case None => throw SchemaException(id.pos, s"unknown virtual table module '$moduleName'")
          case Some(module) =>
            val provider = module.create(tableName, args)
            val vtable = new VirtualTable(tableName, provider)
            db.registerVirtualTable(tableName, vtable)
            db.onMutation()
            CreateTableResult(tableName)
      case DropTableCommand(id @ Ident(table), ifExists, cascade) =>
        if session.hasTempTable(table) then
          session.tempTables.remove(table)
          session.tempDB.dropTable(table)
          DropTableResult(table)
        else
  
          if (!db.hasTable(table)) {
            if (!ifExists) throw UndefinedReferenceException(id.pos, s"unknown table: $table")
            else DropTableResult(table) // IF EXISTS allows missing table
          } else {
            val refs = db.childForeignKeys(table)
            if !cascade then
              if refs.nonEmpty then
                val refTableNames = refs.map(_._1.name).distinct.mkString(", ")
                throw ConstraintException(id.pos, s"cannot drop table '$table' because it is referenced by: $refTableNames")
            else
              // CASCADE: remove FK constraints from child tables that reference this table
              for (childTable, _) <- refs do
                childTable.constraints --= childTable.constraints.collect {
                  case f: ForeignKeySpec if f.referencedTable == table => f
                }
                // Also clear column-level FK references
                for i <- childTable.columns.indices do
                  childTable.columns(i).fk match
                    case Some((refTable, _, _, _)) if refTable == table =>
                      childTable.columns(i) = childTable.columns(i).copy(fk = None)
                    case _ =>
            // Drop owned sequences
            db.sequences.values.filter(_.ownedByTable.contains(table)).map(_.name).toSeq.foreach(db.dropSequence)
            db.dropTable(table)
            DropTableResult(table)
          }
      case CreateEnumCommand(id @ Ident(name), labels) =>

        if db hasType name then throw SchemaException(id.pos, s"duplicate type '$name'")

        db.createEnum(name, labels)
        CreateTypeResult(name)
      case UpdateCommand(id @ Ident(table), sets, from, cond, returning) =>

        val t             = session.getTable(table).getOrElse(throw UndefinedReferenceException(id.pos, s"unknown table: $table"))
        val (cols, exprs) =
          sets map { case UpdateSet(id @ Ident(col), value) =>
            if !t.hasColumn(col) then throw UndefinedReferenceException(id.pos, s"table $table doesn't has column '$col'")
            if t.columns(t.columnMap(col)).generated.isDefined then
              throw ExecutionException(id.pos, s"column \"$col\" can only be updated to DEFAULT")

            col -> rewrite(value)
          } unzip
        var count = 0
        val returnedRows = mutable.ArrayBuffer[Row]()

        val pkCols = t.primaryKey.map(_.columns.toSet).getOrElse(Set.empty)
        val updatedColSet = cols.toSet
        val childFKs = db.foreignKeys(t).filter(fk => fk.columns.exists(updatedColSet.contains))

        val checkConstraints = t.constraints.collect { case c: CheckSpec => c }
        val genCols = t.columns.zipWithIndex.collect { case (spec, idx) if spec.generated.isDefined => (idx, spec.name, spec.generated.get._2) }

        val rwReturning = returning.map(_.map(rewrite))

        def applyUpdate(targetRow: Row, evalRow: Row): Unit =
          targetRow.updater match
            case None    => throw ExecutionException(id.pos, "not updatable")
            case Some(u) =>
              val updates = cols zip (exprs map (e => eval(e, Seq(evalRow))))
              for (col, value) <- updates do
                if value.isNull then
                  if pkCols.contains(col) then
                    sys.error(s"null value in column \"$col\" violates not-null constraint")
                  else if t.columns(t.columnMap(col)).required then
                    sys.error(s"null value in column \"$col\" violates not-null constraint")
              // Recompute generated columns after applying explicit updates
              val allUpdates =
                if genCols.nonEmpty then
                  val newRowData = targetRow.data.toArray
                  for (col, value) <- updates do
                    newRowData(t.columnMap(col)) = value
                  val tmpRow = Row(newRowData.toIndexedSeq, t.meta, None, None)
                  val genUpdates = genCols.map { (idx, name, expr) =>
                    val v = eval(expr, Seq(tmpRow))
                    name -> t.columns(idx).typ.convert(v)
                  }
                  updates ++ genUpdates
                else updates
              // Enforce CHECK constraints on the updated row
              if checkConstraints.nonEmpty then
                val newRowData = targetRow.data.toArray
                for (col, value) <- allUpdates do
                  newRowData(t.columnMap(col)) = value
                val checkRow = Row(newRowData.toIndexedSeq, t.meta, None, None)
                for c <- checkConstraints do
                  if !beval(c.parsedExpr, Seq(checkRow)) then
                    sys.error(s"new row violates check constraint${c.name.map(n => s""" "$n"""").getOrElse("")}")
              db.enforceChildConstraints(table, targetRow, "update", Some(updatedColSet), Some(allUpdates))
              if childFKs.nonEmpty then
                val newRowData = targetRow.data.toArray
                for (col, value) <- allUpdates do
                  newRowData(t.columnMap(col)) = value
                for fk <- childFKs do
                  db.checkParentExists(table, fk, newRowData.toIndexedSeq, t.columnMap)
              u(allUpdates)
              // Evaluate RETURNING against the new row state
              rwReturning.foreach { retExprs =>
                val newRowData = targetRow.data.toArray
                for (col, value) <- allUpdates do
                  newRowData(t.columnMap(col)) = value
                val newRow = Row(newRowData.toIndexedSeq, t.meta, None, None)
                val projected = retExprs.map {
                  case StarExpr() => newRow.data
                  case e          => IndexedSeq(eval(e, Seq(newRow)))
                }.flatten.toIndexedSeq
                returnedRows += Row(projected, Metadata(Vector.empty), None, None)
              }
          count += 1

        from match
          case None =>
            val rows =
              cond match
                case Some(value) => SeqScanProcess(t, rewrite(value))
                case None        => t
            for (r <- rows.iterator(Nil)) applyUpdate(r, r)

          case Some(fromSources) =>
            val fromProcesses = fromSources.map(s => procRewrite(rewrite(s)))
            val fromProc = fromProcesses.reduceLeft((l, r) => CrossProcess(l, r))
            val mergedMeta = Metadata(t.meta.columns ++ fromProc.meta.columns)
            val rwCond = cond.map(rewrite(_))

            for (targetRow <- t.iterator(Nil))
              for (fromRow <- fromProc.iterator(Nil))
                val merged = Row(targetRow.data ++ fromRow.data, mergedMeta, targetRow.updater, targetRow.deleter)
                val matches = rwCond match
                  case Some(c) => beval(c, Seq(merged))
                  case None    => true
                if matches then applyUpdate(targetRow, merged)

        returning match
          case Some(retExprs) =>
            val retMeta = Metadata(
              if retExprs.exists(_.isInstanceOf[StarExpr]) then t.meta.columns
              else retExprs.zipWithIndex.map { case (e, i) =>
                e match
                  case ColumnExpr(_, Ident(name)) => ColumnMetadata(Some(table), name, t.meta.columns.find(_.name == name).map(_.typ).getOrElse(AnyType))
                  case _ => ColumnMetadata(None, s"column${i + 1}", AnyType)
              }.toIndexedSeq
            )
            val fixedRows = returnedRows.map(r => r.copy(meta = retMeta)).toVector
            QueryResult(TableValue(fixedRows, retMeta))
          case None =>
            UpdateResult(count)
      case DeleteCommand(id @ Ident(table), using, cond, returning) =>

        val t    = session.getTable(table).getOrElse(throw UndefinedReferenceException(id.pos, s"unknown table: $table"))
        var count = 0
        val returnedRows = mutable.ArrayBuffer[Row]()
        val rwReturning = returning.map(_.map(rewrite))

        def applyDelete(targetRow: Row, evalRow: Row): Unit =
          db.enforceChildConstraints(table, targetRow, "delete")
          rwReturning.foreach { retExprs =>
            val projected = retExprs.map {
              case StarExpr() => targetRow.data
              case e          => IndexedSeq(eval(e, Seq(evalRow)))
            }.flatten.toIndexedSeq
            returnedRows += Row(projected, Metadata(Vector.empty), None, None)
          }
          targetRow.deleter match
            case Some(d) => d()
            case None    => throw ExecutionException(id.pos, "not updatable")
          count += 1

        using match
          case None =>
            val rows =
              cond match
                case Some(value) => SeqScanProcess(t, rewrite(value))
                case None        => t
            for (r <- rows.iterator(Nil)) applyDelete(r, r)

          case Some(usingSources) =>
            val usingProcesses = usingSources.map(s => procRewrite(rewrite(s)))
            val usingProc = usingProcesses.reduceLeft((l, r) => CrossProcess(l, r))
            val mergedMeta = Metadata(t.meta.columns ++ usingProc.meta.columns)
            val rwCond = cond.map(rewrite(_))
            val deleted = mutable.Set[Any]()

            for (targetRow <- t.iterator(Nil))
              for (usingRow <- usingProc.iterator(Nil))
                val merged = Row(targetRow.data ++ usingRow.data, mergedMeta, targetRow.updater, targetRow.deleter)
                val matches = rwCond match
                  case Some(c) => beval(c, Seq(merged))
                  case None    => true
                if matches && !deleted.contains(targetRow.deleter) then
                  deleted += targetRow.deleter
                  applyDelete(targetRow, merged)

        returning match
          case Some(retExprs) =>
            val retMeta = Metadata(
              if retExprs.exists(_.isInstanceOf[StarExpr]) then t.meta.columns
              else retExprs.zipWithIndex.map { case (e, i) =>
                e match
                  case ColumnExpr(_, Ident(name)) => ColumnMetadata(Some(table), name, t.meta.columns.find(_.name == name).map(_.typ).getOrElse(AnyType))
                  case _ => ColumnMetadata(None, s"column${i + 1}", AnyType)
              }.toIndexedSeq
            )
            val fixedRows = returnedRows.map(r => r.copy(meta = retMeta)).toVector
            QueryResult(TableValue(fixedRows, retMeta))
          case None =>
            DeleteResult(count)
      case TruncateCommand(id @ Ident(table)) =>

        val t = session.getTable(table).getOrElse(throw UndefinedReferenceException(id.pos, s"unknown table: $table"))
        // Enforce FK constraints: fail if any child table has rows referencing this table
        val childFKs = db.childForeignKeys(table)
        for (childTable, fk) <- childFKs do
          for row <- childTable.iterator(Nil) do
            val childValues = fk.columns.map(c => row.data(childTable.columnMap(c)))
            if !childValues.forall(_.isNull) then
              sys.error(s"cannot truncate table '$table': rows in '${childTable.name}' reference it")
        t.truncate()
        TruncateResult(table)
      case CreateIndexCommand(id @ Ident(indexName), tid @ Ident(tableName), columns, unique, where) =>

        if !session.hasTable(tableName) then throw UndefinedReferenceException(tid.pos, s"unknown table: $tableName")
        if db.hasIndex(indexName) then throw SchemaException(id.pos, s"index '$indexName' already exists")
        val t = db.getTable(tableName).get

        // Resolve column names (for plain columns) and validate
        val colNames = columns.map {
          case Left(col @ Ident(colName)) =>
            if !t.hasColumn(colName) then throw UndefinedReferenceException(col.pos, s"column '$colName' not found in table '$tableName'")
            colName
          case Right(_) => null // expression indexes don't map to a single column name
        }

        val exprKeys: Option[Seq[Expr]] =
          if columns.exists(_.isRight) then
            Some(columns.map {
              case Left(Ident(colName)) => ColumnExpr(None, Ident(colName))
              case Right(expr) => rewrite(expr)
            })
          else None

        val resolvedWhere = where.map(rewrite)

        db.createIndex(indexName, tableName, colNames, unique, resolvedWhere, exprKeys)
        CreateIndexResult(indexName)
      case DropIndexCommand(id @ Ident(name), ifExists) =>

        if !db.hasIndex(name) then
          if !ifExists then throw UndefinedReferenceException(id.pos, s"index '$name' not found")
          DropIndexResult(name)
        else
          db.dropIndex(name)
          DropIndexResult(name)
      case DropTypeCommand(id @ Ident(name), ifExists, cascade) =>

        if (!db.hasType(name)) {
          if (!ifExists) throw UndefinedReferenceException(id.pos, s"unknown type: $name")
          else DropTypeResult(name)
        } else {
          db.dropType(name)
          DropTypeResult(name)
        }
      case CreateSequenceCommand(id @ Ident(name), increment, minValue, maxValue, startValue, cycle, ifNotExists) =>
        if db.hasSequence(name) then
          if ifNotExists then CreateSequenceResult(name)
          else throw SchemaException(id.pos, s"sequence '$name' already exists")
        else
          val min = minValue.getOrElse(if increment > 0 then 1L else Long.MinValue / 2)
          val max = maxValue.getOrElse(if increment > 0 then Long.MaxValue / 2 else -1L)
          db.createSequence(name, increment, min, max, startValue, cycle)
          CreateSequenceResult(name)
      case DropSequenceCommand(id @ Ident(name), ifExists) =>
        if !db.hasSequence(name) then
          if !ifExists then throw UndefinedReferenceException(id.pos, s"sequence '$name' does not exist")
          DropSequenceResult(name)
        else
          db.dropSequence(name)
          DropSequenceResult(name)
      case AlterTableCommand(id @ Ident(table), alter) =>

        if !session.hasTable(table) then throw UndefinedReferenceException(id.pos, s"unknown table: $table")
        db.alterTable(table, alter)
        AlterTableResult()
      case CreateViewCommand(id @ Ident(name), queryExpr, orReplace) =>

        if session.hasTable(name) then throw SchemaException(id.pos, s"'$name' is already a table")
        if !orReplace && db.hasView(name) then throw SchemaException(id.pos, s"view '$name' already exists")
        val sql = exprToSQL(queryExpr)
        db.createView(name, sql, orReplace)
        CreateViewResult(name)
      case DropViewCommand(id @ Ident(name), ifExists) =>

        if !db.hasView(name) then
          if !ifExists then throw UndefinedReferenceException(id.pos, s"view '$name' not found")
          DropViewResult(name)
        else
          db.dropView(name)
          DropViewResult(name)
      case CopyFromCommand(id @ Ident(table), columns, file, header, delimiter) =>

        val t = session.getTable(table).getOrElse(throw UndefinedReferenceException(id.pos, s"unknown table: $table"))
        val resolvedColumns = columns.getOrElse(t.columns.map(c => Ident(c.name)).toSeq)

        for (cid @ Ident(c) <- resolvedColumns)
          if !t.hasColumn(c) then throw UndefinedReferenceException(cid.pos, s"unknown column: $c")

        val fks = db.foreignKeys(t)
        val fkCheck: Option[IndexedSeq[Value] => Unit] =
          if fks.isEmpty then None
          else Some { (row: IndexedSeq[Value]) =>
            for fk <- fks do db.checkParentExists(table, fk, row, t.columnMap)
          }

        var count = 0
        var isFirst = true
        val batch = new mutable.ArrayBuffer[Seq[Value]]
        val content = readFile(file)
        CSVRead.fromStringStreamed(content, { row =>
          if isFirst && header then isFirst = false
          else
            isFirst = false
            val values: Seq[Value] = row.map(s => if s.isEmpty then NullValue() else TextValue(s))
            batch += values
            if batch.size >= 100 then
              t.bulkInsert(resolvedColumns.map(_.name), batch.toSeq, None, fkCheck)
              count += batch.size
              batch.clear()
        }, delimiter)
        if batch.nonEmpty then
          t.bulkInsert(resolvedColumns.map(_.name), batch.toSeq, None, fkCheck)
          count += batch.size
        CopyResult(count)

      case CopyToCommand(source, file, header, delimiter) =>
        val (columnNames, rows) = source match
          case Left(id @ Ident(table)) =>
            val t = session.getTable(table).getOrElse(throw UndefinedReferenceException(id.pos, s"unknown table: $table"))
            val names = t.columns.map(_.name).toList
            val data = t.iterator(Nil).map(row => row.data.map(v => if v.isNull then "" else v.toText.s).toList).toList
            (names, data)
          case Right(queryExpr) =>
            val result = eval(rewrite(queryExpr), Nil).asInstanceOf[TableValue]
            val names = result.meta.columns.map(_.name).toList
            val data = result.data.map(row => row.data.map(v => if v.isNull then "" else v.toText.s).toList).toList
            (names, data)
        val output = if header then columnNames :: rows else rows
        writeFile(file, CSVWrite.toString(output, delimiter))
        CopyResult(rows.size)

      case _ => sys.error(s"unexpected command")
    }
  }

// ── EXPLAIN Plan Formatting ──────────────────────────────────────

private def formatPlan(expr: Expr, indent: Int): String =
  val prefix = "  " * indent
  expr match
    case ProcessOperator(proc) => formatProcess(proc, indent)
    case other                 => s"${prefix}Expr(${other.getClass.getSimpleName})"

private def formatProcess(proc: Process, indent: Int): String =
  val prefix = "  " * indent
  proc match
    case p: ProjectProcess     => s"${prefix}Project\n${formatProcess(p.input, indent + 1)}"
    case p: SeqScanProcess     => s"${prefix}Seq Scan (filter)\n${formatProcess(p.input, indent + 1)}"
    case p: SortProcess        => s"${prefix}Sort\n${formatProcess(p.input, indent + 1)}"
    case p: AggregateProcess   => s"${prefix}Aggregate\n${formatProcess(p.input, indent + 1)}"
    case p: TakeProcess        => s"${prefix}Limit\n${formatProcess(p.input, indent + 1)}"
    case _: DropProcess        => s"${prefix}Offset"
    case p: DistinctProcess    => s"${prefix}Distinct\n${formatProcess(p.input, indent + 1)}"
    case p: DistinctOnProcess  => s"${prefix}Distinct On\n${formatProcess(p.input, indent + 1)}"
    case p: HavingProcess      => s"${prefix}Having\n${formatProcess(p.input, indent + 1)}"
    case p: CrossProcess       => s"${prefix}Cross Join\n${formatProcess(p.input1, indent + 1)}\n${formatProcess(p.input2, indent + 1)}"
    case p: AliasProcess       => s"${prefix}Alias (${p.alias})\n${formatProcess(p.input, indent + 1)}"
    case p: IndexScanProcess   => s"${prefix}Index Scan on ${p.table.name} using ${p.index.meta.name}"
    case p: IndexNestedLoopJoinProcess =>
      s"${prefix}Index Nested Loop Join using ${p.index.meta.name} on ${p.table.name}\n${formatProcess(p.outer, indent + 1)}"
    case p: LeftIndexNestedLoopJoinProcess =>
      s"${prefix}Index Nested Loop Left Join using ${p.index.meta.name} on ${p.table.name}\n${formatProcess(p.outer, indent + 1)}"
    case p: RightIndexNestedLoopJoinProcess =>
      s"${prefix}Index Nested Loop Right Join using ${p.index.meta.name} on ${p.table.name}\n${formatProcess(p.outer, indent + 1)}"
    case p: HashJoinProcess      => s"${prefix}Hash Join\n${formatProcess(p.build, indent + 1)}\n${formatProcess(p.probe, indent + 1)}"
    case p: LeftHashJoinProcess  => s"${prefix}Hash Left Join\n${formatProcess(p.build, indent + 1)}\n${formatProcess(p.probe, indent + 1)}"
    case p: RightHashJoinProcess => s"${prefix}Hash Right Join\n${formatProcess(p.build, indent + 1)}\n${formatProcess(p.probe, indent + 1)}"
    case p: FullHashJoinProcess  => s"${prefix}Hash Full Join\n${formatProcess(p.build, indent + 1)}\n${formatProcess(p.probe, indent + 1)}"
    case p: UnionProcess       => s"${prefix}Union${if p.all then " All" else ""}\n${formatProcess(p.input1, indent + 1)}\n${formatProcess(p.input2, indent + 1)}"
    case p: RecursiveCTEProcess  => s"${prefix}Recursive CTE${if p.all then " All" else ""}\n${formatProcess(p.anchor, indent + 1)}\n${formatProcess(p.recursive, indent + 1)}"
    case p: GenerateSeriesProcess => s"${prefix}Generate Series"
    case t: Table              => s"${prefix}Seq Scan on ${t.name}"
    case SingleProcess         => s"${prefix}Result"
    case _                     => s"${prefix}${proc.getClass.getSimpleName}"

// ── Deep Copy Utilities ──────────────────────────────────────────
// AST nodes have a mutable `var typ` that gets set during rewrite().
// To re-execute cached prepared statements we deep-copy the AST to
// get fresh instances with typ = null.

private[engine] def deepCopyExpr(expr: Expr, params: IndexedSeq[Value] = IndexedSeq.empty): Expr =
  val copied: Expr = expr match
    case p @ ParameterExpr(index) =>
      if params.nonEmpty then
        if index < 1 || index > params.length then
          throw ExecutionException(p.pos, s"parameter $$$index is not bound (have ${params.length} parameters)")
        ValueExpr(params(index - 1))
      else ParameterExpr(index)
    case ValueExpr(v)                      => ValueExpr(v)
    case ColumnExpr(table, col)            => ColumnExpr(table, col)
    case VariableExpr(name)                => VariableExpr(name)
    case NumberExpr(n)                     => NumberExpr(n)
    case StringExpr(s)                     => StringExpr(s)
    case BooleanExpr(b)                    => BooleanExpr(b)
    case NullExpr()                        => NullExpr()
    case StarExpr()                        => StarExpr()
    case TableStarExpr(table)              => TableStarExpr(table)
    case AliasExpr(e, alias)               => AliasExpr(deepCopyExpr(e, params), alias)
    case UnaryExpr(op, e)                  => UnaryExpr(op, deepCopyExpr(e, params))
    case BinaryExpr(l, op, r)              => BinaryExpr(deepCopyExpr(l, params), op, deepCopyExpr(r, params))
    case BetweenExpr(v, op, lo, hi)        => BetweenExpr(deepCopyExpr(v, params), op, deepCopyExpr(lo, params), deepCopyExpr(hi, params))
    case OverlapsExpr(a, b, c, d)          => OverlapsExpr(deepCopyExpr(a, params), deepCopyExpr(b, params), deepCopyExpr(c, params), deepCopyExpr(d, params))
    case CaseExpr(whens, els) =>
      CaseExpr(whens.map { case When(w, e) => When(deepCopyExpr(w, params), deepCopyExpr(e, params)) }, els.map(deepCopyExpr(_, params)))
    case ApplyExpr(func, args, filter)     => ApplyExpr(func, args.map(deepCopyExpr(_, params)), filter.map(deepCopyExpr(_, params)))
    case WindowExpr(func, partBy, ordBy, frame) => WindowExpr(deepCopyExpr(func, params), partBy.map(deepCopyExpr(_, params)), ordBy.map { case OrderBy(f, d, n) => OrderBy(deepCopyExpr(f, params), d, n) }, frame)
    case InSeqExpr(v, op, es)              => InSeqExpr(deepCopyExpr(v, params), op, es.map(deepCopyExpr(_, params)))
    case InQueryExpr(v, op, q)             => InQueryExpr(deepCopyExpr(v, params), op, deepCopyExpr(q, params))
    case QuantifiedCompareExpr(v, op, q, e) => QuantifiedCompareExpr(deepCopyExpr(v, params), op, q, deepCopyExpr(e, params))
    case ScalarFunctionExpr(f, args)       => ScalarFunctionExpr(f, args.map(deepCopyExpr(_, params)))
    case AggregateFunctionExpr(f, args, filter) => AggregateFunctionExpr(f, args.map(deepCopyExpr(_, params)), filter.map(deepCopyExpr(_, params)))
    case v: VariableInstanceExpr           => v
    case DefaultExpr                       => DefaultExpr
    case SubqueryExpr(q)                   => SubqueryExpr(deepCopyExpr(q, params))
    case ExistsExpr(q)                     => ExistsExpr(deepCopyExpr(q, params))
    case ObjectExpr(props)                 => ObjectExpr(props.map { case (k, v) => (k, deepCopyExpr(v, params)) })
    case ArrayExpr(elems)                  => ArrayExpr(elems.map(deepCopyExpr(_, params)))
    case TableConstructorExpr(q)           => TableConstructorExpr(deepCopyExpr(q, params))
    case CastExpr(e, t)                    => CastExpr(deepCopyExpr(e, params), t)
    case SetOperationExpr(op, l, r)        => SetOperationExpr(op, deepCopyExpr(l, params), deepCopyExpr(r, params))
    case ValuesExpr(rows)                  => ValuesExpr(rows.map(_.map(deepCopyExpr(_, params))))
    case LateralExpr(q)                    => LateralExpr(deepCopyExpr(q, params))
    case CompoundQueryExpr(q, ob, off, lim) =>
      CompoundQueryExpr(deepCopyExpr(q, params), ob.map(_.map(deepCopyOrderBy(_, params))),
        off.map(c => Count(c.pos, deepCopyExpr(c.expr, params))),
        lim.map(c => Count(c.pos, deepCopyExpr(c.expr, params))))
    case WithExpr(ctes, query, recursive) =>
      WithExpr(
        ctes.map(c => CTEDef(c.name, c.columns, deepCopyExpr(c.query, params))),
        deepCopyExpr(query, params),
        recursive,
      )
    case SQLSelectExpr(exprs, from, where, groupBy, having, orderBy, offset, limit, distinct, distinctOn) =>
      SQLSelectExpr(
        exprs.map(deepCopyExpr(_, params)).to(ArraySeq),
        from.map(_.map(deepCopyExpr(_, params))),
        where.map(deepCopyExpr(_, params)),
        groupBy.map(_.map(deepCopyExpr(_, params))),
        having.map(deepCopyExpr(_, params)),
        orderBy.map(_.map(deepCopyOrderBy(_, params))),
        offset.map(c => Count(c.pos, deepCopyExpr(c.expr, params))),
        limit.map(c => Count(c.pos, deepCopyExpr(c.expr, params))),
        distinct,
        distinctOn.map(_.map(deepCopyExpr(_, params))),
      )
    case AliasOperator(r, a) => AliasOperator(deepCopyExpr(r, params), a)
    case ColumnAliasOperator(r, a, cs) => ColumnAliasOperator(deepCopyExpr(r, params), a, cs)
    case CrossOperator(r1, r2) => CrossOperator(deepCopyExpr(r1, params), deepCopyExpr(r2, params))
    case InnerJoinOperator(r1, r2, on) => InnerJoinOperator(deepCopyExpr(r1, params), deepCopyExpr(r2, params), deepCopyExpr(on, params))
    case LeftJoinOperator(r1, r2, on) => LeftJoinOperator(deepCopyExpr(r1, params), deepCopyExpr(r2, params), deepCopyExpr(on, params))
    case RightJoinOperator(r1, r2, on) => RightJoinOperator(deepCopyExpr(r1, params), deepCopyExpr(r2, params), deepCopyExpr(on, params))
    case FullJoinOperator(r1, r2, on) => FullJoinOperator(deepCopyExpr(r1, params), deepCopyExpr(r2, params), deepCopyExpr(on, params))
    case LateralCrossOperator(r1, r2) => LateralCrossOperator(deepCopyExpr(r1, params), deepCopyExpr(r2, params))
    case TableOperator(t) => TableOperator(t)
    case other => other // ProcessOperator, etc. — should not appear in parsed AST
  if expr.pos != null then copied.setPos(expr.pos)
  copied

private def deepCopyOrderBy(ob: OrderBy, params: IndexedSeq[Value] = IndexedSeq.empty): OrderBy =
  OrderBy(deepCopyExpr(ob.f, params), ob.asc, ob.nullsFirst)

private[engine] def deepCopyCommand(cmd: Command, params: IndexedSeq[Value] = IndexedSeq.empty): Command =
  cmd match
    case QueryCommand(query) =>
      QueryCommand(deepCopyExpr(query, params))
    case InsertCommand(table, columns, rows, returning, onConflict) =>
      InsertCommand(table, columns, rows.map(_.map(deepCopyExpr(_, params))),
        returning.map(_.map(deepCopyExpr(_, params))),
        onConflict.map {
          case OnConflictDoNothing => OnConflictDoNothing
          case OnConflictDoUpdate(cols, updates) =>
            OnConflictDoUpdate(cols, updates.map(s => UpdateSet(s.col, deepCopyExpr(s.value, params))))
        })
    case InsertSelectCommand(table, columns, query, returning, onConflict) =>
      InsertSelectCommand(table, columns, deepCopyExpr(query, params),
        returning.map(_.map(deepCopyExpr(_, params))),
        onConflict.map {
          case OnConflictDoNothing => OnConflictDoNothing
          case OnConflictDoUpdate(cols, updates) =>
            OnConflictDoUpdate(cols, updates.map(s => UpdateSet(s.col, deepCopyExpr(s.value, params))))
        })
    case UpdateCommand(table, sets, from, cond, returning) =>
      UpdateCommand(table, sets.map(s => UpdateSet(s.col, deepCopyExpr(s.value, params))),
        from.map(_.map(deepCopyExpr(_, params))), cond.map(deepCopyExpr(_, params)),
        returning.map(_.map(deepCopyExpr(_, params))))
    case DeleteCommand(table, using, cond, returning) =>
      DeleteCommand(table, using.map(_.map(deepCopyExpr(_, params))), cond.map(deepCopyExpr(_, params)), returning.map(_.map(deepCopyExpr(_, params))))
    case ExplainCommand(inner) =>
      ExplainCommand(deepCopyCommand(inner, params))
    case PrepareCommand(name, cmds) =>
      PrepareCommand(name, cmds.map(deepCopyCommand(_, params)))
    case ExecuteCommand(name, execParams) =>
      ExecuteCommand(name, execParams.map(deepCopyExpr(_, params)))
    case other => other // DDL commands, BEGIN/COMMIT/ROLLBACK — no mutable Expr state

private[engine] def deepCopyCommands(cmds: Seq[Command], params: IndexedSeq[Value] = IndexedSeq.empty): Seq[Command] = cmds.map(deepCopyCommand(_, params))

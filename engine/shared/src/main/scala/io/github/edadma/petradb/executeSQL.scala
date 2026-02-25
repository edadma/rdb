package io.github.edadma.petradb

//import pprint.pprintln

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.language.postfixOps

def executeQuery(query: String)(using session: Session): QueryResult = executeSelect(SQLParser.parseQuery(query))

def executeSelect(query: Expr)(using session: Session) =
  QueryResult(eval(rewrite(query), Nil).asInstanceOf[TableValue])

def executeSQL(sql: String)(using session: Session): Seq[Result] =
  val cs = SQLParser.parseCommands(sql)
  executeCommands(cs)

private[petradb] def executeCommands(cs: Seq[Command])(using session: Session): Seq[Result] =

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

  def guardDDL(): Unit =
    if session.inTransaction then sys.error("DDL not allowed inside a transaction")

  cs map {
    case BeginCommand    => session.beginTransaction(); BeginResult
    case CommitCommand   => session.commitTransaction(); CommitResult
    case RollbackCommand => session.rollbackTransaction(); RollbackResult
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
        val t = db.getTable(table).getOrElse(throw UndefinedReferenceException(id.pos, s"unknown table: $table"))
        val resolvedColumns = columns.getOrElse(t.columns.map(c => Ident(c.name)).toSeq)
        val cols = resolvedColumns.length

        rows find (_.length != cols) match
          case Some(row) => throw ExecutionException(row.head.pos, s"row length (${row.length}) not equal to number of columns ($cols)")
          case None      =>
            val data =
              for (r <- rows)
                yield r map (e => eval(rewrite(e), Nil))

            for (id @ Ident(c) <- resolvedColumns)
              if !t.hasColumn(c) then throw UndefinedReferenceException(id.pos, s"unknown column: $c")

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
                  case Some(ret @ Ident(retCol)) =>
                    if lastResult contains retCol then
                      val (cols, seq) = lastResult filter { case (k, _) => k == retCol } map { case (k, v) =>
                        (ColumnMetadata(Some(table), k, v.vtyp), v)
                      } unzip
                      val metadata = Metadata(cols.toIndexedSeq)
                      (Row(seq.toIndexedSeq, metadata, None, None), metadata)
                    else throw UndefinedReferenceException(ret.pos, s"'$retCol' not found in result from insert")
              InsertResult(lastResult, TableValue(Vector(row), metadata))

            onConflict match
              case None =>
                val result = t.bulkInsert(resolvedColumns map (_.name), data, returning, fkCheck)
                buildInsertResult(result)

              case Some(OnConflictDoNothing) =>
                var lastResult: Map[String, Value] = Map.empty
                for d <- data do
                  try
                    lastResult = t.bulkInsert(resolvedColumns map (_.name), Seq(d), returning, fkCheck)
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
                    lastResult = t.bulkInsert(resolvedColumns.map(_.name), Seq(d), returning, fkCheck)
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
                          returning match
                            case Some(ret @ Ident(retCol)) =>
                              t.columnMap.get(retCol) match
                                case Some(idx) => lastResult = lastResult + (retCol -> updatedData(idx))
                                case None      => throw UndefinedReferenceException(ret.pos, s"'$retCol' not found in result from insert")
                            case None =>
                buildInsertResult(lastResult)
      case InsertSelectCommand(id @ Ident(table), columns, selectQuery, returning, onConflict) =>
        val t = db.getTable(table).getOrElse(throw UndefinedReferenceException(id.pos, s"unknown table: $table"))
        val queryResult = eval(rewrite(selectQuery), Nil).asInstanceOf[TableValue]
        val resolvedColumns = columns.getOrElse(t.columns.map(c => Ident(c.name)).toSeq)
        val cols = resolvedColumns.length

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
              case Some(ret @ Ident(retCol)) =>
                if lastResult contains retCol then
                  val (cols, seq) = lastResult filter { case (k, _) => k == retCol } map { case (k, v) =>
                    (ColumnMetadata(Some(table), k, v.vtyp), v)
                  } unzip
                  val metadata = Metadata(cols.toIndexedSeq)
                  (Row(seq.toIndexedSeq, metadata, None, None), metadata)
                else throw UndefinedReferenceException(ret.pos, s"'$retCol' not found in result from insert")
          InsertResult(lastResult, TableValue(Vector(row), metadata))

        onConflict match
          case None =>
            val result = t.bulkInsert(resolvedColumns map (_.name), data, returning, fkCheck)
            buildSelectInsertResult(result)

          case Some(OnConflictDoNothing) =>
            var lastResult: Map[String, Value] = Map.empty
            for d <- data do
              try
                lastResult = t.bulkInsert(resolvedColumns map (_.name), Seq(d), returning, fkCheck)
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
                lastResult = t.bulkInsert(resolvedColumns.map(_.name), Seq(d), returning, fkCheck)
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
                      returning match
                        case Some(ret @ Ident(retCol)) =>
                          t.columnMap.get(retCol) match
                            case Some(idx) => lastResult = lastResult + (retCol -> updatedData(idx))
                            case None      => throw UndefinedReferenceException(ret.pos, s"'$retCol' not found in result from insert")
                        case None =>
            buildSelectInsertResult(lastResult)
      case QueryCommand(query)                                         => executeSelect(query)
      case CreateTableCommand(id @ Ident(table), columns, constraints, ifNotExists) =>
        guardDDL()
        if (db hasTable table) && ifNotExists then CreateTableResult(table)
        else
          if db hasTable table then throw SchemaException(id.pos, s"duplicate table: $table")

          val names = new mutable.HashSet[String]

          val columnSpecs = columns map { case ColumnDesc(id @ Ident(name), typeDesc, required, unique, default, references, _, pk) =>
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
          val columnPKCols = columns.collect { case ColumnDesc(Ident(name), _, _, _, _, _, _, true) => name }
          val hasTableLevelPK = constraintSpecs.exists(_.isInstanceOf[PrimaryKeySpec])
          if columnPKCols.nonEmpty && hasTableLevelPK then
            throw SchemaException(id.pos, s"cannot specify both column-level and table-level PRIMARY KEY")
          val columnPKSpecs: Seq[PrimaryKeySpec] =
            if columnPKCols.nonEmpty then Seq(PrimaryKeySpec(columnPKCols, None))
            else Nil

          // Convert column-level CHECK constraints to CheckSpec
          val columnCheckSpecs: Seq[CheckSpec] = columns.collect {
            case ColumnDesc(_, _, _, _, _, _, Some(expr), _) =>
              CheckSpec(exprToSQL(expr), expr, None)
          }

          // Validate FK references
          for spec <- columnSpecs do
            spec match
              case cs: ColumnSpec if cs.fk.isDefined =>
                val (refTableName, refColName, _, _) = cs.fk.get
                val refTable = db.getTable(refTableName).getOrElse(
                  throw UndefinedReferenceException(id.pos, s"referenced table '$refTableName' does not exist"))
                if !refTable.hasColumn(refColName) then
                  throw UndefinedReferenceException(id.pos, s"referenced column '$refColName' not found in table '$refTableName'")
              case _ =>
          for spec <- constraintSpecs do
            spec match
              case fk: ForeignKeySpec =>
                val refTable = db.getTable(fk.referencedTable).getOrElse(
                  throw UndefinedReferenceException(id.pos, s"referenced table '${fk.referencedTable}' does not exist"))
                for col <- fk.referencedColumns do
                  if !refTable.hasColumn(col) then
                    throw UndefinedReferenceException(id.pos, s"referenced column '$col' not found in table '${fk.referencedTable}'")
              case _ =>

          val allSpecs = columnSpecs ++ constraintSpecs ++ columnPKSpecs ++ columnCheckSpecs
          db.createTable(table, allSpecs)
          CreateTableResult(table)
      case DropTableCommand(id @ Ident(table), ifExists, cascade) =>
        guardDDL()
        if (!db.hasTable(table)) {
          if (!ifExists) throw UndefinedReferenceException(id.pos, s"unknown table: $table")
          else DropTableResult(table) // IF EXISTS allows missing table
        } else {
          if !cascade then
            val refs = db.childForeignKeys(table)
            if refs.nonEmpty then
              val refTableNames = refs.map(_._1.name).distinct.mkString(", ")
              throw ConstraintException(id.pos, s"cannot drop table '$table' because it is referenced by: $refTableNames")
          db.dropTable(table)
          DropTableResult(table)
        }
      case CreateEnumCommand(id @ Ident(name), labels) =>
        guardDDL()
        if db hasType name then throw SchemaException(id.pos, s"duplicate type '$name'")

        db.createEnum(name, labels)
        CreateTypeResult(name)
      case UpdateCommand(id @ Ident(table), sets, from, cond, returning) =>
        val t             = db.getTable(table).getOrElse(throw UndefinedReferenceException(id.pos, s"unknown table: $table"))
        val (cols, exprs) =
          sets map { case UpdateSet(id @ Ident(col), value) =>
            if !t.hasColumn(col) then throw UndefinedReferenceException(id.pos, s"table $table doesn't has column '$col'")

            col -> rewrite(value)
          } unzip
        var count = 0
        val returnedRows = mutable.ArrayBuffer[Row]()

        val pkCols = t.primaryKey.map(_.columns.toSet).getOrElse(Set.empty)
        val updatedColSet = cols.toSet
        val childFKs = db.foreignKeys(t).filter(fk => fk.columns.exists(updatedColSet.contains))

        val checkConstraints = t.constraints.collect { case c: CheckSpec => c }

        val rwReturning = returning.map(_.map(rewrite))

        def applyUpdate(targetRow: Row, evalRow: Row): Unit =
          targetRow.updater match
            case None    => throw ExecutionException(id.pos, "not updatable")
            case Some(u) =>
              val updates = cols zip (exprs map (e => eval(e, Seq(evalRow))))
              for (col, value) <- updates do
                if pkCols.contains(col) && value.isNull then
                  sys.error(s"null value in column \"$col\" violates not-null constraint")
              // Enforce CHECK constraints on the updated row
              if checkConstraints.nonEmpty then
                val newRowData = targetRow.data.toArray
                for (col, value) <- updates do
                  newRowData(t.columnMap(col)) = value
                val checkRow = Row(newRowData.toIndexedSeq, t.meta, None, None)
                for c <- checkConstraints do
                  if !beval(c.parsedExpr, Seq(checkRow)) then
                    sys.error(s"new row violates check constraint${c.name.map(n => s""" "$n"""").getOrElse("")}")
              db.enforceChildConstraints(table, targetRow, "update", Some(updatedColSet), Some(updates))
              if childFKs.nonEmpty then
                val newRowData = targetRow.data.toArray
                for (col, value) <- updates do
                  newRowData(t.columnMap(col)) = value
                for fk <- childFKs do
                  db.checkParentExists(table, fk, newRowData.toIndexedSeq, t.columnMap)
              u(updates)
              // Evaluate RETURNING against the new row state
              rwReturning.foreach { retExprs =>
                val newRowData = targetRow.data.toArray
                for (col, value) <- updates do
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
      case DeleteCommand(id @ Ident(table), cond, returning) =>
        val t    = db.getTable(table).getOrElse(throw UndefinedReferenceException(id.pos, s"unknown table: $table"))
        val rows =
          cond match
            case Some(value) => SeqScanProcess(t, rewrite(value))
            case None        => t
        var count = 0
        val returnedRows = mutable.ArrayBuffer[Row]()
        val rwReturning = returning.map(_.map(rewrite))

        for (r <- rows.iterator(Nil))
          db.enforceChildConstraints(table, r, "delete")
          // Evaluate RETURNING before deleting
          rwReturning.foreach { retExprs =>
            val projected = retExprs.map {
              case StarExpr() => r.data
              case e          => IndexedSeq(eval(e, Seq(r)))
            }.flatten.toIndexedSeq
            returnedRows += Row(projected, Metadata(Vector.empty), None, None)
          }
          r.deleter match
            case Some(d) => d()
            case None    => throw ExecutionException(id.pos, "not updatable")

          count += 1

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
        val t = db.getTable(table).getOrElse(throw UndefinedReferenceException(id.pos, s"unknown table: $table"))
        // Enforce FK constraints: fail if any child table has rows referencing this table
        val childFKs = db.childForeignKeys(table)
        for (childTable, fk) <- childFKs do
          for row <- childTable.iterator(Nil) do
            val childValues = fk.columns.map(c => row.data(childTable.columnMap(c)))
            if !childValues.forall(_.isNull) then
              sys.error(s"cannot truncate table '$table': rows in '${childTable.name}' reference it")
        t.truncate()
        TruncateResult(table)
      case CreateIndexCommand(id @ Ident(indexName), tid @ Ident(tableName), columns, unique) =>
        guardDDL()
        if !db.hasTable(tableName) then throw UndefinedReferenceException(tid.pos, s"unknown table: $tableName")
        if db.hasIndex(indexName) then throw SchemaException(id.pos, s"index '$indexName' already exists")
        val t = db.getTable(tableName).get
        for col @ Ident(colName) <- columns do
          if !t.hasColumn(colName) then throw UndefinedReferenceException(col.pos, s"column '$colName' not found in table '$tableName'")
        db.createIndex(indexName, tableName, columns.map(_.name), unique)
        CreateIndexResult(indexName)
      case DropIndexCommand(id @ Ident(name), ifExists) =>
        guardDDL()
        if !db.hasIndex(name) then
          if !ifExists then throw UndefinedReferenceException(id.pos, s"index '$name' not found")
          DropIndexResult(name)
        else
          db.dropIndex(name)
          DropIndexResult(name)
      case DropTypeCommand(id @ Ident(name), ifExists, cascade) =>
        guardDDL()
        if (!db.hasType(name)) {
          if (!ifExists) throw UndefinedReferenceException(id.pos, s"unknown type: $name")
          else DropTypeResult(name)
        } else {
          db.dropType(name)
          DropTypeResult(name)
        }
      case AlterTableCommand(id @ Ident(table), alter) =>
        guardDDL()
        if !db.hasTable(table) then throw UndefinedReferenceException(id.pos, s"unknown table: $table")
        db.alterTable(table, alter)
        AlterTableResult()
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
    case p: UnionProcess       => s"${prefix}Union${if p.all then " All" else ""}\n${formatProcess(p.input1, indent + 1)}\n${formatProcess(p.input2, indent + 1)}"
    case p: GenerateSeriesProcess => s"${prefix}Generate Series"
    case t: Table              => s"${prefix}Seq Scan on ${t.name}"
    case SingleProcess         => s"${prefix}Result"
    case _                     => s"${prefix}${proc.getClass.getSimpleName}"

// ── Deep Copy Utilities ──────────────────────────────────────────
// AST nodes have a mutable `var typ` that gets set during rewrite().
// To re-execute cached prepared statements we deep-copy the AST to
// get fresh instances with typ = null.

private[petradb] def deepCopyExpr(expr: Expr, params: IndexedSeq[Value] = IndexedSeq.empty): Expr =
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
    case ApplyExpr(func, args)             => ApplyExpr(func, args.map(deepCopyExpr(_, params)))
    case InSeqExpr(v, op, es)              => InSeqExpr(deepCopyExpr(v, params), op, es.map(deepCopyExpr(_, params)))
    case InQueryExpr(v, op, q)             => InQueryExpr(deepCopyExpr(v, params), op, deepCopyExpr(q, params))
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
      CompoundQueryExpr(deepCopyExpr(q, params), ob.map(_.map(deepCopyOrderBy(_, params))), off, lim)
    case SQLSelectExpr(exprs, from, where, groupBy, having, orderBy, offset, limit, distinct) =>
      SQLSelectExpr(
        exprs.map(deepCopyExpr(_, params)).to(ArraySeq),
        from.map(_.map(deepCopyExpr(_, params))),
        where.map(deepCopyExpr(_, params)),
        groupBy.map(_.map(deepCopyExpr(_, params))),
        having.map(deepCopyExpr(_, params)),
        orderBy.map(_.map(deepCopyOrderBy(_, params))),
        offset,
        limit,
        distinct,
      )
    case ColumnAliasOperator(r, a, cs) => ColumnAliasOperator(deepCopyExpr(r, params), a, cs)
    case other => other // ProcessOperator, etc. — should not appear in parsed AST
  if expr.pos != null then copied.setPos(expr.pos)
  copied

private def deepCopyOrderBy(ob: OrderBy, params: IndexedSeq[Value] = IndexedSeq.empty): OrderBy =
  OrderBy(deepCopyExpr(ob.f, params), ob.asc, ob.nullsFirst)

private[petradb] def deepCopyCommand(cmd: Command, params: IndexedSeq[Value] = IndexedSeq.empty): Command =
  cmd match
    case QueryCommand(query) =>
      QueryCommand(deepCopyExpr(query, params))
    case InsertCommand(table, columns, rows, returning, onConflict) =>
      InsertCommand(table, columns, rows.map(_.map(deepCopyExpr(_, params))), returning,
        onConflict.map {
          case OnConflictDoNothing => OnConflictDoNothing
          case OnConflictDoUpdate(cols, updates) =>
            OnConflictDoUpdate(cols, updates.map(s => UpdateSet(s.col, deepCopyExpr(s.value, params))))
        })
    case InsertSelectCommand(table, columns, query, returning, onConflict) =>
      InsertSelectCommand(table, columns, deepCopyExpr(query, params), returning,
        onConflict.map {
          case OnConflictDoNothing => OnConflictDoNothing
          case OnConflictDoUpdate(cols, updates) =>
            OnConflictDoUpdate(cols, updates.map(s => UpdateSet(s.col, deepCopyExpr(s.value, params))))
        })
    case UpdateCommand(table, sets, from, cond, returning) =>
      UpdateCommand(table, sets.map(s => UpdateSet(s.col, deepCopyExpr(s.value, params))),
        from.map(_.map(deepCopyExpr(_, params))), cond.map(deepCopyExpr(_, params)),
        returning.map(_.map(deepCopyExpr(_, params))))
    case DeleteCommand(table, cond, returning) =>
      DeleteCommand(table, cond.map(deepCopyExpr(_, params)), returning.map(_.map(deepCopyExpr(_, params))))
    case ExplainCommand(inner) =>
      ExplainCommand(deepCopyCommand(inner, params))
    case PrepareCommand(name, cmds) =>
      PrepareCommand(name, cmds.map(deepCopyCommand(_, params)))
    case ExecuteCommand(name, execParams) =>
      ExecuteCommand(name, execParams.map(deepCopyExpr(_, params)))
    case other => other // DDL commands, BEGIN/COMMIT/ROLLBACK — no mutable Expr state

private[petradb] def deepCopyCommands(cmds: Seq[Command], params: IndexedSeq[Value] = IndexedSeq.empty): Seq[Command] = cmds.map(deepCopyCommand(_, params))

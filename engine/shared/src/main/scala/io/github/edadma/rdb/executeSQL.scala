package io.github.edadma.rdb

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

private[rdb] def executeCommands(cs: Seq[Command])(using session: Session): Seq[Result] =

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
        problem(id, s"prepared statement '$name' already exists")
      session.preparedStatements(name) = PreparedStatement(name, cmds)
      PrepareResult(name)
    case ExecuteCommand(id @ Ident(name), paramExprs) =>
      val ps = session.preparedStatements.getOrElse(name, problem(id, s"prepared statement '$name' not found"))
      val paramValues = paramExprs.map(e => eval(rewrite(e), Nil)).toIndexedSeq
      val copied = deepCopyCommands(ps.commands, paramValues)
      executeCommands(copied).last
    case DeallocateCommand(id @ Ident(name)) =>
      if !session.preparedStatements.contains(name) then
        problem(id, s"prepared statement '$name' not found")
      session.preparedStatements.remove(name)
      DeallocateResult(name)
    case cmd             => guardTransaction { cmd match
      case InsertCommand(id @ Ident(table), columns, rows, returning) =>
        val t = db.getTable(table).getOrElse(problem(id, s"unknown table: $table"))
        val resolvedColumns = columns.getOrElse(t.columns.map(c => Ident(c.name)).toSeq)
        val cols = resolvedColumns.length

        rows find (_.length != cols) match
          case Some(row) => problem(row.head, s"row length (${row.length}) not equal to number of columns ($cols)")
          case None      =>
            val data =
              for (r <- rows)
                yield r map (e => eval(rewrite(e), Nil))

            for (id @ Ident(c) <- resolvedColumns)
              if !t.hasColumn(c) then problem(id, s"unknown column: $c")

            val fks = db.foreignKeys(t)
            val fkCheck: Option[IndexedSeq[Value] => Unit] =
              if fks.isEmpty then None
              else Some { (row: IndexedSeq[Value]) =>
                for fk <- fks do db.checkParentExists(table, fk, row, t.columnMap)
              }

            val result = t.bulkInsert(resolvedColumns map (_.name), data, returning, fkCheck)

            val (row, metadata) =
              returning match
                case None =>
                  val (cols, seq) = result map { case (k, v) => (ColumnMetadata(Some(table), k, v.vtyp), v) } unzip
                  val metadata    = Metadata(cols.toIndexedSeq)

                  (Row(seq.toIndexedSeq, metadata, None, None), metadata)
                case Some(ret @ Ident(returning)) =>
                  if result contains returning then
                    val (cols, seq) = result filter { case (k, _) => k == returning } map { case (k, v) =>
                      (ColumnMetadata(Some(table), k, v.vtyp), v)
                    } unzip
                    val metadata = Metadata(cols.toIndexedSeq)

                    (Row(seq.toIndexedSeq, metadata, None, None), metadata)
                  else problem(ret, s"'$returning' not found in result from insert")

            InsertResult(result, TableValue(Vector(row), metadata))
      case InsertSelectCommand(id @ Ident(table), columns, selectQuery, returning) =>
        val t = db.getTable(table).getOrElse(problem(id, s"unknown table: $table"))
        val queryResult = eval(rewrite(selectQuery), Nil).asInstanceOf[TableValue]
        val resolvedColumns = columns.getOrElse(t.columns.map(c => Ident(c.name)).toSeq)
        val cols = resolvedColumns.length

        for (id @ Ident(c) <- resolvedColumns)
          if !t.hasColumn(c) then problem(id, s"unknown column: $c")

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

        val result = t.bulkInsert(resolvedColumns map (_.name), data, returning, fkCheck)

        val (row, metadata) =
          returning match
            case None =>
              val (cols, seq) = result map { case (k, v) => (ColumnMetadata(Some(table), k, v.vtyp), v) } unzip
              val metadata = Metadata(cols.toIndexedSeq)
              (Row(seq.toIndexedSeq, metadata, None, None), metadata)
            case Some(ret @ Ident(returning)) =>
              if result contains returning then
                val (cols, seq) = result filter { case (k, _) => k == returning } map { case (k, v) =>
                  (ColumnMetadata(Some(table), k, v.vtyp), v)
                } unzip
                val metadata = Metadata(cols.toIndexedSeq)
                (Row(seq.toIndexedSeq, metadata, None, None), metadata)
              else problem(ret, s"'$returning' not found in result from insert")

        InsertResult(result, TableValue(Vector(row), metadata))
      case QueryCommand(query)                                         => executeSelect(query)
      case CreateTableCommand(id @ Ident(table), columns, constraints, ifNotExists) =>
        guardDDL()
        if (db hasTable table) && ifNotExists then CreateTableResult(table)
        else
          if db hasTable table then problem(id, s"duplicate table: $table")

          val names = new mutable.HashSet[String]

          val columnSpecs = columns map { case ColumnDesc(id @ Ident(name), typeDesc, required, unique, default, references, _, pk) =>
            if names contains name then problem(id, s"duplicate column name: $name")

            names += name

            val typ =
              typeDesc match
                case Left(primitive)             => primitive
                case Right(tid @ Ident(defined)) =>
                  db getType defined match
                    case None    => problem(tid, s"type '$defined' is undefined")
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
            problem(id, s"cannot specify both column-level and table-level PRIMARY KEY")
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
                  problem(id, s"referenced table '$refTableName' does not exist"))
                if !refTable.hasColumn(refColName) then
                  problem(id, s"referenced column '$refColName' not found in table '$refTableName'")
              case _ =>
          for spec <- constraintSpecs do
            spec match
              case fk: ForeignKeySpec =>
                val refTable = db.getTable(fk.referencedTable).getOrElse(
                  problem(id, s"referenced table '${fk.referencedTable}' does not exist"))
                for col <- fk.referencedColumns do
                  if !refTable.hasColumn(col) then
                    problem(id, s"referenced column '$col' not found in table '${fk.referencedTable}'")
              case _ =>

          val allSpecs = columnSpecs ++ constraintSpecs ++ columnPKSpecs ++ columnCheckSpecs
          db.createTable(table, allSpecs)
          CreateTableResult(table)
      case DropTableCommand(id @ Ident(table), ifExists, cascade) =>
        guardDDL()
        if (!db.hasTable(table)) {
          if (!ifExists) problem(id, s"unknown table: $table")
          else DropTableResult(table) // IF EXISTS allows missing table
        } else {
          if !cascade then
            val refs = db.childForeignKeys(table)
            if refs.nonEmpty then
              val refTableNames = refs.map(_._1.name).distinct.mkString(", ")
              problem(id, s"cannot drop table '$table' because it is referenced by: $refTableNames")
          db.dropTable(table)
          DropTableResult(table)
        }
      case CreateEnumCommand(id @ Ident(name), labels) =>
        guardDDL()
        if db hasType name then problem(id, s"duplicate type '$name'")

        db.createEnum(name, labels)
        CreateTypeResult(name)
      case UpdateCommand(id @ Ident(table), sets, from, cond) =>
        val t             = db.getTable(table) getOrElse problem(id, s"unknown table: $table")
        val (cols, exprs) =
          sets map { case UpdateSet(id @ Ident(col), value) =>
            if !t.hasColumn(col) then problem(id, s"table $table doesn't has column '$col'")

            col -> rewrite(value)
          } unzip
        var count = 0

        val pkCols = t.primaryKey.map(_.columns.toSet).getOrElse(Set.empty)
        val updatedColSet = cols.toSet
        val childFKs = db.foreignKeys(t).filter(fk => fk.columns.exists(updatedColSet.contains))

        val checkConstraints = t.constraints.collect { case c: CheckSpec => c }

        def applyUpdate(targetRow: Row, evalRow: Row): Unit =
          targetRow.updater match
            case None    => problem(id, "not updatable")
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

        UpdateResult(count)
      case DeleteCommand(id @ Ident(table), cond) =>
        val t    = db.getTable(table) getOrElse problem(id, s"unknown table: $table")
        val rows =
          cond match
            case Some(value) => SeqScanProcess(t, rewrite(value))
            case None        => t
        var count = 0

        for (r <- rows.iterator(Nil))
          db.enforceChildConstraints(table, r, "delete")
          r.deleter match
            case Some(d) => d()
            case None    => problem(id, "not updatable")

          count += 1

        DeleteResult(count)
      case TruncateCommand(id @ Ident(table)) =>
        val t = db.getTable(table) getOrElse problem(id, s"unknown table: $table")
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
        if !db.hasTable(tableName) then problem(tid, s"unknown table: $tableName")
        if db.hasIndex(indexName) then problem(id, s"index '$indexName' already exists")
        val t = db.getTable(tableName).get
        for col @ Ident(colName) <- columns do
          if !t.hasColumn(colName) then problem(col, s"column '$colName' not found in table '$tableName'")
        db.createIndex(indexName, tableName, columns.map(_.name), unique)
        CreateIndexResult(indexName)
      case DropIndexCommand(id @ Ident(name), ifExists) =>
        guardDDL()
        if !db.hasIndex(name) then
          if !ifExists then problem(id, s"index '$name' not found")
          DropIndexResult(name)
        else
          db.dropIndex(name)
          DropIndexResult(name)
      case DropTypeCommand(id @ Ident(name), ifExists, cascade) =>
        guardDDL()
        if (!db.hasType(name)) {
          if (!ifExists) problem(id, s"unknown type: $name")
          else DropTypeResult(name)
        } else {
          db.dropType(name)
          DropTypeResult(name)
        }
      case AlterTableCommand(id @ Ident(table), alter) =>
        guardDDL()
        val t = db.getTable(table) getOrElse problem(id, s"unknown table: $table")
        alter match
          case AddColumnTableAlteration(ColumnDesc(cid @ Ident(colName), typeDesc, required, unique, default, references, _, _)) =>
            if t.hasColumn(colName) then problem(cid, s"column '$colName' already exists")
            val typ = typeDesc match
              case Left(primitive) => primitive
              case Right(tid @ Ident(defined)) => db.getType(defined).getOrElse(problem(tid, s"type '$defined' is undefined"))
            val fk = references.map { case (tbl, col, onDel, onUpd) => (tbl.name, col.name, onDel, onUpd) }
            val defaultValue = default.map(expr => eval(rewrite(expr), Nil)).getOrElse(NullValue())
            val spec = ColumnSpec(colName, typ, required, false, unique, fk, default.map(expr => eval(rewrite(expr), Nil)))
            t.addColumnToTable(spec, defaultValue)
          case DropColumnTableAlteration(cid @ Ident(colName)) =>
            if !t.hasColumn(colName) then problem(cid, s"column '$colName' not found")
            t.dropColumnFromTable(colName)
          case AlterColumnTableAlteration(cid @ Ident(colName), mod) =>
            if !t.hasColumn(colName) then problem(cid, s"column '$colName' not found")
            mod match
              case SetDataTypeColumnModification(typeDesc) =>
                val typ = typeDesc match
                  case Left(primitive) => primitive
                  case Right(tid @ Ident(defined)) => db.getType(defined).getOrElse(problem(tid, s"type '$defined' is undefined"))
                t.alterColumnType(colName, typ)
              case SetDefaultColumnModification(expr) =>
                t.alterColumnSetDefault(colName, eval(rewrite(expr), Nil))
              case DropDefaultColumnModification() =>
                t.alterColumnDropDefault(colName)
              case SetNotNullColumnModification() =>
                t.alterColumnSetNotNull(colName)
              case DropNotNullColumnModification() =>
                t.alterColumnDropNotNull(colName)
          case AddConstraintTableAlteration(constraint) =>
            val spec = constraint match
              case UniqueConstraint(cname, cols) => UniqueSpec(cols.map(_.name), cname)
              case PrimaryKeyConstraint(cname, cols) => PrimaryKeySpec(cols.map(_.name), cname)
              case ForeignKeyConstraint(cname, cols, refTable, refCols, onDel, onUpd) =>
                ForeignKeySpec(cols.map(_.name), refTable.name, refCols.map(_.name), cname, onDel, onUpd)
              case CheckConstraint(cname, expr) =>
                CheckSpec(exprToSQL(expr), expr, cname)
            t.addConstraintToTable(spec)
          case DropConstraintTableAlteration(cid @ Ident(constraintName)) =>
            t.dropConstraintFromTable(constraintName)
          case RenameTableAlteration(Ident(newName)) =>
            db.renameTable(table, newName)
          case RenameColumnTableAlteration(cid @ Ident(oldName), Ident(newName)) =>
            if !t.hasColumn(oldName) then problem(cid, s"column '$oldName' not found")
            t.renameColumnInTable(oldName, newName)
          case AddForeignKeyTableAlteration(fk, ref) =>
            val spec = ForeignKeySpec(Seq(fk.name), ref.name, Seq(fk.name), None)
            t.addConstraintToTable(spec)
          case AddForeignKeyConstraintTableAlteration(constraint) =>
            val spec = ForeignKeySpec(constraint.columns.map(_.name), constraint.referencedTable.name, constraint.referencedColumns.map(_.name), constraint.name, constraint.onDelete, constraint.onUpdate)
            t.addConstraintToTable(spec)
        AlterTableResult()
      case _ => sys.error(s"unexpected command")
    }
  }

// ── Deep Copy Utilities ──────────────────────────────────────────
// AST nodes have a mutable `var typ` that gets set during rewrite().
// To re-execute cached prepared statements we deep-copy the AST to
// get fresh instances with typ = null.

private[rdb] def deepCopyExpr(expr: Expr, params: IndexedSeq[Value] = IndexedSeq.empty): Expr =
  val copied: Expr = expr match
    case p @ ParameterExpr(index) =>
      if params.nonEmpty then
        if index < 1 || index > params.length then
          problem(p, s"parameter $$$index is not bound (have ${params.length} parameters)")
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

private[rdb] def deepCopyCommand(cmd: Command, params: IndexedSeq[Value] = IndexedSeq.empty): Command =
  cmd match
    case QueryCommand(query) =>
      QueryCommand(deepCopyExpr(query, params))
    case InsertCommand(table, columns, rows, returning) =>
      InsertCommand(table, columns, rows.map(_.map(deepCopyExpr(_, params))), returning)
    case InsertSelectCommand(table, columns, query, returning) =>
      InsertSelectCommand(table, columns, deepCopyExpr(query, params), returning)
    case UpdateCommand(table, sets, from, cond) =>
      UpdateCommand(table, sets.map(s => UpdateSet(s.col, deepCopyExpr(s.value, params))),
        from.map(_.map(deepCopyExpr(_, params))), cond.map(deepCopyExpr(_, params)))
    case DeleteCommand(table, cond) =>
      DeleteCommand(table, cond.map(deepCopyExpr(_, params)))
    case PrepareCommand(name, cmds) =>
      PrepareCommand(name, cmds.map(deepCopyCommand(_, params)))
    case ExecuteCommand(name, execParams) =>
      ExecuteCommand(name, execParams.map(deepCopyExpr(_, params)))
    case other => other // DDL commands, BEGIN/COMMIT/ROLLBACK — no mutable Expr state

private[rdb] def deepCopyCommands(cmds: Seq[Command], params: IndexedSeq[Value] = IndexedSeq.empty): Seq[Command] = cmds.map(deepCopyCommand(_, params))

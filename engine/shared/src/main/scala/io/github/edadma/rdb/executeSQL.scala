package io.github.edadma.rdb

//import pprint.pprintln

import scala.collection.mutable
import scala.language.postfixOps

def executeQuery(query: String)(using db: DB): QueryResult = executeSelect(SQLParser.parseQuery(query))

def executeSelect(query: Expr)(using db: DB) =
  QueryResult(eval(rewrite(query), Nil).asInstanceOf[TableValue])

def executeSQL(sql: String)(using db: DB): Seq[Result] =
  val cs = SQLParser.parseCommands(sql)

  // pprintln(com)

  def guardTransaction[T](fn: => T): T =
    if db.isTransactionAborted then sys.error("current transaction is aborted, use ROLLBACK")
    if db.inTransaction then
      try fn
      catch
        case e: Throwable =>
          db.markTransactionAborted()
          throw e
    else fn

  cs map {
    case BeginCommand    => db.beginTransaction(); BeginResult
    case CommitCommand   => db.commitTransaction(); CommitResult
    case RollbackCommand => db.rollbackTransaction(); RollbackResult
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
      case QueryCommand(query)                                         => executeSelect(query)
      case CreateTableCommand(id @ Ident(table), columns, constraints) =>
        if db hasTable table then problem(id, s"duplicate table: $table")

        val names = new mutable.HashSet[String]

        val columnSpecs = columns map { case ColumnDesc(id @ Ident(name), typeDesc, required, unique, default, references) =>
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
            required,
            false, // indexed
            unique,
            fkTuple,
            default.map(expr => eval(rewrite(expr), Nil)),
          )
        }

        val constraintSpecs = constraints map {
          case PrimaryKeyConstraint(name, cols) =>
            PrimaryKeySpec(cols.map(_.name), name)
          case UniqueConstraint(name, cols) =>
            UniqueSpec(cols.map(_.name), name)
          case ForeignKeyConstraint(name, cols, refTable, refCols, onDel, onUpd) =>
            ForeignKeySpec(cols.map(_.name), refTable.name, refCols.map(_.name), name, onDel, onUpd)
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

        val allSpecs = columnSpecs ++ constraintSpecs
        db.createTable(table, allSpecs)
        CreateTableResult(table)
      case DropTableCommand(id @ Ident(table), ifExists, cascade) =>
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
        if db hasType name then problem(id, s"duplicate type '$name'")

        db.createEnum(name, labels)
        CreateTypeResult(name)
      case UpdateCommand(id @ Ident(table), sets, cond) =>
        val t             = db.getTable(table) getOrElse problem(id, s"unknown table: $table")
        val (cols, exprs) =
          sets map { case UpdateSet(id @ Ident(col), value) =>
            if !t.hasColumn(col) then problem(id, s"table $table doesn't has column '$col'")

            col -> rewrite(value)
          } unzip
        val rows =
          cond match
            case Some(value) => SeqScanProcess(t, rewrite(value))
            case None        => t
        var count = 0

        val pkCols = t.primaryKey.map(_.columns.toSet).getOrElse(Set.empty)
        val updatedColSet = cols.toSet
        val childFKs = db.foreignKeys(t).filter(fk => fk.columns.exists(updatedColSet.contains))

        for (r <- rows.iterator(Nil))
          r.updater match
            case None    => problem(id, "not updatable")
            case Some(u) =>
              val updates = cols zip (exprs map (e => eval(e, Seq(r))))
              // Enforce NOT NULL for PRIMARY KEY columns
              for (col, value) <- updates do
                if pkCols.contains(col) && value.isNull then
                  sys.error(s"null value in column \"$col\" violates not-null constraint")
              // Parent-side FK: enforce child constraints on old values
              db.enforceChildConstraints(table, r, "update", Some(updatedColSet), Some(updates))
              // Child-side FK: check new values reference existing parents
              if childFKs.nonEmpty then
                val newRowData = r.data.toArray
                for (col, value) <- updates do
                  newRowData(t.columnMap(col)) = value
                for fk <- childFKs do
                  db.checkParentExists(table, fk, newRowData.toIndexedSeq, t.columnMap)
              u(updates)
          count += 1

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
      case CreateIndexCommand(id @ Ident(indexName), tid @ Ident(tableName), columns, unique) =>
        if !db.hasTable(tableName) then problem(tid, s"unknown table: $tableName")
        if db.hasIndex(indexName) then problem(id, s"index '$indexName' already exists")
        val t = db.getTable(tableName).get
        for col @ Ident(colName) <- columns do
          if !t.hasColumn(colName) then problem(col, s"column '$colName' not found in table '$tableName'")
        db.createIndex(indexName, tableName, columns.map(_.name), unique)
        CreateIndexResult(indexName)
      case DropIndexCommand(id @ Ident(name), ifExists) =>
        if !db.hasIndex(name) then
          if !ifExists then problem(id, s"index '$name' not found")
          DropIndexResult(name)
        else
          db.dropIndex(name)
          DropIndexResult(name)
      case DropTypeCommand(id @ Ident(name), ifExists, cascade) =>
        if (!db.hasType(name)) {
          if (!ifExists) problem(id, s"unknown type: $name")
          else DropTypeResult(name)
        } else {
          db.dropType(name)
          DropTypeResult(name)
        }
      case AlterTableCommand(id @ Ident(table), alter) =>
        val t = db.getTable(table) getOrElse problem(id, s"unknown table: $table")
        alter match
          case AddColumnTableAlteration(ColumnDesc(cid @ Ident(colName), typeDesc, required, unique, default, references)) =>
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

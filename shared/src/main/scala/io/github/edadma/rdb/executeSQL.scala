package io.github.edadma.rdb

//import pprint.pprintln

import scala.collection.mutable
import scala.language.postfixOps

def executeQuery(query: String)(using db: DB): QueryResult = executeSelect(SQLParser.parseQuery(query))

def executeSelect(query: SQLSelectExpr)(using db: DB) =
  QueryResult(eval(rewrite(query), Nil).asInstanceOf[TableValue])

def executeSQL(sql: String)(using db: DB): Seq[Result] =
  val cs = SQLParser.parseCommands(sql)

  // pprintln(com)

  cs map {
    case InsertCommand(id @ Ident(table), columns, rows, returning) =>
      val t    = db.getTable(table).getOrElse(problem(id, s"unknown table: $table"))
      val cols = columns.length

      rows find (_.length != cols) match
        case Some(row) => problem(row.head, s"row length (${row.length}) not equal to number of columns ($cols)")
        case None      =>
          val data =
            for (r <- rows)
              yield r map (e => eval(rewrite(e), Nil))

          for (id @ Ident(c) <- columns)
            if !t.hasColumn(c) then problem(id, s"unknown column: $c")

          val result = t.bulkInsert(columns map (_.name), data, returning)

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
        val fkTuple = references.map { case (table, col) => (table.name, col.name) }

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
        case ForeignKeyConstraint(name, cols, refTable, refCols) =>
          ForeignKeySpec(cols.map(_.name), refTable.name, refCols.map(_.name), name)
      }

      val allSpecs = columnSpecs ++ constraintSpecs
      db.createTable(table, allSpecs)
      CreateTableResult(table)
    case DropTableCommand(id @ Ident(table), ifExists, cascade) =>
      if (!db.hasTable(table)) {
        if (!ifExists) problem(id, s"unknown table: $table")
        else DropTableResult(table) // IF EXISTS allows missing table
      } else {
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
          case Some(value) => FilterProcess(t, rewrite(value))
          case None        => t
      var count = 0

      for (r <- rows.iterator(Nil))
        r.updater match
          case None    => problem(id, "not updatable")
          case Some(u) => u(cols zip (exprs map (e => eval(e, Seq(r)))))
        count += 1

      UpdateResult(count)
    case DeleteCommand(id @ Ident(table), cond) =>
      val t    = db.getTable(table) getOrElse problem(id, s"unknown table: $table")
      val rows =
        cond match
          case Some(value) => FilterProcess(t, rewrite(value))
          case None        => t
      var count = 0

      for (r <- rows.iterator(Nil))
        r.deleter match
          case Some(d) => d()
          case None    => problem(id, "not updatable")

        count += 1

      DeleteResult(count)
    case DropIndexCommand(id @ Ident(name), ifExists) =>
      // Index operations not implemented yet
      if (!ifExists) problem(id, s"indexes not implemented yet")
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
        case AddColumnTableAlteration(column) =>
          // TODO: Implement adding columns
          problem(id, "ALTER TABLE ADD COLUMN not implemented yet")
        case DropColumnTableAlteration(col) =>
          // TODO: Implement dropping columns
          problem(id, "ALTER TABLE DROP COLUMN not implemented yet")
        case AlterColumnTableAlteration(col, modification) =>
          // TODO: Implement altering columns
          problem(id, "ALTER TABLE ALTER COLUMN not implemented yet")
        case AddConstraintTableAlteration(constraint) =>
          // TODO: Implement adding constraints
          problem(id, "ALTER TABLE ADD CONSTRAINT not implemented yet")
        case DropConstraintTableAlteration(name) =>
          // TODO: Implement dropping constraints
          problem(id, "ALTER TABLE DROP CONSTRAINT not implemented yet")
        case RenameTableAlteration(newName) =>
          // TODO: Implement table renaming
          problem(id, "ALTER TABLE RENAME TO not implemented yet")
        case RenameColumnTableAlteration(oldName, newName) =>
          // TODO: Implement column renaming
          problem(id, "ALTER TABLE RENAME COLUMN not implemented yet")
        case AddForeignKeyTableAlteration(fk, ref) =>
          // Legacy support - TODO: implement
          problem(id, "ALTER TABLE ADD FOREIGN KEY not implemented yet")
        case AddForeignKeyConstraintTableAlteration(constraint) =>
          // Legacy support - TODO: implement
          problem(id, "ALTER TABLE ADD CONSTRAINT (FK) not implemented yet")
      AlterTableResult()
  }

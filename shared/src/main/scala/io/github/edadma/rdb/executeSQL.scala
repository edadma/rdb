package io.github.edadma.rdb

//import pprint.pprintln

import scala.collection.mutable

def executeSQL(sql: String)(implicit db: DB): Seq[Result] =
  val cs = SQLParser.parseCommands(sql)

  // pprintln(com)

  cs map {
    case InsertCommand(id @ Ident(table), columns, rows, returning) =>
      val t = db.getTable(table).getOrElse(problem(id, s"unknown table: $table"))
      val cols = columns.length

      rows find (_.length != cols) match
        case Some(row) => problem(row.head, s"row length (${row.length}) not equal to number of columns ($cols)")
        case None =>
          val data =
            for (r <- rows)
              yield r map (e => eval(rewrite(e), Nil, AggregateMode.Disallow))

          for (id @ Ident(c) <- columns)
            if !t.hasColumn(c) then problem(id, s"unknown column: $c")

          val result = t.bulkInsert(columns map (_.name), data)

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
    case QueryCommand(query) =>
      QueryResult(eval(rewrite(query)(db), Nil, AggregateMode.Return).asInstanceOf[TableValue])
    case CreateTableCommand(id @ Ident(table), columns) =>
      if db hasTable table then problem(id, s"duplicate table: $table")

      val specs =
        val names = new mutable.HashSet[String]

        columns map { case ColumnDesc(id @ Ident(name), typeDesc, auto, required, pk) =>
          if names contains name then problem(id, s"duplicate column name: $name")

          names += name

          val typ =
            typeDesc match
              case Left(primitive) => primitive
              case Right(tid @ Ident(defined)) =>
                db getType defined match
                  case None    => problem(tid, s"type '$defined' is undefined")
                  case Some(t) => t

          ColumnSpec(name, typ, auto, required, pk)
        }

      db.createTable(table, specs)
      CreateTableResult(table)
    case CreateEnumCommand(id @ Ident(name), labels) =>
      if db hasType name then problem(id, s"duplicate type '$name'")

      db.createEnum(name, labels)
      CreateTypeResult(name)
    case UpdateCommand(id @ Ident(table), sets, cond) =>
      val t = db.getTable(table) getOrElse problem(id, s"unknown table: $table")
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
          case Some(u) => u(cols zip (exprs map (e => eval(e, Seq(r), AggregateMode.Disallow))))
        count += 1

      UpdateResult(count)
    case DeleteCommand(id @ Ident(table), cond) =>
      val t = db.getTable(table) getOrElse problem(id, s"unknown table: $table")
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
    case AlterTableCommand(table, alter) => AlterTableResult()
  }

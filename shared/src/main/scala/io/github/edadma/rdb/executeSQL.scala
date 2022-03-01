package io.github.edadma.rdb

//import pprint.pprintln

import scala.collection.mutable

def executeSQL(sql: String)(implicit db: DB): Result =
  val com = SQLParser.parseCommand(sql)

  // pprintln(com)

  com match
    case InsertCommand(id @ Ident(table), columns, rows) =>
      val t = db.table(table).getOrElse(problem(id, s"unknown table: $table"))
      val cols = columns.length

      rows find (_.length != cols) match
        case Some(row) => problem(row.head, s"row length (${row.length}) not equal to number of columns ($cols)")
        case None =>
          val data =
            for (r <- rows)
              yield r map (e => eval(rewrite(e), Nil, AggregateMode.Disallow))

          for (id @ Ident(c) <- columns)
            if !t.hasColumn(c) then problem(id, s"unknown column: $c")

          InsertResult(t.bulkInsert(columns map (_.name), data))
    case QueryCommand(query) =>
      QueryResult(eval(rewrite(query)(db), Nil, AggregateMode.Return).asInstanceOf[TableValue])
    case CreateTableCommand(id @ Ident(table), columns) =>
      if db hasTable table then problem(id, s"duplicate table: $table")

      val specs =
        val names = new mutable.HashSet[String]

        columns map { case ColumnDesc(id @ Ident(name), typ, pk, required) =>
          if names contains name then problem(id, s"duplicate column name: $name")

          names += name

          val t =
            typ match
              case "INT" | "INTEGER" => IntegerType
              case "DOUBLE"          => DoubleType
              case "TEXT"            => TextType
              case "JSON"            => JSONType
              case "BOOLEAN"         => BooleanType
              case "TIMESTAMP"       => TimestampType

          ColumnSpec(name, t, pk = pk, required = required)
        }

      db.create(table, specs)
      CreateTableResult(table)
    case UpdateCommand(id @ Ident(table), sets, cond) =>
      val t = db.table(table) getOrElse problem(id, s"unknown table: $table")
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
      val t = db.table(table) getOrElse problem(id, s"unknown table: $table")
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

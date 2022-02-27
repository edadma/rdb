package io.github.edadma.rdb

import pprint.pprintln

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

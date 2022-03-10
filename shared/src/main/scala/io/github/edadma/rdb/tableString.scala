package io.github.edadma.rdb

import io.github.edadma.table

def tableString(t: TableValue): String =
  val sameTable = t.meta.columns.map(_.table).distinct.length == 1
  val cols =
    t.meta.columns map {
      case ColumnMetadata(_, name, _) if sameTable => name
      case ColumnMetadata(None, name, _)           => name
      case ColumnMetadata(Some(tab), col, _)       => s"$tab.$col"
    }
  val res =
    new table.TextTable {
      headerSeq(cols)
    }

  for ((ColumnMetadata(_, _, t), i) <- t.meta.columns.zipWithIndex)
    if t != null && t.isNumber then res.rightAlignment(i + 1)

  for (r <- t.data)
    res.rowSeq(r.data map (_.render))

  val count =
    t.length match
      case 0 => "empty result"
      case 1 => "1 row"
      case _ => s"${t.length} rows"

  res.toString ++ s"\n($count)\n"

package io.github.edadma.petradb

def exprToSQL(expr: Expr): String = exprToSQLInner(expr)._1

def queryToSQL(expr: Expr): String = exprToSQL(expr)

private def orderByToSQL(ob: OrderBy): String =
  val e = exprToSQLInner(ob.f)._1
  val dir = if ob.asc then "" else " DESC"
  val nulls = if ob.asc && !ob.nullsFirst then "" else if !ob.asc && ob.nullsFirst then "" else if ob.nullsFirst then " NULLS FIRST" else " NULLS LAST"
  s"$e$dir$nulls"

private def relToSQL(expr: Expr): String =
  expr match
    case TableOperator(Ident(name)) => name
    case AliasOperator(rel, Ident(alias)) =>
      val inner = relToSQL(rel)
      rel match
        case _: SQLSelectExpr | _: CompoundQueryExpr | _: SetOperationExpr | _: ValuesExpr =>
          s"($inner) AS $alias"
        case _ => s"$inner AS $alias"
    case ColumnAliasOperator(rel, Ident(alias), columns) =>
      val inner = relToSQL(rel)
      val cols = columns.map(_.name).mkString(", ")
      s"($inner) AS $alias($cols)"
    case InnerJoinOperator(rel1, rel2, on) =>
      s"${relToSQL(rel1)} INNER JOIN ${relToSQL(rel2)} ON ${exprToSQLInner(on)._1}"
    case LeftJoinOperator(rel1, rel2, on) =>
      s"${relToSQL(rel1)} LEFT JOIN ${relToSQL(rel2)} ON ${exprToSQLInner(on)._1}"
    case RightJoinOperator(rel1, rel2, on) =>
      s"${relToSQL(rel1)} RIGHT JOIN ${relToSQL(rel2)} ON ${exprToSQLInner(on)._1}"
    case FullJoinOperator(rel1, rel2, on) =>
      s"${relToSQL(rel1)} FULL JOIN ${relToSQL(rel2)} ON ${exprToSQLInner(on)._1}"
    case CrossOperator(rel1, rel2) =>
      s"${relToSQL(rel1)} CROSS JOIN ${relToSQL(rel2)}"
    case LateralCrossOperator(rel1, rel2) =>
      s"${relToSQL(rel1)}, LATERAL ${relToSQL(rel2)}"
    case _ => exprToSQLInner(expr)._1

private def exprToSQLInner(expr: Expr): (String, Int) =
  expr match
    case NumberExpr(n) =>
      val s = if n.doubleValue == n.longValue.toDouble && !n.toString.contains(".") then n.longValue.toString else n.toString
      (s, 99)
    case StringExpr(s) =>
      val escaped = s.replace("'", "''")
      (s"'$escaped'", 99)
    case BooleanExpr(b) => (if b then "TRUE" else "FALSE", 99)
    case NullExpr()     => ("NULL", 99)
    case ColumnExpr(Some(table), col) => (s"${table.name}.${col.name}", 99)
    case ColumnExpr(None, col)        => (col.name, 99)
    case ApplyExpr(func, args) =>
      val argStrs = args.map(a => exprToSQLInner(a)._1)
      (s"${func.name}(${argStrs.mkString(", ")})", 99)
    case UnaryExpr("NOT", e) =>
      val (s, p) = exprToSQLInner(e)
      val child = if p < 3 then s"($s)" else s
      (s"NOT $child", 3)
    case UnaryExpr("-", e) =>
      val (s, p) = exprToSQLInner(e)
      val child = if p < 9 then s"($s)" else s
      (s"-$child", 9)
    case UnaryExpr("~", e) =>
      val (s, p) = exprToSQLInner(e)
      val child = if p < 9 then s"($s)" else s
      (s"~$child", 9)
    case UnaryExpr("IS NULL", e) =>
      val (s, p) = exprToSQLInner(e)
      val child = if p < 4 then s"($s)" else s
      (s"$child IS NULL", 4)
    case UnaryExpr("IS NOT NULL", e) =>
      val (s, p) = exprToSQLInner(e)
      val child = if p < 4 then s"($s)" else s
      (s"$child IS NOT NULL", 4)
    case UnaryExpr(op @ ("IS TRUE" | "IS NOT TRUE" | "IS FALSE" | "IS NOT FALSE" | "IS UNKNOWN" | "IS NOT UNKNOWN"), e) =>
      val (s, p) = exprToSQLInner(e)
      val child = if p < 4 then s"($s)" else s
      (s"$child $op", 4)
    case BinaryExpr(left, op, right) =>
      val prec = opPrec(op)
      val (ls, lp) = exprToSQLInner(left)
      val (rs, rp) = exprToSQLInner(right)
      val l = if lp < prec then s"($ls)" else ls
      val r = if rp <= prec then s"($rs)" else rs
      (s"$l $op $r", prec)
    case QuantifiedCompareExpr(value, op, quantifier, expr) =>
      val (vs, vp) = exprToSQLInner(value)
      val v = if vp < 4 then s"($vs)" else vs
      val (es, _) = exprToSQLInner(expr)
      (s"$v $op $quantifier($es)", 4)
    case InSeqExpr(value, op, exprs) =>
      val (vs, vp) = exprToSQLInner(value)
      val v = if vp < 4 then s"($vs)" else vs
      val es = exprs.map(e => exprToSQLInner(e)._1)
      (s"$v $op (${es.mkString(", ")})", 4)
    case BetweenExpr(value, op, lower, upper) =>
      val (vs, vp) = exprToSQLInner(value)
      val v = if vp < 4 then s"($vs)" else vs
      val (ls, _) = exprToSQLInner(lower)
      val (us, _) = exprToSQLInner(upper)
      (s"$v $op $ls AND $us", 4)
    case CaseExpr(whens, els) =>
      val ws = whens.map { case When(w, e) =>
        s"WHEN ${exprToSQLInner(w)._1} THEN ${exprToSQLInner(e)._1}"
      }
      val e = els.map(e => s" ELSE ${exprToSQLInner(e)._1}").getOrElse("")
      (s"CASE ${ws.mkString(" ")}$e END", 99)
    case CastExpr(e, t) =>
      val (s, p) = exprToSQLInner(e)
      val child = if p < 10 then s"($s)" else s
      (s"$child::${t.name}", 10)
    case ArrayExpr(elems) =>
      val es = elems.map(e => exprToSQLInner(e)._1)
      (s"ARRAY[${es.mkString(", ")}]", 99)
    // ── Query-level nodes ────────────────────────────────────────────
    case SQLSelectExpr(exprs, from, where, groupBy, having, orderBy, offset, limit, distinct) =>
      val sb = new StringBuilder("SELECT ")
      if distinct then sb.append("DISTINCT ")
      sb.append(exprs.map(e => exprToSQLInner(e)._1).mkString(", "))
      from.foreach { sources =>
        sb.append(" FROM ")
        sb.append(sources.map(relToSQL).mkString(", "))
      }
      where.foreach(w => sb.append(s" WHERE ${exprToSQLInner(w)._1}"))
      groupBy.foreach(gb => sb.append(s" GROUP BY ${gb.map(e => exprToSQLInner(e)._1).mkString(", ")}"))
      having.foreach(h => sb.append(s" HAVING ${exprToSQLInner(h)._1}"))
      orderBy.foreach(ob => sb.append(s" ORDER BY ${ob.map(orderByToSQL).mkString(", ")}"))
      limit.foreach(l => sb.append(s" LIMIT ${exprToSQLInner(l.expr)._1}"))
      offset.foreach(o => sb.append(s" OFFSET ${exprToSQLInner(o.expr)._1}"))
      (sb.toString, 99)
    case CompoundQueryExpr(query, orderBy, offset, limit) =>
      val sb = new StringBuilder(exprToSQLInner(query)._1)
      orderBy.foreach(ob => sb.append(s" ORDER BY ${ob.map(orderByToSQL).mkString(", ")}"))
      limit.foreach(l => sb.append(s" LIMIT ${exprToSQLInner(l.expr)._1}"))
      offset.foreach(o => sb.append(s" OFFSET ${exprToSQLInner(o.expr)._1}"))
      (sb.toString, 99)
    case SetOperationExpr(op, left, right) =>
      (s"${exprToSQLInner(left)._1} $op ${exprToSQLInner(right)._1}", 99)
    // ── Relational nodes (Operators) ─────────────────────────────────
    case TableOperator(Ident(name)) => (name, 99)
    case AliasOperator(rel, Ident(alias)) =>
      val inner = relToSQL(rel)
      rel match
        case _: SQLSelectExpr | _: CompoundQueryExpr | _: SetOperationExpr | _: ValuesExpr =>
          (s"($inner) AS $alias", 99)
        case _ => (s"$inner AS $alias", 99)
    case ColumnAliasOperator(rel, Ident(alias), columns) =>
      val inner = relToSQL(rel)
      val cols = columns.map(_.name).mkString(", ")
      (s"($inner) AS $alias($cols)", 99)
    case InnerJoinOperator(rel1, rel2, on) =>
      (s"${relToSQL(rel1)} INNER JOIN ${relToSQL(rel2)} ON ${exprToSQLInner(on)._1}", 99)
    case LeftJoinOperator(rel1, rel2, on) =>
      (s"${relToSQL(rel1)} LEFT JOIN ${relToSQL(rel2)} ON ${exprToSQLInner(on)._1}", 99)
    case RightJoinOperator(rel1, rel2, on) =>
      (s"${relToSQL(rel1)} RIGHT JOIN ${relToSQL(rel2)} ON ${exprToSQLInner(on)._1}", 99)
    case FullJoinOperator(rel1, rel2, on) =>
      (s"${relToSQL(rel1)} FULL JOIN ${relToSQL(rel2)} ON ${exprToSQLInner(on)._1}", 99)
    case CrossOperator(rel1, rel2) =>
      (s"${relToSQL(rel1)} CROSS JOIN ${relToSQL(rel2)}", 99)
    case LateralCrossOperator(rel1, rel2) =>
      (s"${relToSQL(rel1)}, LATERAL ${relToSQL(rel2)}", 99)
    // ── Missing expression nodes ─────────────────────────────────────
    case StarExpr()               => ("*", 99)
    case TableStarExpr(Ident(t))  => (s"$t.*", 99)
    case AliasExpr(expr, Ident(alias)) =>
      (s"${exprToSQLInner(expr)._1} AS $alias", 99)
    case ExistsExpr(subquery) =>
      (s"EXISTS (${exprToSQLInner(subquery)._1})", 99)
    case InQueryExpr(value, op, query) =>
      val (vs, vp) = exprToSQLInner(value)
      val v = if vp < 4 then s"($vs)" else vs
      (s"$v $op (${exprToSQLInner(query)._1})", 4)
    case SubqueryExpr(query) =>
      (s"(${exprToSQLInner(query)._1})", 99)
    case TableConstructorExpr(query) =>
      (s"(${exprToSQLInner(query)._1})", 99)
    case LateralExpr(query) =>
      (s"LATERAL (${exprToSQLInner(query)._1})", 99)
    case ValuesExpr(rows) =>
      val rs = rows.map(r => s"(${r.map(e => exprToSQLInner(e)._1).mkString(", ")})").mkString(", ")
      (s"VALUES $rs", 99)
    case OverlapsExpr(s1, e1, s2, e2) =>
      (s"(${exprToSQLInner(s1)._1}, ${exprToSQLInner(e1)._1}) OVERLAPS (${exprToSQLInner(s2)._1}, ${exprToSQLInner(e2)._1})", 99)
    case ObjectExpr(properties) =>
      val ps = properties.map { case (Ident(k), v) => s"'$k': ${exprToSQLInner(v)._1}" }
      (s"{${ps.mkString(", ")}}", 99)
    case ParameterExpr(index) => (s"$$$index", 99)
    case _ => (expr.toString, 99)

private def opPrec(op: String): Int =
  op.toUpperCase match
    case "OR"                                     => 1
    case "AND"                                    => 2
    case "=" | "!=" | "<" | ">" | "<=" | ">=" |
         "LIKE" | "ILIKE" | "NOT LIKE" | "NOT ILIKE" => 4
    case "||"                                     => 5
    case "&" | "|" | "#" | "<<" | ">>"           => 6
    case "+" | "-"                                => 7
    case "*" | "/" | "%"                           => 8
    case "->" | "->>" | "#>" | "#>>"              => 11
    case "@>" | "<@" | "&&" | "?" | "?|" | "?&"
       | "IS DISTINCT FROM" | "IS NOT DISTINCT FROM" => 4
    case _                                        => 4

package io.github.edadma.rdb

def exprToSQL(expr: Expr): String = exprToSQLInner(expr)._1

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
    case "@>" | "<@" | "?" | "?|" | "?&"         => 4
    case _                                        => 4

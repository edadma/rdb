package io.github.edadma.rdb

def rewrite(expr: Expr): Expr =
  expr match
    case ApplyExpr(name, args) =>
      scalarFunction get name match
        case None    => sys.error(s"unknown function '$name'")
        case Some(f) => ScalarFunctionExpr(f, f.typ)
    case BinaryExpr(left, op, right, _) =>
      val l = rewrite(left)
      val r = rewrite(right)

      if (l.typ != r.typ) sys.error(s"type mismatch: ${l.typ}, ${r.typ}")

      BinaryExpr(l, op, r, l.typ)
    case SQLSelectExpr(exprs, from, where) =>

    case _ => expr

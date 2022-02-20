package io.github.edadma.rdb

def rewrite(expr: Expr)(implicit db: DB): Expr =
  expr match
    case ApplyExpr(Ident(func, pos), args) =>
      scalarFunction get func match
        case None    => sys.error(s"unknown function '$func'")
        case Some(f) => ScalarFunctionExpr(f, args map rewrite, f.typ)
    case BinaryExpr(left, op, right, UnknownType) =>
      val l = rewrite(left)
      val r = rewrite(right)

      if (l.typ != r.typ) sys.error(s"type mismatch: ${l.typ}, ${r.typ}")

      BinaryExpr(l, op, r, l.typ)
    case SQLSelectExpr(exprs, from, where) =>
      def cross(es: Seq[Expr]): Expr =
        es match
          case Seq(e)  => e
          case e :: tl => CrossExpr(e, cross(tl))

      val r = cross(from map rewrite)
      val r1 =
        where match
          case Some(cond) => SelectExpr(r, rewrite(cond))
          case None       => r

      val r2 =
        if (exprs == Seq(StarExpr)) r1
        else ProjectExpr(r1, exprs map rewrite)

      rewrite(r2)
    case AliasExpr(rel, Ident(alias, pos)) => OperatorExpr(AliasOperator(orewrite(rel), alias))
    case TableExpr(Ident(name, pos)) =>
      db.table(name) match
        case Some(t) => OperatorExpr(t)
        case None    => sys.error(s"table '$name' not found")
    case ProjectExpr(rel, projs) => OperatorExpr(ProjectOperator(orewrite(rel), projs.toIndexedSeq map rewrite))
    case CrossExpr(rel1, rel2)   => OperatorExpr(CrossOperator(orewrite(rel1), orewrite(rel2)))
    case SelectExpr(rel, cond)   => OperatorExpr(FilterOperator(orewrite(rel), rewrite(cond)))
    case _                       => expr

def orewrite(expr: Expr)(implicit db: DB): Operator = rewrite(expr).asInstanceOf[OperatorExpr].oper

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
    case SelectExpr(exprs, from, where) =>
      def cross(es: Seq[Expr]): Expr =
        es match
          case Seq(e)  => e
          case e :: tl => CrossOperator(e, cross(tl))

      val r = cross(from map rewrite)
      val r1 =
        where match
          case Some(cond) => SelectOperator(r, rewrite(cond))
          case None       => r

      val r2 =
        if (exprs == Seq(StarExpr)) r1
        else ProjectOperator(r1, exprs map rewrite)

      rewrite(r2)
    case LeftJoinOperator(rel1, rel2, on) =>
      ProcessOperator(LeftCrossJoinProcess(procRewrite(rel1), procRewrite(rel2), rewrite(on)))
    case AliasOperator(rel, Ident(alias, pos)) => ProcessOperator(AliasProcess(procRewrite(rel), alias))
    case TableOperator(Ident(name, pos)) =>
      db.table(name) match
        case Some(t) => ProcessOperator(t)
        case None    => sys.error(s"table '$name' not found")
    case ProjectOperator(rel, projs) =>
      ProcessOperator(ProjectProcess(procRewrite(rel), projs.toIndexedSeq map rewrite))
    case CrossOperator(rel1, rel2) => ProcessOperator(CrossProcess(procRewrite(rel1), procRewrite(rel2)))
    case SelectOperator(rel, cond) => ProcessOperator(FilterProcess(procRewrite(rel), rewrite(cond)))
    // case SelectOperator(CrossOperator(rel1, rel2), cond) => // optimize
    case _ => expr

def procRewrite(expr: Expr)(implicit db: DB): Process = rewrite(expr).asInstanceOf[ProcessOperator].proc

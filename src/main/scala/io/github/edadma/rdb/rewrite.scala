package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq

def rewrite(expr: Expr)(implicit db: DB): Expr =
  expr match
    case _ if expr.typ != null => expr
    case ApplyExpr(Ident(func), args) =>
      scalarFunction get func match
        case None =>
          aggregateFunction get func match
            case None                        => sys.error(s"unknown function '$func'")
            case Some(f) if args.length != 1 => sys.error("aggregate function take one argument")
            case Some(f)                     => AggregateFunctionExpr(f, rewrite(args.head))
        case Some(f) => ScalarFunctionExpr(f, args map rewrite)
    case InExpr(value, array) => InExpr(rewrite(value), rewrite(array))
    case ExistsExpr(subquery) => ExistsExpr(rewrite(subquery))
    case UnaryExpr(op, expr) =>
      val e = rewrite(expr)

      UnaryExpr(op, e)
    case BinaryExpr(left, op @ ("+" | "-" | "*" | "/" | "and" | "or"), right) =>
      val l = rewrite(left)
      val r = rewrite(right)

      if (l.typ != r.typ) sys.error(s"type mismatch: ${l.typ}, ${r.typ}")

      BinaryExpr(l, op, r)
    case BinaryExpr(left, op @ ("<=" | ">=" | "!=" | "=" | "<" | ">"), right) =>
      BinaryExpr(rewrite(left), op, rewrite(right))
    case BetweenExpr(value, op, lower, upper) =>
      val v = rewrite(value)
      val l = rewrite(lower)
      val r = rewrite(upper)

      if op == "BETWEEN" then BinaryExpr(BinaryExpr(lower, "<=", value), "AND", BinaryExpr(value, "<=", upper))
      else BinaryExpr(BinaryExpr(value, "<", lower), "OR", BinaryExpr(value, ">", upper))
    case SQLSelectExpr(exprs, Nil, where, opos, offset, lpos, limit) =>
      if where.isDefined then problem(where.get, "WHERE clause no allowed here")
      if offset.isDefined then problem(opos, "OFFSET clause no allowed here")
      if limit.isDefined then problem(lpos, "LIMIT clause no allowed here")

      val rewritten_projs = exprs map rewrite
      val aggregates = rewritten_projs exists aggregate

      ProcessOperator(ProjectProcess(SingleProcess, rewritten_projs, aggregates))
    case SQLSelectExpr(exprs, from, where, opos, offset, lpos, limit) =>
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
        exprs match
          case Seq(StarExpr()) => r1
          case _               => ProjectOperator(r1, exprs map rewrite)
      val r3 =
        if offset.isDefined then OffsetOperator(r2, offset.get)
        else r2
      val r4 =
        if limit.isDefined then LimitOperator(r3, limit.get)
        else r3

      rewrite(r4)
    case OffsetOperator(rel, offset) => ProcessOperator(DropProcess(procRewrite(rel), offset))
    case LimitOperator(rel, limit)   => ProcessOperator(TakeProcess(procRewrite(rel), limit))
    case InnerJoinOperator(rel1, rel2, on) =>
      ProcessOperator(FilterProcess(CrossProcess(procRewrite(rel1), procRewrite(rel2)), rewrite(on)))
    case LeftJoinOperator(rel1, rel2, on) =>
      ProcessOperator(LeftCrossJoinProcess(procRewrite(rel1), procRewrite(rel2), rewrite(on)))
    case AliasOperator(rel, Ident(alias)) => ProcessOperator(AliasProcess(procRewrite(rel), alias))
    case TableOperator(id @ Ident(name)) =>
      db.table(name) match
        case Some(t) => ProcessOperator(t)
        case None    => problem(id, s"table '$name' not found")
    case ProjectOperator(rel, projs) =>
      val rewritten_projs = projs map rewrite
      val aggregates = rewritten_projs exists aggregate
      val columns = rewritten_projs exists column
      val rewritten_proc = procRewrite(rel)

      ProcessOperator(
        ProjectProcess(
          if aggregates then UngroupedProcess(rewritten_proc, columns) else rewritten_proc,
          rewritten_projs,
          aggregates
        )
      )
    case CrossOperator(rel1, rel2) => ProcessOperator(CrossProcess(procRewrite(rel1), procRewrite(rel2)))
    case SelectOperator(rel, cond) => ProcessOperator(FilterProcess(procRewrite(rel), rewrite(cond)))
    case _                         => expr

def aggregate(expr: Expr): Boolean =
  expr match
    case _: AggregateFunctionExpr    => true
    case ScalarFunctionExpr(_, args) => args exists aggregate
    case UnaryExpr(_, expr)          => aggregate(expr)
    case BinaryExpr(left, _, right)  => aggregate(left) | aggregate(right)
    case _                           => false

def column(expr: Expr): Boolean =
  expr match
    case _: (ColumnExpr | Operator)  => true
    case ScalarFunctionExpr(_, args) => args exists column
    case UnaryExpr(_, expr)          => column(expr)
    case BinaryExpr(left, _, right)  => column(left) | column(right)
    case _                           => false

def procRewrite(expr: Expr)(implicit db: DB): Process = rewrite(expr).asInstanceOf[ProcessOperator].proc

// todo: case SelectOperator(CrossOperator(rel1, rel2), cond) => // optimize
// todo: grouped: case ProjectOperator(rel, projs) => ProcessOperator(ProjectProcess(procRewrite(rel), projs map rewrite))

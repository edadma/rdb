package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq

def rewrite(expr: Expr)(implicit db: DB): Expr =
  expr match
    case ApplyExpr(Ident(pos, func), args) =>
      scalarFunction get func match
        case None =>
          aggregateFunction get func match
            case None                        => sys.error(s"unknown function '$func'")
            case Some(f) if args.length != 1 => sys.error("aggregate function take one argument")
            case Some(f)                     => AggregateFunctionExpr(f, rewrite(args.head), f.typ)
        case Some(f) => ScalarFunctionExpr(f, args map rewrite, f.typ)
    case InExpr(value, array) => InExpr(rewrite(value), rewrite(array))
    case ExistsExpr(subquery) => ExistsExpr(rewrite(subquery))
    case UnaryExpr(op, pos, expr, UnknownType) =>
      val e = rewrite(expr)

      UnaryExpr(op, pos, e, e.typ)
    case BinaryExpr(lp, left, op @ ("+" | "-" | "*" | "/" | "and" | "or"), rp, right, UnknownType) =>
      val l = rewrite(left)
      val r = rewrite(right)

      if (l.typ != r.typ) sys.error(s"type mismatch: ${l.typ}, ${r.typ}")

      BinaryExpr(lp, l, op, rp, r, l.typ)
    case BinaryExpr(lp, left, op @ ("<=" | ">=" | "!=" | "=" | "<" | ">"), rp, right, _) =>
      BinaryExpr(lp, rewrite(left), op, rp, rewrite(right))
    case SQLSelectExpr(exprs, from, where, offset, limit) =>
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
          case Seq(StarExpr(_)) => r1
          case _                => ProjectOperator(r1, exprs map rewrite)
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
    case AliasOperator(rel, Ident(pos, alias)) => ProcessOperator(AliasProcess(procRewrite(rel), alias))
    case TableOperator(Ident(pos, name)) =>
      db.table(name) match
        case Some(t) => ProcessOperator(t)
        case None    => sys.error(s"table '$name' not found")
    // grouped: case ProjectOperator(rel, projs) => ProcessOperator(ProjectProcess(procRewrite(rel), projs map rewrite))
    case ProjectOperator(rel, projs) =>
      def aggregate(expr: Expr): Boolean =
        expr match
          case _: AggregateFunctionExpr            => true
          case ScalarFunctionExpr(_, args, _)      => args exists aggregate
          case UnaryExpr(_, _, expr, _)            => aggregate(expr)
          case BinaryExpr(_, left, _, _, right, _) => aggregate(left) | aggregate(right)
          case _                                   => false

      def column(expr: Expr): Boolean =
        expr match
          case _: (ColumnExpr | Operator)          => true
          case ScalarFunctionExpr(_, args, _)      => args exists column
          case UnaryExpr(_, _, expr, _)            => column(expr)
          case BinaryExpr(_, left, _, _, right, _) => column(left) | column(right)
          case _                                   => false

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
    // case SelectOperator(CrossOperator(rel1, rel2), cond) => // optimize
    case _ => expr

def procRewrite(expr: Expr)(implicit db: DB): Process = rewrite(expr).asInstanceOf[ProcessOperator].proc

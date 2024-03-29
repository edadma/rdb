package io.github.edadma.rdb

//import pprint.*

import scala.collection.immutable.ArraySeq

def rewrite(expr: Expr)(implicit db: DB): Expr =
  expr match
    case _ if expr.typ != null => expr
    case SubqueryExpr(query)   => SubqueryExpr(rewrite(query))
    case CaseExpr(whens, els) =>
      CaseExpr(whens map { case When(when, expr) => When(rewrite(when), rewrite(expr)) }, els map rewrite)
    case InSeqExpr(value, op, exprs)   => InSeqExpr(rewrite(value), op, exprs map rewrite)
    case InQueryExpr(value, op, array) => InQueryExpr(rewrite(value), op, rewrite(array))
    case TableConstructorExpr(expr)    => TableConstructorExpr(rewrite(expr))
    case ApplyExpr(id @ Ident(func), args) =>
      scalarFunction get func.toLowerCase match
        case None =>
          aggregateFunction get func.toLowerCase match
            case None                        => problem(id, s"unknown function '$func'")
            case Some(f) if args.length != 1 => problem(id, "aggregate function takes one argument")
            case Some(f) =>
              val (instance, typ) = f.instantiate

              AggregateFunctionExpr(instance, rewrite(args.head)) setType typ
        case Some(f) => ScalarFunctionExpr(f, args map rewrite)
    case VariableExpr(id @ Ident(name)) =>
      scalarVariable get name match
        case None    => problem(id, s"unknown variable '$name'")
        case Some(v) => VariableInstanceExpr(v.instance)
    case ExistsExpr(subquery) => ExistsExpr(rewrite(subquery)) setType BooleanType
    case UnaryExpr(op, expr) =>
      val e = rewrite(expr)

      UnaryExpr(op, e) setType e.typ
    case BinaryExpr(left, op @ ("+" | "-" | "*" | "/" | "and" | "or"), right) =>
      val l = rewrite(left)
      val r = rewrite(right)

//      if (l.typ != r.typ) sys.error(s"type mismatch: ${l.typ}, ${r.typ}") // todo: rewrite needs context to determine types

      BinaryExpr(l, op, r) setType l.typ
    case BinaryExpr(left, op @ ("<=" | ">=" | "!=" | "=" | "<" | ">"), right) =>
      BinaryExpr(rewrite(left), op, rewrite(right)) setType BooleanType
    case BetweenExpr(value, op, lower, upper) =>
      val v = rewrite(value)
      val l = rewrite(lower)
      val r = rewrite(upper)

      (if op == "BETWEEN" then BinaryExpr(BinaryExpr(lower, "<=", value), "AND", BinaryExpr(value, "<=", upper))
       else BinaryExpr(BinaryExpr(value, "<", lower), "OR", BinaryExpr(value, ">", upper))) setType BooleanType
    case SQLSelectExpr(exprs, None, where, groupBy, orderBy, offset, limit) =>
      if where.isDefined then problem(where.get, "WHERE clause not allowed here")
      if groupBy.isDefined then problem(where.get, "GROUP BY clause not allowed here")
      if orderBy.isDefined then problem(where.get, "ORDER BY clause not allowed here")
      if offset.isDefined then problem(offset.get.pos, "OFFSET clause not allowed here")
      if limit.isDefined then problem(limit.get.pos, "LIMIT clause not allowed here")

      val rewritten_projs = exprs map rewrite

      ProcessOperator(ProjectProcess(SingleProcess, rewritten_projs))
    case SQLSelectExpr(exprs, Some(from), where, groupBy, orderBy, offset, limit) =>
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
        groupBy match
          case None     => r1
          case Some(es) => GroupOperator(r1, es map rewrite)
      val r3 =
        exprs match
          case Seq(StarExpr()) => r2
          case _               => ProjectOperator(r2, exprs map rewrite)
      val r4 =
        orderBy match
          case None     => r3
          case Some(os) => SortOperator(r3, os map { case OrderBy(f, d, n) => OrderBy(rewrite(f), d, n) })
      val r5 =
        offset match
          case Some(Count(pos, count)) =>
            if count < 1 then problem(pos, s"offset should be positive: $count")

            OffsetOperator(r4, count)
          case None => r4
      val r6 =
        limit match
          case Some(Count(pos, count)) =>
            if count < 1 then problem(pos, s"limit should be positive: $count")

            LimitOperator(r5, count)
          case None => r5

      rewrite(r6)
    case SortOperator(rel, by)       => ProcessOperator(SortProcess(procRewrite(rel), by))
    case GroupOperator(rel, by)      => ProcessOperator(GroupProcess(procRewrite(rel), by))
    case OffsetOperator(rel, offset) => ProcessOperator(DropProcess(procRewrite(rel), offset))
    case LimitOperator(rel, limit)   => ProcessOperator(TakeProcess(procRewrite(rel), limit))
    case InnerJoinOperator(rel1, rel2, on) =>
      ProcessOperator(FilterProcess(CrossProcess(procRewrite(rel1), procRewrite(rel2)), rewrite(on)))
    case LeftJoinOperator(rel1, rel2, on) =>
      ProcessOperator(LeftCrossJoinProcess(procRewrite(rel1), procRewrite(rel2), rewrite(on)))
    case AliasOperator(rel, Ident(alias)) => ProcessOperator(AliasProcess(procRewrite(rel), alias))
    case TableOperator(id @ Ident(name)) =>
      db.getTable(name) match
        case Some(t) => ProcessOperator(t)
        case None    => problem(id, s"table '$name' not found")
    case ProjectOperator(rel, projs) =>
      val rewritten_projs = projs map rewrite
      val aggregates = rewritten_projs exists aggregate
      val columns = rewritten_projs exists column
      val rewritten_proc = procRewrite(rel)

      ProcessOperator(
        ProjectProcess(
          if aggregates && !rel.isInstanceOf[GroupOperator] then UngroupedProcess(rewritten_proc, columns)
          else rewritten_proc,
          rewritten_projs,
        ),
      )
    case CrossOperator(rel1, rel2) => ProcessOperator(CrossProcess(procRewrite(rel1), procRewrite(rel2)))
    case SelectOperator(rel, cond) => ProcessOperator(FilterProcess(procRewrite(rel), rewrite(cond)))
    // todo: ColumnExpr, VariableExpr
    case _ => expr

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

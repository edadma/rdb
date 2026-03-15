package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import scala.collection.immutable.ArraySeq

private[engine] class BlockReturnException(val value: Option[Value] = None) extends RuntimeException
class RaiseException(message: String) extends RuntimeException(message)

def executeBlockForValue(block: Block, parentEnv: Option[BlockEnv] = None)(using session: Session): Value =
  val env = new BlockEnv(parentEnv)

  for VarDecl(Ident(name), typRef, default) <- block.declarations do
    val typ = typRef match
      case Left(t)         => t
      case Right(Ident(n)) => session.db.getType(n).getOrElse(sys.error(s"type '$n' not found"))
    val init = default match
      case Some(expr) => eval(rewriteWithEnv(expr, env), Nil)
      case None       => NullValue()
    env.declare(name, typ, init)

  try
    executeStatements(block.body, env)
    NullValue()
  catch
    case e: BlockReturnException => e.value.getOrElse(NullValue())

def executeBlock(block: Block, parentEnv: Option[BlockEnv] = None)(using session: Session): Result =
  val env = new BlockEnv(parentEnv)

  for VarDecl(Ident(name), typRef, default) <- block.declarations do
    val typ = typRef match
      case Left(t)          => t
      case Right(Ident(n))  => session.db.getType(n).getOrElse(sys.error(s"type '$n' not found"))
    val init = default match
      case Some(expr) => eval(rewriteWithEnv(expr, env), Nil)
      case None       => NullValue()
    env.declare(name, typ, init)

  try
    executeStatements(block.body, env)
  catch
    case _: BlockReturnException => ()
    case e: Exception if block.exceptionHandlers.nonEmpty =>
      val condName = e match
        case _: RaiseException      => "raise_exception"
        case _: SchemaException     => "duplicate_object"
        case _: ConstraintException => "unique_violation"
        case e: IllegalArgumentException if e.getMessage != null && e.getMessage.contains("already exists") => "duplicate_object"
        case _                      => "others"
      block.exceptionHandlers.find(h => h.condition == condName || h.condition == "others") match
        case Some(handler) => executeStatements(handler.body, env)
        case None          => throw e

  DoBlockResult

private def executeStatements(stmts: Seq[Statement], env: BlockEnv)(using Session): Unit =
  for stmt <- stmts do executeStatement(stmt, env)

private def executeStatement(stmt: Statement, env: BlockEnv)(using session: Session): Unit =
  stmt match
    case SqlStatement(cmd) =>
      val rewritten = substituteCommandVars(cmd, env)
      executeCommands(Seq(rewritten))

    case AssignStatement(Ident(name), expr) =>
      val value = eval(rewriteWithEnv(expr, env), Nil)
      env.set(name, value)

    case IfStatement(cond, thenBody, elsifs, elseBody) =>
      if beval(rewriteWithEnv(cond, env), Nil) then
        executeStatements(thenBody, env)
      else
        elsifs.find((c, _) => beval(rewriteWithEnv(c, env), Nil)) match
          case Some((_, body)) => executeStatements(body, env)
          case None            => elseBody.foreach(executeStatements(_, env))

    case WhileStatement(cond, body) =>
      var iterations = 0
      while beval(rewriteWithEnv(cond, env), Nil) do
        executeStatements(body, env)
        iterations += 1
        if iterations > 10000 then sys.error("WHILE loop exceeded maximum iterations (10000)")

    case ForRangeStatement(Ident(varName), lower, upper, body) =>
      val lo = eval(rewriteWithEnv(lower, env), Nil).intValue
      val hi = eval(rewriteWithEnv(upper, env), Nil).intValue
      for i <- lo to hi do
        env.set(varName, NumberValue(i))
        executeStatements(body, env)

    case ForQueryStatement(Ident(varName), queryCmd, body) =>
      val results = executeCommands(Seq(queryCmd), Some(env))
      results.lastOption match
        case Some(QueryResult(table)) =>
          for row <- table.data do
            val value = if row.data.length == 1 then row.data(0)
            else ObjectValue(row.meta.columns.zip(row.data).map((c, v) => (c.name, v)).toSeq)
            env.set(varName, value)
            executeStatements(body, env)
        case _ => ()

    case ReturnStatement =>
      throw new BlockReturnException()

    case ReturnValueStatement(expr) =>
      val value = eval(rewriteWithEnv(expr, env), Nil)
      throw new BlockReturnException(Some(value))

    case RaiseStatement(level, fmt, args) =>
      val values = args.map(e => eval(rewriteWithEnv(e, env), Nil))
      val message = formatRaise(fmt, values)
      level.toLowerCase match
        case "notice"    => System.err.println(s"NOTICE:  $message")
        case "exception" => throw new RaiseException(message)
        case other       => System.err.println(s"${other.toUpperCase}:  $message")

    case PerformStatement(query) =>
      eval(rewriteWithEnv(query, env), Nil)

    case NullStatement => ()

private[engine] def rewriteWithEnv(expr: Expr, env: BlockEnv)(using Session): Expr =
  val substituted = substituteBlockVars(expr, env)
  rewrite(substituted)

private def substituteBlockVars(expr: Expr, env: BlockEnv): Expr =
  expr match
    case VariableExpr(id @ Ident(name)) if env.isDeclared(name) =>
      BlockVariableExpr(name, env)
    case ColumnExpr(None, Ident(name)) if env.isDeclared(name) =>
      BlockVariableExpr(name, env)
    case BinaryExpr(l, op, r) =>
      BinaryExpr(substituteBlockVars(l, env), op, substituteBlockVars(r, env)).setPos(expr.pos).asInstanceOf[Expr]
    case UnaryExpr(op, e) =>
      UnaryExpr(op, substituteBlockVars(e, env)).setPos(expr.pos).asInstanceOf[Expr]
    case ApplyExpr(name, args, filter) =>
      ApplyExpr(name, args.map(substituteBlockVars(_, env)), filter.map(substituteBlockVars(_, env))).setPos(expr.pos).asInstanceOf[Expr]
    case CaseExpr(whens, els) =>
      CaseExpr(whens.map { case When(c, e) => When(substituteBlockVars(c, env), substituteBlockVars(e, env)) }, els.map(substituteBlockVars(_, env))).setPos(expr.pos).asInstanceOf[Expr]
    case CastExpr(e, t) =>
      CastExpr(substituteBlockVars(e, env), t).setPos(expr.pos).asInstanceOf[Expr]
    case InSeqExpr(e, op, exprs) =>
      InSeqExpr(substituteBlockVars(e, env), op, exprs.map(substituteBlockVars(_, env))).setPos(expr.pos).asInstanceOf[Expr]
    case AliasExpr(e, a) =>
      AliasExpr(substituteBlockVars(e, env), a).setPos(expr.pos).asInstanceOf[Expr]
    case _ => expr

private def substituteCommandVars(cmd: Command, env: BlockEnv): Command =
  cmd match
    case InsertCommand(table, columns, rows, returning, onConflict) =>
      InsertCommand(table, columns, rows.map(_.map(substituteBlockVars(_, env))), returning.map(_.map(substituteBlockVars(_, env))), onConflict)
    case UpdateCommand(table, sets, from, cond, returning) =>
      UpdateCommand(table, sets.map(s => UpdateSet(s.col, substituteBlockVars(s.value, env))), from, cond.map(substituteBlockVars(_, env)), returning)
    case DeleteCommand(table, using, cond, returning) =>
      DeleteCommand(table, using, cond.map(substituteBlockVars(_, env)), returning)
    case QueryCommand(query) =>
      QueryCommand(substituteBlockVars(query, env))
    case other => other

private def formatRaise(fmt: String, values: Seq[Value]): String =
  val sb = new StringBuilder
  var i = 0
  var argIdx = 0
  while i < fmt.length do
    if fmt(i) == '%' then
      if argIdx < values.length then
        sb.append(values(argIdx).string)
        argIdx += 1
      else sb.append('%')
    else
      sb.append(fmt(i))
    i += 1
  sb.toString

package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

// ── PL/pgSQL statement AST ─────────────────────────────────────────

sealed trait Statement

case class VarDecl(name: Ident, typ: Either[Type, Ident], default: Option[Expr])

case class Block(
    declarations: Seq[VarDecl],
    body: Seq[Statement],
    exceptionHandlers: Seq[ExceptionHandler] = Nil,
)

case class ExceptionHandler(condition: String, body: Seq[Statement])

case class SqlStatement(command: Command) extends Statement
case class AssignStatement(target: Ident, value: Expr) extends Statement
case class IfStatement(
    condition: Expr,
    thenBody: Seq[Statement],
    elsifClauses: Seq[(Expr, Seq[Statement])],
    elseBody: Option[Seq[Statement]],
) extends Statement
case class WhileStatement(condition: Expr, body: Seq[Statement]) extends Statement
case class ForRangeStatement(variable: Ident, lower: Expr, upper: Expr, body: Seq[Statement]) extends Statement
case class ForQueryStatement(variable: Ident, query: Command, body: Seq[Statement]) extends Statement
case object ReturnStatement extends Statement
case class ReturnValueStatement(value: Expr) extends Statement
case class RaiseStatement(level: String, format: String, args: Seq[Expr]) extends Statement
case class PerformStatement(query: Expr) extends Statement
case object NullStatement extends Statement

// ── Stored routines ────────────────────────────────────────────────

case class StoredFunction(
    name: String,
    params: Seq[(String, Type)],
    returnType: Type,
    block: Block,
    source: String,
)

case class StoredProcedure(
    name: String,
    params: Seq[(String, Type)],
    block: Block,
    source: String,
)

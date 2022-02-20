package io.github.edadma.rdb

type Pos = Int

case class Ident(name: String, pos: Option[Pos] = None)

trait Expr:
  val typ: Type

case class ColumnExpr(col: Ident, typ: Type = UnknownType) extends Expr
case class UnaryExpr(op: String, expr: Expr, typ: Type = UnknownType) extends Expr
case class BinaryExpr(left: Expr, op: String, right: Expr, typ: Type = UnknownType) extends Expr
case class StringExpr(s: String, pos: Option[Pos] = None) extends Expr { val typ: Type = StringType }
case class NumberExpr(n: Number, pos: Option[Pos] = None) extends Expr { val typ: Type = NumberType }

trait UnknownTypeExpr extends Expr { val typ: Type = UnknownType }

case class ApplyExpr(func: Ident, args: Seq[Expr]) extends UnknownTypeExpr

case class ScalarFunctionExpr(f: ScalarFunction, args: Seq[Expr], typ: Type) extends Expr
case class AggregateFunctionExpr(f: ScalarFunction, arg: Expr, typ: Type) extends Expr

// SQL

case class SQLSelectExpr(exprs: Seq[Expr], from: Seq[Expr], where: Option[Expr]) extends Operator

case object StarExpr extends UnknownTypeExpr
case class TableStarExpr(table: Ident) extends UnknownTypeExpr

// relational

trait Operator extends Expr { val typ: Type = TableType }

case class AliasOperator(rel: Expr, alias: Ident) extends Operator
case class ProjectOperator(rel: Expr, projs: Seq[Expr]) extends Operator
case class TableOperator(table: Ident) extends Operator
case class SelectOperator(rel: Expr, cond: Expr) extends Operator
case class CrossOperator(rel1: Expr, rel2: Expr) extends Operator

// operator

case class OperatorExpr(proc: Process) extends Operator

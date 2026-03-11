import type { ASTExpr, ASTBinary, ASTUnary, ASTIn, ASTBetween, ASTApply } from './ast.js'
import type { ColumnDef, TableDef, ColumnsConfig } from './schema.js'

// ── Column reference ──

export function col<T extends TableDef<any, any>>(
  table: T,
  column: keyof T['_columns'] & string,
): ASTExpr {
  return { kind: 'column', table: table._name, name: table._columns[column]._columnName }
}

// ── Comparison operators ──

export function eq(left: ASTExpr, right: ASTExpr | string | number | boolean | null): ASTBinary {
  return { kind: 'binary', left, op: '=', right: toExpr(right) }
}

export function ne(left: ASTExpr, right: ASTExpr | string | number | boolean | null): ASTBinary {
  return { kind: 'binary', left, op: '!=', right: toExpr(right) }
}

export function gt(left: ASTExpr, right: ASTExpr | string | number | boolean): ASTBinary {
  return { kind: 'binary', left, op: '>', right: toExpr(right) }
}

export function gte(left: ASTExpr, right: ASTExpr | string | number | boolean): ASTBinary {
  return { kind: 'binary', left, op: '>=', right: toExpr(right) }
}

export function lt(left: ASTExpr, right: ASTExpr | string | number | boolean): ASTBinary {
  return { kind: 'binary', left, op: '<', right: toExpr(right) }
}

export function lte(left: ASTExpr, right: ASTExpr | string | number | boolean): ASTBinary {
  return { kind: 'binary', left, op: '<=', right: toExpr(right) }
}

export function like(left: ASTExpr, pattern: string): ASTBinary {
  return { kind: 'binary', left, op: 'LIKE', right: { kind: 'string', value: pattern } }
}

export function ilike(left: ASTExpr, pattern: string): ASTBinary {
  return { kind: 'binary', left, op: 'ILIKE', right: { kind: 'string', value: pattern } }
}

// ── Logical operators ──

export function and(...exprs: ASTExpr[]): ASTExpr {
  if (exprs.length === 0) throw new Error('and() requires at least one expression')
  if (exprs.length === 1) return exprs[0]
  return exprs.reduce((acc, expr) => ({ kind: 'binary', left: acc, op: 'AND', right: expr }))
}

export function or(...exprs: ASTExpr[]): ASTExpr {
  if (exprs.length === 0) throw new Error('or() requires at least one expression')
  if (exprs.length === 1) return exprs[0]
  return exprs.reduce((acc, expr) => ({ kind: 'binary', left: acc, op: 'OR', right: expr }))
}

export function not(expr: ASTExpr): ASTUnary {
  return { kind: 'unary', op: 'NOT', expr }
}

// ── Null checks ──

export function isNull(expr: ASTExpr): ASTUnary {
  return { kind: 'unary', op: 'IS NULL', expr }
}

export function isNotNull(expr: ASTExpr): ASTUnary {
  return { kind: 'unary', op: 'IS NOT NULL', expr }
}

// ── IN / BETWEEN ──

export function inList(expr: ASTExpr, values: (ASTExpr | string | number | boolean | null)[]): ASTIn {
  return { kind: 'in', value: expr, op: 'IN', exprs: values.map(toExpr) }
}

export function notInList(expr: ASTExpr, values: (ASTExpr | string | number | boolean | null)[]): ASTIn {
  return { kind: 'in', value: expr, op: 'NOT IN', exprs: values.map(toExpr) }
}

export function between(
  expr: ASTExpr,
  lower: ASTExpr | string | number,
  upper: ASTExpr | string | number,
): ASTBetween {
  return { kind: 'between', value: expr, op: 'BETWEEN', lower: toExpr(lower), upper: toExpr(upper) }
}

// ── Arithmetic ──

export function add(left: ASTExpr, right: ASTExpr | number): ASTBinary {
  return { kind: 'binary', left, op: '+', right: toExpr(right) }
}

export function sub(left: ASTExpr, right: ASTExpr | number): ASTBinary {
  return { kind: 'binary', left, op: '-', right: toExpr(right) }
}

export function mul(left: ASTExpr, right: ASTExpr | number): ASTBinary {
  return { kind: 'binary', left, op: '*', right: toExpr(right) }
}

export function div(left: ASTExpr, right: ASTExpr | number): ASTBinary {
  return { kind: 'binary', left, op: '/', right: toExpr(right) }
}

// ── Aggregate functions ──

export function count(expr?: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'count', args: expr ? [expr] : [{ kind: 'star' }] }
}

export function sum(expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'sum', args: [expr] }
}

export function avg(expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'avg', args: [expr] }
}

export function min(expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'min', args: [expr] }
}

export function max(expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'max', args: [expr] }
}

// ── Scalar functions ──

export function fn(name: string, ...args: (ASTExpr | string | number | boolean | null)[]): ASTApply {
  return { kind: 'apply', func: name, args: args.map(toExpr) }
}

// ── Alias ──

export function alias(expr: ASTExpr, name: string): ASTExpr {
  return { kind: 'alias', expr, alias: name }
}

// ── Literal helpers ──

export function literal(value: string | number | boolean | null): ASTExpr {
  return toExpr(value)
}

// ── Internal ──

function toExpr(value: ASTExpr | string | number | boolean | null): ASTExpr {
  if (value === null) return { kind: 'null' }
  if (typeof value === 'object' && 'kind' in value) return value as ASTExpr
  if (typeof value === 'string') return { kind: 'string', value }
  if (typeof value === 'number') return { kind: 'number', value }
  if (typeof value === 'boolean') return { kind: 'boolean', value }
  return { kind: 'null' }
}

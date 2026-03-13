import type { ASTExpr, ASTBinary, ASTUnary, ASTIn, ASTInQuery, ASTBetween, ASTApply, ASTCase, ASTWhen, ASTCast, ASTExists, ASTSubquery } from './ast.js'
import type { ColumnDef, TableDef, ColumnsConfig } from './schema.js'

// ── Column reference ──

export function col<T extends TableDef<any, any>>(
  table: T,
  column: keyof T['_columns'] & string,
): ASTExpr {
  return { kind: 'column', table: table._name, name: table._columns[column]._columnName }
}

// ── Generic operator helpers ──

export function op(left: ASTExpr, operator: string, right: ASTExpr | string | number | boolean | null): ASTBinary {
  return { kind: 'binary', left, op: operator, right: toExpr(right) }
}

export function unaryOp(operator: string, expr: ASTExpr): ASTUnary {
  return { kind: 'unary', op: operator, expr }
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

export function isDistinctFrom(left: ASTExpr, right: ASTExpr | string | number | boolean | null): ASTBinary {
  return { kind: 'binary', left, op: 'IS DISTINCT FROM', right: toExpr(right) }
}

export function isNotDistinctFrom(left: ASTExpr, right: ASTExpr | string | number | boolean | null): ASTBinary {
  return { kind: 'binary', left, op: 'IS NOT DISTINCT FROM', right: toExpr(right) }
}

// ── Pattern matching ──

export function like(left: ASTExpr, pattern: string): ASTBinary {
  return { kind: 'binary', left, op: 'LIKE', right: { kind: 'string', value: pattern } }
}

export function notLike(left: ASTExpr, pattern: string): ASTBinary {
  return { kind: 'binary', left, op: 'NOT LIKE', right: { kind: 'string', value: pattern } }
}

export function ilike(left: ASTExpr, pattern: string): ASTBinary {
  return { kind: 'binary', left, op: 'ILIKE', right: { kind: 'string', value: pattern } }
}

export function notIlike(left: ASTExpr, pattern: string): ASTBinary {
  return { kind: 'binary', left, op: 'NOT ILIKE', right: { kind: 'string', value: pattern } }
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

// ── Boolean tests ──

export function isTrue(expr: ASTExpr): ASTUnary {
  return { kind: 'unary', op: 'IS TRUE', expr }
}

export function isNotTrue(expr: ASTExpr): ASTUnary {
  return { kind: 'unary', op: 'IS NOT TRUE', expr }
}

export function isFalse(expr: ASTExpr): ASTUnary {
  return { kind: 'unary', op: 'IS FALSE', expr }
}

export function isNotFalse(expr: ASTExpr): ASTUnary {
  return { kind: 'unary', op: 'IS NOT FALSE', expr }
}

export function isUnknown(expr: ASTExpr): ASTUnary {
  return { kind: 'unary', op: 'IS UNKNOWN', expr }
}

export function isNotUnknown(expr: ASTExpr): ASTUnary {
  return { kind: 'unary', op: 'IS NOT UNKNOWN', expr }
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

export function notBetween(
  expr: ASTExpr,
  lower: ASTExpr | string | number,
  upper: ASTExpr | string | number,
): ASTBetween {
  return { kind: 'between', value: expr, op: 'NOT BETWEEN', lower: toExpr(lower), upper: toExpr(upper) }
}

export function betweenSymmetric(
  expr: ASTExpr,
  lower: ASTExpr | string | number,
  upper: ASTExpr | string | number,
): ASTBetween {
  return { kind: 'between', value: expr, op: 'BETWEEN SYMMETRIC', lower: toExpr(lower), upper: toExpr(upper) }
}

export function notBetweenSymmetric(
  expr: ASTExpr,
  lower: ASTExpr | string | number,
  upper: ASTExpr | string | number,
): ASTBetween {
  return { kind: 'between', value: expr, op: 'NOT BETWEEN SYMMETRIC', lower: toExpr(lower), upper: toExpr(upper) }
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

export function mod(left: ASTExpr, right: ASTExpr | number): ASTBinary {
  return { kind: 'binary', left, op: '%', right: toExpr(right) }
}

export function pow(left: ASTExpr, right: ASTExpr | number): ASTBinary {
  return { kind: 'binary', left, op: '^', right: toExpr(right) }
}

export function neg(expr: ASTExpr): ASTUnary {
  return { kind: 'unary', op: '-', expr }
}

// ── String operators ──

export function concat(left: ASTExpr, right: ASTExpr | string): ASTBinary {
  return { kind: 'binary', left, op: '||', right: toExpr(right) }
}

// ── Bitwise operators ──

export function bitAnd(left: ASTExpr, right: ASTExpr | number): ASTBinary {
  return { kind: 'binary', left, op: '&', right: toExpr(right) }
}

export function bitOr(left: ASTExpr, right: ASTExpr | number): ASTBinary {
  return { kind: 'binary', left, op: '|', right: toExpr(right) }
}

export function bitXor(left: ASTExpr, right: ASTExpr | number): ASTBinary {
  return { kind: 'binary', left, op: '#', right: toExpr(right) }
}

export function bitNot(expr: ASTExpr): ASTUnary {
  return { kind: 'unary', op: '~', expr }
}

export function leftShift(left: ASTExpr, right: ASTExpr | number): ASTBinary {
  return { kind: 'binary', left, op: '<<', right: toExpr(right) }
}

export function rightShift(left: ASTExpr, right: ASTExpr | number): ASTBinary {
  return { kind: 'binary', left, op: '>>', right: toExpr(right) }
}

// ── JSON operators ──

export function jsonGet(left: ASTExpr, right: ASTExpr | string | number): ASTBinary {
  return { kind: 'binary', left, op: '->', right: toExpr(right) }
}

export function jsonGetText(left: ASTExpr, right: ASTExpr | string | number): ASTBinary {
  return { kind: 'binary', left, op: '->>', right: toExpr(right) }
}

export function jsonPath(left: ASTExpr, right: ASTExpr): ASTBinary {
  return { kind: 'binary', left, op: '#>', right }
}

export function jsonPathText(left: ASTExpr, right: ASTExpr): ASTBinary {
  return { kind: 'binary', left, op: '#>>', right }
}

export function jsonContains(left: ASTExpr, right: ASTExpr): ASTBinary {
  return { kind: 'binary', left, op: '@>', right }
}

export function jsonContainedBy(left: ASTExpr, right: ASTExpr): ASTBinary {
  return { kind: 'binary', left, op: '<@', right }
}

export function jsonHasKey(left: ASTExpr, right: ASTExpr | string): ASTBinary {
  return { kind: 'binary', left, op: '?', right: toExpr(right) }
}

export function jsonHasAnyKey(left: ASTExpr, right: ASTExpr): ASTBinary {
  return { kind: 'binary', left, op: '?|', right }
}

export function jsonHasAllKeys(left: ASTExpr, right: ASTExpr): ASTBinary {
  return { kind: 'binary', left, op: '?&', right }
}

// ── Array operators ──

export function arrayOverlap(left: ASTExpr, right: ASTExpr): ASTBinary {
  return { kind: 'binary', left, op: '&&', right }
}

// ── CASE expression ──

export function caseWhen(
  whens: { when: ASTExpr; then: ASTExpr }[],
  els?: ASTExpr | string | number | boolean | null,
): ASTCase {
  return {
    kind: 'case',
    whens: whens.map((w) => ({ when: w.when, expr: w.then })),
    els: els !== undefined ? toExpr(els) : undefined,
  }
}

// ── CAST expression ──

export function cast(expr: ASTExpr, targetType: string): ASTCast {
  return { kind: 'cast', expr, targetType }
}

// ── Subquery expressions ──

export function subquery(query: ASTExpr): ASTSubquery {
  return { kind: 'subquery', query }
}

export function exists(subquery: ASTExpr): ASTExists {
  return { kind: 'exists', subquery }
}

export function inSubquery(expr: ASTExpr, query: ASTExpr): ASTInQuery {
  return { kind: 'inQuery', value: expr, op: 'IN', query }
}

export function notInSubquery(expr: ASTExpr, query: ASTExpr): ASTInQuery {
  return { kind: 'inQuery', value: expr, op: 'NOT IN', query }
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

export function stringAgg(expr: ASTExpr, separator: ASTExpr | string): ASTApply {
  return { kind: 'apply', func: 'string_agg', args: [expr, toExpr(separator)] }
}

export function arrayAgg(expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'array_agg', args: [expr] }
}

export function boolAnd(expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'bool_and', args: [expr] }
}

export function boolOr(expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'bool_or', args: [expr] }
}

export function jsonAgg(expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'json_agg', args: [expr] }
}

export function jsonObjectAgg(key: ASTExpr, value: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'json_object_agg', args: [key, value] }
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

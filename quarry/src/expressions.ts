import type { ASTExpr, ASTBinary, ASTUnary, ASTIn, ASTInQuery, ASTBetween, ASTApply, ASTCase, ASTWhen, ASTCast, ASTExists, ASTSubquery, ASTWindow, ASTOrderBy, ASTFrameSpec, ASTWith, ASTCTEDef } from './ast.js'
import type { ColumnDef, TableDef, ColumnsConfig } from './schema.js'

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

// ── Statistical aggregates ──

export function variance(expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'variance', args: [expr] }
}

export function varSamp(expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'var_samp', args: [expr] }
}

export function varPop(expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'var_pop', args: [expr] }
}

export function stddev(expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'stddev', args: [expr] }
}

export function stddevSamp(expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'stddev_samp', args: [expr] }
}

export function stddevPop(expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'stddev_pop', args: [expr] }
}

// ── Bitwise aggregates ──

export function bitAndAgg(expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'bit_and', args: [expr] }
}

export function bitOrAgg(expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'bit_or', args: [expr] }
}

export function bitXorAgg(expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'bit_xor', args: [expr] }
}

// ── EVERY (alias for bool_and) ──

export function every(expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'every', args: [expr] }
}

// ── Aggregate FILTER ──

export function filter(agg: ASTApply, condition: ASTExpr): ASTApply {
  return { ...agg, filter: condition }
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

// ── Window functions ──

export interface OverOptions {
  partitionBy?: ASTExpr[]
  orderBy?: ASTOrderBy[]
  frame?: ASTFrameSpec
}

export function over(func: ASTExpr, opts: OverOptions = {}): ASTWindow {
  return {
    kind: 'window',
    func,
    partitionBy: opts.partitionBy,
    orderBy: opts.orderBy,
    frame: opts.frame,
  }
}

export function rowNumber(opts: OverOptions = {}): ASTWindow {
  return over({ kind: 'apply', func: 'row_number', args: [] }, opts)
}

export function rank(opts: OverOptions = {}): ASTWindow {
  return over({ kind: 'apply', func: 'rank', args: [] }, opts)
}

export function denseRank(opts: OverOptions = {}): ASTWindow {
  return over({ kind: 'apply', func: 'dense_rank', args: [] }, opts)
}

export function ntile(buckets: number, opts: OverOptions = {}): ASTWindow {
  return over({ kind: 'apply', func: 'ntile', args: [{ kind: 'number', value: buckets }] }, opts)
}

export function lag(expr: ASTExpr, offset: number = 1, defaultVal?: ASTExpr | string | number | boolean | null, opts: OverOptions = {}): ASTWindow {
  const args: ASTExpr[] = [expr, { kind: 'number', value: offset }]
  if (defaultVal !== undefined) args.push(toExpr(defaultVal))
  return over({ kind: 'apply', func: 'lag', args }, opts)
}

export function lead(expr: ASTExpr, offset: number = 1, defaultVal?: ASTExpr | string | number | boolean | null, opts: OverOptions = {}): ASTWindow {
  const args: ASTExpr[] = [expr, { kind: 'number', value: offset }]
  if (defaultVal !== undefined) args.push(toExpr(defaultVal))
  return over({ kind: 'apply', func: 'lead', args }, opts)
}

export function firstValue(expr: ASTExpr, opts: OverOptions = {}): ASTWindow {
  return over({ kind: 'apply', func: 'first_value', args: [expr] }, opts)
}

export function lastValue(expr: ASTExpr, opts: OverOptions = {}): ASTWindow {
  return over({ kind: 'apply', func: 'last_value', args: [expr] }, opts)
}

export function nthValue(expr: ASTExpr, n: number, opts: OverOptions = {}): ASTWindow {
  return over({ kind: 'apply', func: 'nth_value', args: [expr, { kind: 'number', value: n }] }, opts)
}

// ── Frame bound helpers ──

export const unboundedPreceding = { kind: 'unboundedPreceding' as const }
export const unboundedFollowing = { kind: 'unboundedFollowing' as const }
export const currentRow = { kind: 'currentRow' as const }
export function preceding(n: number) { return { kind: 'preceding' as const, n } }
export function following(n: number) { return { kind: 'following' as const, n } }

// ── CTE helpers ──

export function withCTE(
  ctes: { name: string; columns?: string[]; query: ASTExpr }[],
  query: ASTExpr,
  opts?: { recursive?: boolean },
): ASTExpr {
  return {
    kind: 'with',
    ctes: ctes.map((c) => ({ name: c.name, columns: c.columns, query: c.query })),
    query,
    recursive: opts?.recursive,
  }
}

// ── Named scalar function helpers ──

// String functions
export function lower(expr: ASTExpr): ASTApply { return { kind: 'apply', func: 'lower', args: [expr] } }
export function upper(expr: ASTExpr): ASTApply { return { kind: 'apply', func: 'upper', args: [expr] } }
export function length(expr: ASTExpr): ASTApply { return { kind: 'apply', func: 'length', args: [expr] } }
export function trim(expr: ASTExpr): ASTApply { return { kind: 'apply', func: 'trim', args: [expr] } }
export function ltrim(expr: ASTExpr): ASTApply { return { kind: 'apply', func: 'ltrim', args: [expr] } }
export function rtrim(expr: ASTExpr): ASTApply { return { kind: 'apply', func: 'rtrim', args: [expr] } }
export function substring(expr: ASTExpr, from: number, len?: number): ASTApply {
  const args: ASTExpr[] = [expr, { kind: 'number', value: from }]
  if (len !== undefined) args.push({ kind: 'number', value: len })
  return { kind: 'apply', func: 'substring', args }
}
export function replace(expr: ASTExpr, from: ASTExpr | string, to: ASTExpr | string): ASTApply {
  return { kind: 'apply', func: 'replace', args: [expr, toExpr(from), toExpr(to)] }
}
export function concatWs(separator: string, ...exprs: ASTExpr[]): ASTApply {
  return { kind: 'apply', func: 'concat_ws', args: [{ kind: 'string', value: separator }, ...exprs] }
}
export function reverse(expr: ASTExpr): ASTApply { return { kind: 'apply', func: 'reverse', args: [expr] } }
export function repeat(expr: ASTExpr, n: number): ASTApply {
  return { kind: 'apply', func: 'repeat', args: [expr, { kind: 'number', value: n }] }
}
export function lpad(expr: ASTExpr, len: number, fill?: string): ASTApply {
  const args: ASTExpr[] = [expr, { kind: 'number', value: len }]
  if (fill !== undefined) args.push({ kind: 'string', value: fill })
  return { kind: 'apply', func: 'lpad', args }
}
export function rpad(expr: ASTExpr, len: number, fill?: string): ASTApply {
  const args: ASTExpr[] = [expr, { kind: 'number', value: len }]
  if (fill !== undefined) args.push({ kind: 'string', value: fill })
  return { kind: 'apply', func: 'rpad', args }
}

// Math functions
export function abs(expr: ASTExpr): ASTApply { return { kind: 'apply', func: 'abs', args: [expr] } }
export function ceil(expr: ASTExpr): ASTApply { return { kind: 'apply', func: 'ceil', args: [expr] } }
export function floor(expr: ASTExpr): ASTApply { return { kind: 'apply', func: 'floor', args: [expr] } }
export function round(expr: ASTExpr, scale?: number): ASTApply {
  const args: ASTExpr[] = [expr]
  if (scale !== undefined) args.push({ kind: 'number', value: scale })
  return { kind: 'apply', func: 'round', args }
}
export function trunc(expr: ASTExpr, scale?: number): ASTApply {
  const args: ASTExpr[] = [expr]
  if (scale !== undefined) args.push({ kind: 'number', value: scale })
  return { kind: 'apply', func: 'trunc', args }
}
export function sqrt(expr: ASTExpr): ASTApply { return { kind: 'apply', func: 'sqrt', args: [expr] } }
export function sign(expr: ASTExpr): ASTApply { return { kind: 'apply', func: 'sign', args: [expr] } }
export function random(): ASTApply { return { kind: 'apply', func: 'random', args: [] } }
export function greatest(...exprs: (ASTExpr | string | number)[]): ASTApply {
  return { kind: 'apply', func: 'greatest', args: exprs.map(toExpr) }
}
export function least(...exprs: (ASTExpr | string | number)[]): ASTApply {
  return { kind: 'apply', func: 'least', args: exprs.map(toExpr) }
}

// Null-handling
export function coalesce(...exprs: (ASTExpr | string | number | boolean | null)[]): ASTApply {
  return { kind: 'apply', func: 'coalesce', args: exprs.map(toExpr) }
}
export function nullif(expr1: ASTExpr, expr2: ASTExpr | string | number | boolean | null): ASTApply {
  return { kind: 'apply', func: 'nullif', args: [expr1, toExpr(expr2)] }
}

// Date/time
export function now(): ASTApply { return { kind: 'apply', func: 'now', args: [] } }
export function currentDate(): ASTApply { return { kind: 'apply', func: 'current_date', args: [] } }
export function currentTime(): ASTApply { return { kind: 'apply', func: 'current_time', args: [] } }
export function datePart(part: string, expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'date_part', args: [{ kind: 'string', value: part }, expr] }
}
export function dateTrunc(part: string, expr: ASTExpr): ASTApply {
  return { kind: 'apply', func: 'date_trunc', args: [{ kind: 'string', value: part }, expr] }
}
export function toChar(expr: ASTExpr, format: string): ASTApply {
  return { kind: 'apply', func: 'to_char', args: [expr, { kind: 'string', value: format }] }
}

// UUID
export function genRandomUuid(): ASTApply { return { kind: 'apply', func: 'gen_random_uuid', args: [] } }

// ── Internal ──

function toExpr(value: ASTExpr | string | number | boolean | null): ASTExpr {
  if (value === null) return { kind: 'null' }
  if (typeof value === 'object' && 'kind' in value) return value as ASTExpr
  if (typeof value === 'string') return { kind: 'string', value }
  if (typeof value === 'number') return { kind: 'number', value }
  if (typeof value === 'boolean') return { kind: 'boolean', value }
  return { kind: 'null' }
}

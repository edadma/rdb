export type { ASTExpr, ASTCommand, ASTOrderBy, ASTCase, ASTWhen, ASTCast, ASTExists, ASTSubquery, ASTInQuery, ASTSelect, ASTOnConflict, ASTOnConflictDoNothing, ASTOnConflictDoUpdate } from './ast.js'
export {
  serial,
  bigserial,
  text,
  varchar,
  char,
  integer,
  smallint,
  bigint,
  doublePrecision,
  real,
  numeric,
  boolean,
  uuid,
  timestamp,
  timestamptz,
  date,
  time,
  timetz,
  interval,
  json,
  bytea,
  table,
} from './schema.js'
export type { ColumnDef, TableDef, ColumnsConfig, InferSelect, InferInsert, Nullable } from './schema.js'
export {
  col,
  // Generic operator helpers
  op,
  unaryOp,
  // Comparison
  eq,
  ne,
  gt,
  gte,
  lt,
  lte,
  isDistinctFrom,
  isNotDistinctFrom,
  // Pattern matching
  like,
  notLike,
  ilike,
  notIlike,
  // Logical
  and,
  or,
  not,
  // Null checks
  isNull,
  isNotNull,
  // Boolean tests
  isTrue,
  isNotTrue,
  isFalse,
  isNotFalse,
  isUnknown,
  isNotUnknown,
  // IN / BETWEEN
  inList,
  notInList,
  between,
  notBetween,
  betweenSymmetric,
  notBetweenSymmetric,
  // Arithmetic
  add,
  sub,
  mul,
  div,
  mod,
  pow,
  neg,
  // String operators
  concat,
  // Bitwise operators
  bitAnd,
  bitOr,
  bitXor,
  bitNot,
  leftShift,
  rightShift,
  // JSON operators
  jsonGet,
  jsonGetText,
  jsonPath,
  jsonPathText,
  jsonContains,
  jsonContainedBy,
  jsonHasKey,
  jsonHasAnyKey,
  jsonHasAllKeys,
  // Array operators
  arrayOverlap,
  // CASE / CAST / EXISTS / Subqueries
  caseWhen,
  cast,
  subquery,
  exists,
  inSubquery,
  notInSubquery,
  // Aggregate functions
  count,
  sum,
  avg,
  min,
  max,
  stringAgg,
  arrayAgg,
  boolAnd,
  boolOr,
  jsonAgg,
  jsonObjectAgg,
  // Scalar functions
  fn,
  // Alias / Literal
  alias,
  literal,
} from './expressions.js'
export { quarry, asc, desc, QuarryDB, SelectBuilder, InsertBuilder, UpdateBuilder, DeleteBuilder } from './builder.js'
export type { QuarrySession } from './builder.js'

// Compile-time type tests — run with: tsc --noEmit -p test/tsconfig.json
// All @ts-expect-error lines MUST produce errors (if they don't, the test fails)

import {
  table,
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
  eq,
  ne,
  gt,
  gte,
  lt,
  lte,
  and,
  or,
  not,
  isNull,
  isNotNull,
  isDistinctFrom,
  isNotDistinctFrom,
  like,
  notLike,
  ilike,
  notIlike,
  isTrue,
  isNotTrue,
  isFalse,
  isNotFalse,
  isUnknown,
  isNotUnknown,
  inList,
  notInList,
  between,
  notBetween,
  betweenSymmetric,
  notBetweenSymmetric,
  add,
  sub,
  mul,
  div,
  mod,
  pow,
  neg,
  concat,
  bitAnd,
  bitOr,
  bitXor,
  bitNot,
  leftShift,
  rightShift,
  jsonGet,
  jsonGetText,
  jsonPath,
  jsonPathText,
  jsonContains,
  jsonContainedBy,
  jsonHasKey,
  jsonHasAnyKey,
  jsonHasAllKeys,
  arrayOverlap,
  op,
  unaryOp,
  caseWhen,
  cast,
  exists,
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
  variance,
  varSamp,
  varPop,
  stddev,
  stddevSamp,
  stddevPop,
  bitAndAgg,
  bitOrAgg,
  bitXorAgg,
  every,
  filter,
  fn,
  alias,
  literal,
  asc,
  desc,
  quarry,
  InsertSelectBuilder,
  tableAs,
  TableName,
  Columns,
  OriginalName,
} from '@petradb/quarry'
import type {
  ASTExpr,
  ASTColumn,
  ASTApply,
  ASTCase,
  ASTCast,
  ASTExists,
  InferSelect,
  InferInsert,
  Nullable,
  TableDef,
  ColumnDef,
  QuarrySession,
} from '@petradb/quarry'

// ══════════════════════════════════════════════════════════════════════
// 1. SCHEMA COLUMN TYPES — verify each type infers the correct TS type
// ══════════════════════════════════════════════════════════════════════

const allTypes = table('all_types', {
  // Auto-increment integers
  a: serial('a').primaryKey(),
  b: bigserial('b'),
  // Strings
  c: text('c').notNull(),
  d: varchar('d', 100),
  e: char('e', 10),
  // Integers
  f: integer('f'),
  g: smallint('g'),
  h: bigint('h'),
  // Floating point
  i: doublePrecision('i'),
  j: real('j'),
  k: numeric('k', 10, 2),
  // Boolean
  l: boolean('l').notNull(),
  // UUID
  m: uuid('m'),
  // Date/Time
  n: timestamp('n'),
  o: timestamptz('o'),
  p: date('p'),
  q: time('q'),
  r: timetz('r'),
  s: interval('s'),
  // JSON
  t: json('t'),
  // Binary
  u: bytea('u'),
})

type AllSelect = InferSelect<typeof allTypes>

// serial → number (notNull via primaryKey)
const _checkA: number = undefined as unknown as AllSelect['a']

// bigserial → number (notNull)
const _checkB: number = undefined as unknown as AllSelect['b']

// text notNull → string
const _checkC: string = undefined as unknown as AllSelect['c']

// varchar nullable → string | null
const _checkD: string | null = undefined as unknown as AllSelect['d']

// char nullable → string | null
const _checkE: string | null = undefined as unknown as AllSelect['e']

// integer nullable → number | null
const _checkF: number | null = undefined as unknown as AllSelect['f']

// smallint nullable → number | null
const _checkG: number | null = undefined as unknown as AllSelect['g']

// bigint nullable → number | null
const _checkH: number | null = undefined as unknown as AllSelect['h']

// doublePrecision nullable → number | null
const _checkI: number | null = undefined as unknown as AllSelect['i']

// real nullable → number | null
const _checkJ: number | null = undefined as unknown as AllSelect['j']

// numeric nullable → number | null
const _checkK: number | null = undefined as unknown as AllSelect['k']

// boolean notNull → boolean
const _checkL: boolean = undefined as unknown as AllSelect['l']

// uuid nullable → string | null
const _checkM: string | null = undefined as unknown as AllSelect['m']

// timestamp nullable → string | null
const _checkN: string | null = undefined as unknown as AllSelect['n']

// timestamptz nullable → string | null
const _checkO: string | null = undefined as unknown as AllSelect['o']

// date nullable → string | null
const _checkP: string | null = undefined as unknown as AllSelect['p']

// time nullable → string | null
const _checkQ: string | null = undefined as unknown as AllSelect['q']

// timetz nullable → string | null
const _checkR: string | null = undefined as unknown as AllSelect['r']

// interval nullable → string | null
const _checkS: string | null = undefined as unknown as AllSelect['s']

// json nullable → unknown | null
const _checkT: unknown | null = undefined as unknown as AllSelect['t']

// bytea nullable → number[] | null
const _checkU: number[] | null = undefined as unknown as AllSelect['u']

// Negative: serial cannot be null
// @ts-expect-error — serial is notNull
const _badA: AllSelect['a'] = null as null

// Negative: text notNull cannot be null
// @ts-expect-error — text notNull
const _badC: AllSelect['c'] = null as null

// Negative: boolean notNull cannot be null
// @ts-expect-error — boolean notNull
const _badL: AllSelect['l'] = null as null

// Negative: wrong type for integer field
// @ts-expect-error — integer is number, not string
const _badF: AllSelect = { a: 1, b: 1, c: 'x', d: null, e: null, f: 'wrong' as any as string, g: null, h: null, i: null, j: null, k: null, l: true, m: null, n: null, o: null, p: null, q: null, r: null, s: null, t: null, u: null }

// ══════════════════════════════════════════════════════════════════════
// 2. INFERINSERT — required vs optional with new types
// ══════════════════════════════════════════════════════════════════════

type AllInsert = InferInsert<typeof allTypes>

// Positive: minimal insert — only c (notNull, no default) and l (notNull, no default) required
// a is serial (has default), b is bigserial (has default)
const ins1: AllInsert = { c: 'hello', l: true }

// Positive: with optional fields
const ins2: AllInsert = { c: 'hello', l: true, d: 'varchar', f: 42, m: '550e8400-e29b-41d4-a716-446655440000' }

// Positive: nullable fields accept null in insert
const ins3: AllInsert = { c: 'hello', l: true, d: null, f: null, t: null }

// Negative: missing c (required)
// @ts-expect-error — c is required (notNull, no default)
const badIns1: AllInsert = { l: true }

// Negative: missing l (required)
// @ts-expect-error — l is required (notNull, no default)
const badIns2: AllInsert = { c: 'hello' }

// Negative: wrong type for uuid
// @ts-expect-error — uuid is string, not number
const badIns3: AllInsert = { c: 'x', l: true, m: 42 }

// ══════════════════════════════════════════════════════════════════════
// 3. ORIGINAL TABLE TESTS (users, posts, comments)
// ══════════════════════════════════════════════════════════════════════

const users = table('users', {
  id: serial('id').primaryKey(),
  name: text('name').notNull(),
  email: text('email').notNull().unique(),
  age: integer('age'),
  bio: text('bio'),
  active: boolean('active').notNull().default(true),
})

const posts = table('posts', {
  id: serial('id').primaryKey(),
  userId: integer('user_id').notNull(),
  title: text('title').notNull(),
  body: text('body'),
})

const comments = table('comments', {
  id: serial('id').primaryKey(),
  postId: integer('post_id').notNull(),
  authorId: integer('author_id'),
  content: text('content').notNull(),
})

type UserSelect = InferSelect<typeof users>
type UserInsert = InferInsert<typeof users>

// Positive: correct types
const user1: UserSelect = { id: 1, name: 'Alice', email: 'a@b.com', age: 30, bio: 'hello', active: true }
const user2: UserSelect = { id: 2, name: 'Bob', email: 'b@b.com', age: null, bio: null, active: false }

// Positive: nullable fields accept null
const _ageNull: UserSelect['age'] = null
const _bioNull: UserSelect['bio'] = null

// Positive: non-null fields are the correct type
const _id: number = user1.id
const _name: string = user1.name
const _active: boolean = user1.active

// Negative tests
// @ts-expect-error — id cannot be null
const badUser1: UserSelect = { id: null, name: 'X', email: 'x@x.com', age: null, bio: null, active: true }
// @ts-expect-error — name cannot be null
const badUser2: UserSelect = { id: 1, name: null, email: 'x@x.com', age: null, bio: null, active: true }
// @ts-expect-error — active cannot be null
const badUser3: UserSelect = { id: 1, name: 'X', email: 'x@x.com', age: null, bio: null, active: null }
// @ts-expect-error — missing required field 'name'
const badUser4: UserSelect = { id: 1, email: 'x@x.com', age: null, bio: null, active: true }
// @ts-expect-error — wrong type for name
const badUser5: UserSelect = { id: 1, name: 42, email: 'x@x.com', age: null, bio: null, active: true }
// @ts-expect-error — wrong type for active
const badUser6: UserSelect = { id: 1, name: 'X', email: 'x@x.com', age: null, bio: null, active: 'true' }

// InferInsert
const uIns1: UserInsert = { name: 'Alice', email: 'a@b.com' }
const uIns2: UserInsert = { name: 'Alice', email: 'a@b.com', age: 30, active: false }
const uIns3: UserInsert = { id: 99, name: 'Manual', email: 'm@b.com' }
const uIns4: UserInsert = { name: 'N', email: 'n@b.com', age: null, bio: null }
// @ts-expect-error — name is required
const uBadIns1: UserInsert = { email: 'a@b.com' }
// @ts-expect-error — email is required
const uBadIns2: UserInsert = { name: 'Alice' }
// @ts-expect-error — name must be string
const uBadIns3: UserInsert = { name: 123, email: 'a@b.com' }
// @ts-expect-error — active must be boolean
const uBadIns4: UserInsert = { name: 'X', email: 'x@x.com', active: 'yes' }

// ══════════════════════════════════════════════════════════════════════
// 5. EXPRESSION COMPOSITION — all operators return ASTExpr-compatible
// ══════════════════════════════════════════════════════════════════════

// Comparison
const _e1: ASTExpr = eq(users.id, 1)
const _e2: ASTExpr = ne(users.id, 1)
const _e3: ASTExpr = gt(users.age, 20)
const _e4: ASTExpr = gte(users.age, 20)
const _e5: ASTExpr = lt(users.age, 20)
const _e6: ASTExpr = lte(users.age, 20)
const _e7: ASTExpr = isDistinctFrom(users.age, null)
const _e8: ASTExpr = isNotDistinctFrom(users.age, null)

// Pattern matching
const _e9: ASTExpr = like(users.name, '%a%')
const _e10: ASTExpr = notLike(users.name, '%a%')
const _e11: ASTExpr = ilike(users.name, '%a%')
const _e12: ASTExpr = notIlike(users.name, '%a%')

// Logical
const _e13: ASTExpr = and(_e1, _e2, _e3)
const _e14: ASTExpr = or(_e1, _e2, _e3)
const _e15: ASTExpr = not(_e1)

// Null checks
const _e16: ASTExpr = isNull(users.age)
const _e17: ASTExpr = isNotNull(users.age)

// Boolean tests
const _e18: ASTExpr = isTrue(users.active)
const _e19: ASTExpr = isNotTrue(users.active)
const _e20: ASTExpr = isFalse(users.active)
const _e21: ASTExpr = isNotFalse(users.active)
const _e22: ASTExpr = isUnknown(users.active)
const _e23: ASTExpr = isNotUnknown(users.active)

// IN / BETWEEN
const _e24: ASTExpr = inList(users.name, ['Alice', 'Bob'])
const _e25: ASTExpr = notInList(users.name, ['Alice', 'Bob'])
const _e26: ASTExpr = between(users.age, 20, 30)
const _e27: ASTExpr = notBetween(users.age, 20, 30)
const _e28: ASTExpr = betweenSymmetric(users.age, 30, 20)
const _e29: ASTExpr = notBetweenSymmetric(users.age, 30, 20)

// Arithmetic
const _e30: ASTExpr = add(users.age, 10)
const _e31: ASTExpr = sub(users.age, 10)
const _e32: ASTExpr = mul(users.age, 2)
const _e33: ASTExpr = div(users.age, 2)
const _e34: ASTExpr = mod(users.age, 2)
const _e35: ASTExpr = pow(users.age, 2)
const _e36: ASTExpr = neg(users.age)

// String
const _e37: ASTExpr = concat(users.name, ' suffix')

// Bitwise
const _e38: ASTExpr = bitAnd(users.age, 0xFF)
const _e39: ASTExpr = bitOr(users.age, 1)
const _e40: ASTExpr = bitXor(users.age, 1)
const _e41: ASTExpr = bitNot(users.age)
const _e42: ASTExpr = leftShift(users.age, 2)
const _e43: ASTExpr = rightShift(users.age, 2)

// JSON
const _e44: ASTExpr = jsonGet(users.name, 'key')
const _e45: ASTExpr = jsonGetText(users.name, 'key')
const _e46: ASTExpr = jsonPath(users.name, users.name)
const _e47: ASTExpr = jsonPathText(users.name, users.name)
const _e48: ASTExpr = jsonContains(users.name, users.name)
const _e49: ASTExpr = jsonContainedBy(users.name, users.name)
const _e50: ASTExpr = jsonHasKey(users.name, 'key')
const _e51: ASTExpr = jsonHasAnyKey(users.name, users.name)
const _e52: ASTExpr = jsonHasAllKeys(users.name, users.name)

// Array
const _e53: ASTExpr = arrayOverlap(users.name, users.name)

// Generic
const _e54: ASTExpr = op(users.age, '@@', 'test')
const _e55: ASTExpr = unaryOp('~', users.age)

// Aggregates
const _e56: ASTExpr = count()
const _e57: ASTExpr = count(users.age)
const _e58: ASTExpr = sum(users.age)
const _e59: ASTExpr = avg(users.age)
const _e60: ASTExpr = min(users.age)
const _e61: ASTExpr = max(users.age)
const _e62: ASTExpr = stringAgg(users.name, ', ')
const _e63: ASTExpr = arrayAgg(users.name)
const _e64: ASTExpr = boolAnd(users.active)
const _e65: ASTExpr = boolOr(users.active)
const _e66: ASTExpr = jsonAgg(users.name)
const _e67: ASTExpr = jsonObjectAgg(users.name, users.age)

// fn() for arbitrary functions
const _e68: ASTExpr = fn('upper', users.name)
const _e69: ASTExpr = fn('coalesce', users.age, 0)

// Alias and literal
const _e70: ASTExpr = alias(count(), 'total')
const _e71: ASTExpr = literal('hello')
const _e72: ASTExpr = literal(42)
const _e73: ASTExpr = literal(true)
const _e74: ASTExpr = literal(null)

// Nested composition — all expression types compose with each other
const _complex: ASTExpr = and(
  or(eq(users.name, 'A'), like(users.email, '%@test%')),
  not(isNull(users.age)),
  between(add(users.age, 1), 20, 40),
  gt(fn('length', users.name), 3),
)

// ══════════════════════════════════════════════════════════════════════
// 6. CASE / CAST / EXISTS — return correct AST types
// ══════════════════════════════════════════════════════════════════════

// caseWhen returns ASTCase
const _case1: ASTCase = caseWhen(
  [{ when: gt(users.age, 30), then: literal('old') }],
  'young',
)

// caseWhen without else
const _case2: ASTCase = caseWhen([{ when: isNull(users.age), then: literal('unknown') }])

// caseWhen is also ASTExpr (usable in any expression context)
const _case3: ASTExpr = caseWhen([{ when: gt(users.age, 30), then: literal('old') }], 'young')

// cast returns ASTCast
const _cast1: ASTCast = cast(users.age, 'text')
const _cast2: ASTExpr = cast(users.age, 'double')

// exists returns ASTExists
const _exists1: ASTExists = exists({ kind: 'select', exprs: [{ kind: 'star' }], from: [{ kind: 'table', name: 'users' }] })
const _exists2: ASTExpr = exists({ kind: 'select', exprs: [{ kind: 'star' }], from: [{ kind: 'table', name: 'users' }] })

// All usable as where conditions
const _whereCase: ASTExpr = eq(_case3, 'old')
const _whereCast: ASTExpr = gt(_cast2, 30)

// ══════════════════════════════════════════════════════════════════════
// 7. BUILDER RETURN TYPES
// ══════════════════════════════════════════════════════════════════════

declare const mockSession: QuarrySession
const db = quarry(mockSession)

// -- Select returns array of InferSelect --

const selectResult = db.select(users).execute()
type SelectReturn = Awaited<typeof selectResult>
const _sr: SelectReturn = undefined as unknown as UserSelect[]

// -- Insert returns array of InferSelect --

const insertResult = db.insert(users).values({ name: 'A', email: 'a@b.com' }).execute()
type InsertReturn = Awaited<typeof insertResult>
// Verify it's an array
const _ir: InsertReturn = undefined as unknown as InferSelect<typeof users>[]
const _irElem: InferSelect<typeof users> = undefined as unknown as InsertReturn[number]

// -- Update returns { rowCount, rows } --

const updateResult = db.update(users).set({ age: 31 }).where(eq(users.id, 1)).execute()
type UpdateReturn = Awaited<typeof updateResult>
const _ur: UpdateReturn = { rowCount: 1, rows: [{ id: 1, name: 'A', email: 'a@b.com', age: 31, bio: null, active: true }] }
const _urCount: number = _ur.rowCount
const _urRows: InferSelect<typeof users>[] = _ur.rows

// -- Delete returns { rowCount, rows } --

const deleteResult = db.delete(users).where(eq(users.id, 1)).execute()
type DeleteReturn = Awaited<typeof deleteResult>
const _dr: DeleteReturn = { rowCount: 1, rows: [{ id: 1, name: 'A', email: 'a@b.com', age: null, bio: null, active: true }] }
const _drCount: number = _dr.rowCount
const _drRows: InferSelect<typeof users>[] = _dr.rows

// -- Transaction returns the callback's return type --

const txResult1 = db.transaction(async (tx) => {
  const rows = await tx.select(users).execute()
  return rows.length
})
type TxReturn1 = Awaited<typeof txResult1>
const _tx1: number = undefined as unknown as TxReturn1

const txResult2 = db.transaction(async (tx) => {
  const [row] = await tx.insert(users).values({ name: 'A', email: 'a@b.com' }).execute()
  return row
})
type TxReturn2 = Awaited<typeof txResult2>
const _tx2: InferSelect<typeof users> = undefined as unknown as TxReturn2

// ══════════════════════════════════════════════════════════════════════
// 8. JOIN RESULT TYPES
// ══════════════════════════════════════════════════════════════════════

// Inner join: result is intersection
const innerJoinQuery = db.select(users).innerJoin(posts, eq(users.id, posts.userId))
type InnerJoinResult = Awaited<ReturnType<typeof innerJoinQuery.execute>>[number]

const ijRow: InnerJoinResult = {
  id: 1, name: 'Alice', email: 'a@b.com', age: 30, bio: null, active: true,
  userId: 1, title: 'Hello', body: 'World',
}
const _ijAge: InnerJoinResult['age'] = null
const _ijBody: InnerJoinResult['body'] = null
const _ijTitle: string = ijRow.title
const _ijName: string = ijRow.name
// @ts-expect-error — title is string, not string | null
const _badIjTitle: InnerJoinResult['title'] = null as null
// @ts-expect-error — name is string, not string | null
const _badIjName: InnerJoinResult['name'] = null as null

// Left join: joined table columns become nullable
const leftJoinQuery = db.select(users).leftJoin(posts, eq(users.id, posts.userId))
type LeftJoinResult = Awaited<ReturnType<typeof leftJoinQuery.execute>>[number]

const ljRowNulls: LeftJoinResult = {
  id: 1, name: 'Alice', email: 'a@b.com', age: null, bio: null, active: true,
  userId: null, title: null, body: null,
}
const _ljTitle: string | null = ljRowNulls.title
const _ljUserId: number | null = ljRowNulls.userId
// @ts-expect-error — name is string (base table, not affected by left join)
const _badLjName: LeftJoinResult['name'] = null as null

// Multi-join: inner + left
const multiJoinQuery = db
  .select(users)
  .innerJoin(posts, eq(users.id, posts.userId))
  .leftJoin(comments, eq(posts.id, comments.postId))
type MultiJoinResult = Awaited<ReturnType<typeof multiJoinQuery.execute>>[number]

const mjRow: MultiJoinResult = {
  id: 1, name: 'Alice', email: 'a@b.com', age: 30, bio: null, active: true,
  userId: 1, title: 'Hello', body: 'World',
  postId: 1, authorId: 5, content: 'Nice!',
}
const mjRowNulls: MultiJoinResult = {
  id: 1, name: 'Alice', email: 'a@b.com', age: null, bio: null, active: true,
  userId: 1, title: 'Hello', body: null,
  postId: null, authorId: null, content: null,
}
const _mjContent: string | null = mjRowNulls.content
const _mjTitle: string = mjRow.title
// @ts-expect-error — title is string (inner join keeps notNull)
const _badMjTitle: MultiJoinResult['title'] = null as null

// Chaining preserves types
const chainedQuery = db
  .select(users)
  .innerJoin(posts, eq(users.id, posts.userId))
  .where(eq(users.name, 'Alice'))
  .orderBy(asc(posts.title))
  .limit(10)
type ChainedResult = Awaited<ReturnType<typeof chainedQuery.execute>>[number]
const _chainedName: string = undefined as unknown as ChainedResult['name']
const _chainedTitle: string = undefined as unknown as ChainedResult['title']

// Nullable utility
type NullablePosts = Nullable<InferSelect<typeof posts>>
const np: NullablePosts = { id: null, userId: null, title: null, body: null }

// Double left join
const doubleLeftQuery = db
  .select(users)
  .leftJoin(posts, eq(users.id, posts.userId))
  .leftJoin(comments, eq(posts.id, comments.postId))
type DoubleLeftResult = Awaited<ReturnType<typeof doubleLeftQuery.execute>>[number]
const dlRow: DoubleLeftResult = {
  id: 1, name: 'Alice', email: 'a@b.com', age: null, bio: null, active: true,
  userId: null, title: null, body: null,
  postId: null, authorId: null, content: null,
}
// @ts-expect-error — base table name still not nullable
const _badDlName: DoubleLeftResult['name'] = null as null

// ══════════════════════════════════════════════════════════════════════
// 9. TABLE DEFINITION TYPES
// ══════════════════════════════════════════════════════════════════════

type UsersTable = typeof users
type _CheckTableDef = UsersTable extends TableDef<'users', any> ? true : never
const _checkTable: _CheckTableDef = true

type _IdCol = (typeof users)[typeof Columns]['id']
type _CheckSerial = _IdCol extends ColumnDef<'id', number, true, true> ? true : never
const _checkSerial: _CheckSerial = true

type _NameCol = (typeof users)[typeof Columns]['name']
type _CheckText = _NameCol extends ColumnDef<'name', string, true, false> ? true : never
const _checkText: _CheckText = true

type _AgeCol = (typeof users)[typeof Columns]['age']
type _CheckNullable = _AgeCol extends ColumnDef<'age', number, false, false> ? true : never
const _checkNullable: _CheckNullable = true

type _ActiveCol = (typeof users)[typeof Columns]['active']
type _CheckDefault = _ActiveCol extends ColumnDef<'active', boolean, true, true> ? true : never
const _checkDefault: _CheckDefault = true

// New type ColumnDef checks
type _UuidCol = (typeof allTypes)[typeof Columns]['m']
type _CheckUuid = _UuidCol extends ColumnDef<'m', string, false, false> ? true : never
const _checkUuid: _CheckUuid = true

type _JsonCol = (typeof allTypes)[typeof Columns]['t']
type _CheckJson = _JsonCol extends ColumnDef<'t', unknown, false, false> ? true : never
const _checkJson: _CheckJson = true

type _ByteaCol = (typeof allTypes)[typeof Columns]['u']
type _CheckBytea = _ByteaCol extends ColumnDef<'u', number[], false, false> ? true : never
const _checkBytea: _CheckBytea = true

type _BigserialCol = (typeof allTypes)[typeof Columns]['b']
type _CheckBigserial = _BigserialCol extends ColumnDef<'b', number, true, true> ? true : never
const _checkBigserial: _CheckBigserial = true

// ══════════════════════════════════════════════════════════════════════
// 10. QUARRYSESSION INTERFACE
// ══════════════════════════════════════════════════════════════════════

// QuarrySession requires both executeAST and execute
type _CheckExecuteAST = QuarrySession['executeAST']
type _CheckExecute = QuarrySession['execute']

// Session from @petradb/engine satisfies QuarrySession shape
// (can't import it here, but the interface is what matters)
const _mockSatisfies: QuarrySession = {
  executeAST: async () => [],
  execute: async () => [],
}

// ══════════════════════════════════════════════════════════════════════
// 11. SUBQUERY TYPES
// ══════════════════════════════════════════════════════════════════════

import type { ASTSubquery, ASTInQuery, ASTSelect } from '@petradb/quarry'
import { subquery, inSubquery, notInSubquery } from '@petradb/quarry'

// -- SelectBuilder.toExpr() returns ASTExpr (specifically ASTSelect) --

const selectExpr = db.select(users).columns(users.id).toExpr()
const _seKind: typeof selectExpr.kind = 'select'
const _seAssign: ASTExpr = selectExpr

// toExpr preserves clauses
const selectExprFull = db
  .select(users)
  .columns(users.id)
  .where(gt(users.age, 20))
  .limit(10)
  .toExpr()
const _sefAssign: ASTExpr = selectExprFull

// toAST wraps in ASTQueryCommand — different from toExpr
const selectAST = db.select(users).toAST()
const _saKind: typeof selectAST.kind = 'query'

// -- subquery() returns ASTSubquery --

const subq = subquery(selectExpr)
const _subqType: ASTSubquery = subq
const _subqExpr: ASTExpr = subq // ASTSubquery is ASTExpr
const _subqKind: typeof subq.kind = 'subquery'

// -- inSubquery() returns ASTInQuery --

const inSubq = inSubquery(users.id, selectExpr)
const _inSubqType: ASTInQuery = inSubq
const _inSubqExpr: ASTExpr = inSubq // ASTInQuery is ASTExpr
const _inSubqKind: typeof inSubq.kind = 'inQuery'

// -- notInSubquery() returns ASTInQuery --

const notInSubq = notInSubquery(users.id, selectExpr)
const _notInSubqType: ASTInQuery = notInSubq
const _notInSubqExpr: ASTExpr = notInSubq
const _notInSubqKind: typeof notInSubq.kind = 'inQuery'

// -- Subqueries compose with other expressions --

const _subqWhere: ASTExpr = gt(users.age, subq)
const _inSubqWhere: ASTExpr = and(inSubq, gt(users.age, 20))
const _existsSubq: ASTExpr = exists(selectExpr)

// -- Subquery can be aliased --

const _aliasedSubq: ASTExpr = alias(subq, 'max_val')

// -- Negative: subquery() requires ASTExpr, not random objects --

// @ts-expect-error — subquery requires ASTExpr, not a string
const _badSubq1 = subquery('SELECT 1')

// @ts-expect-error — inSubquery requires ASTExpr for both args
const _badSubq2 = inSubquery('id', selectExpr)

// @ts-expect-error — inSubquery requires ASTExpr for query arg
const _badSubq3 = inSubquery(users.id, 'SELECT id FROM users')

// @ts-expect-error — notInSubquery requires ASTExpr for query arg
const _badSubq4 = notInSubquery(users.id, 42)

// ══════════════════════════════════════════════════════════════════════
// 12. UPSERT TYPES
// ══════════════════════════════════════════════════════════════════════

import type { ASTOnConflict, ASTOnConflictDoNothing, ASTOnConflictDoUpdate } from '@petradb/quarry'

// -- onConflictDoNothing() returns this (chainable) --

const _upsertChain1 = db.insert(users).values({ name: 'A', email: 'a@b.com' }).onConflictDoNothing()
// Can still call execute after onConflictDoNothing
const _upsertExec1: Promise<InferSelect<typeof users>[]> = _upsertChain1.execute()

// -- onConflictDoUpdate() returns this (chainable) --

const _upsertChain2 = db
  .insert(users)
  .values({ name: 'A', email: 'a@b.com' })
  .onConflictDoUpdate(['email'], { name: 'B' })
// Can still call execute after onConflictDoUpdate
const _upsertExec2: Promise<InferSelect<typeof users>[]> = _upsertChain2.execute()

// -- onConflictDoUpdate conflict columns are type-safe --

// @ts-expect-error — 'nonexistent' is not a column key
db.insert(users).values({ name: 'A', email: 'a@b.com' }).onConflictDoUpdate(['nonexistent'], { name: 'B' })

// -- onConflictDoUpdate updates are type-safe (Partial<InferSelect>) --

// Positive: partial update with correct types
db.insert(users).values({ name: 'A', email: 'a@b.com' }).onConflictDoUpdate(['email'], { name: 'B', age: 30 })

// Positive: update with null on nullable field
db.insert(users).values({ name: 'A', email: 'a@b.com' }).onConflictDoUpdate(['email'], { age: null })

// -- AST types --

const _ocDoNothing: ASTOnConflictDoNothing = { kind: 'doNothing' }
const _ocDoUpdate: ASTOnConflictDoUpdate = { kind: 'doUpdate', conflictColumns: ['email'], updates: [{ col: 'name', value: { kind: 'string', value: 'X' } }] }
const _ocUnion: ASTOnConflict = _ocDoNothing
const _ocUnion2: ASTOnConflict = _ocDoUpdate

// -- onConflict in ASTInsertCommand --

const upsertAST = db.insert(users).values({ name: 'A', email: 'a@b.com' }).onConflictDoNothing().toAST()
const _upsertOC: typeof upsertAST.onConflict = { kind: 'doNothing' }

// -- Chaining order: values -> onConflict -> returning -> execute --

const _fullChain = db
  .insert(users)
  .values({ name: 'A', email: 'a@b.com' })
  .onConflictDoNothing()
  .returning(users.id)
  .execute()

// ══════════════════════════════════════════════════════════════════════
// 13. TABLE ALIASES
// ══════════════════════════════════════════════════════════════════════

// -- as() returns a TableDef with the alias name --

const u1 = tableAs(users, 'u1')
type U1Name = typeof u1[typeof TableName]
const _u1NameCheck: U1Name = 'u1'
// @ts-expect-error — alias name is 'u1', not 'users'
const _u1BadName: U1Name = 'users' as 'users'

// -- Aliased table preserves column types --

type U1Select = InferSelect<typeof u1>
const _u1Row: U1Select = { id: 1, name: 'Alice', email: 'a@b.com', age: 30, bio: null, active: true }
// @ts-expect-error — name is still required string
const _u1BadRow: U1Select = { id: 1, name: null, email: 'a@b.com', age: null, bio: null, active: true }

// -- Direct access on aliased table is type-safe --

const _u1Col = u1.name
const _u1ColExpr: ASTExpr = _u1Col
// @ts-expect-error — 'nonexistent' is not a column
const _u1BadCol = u1.nonexistent

// -- Two aliases have different name types --

const u2 = tableAs(users, 'u2')
type U2Name = typeof u2[typeof TableName]
const _u2NameCheck: U2Name = 'u2'

// -- Aliased table works with select --

const aliasedSelect = db.select(u1).execute()
type AliasedSelectResult = Awaited<typeof aliasedSelect>
const _asrCheck: AliasedSelectResult = undefined as unknown as InferSelect<typeof u1>[]

// -- Self-join preserves both aliases' types --

const selfJoinQuery = db.select(u1).innerJoin(u2, eq(u1.id, u2.id))
type SelfJoinResult = Awaited<ReturnType<typeof selfJoinQuery.execute>>[number]
// Both aliases have InferSelect<users> shape
const _sjName: string = undefined as unknown as SelfJoinResult['name']
const _sjAge: number | null = undefined as unknown as SelfJoinResult['age']

// -- Left join with alias makes joined columns nullable --

const leftAliasQuery = db.select(u1).leftJoin(u2, eq(u1.id, u2.id))
type LeftAliasResult = Awaited<ReturnType<typeof leftAliasQuery.execute>>[number]
const _laName: string | null = undefined as unknown as LeftAliasResult['name'] // nullable from left join

// -- OriginalName exists on aliased tables --

const _origName: string | undefined = u1[OriginalName]

// -- tableAs is chainable (re-aliasing) --

const u3 = tableAs(u1, 'u3')
type U3Name = typeof u3[typeof TableName]
const _u3NameCheck: U3Name = 'u3'

// ══════════════════════════════════════════════════════════════════════
// 14. RIGHT / FULL / CROSS JOIN RESULT TYPES
// ══════════════════════════════════════════════════════════════════════

// Right join: base table columns become nullable, joined table columns are not
const rightJoinQuery = db.select(users).rightJoin(posts, eq(users.id, posts.userId))
type RightJoinResult = Awaited<ReturnType<typeof rightJoinQuery.execute>>[number]

// Base table (users) becomes nullable after right join
const _rjName: string | null = undefined as unknown as RightJoinResult['name']
const _rjAge: number | null = undefined as unknown as RightJoinResult['age']

// Joined table (posts) is NOT nullable
const _rjTitle: string = undefined as unknown as RightJoinResult['title']
// @ts-expect-error — posts.title is notNull (right side of right join)
const _badRjTitle: RightJoinResult['title'] = null as null

// Full join: both sides become nullable
const fullJoinQuery = db.select(users).fullJoin(posts, eq(users.id, posts.userId))
type FullJoinResult = Awaited<ReturnType<typeof fullJoinQuery.execute>>[number]

// Both sides become nullable
const _fjName: string | null = undefined as unknown as FullJoinResult['name']
const _fjTitle: string | null = undefined as unknown as FullJoinResult['title']
const _fjUserId: number | null = undefined as unknown as FullJoinResult['userId']

// Cross join: no nullability change
const crossJoinQuery = db.select(users).crossJoin(posts)
type CrossJoinResult = Awaited<ReturnType<typeof crossJoinQuery.execute>>[number]

const _cjName: string = undefined as unknown as CrossJoinResult['name']
const _cjTitle: string = undefined as unknown as CrossJoinResult['title']
// @ts-expect-error — name is still notNull in cross join
const _badCjName: CrossJoinResult['name'] = null as null
// @ts-expect-error — title is still notNull in cross join
const _badCjTitle: CrossJoinResult['title'] = null as null

// Cross join takes no on argument
// @ts-expect-error — crossJoin only takes a table, not an on condition
db.select(users).crossJoin(posts, eq(users.id, posts.userId))

// ══════════════════════════════════════════════════════════════════════
// 15. NULLS FIRST / LAST
// ══════════════════════════════════════════════════════════════════════

import type { ASTOrderBy } from '@petradb/quarry'

// asc/desc accept optional nulls option
const _ob1: ASTOrderBy = asc(users.id)
const _ob2: ASTOrderBy = asc(users.id, { nulls: 'first' })
const _ob3: ASTOrderBy = desc(users.id, { nulls: 'last' })

// @ts-expect-error — invalid nulls value
const _obBad: ASTOrderBy = asc(users.id, { nulls: 'middle' })

// ══════════════════════════════════════════════════════════════════════
// 16. STATISTICAL / BITWISE AGGREGATES + EVERY + FILTER
// ══════════════════════════════════════════════════════════════════════

// All return ASTApply (which extends ASTExpr)
const _eVariance: ASTApply = variance(users.age)
const _eVarSamp: ASTApply = varSamp(users.age)
const _eVarPop: ASTApply = varPop(users.age)
const _eStddev: ASTApply = stddev(users.age)
const _eStddevSamp: ASTApply = stddevSamp(users.age)
const _eStddevPop: ASTApply = stddevPop(users.age)
const _eBitAndAgg: ASTApply = bitAndAgg(users.age)
const _eBitOrAgg: ASTApply = bitOrAgg(users.age)
const _eBitXorAgg: ASTApply = bitXorAgg(users.age)
const _eEvery: ASTApply = every(users.active)

// All are also ASTExpr
const _eVarExpr: ASTExpr = variance(users.age)
const _eEveryExpr: ASTExpr = every(users.active)

// filter wraps an aggregate with a condition
const _filtered: ASTApply = filter(count(), gt(users.age, 20))
const _filteredExpr: ASTExpr = filter(sum(users.age), eq(users.active, true))

// filter preserves the original aggregate's fields
const _filteredFunc: string = _filtered.func
const _filteredArgs: ASTExpr[] = _filtered.args
const _filteredFilter: ASTExpr | undefined = _filtered.filter

// ══════════════════════════════════════════════════════════════════════
// 17. DISTINCT ON
// ══════════════════════════════════════════════════════════════════════

// distinctOn returns SelectBuilder with same result type
const distinctOnQuery = db.select(users).distinctOn(users.name)
type DistinctOnResult = Awaited<ReturnType<typeof distinctOnQuery.execute>>[number]
const _doName: string = undefined as unknown as DistinctOnResult['name']
const _doAge: number | null = undefined as unknown as DistinctOnResult['age']

// distinctOn accepts multiple expressions
const _doMulti = db.select(users).distinctOn(users.name, users.active)

// distinctOn is chainable with other clauses
const _doChained = db
  .select(users)
  .distinctOn(users.name)
  .orderBy(asc(users.name))
  .limit(10)

// ══════════════════════════════════════════════════════════════════════
// 18. INSERT...SELECT
// ══════════════════════════════════════════════════════════════════════

// insertFrom returns InsertSelectBuilder with correct table type
const insertFromBuilder = db.insertFrom(
  users,
  db.select(users).columns(users.name, users.email).toExpr(),
  ['name', 'email'],
)
const _isb: InsertSelectBuilder<typeof users> = insertFromBuilder

// insertFrom execute returns InferSelect<T>[]
const insertFromResult = insertFromBuilder.execute()
type InsertFromReturn = Awaited<typeof insertFromResult>
const _ifrCheck: InsertFromReturn = undefined as unknown as InferSelect<typeof users>[]

// insertFrom columns are type-safe
// @ts-expect-error — 'nonexistent' is not a column key
db.insertFrom(users, db.select(users).toExpr(), ['nonexistent'])

// insertFrom without columns is valid
const _isfNoCol = db.insertFrom(users, db.select(users).toExpr())

// insertFrom supports chaining
const _isfChain = db
  .insertFrom(users, db.select(users).toExpr(), ['name', 'email'])
  .onConflictDoNothing()
  .returning(users.id)

// ══════════════════════════════════════════════════════════════════════
// 19. UPDATE...FROM
// ══════════════════════════════════════════════════════════════════════

import type { ASTUpdateCommand, ASTDeleteCommand } from '@petradb/quarry'

// from() is chainable on UpdateBuilder
const _ufChain = db
  .update(users)
  .set({ age: 30 })
  .from(posts)
  .where(eq(users.id, posts.userId))

// from() accepts multiple tables
const _ufMulti = db
  .update(users)
  .set({ age: 30 })
  .from(posts, comments)
  .where(eq(users.id, posts.userId))

// AST has from field
const ufAST = db.update(users).set({ age: 30 }).from(posts).where(eq(users.id, 1)).toAST()
const _ufFrom: ASTExpr[] | undefined = ufAST.from

// ══════════════════════════════════════════════════════════════════════
// 20. DELETE...USING
// ══════════════════════════════════════════════════════════════════════

// using() is chainable on DeleteBuilder
const _duChain = db
  .delete(users)
  .using(posts)
  .where(eq(users.id, posts.userId))

// using() accepts multiple tables
const _duMulti = db
  .delete(users)
  .using(posts, comments)
  .where(eq(users.id, posts.userId))

// AST has using field
const duAST = db.delete(users).using(posts).where(eq(users.id, 1)).toAST()
const _duUsing: ASTExpr[] | undefined = duAST.using

// ══════════════════════════════════════════════════════════════════════
// 21. DIRECT COLUMN ACCESS (no col() wrapper needed)
// ══════════════════════════════════════════════════════════════════════

// Direct access returns ASTColumn (which is ASTExpr)
const _directId: ASTColumn = users.id
const _directName: ASTColumn = users.name
const _directAge: ASTColumn = users.age
const _directActive: ASTColumn = users.active

// ASTColumn is assignable to ASTExpr
const _directAsExpr: ASTExpr = users.id

// Direct access has correct kind
const _directKind: 'column' = users.id.kind

// Direct access works in expressions — no col() needed
const _directEq: ASTExpr = eq(users.name, 'Alice')
const _directGt: ASTExpr = gt(users.age, 21)
const _directAnd: ASTExpr = and(eq(users.active, true), gt(users.age, 18))

// Direct access works in builders
const _directSelect = db.select(users).where(eq(users.name, 'Alice')).columns(users.name, users.email)
const _directOrderBy = db.select(users).orderBy(asc(users.name))

// @ts-expect-error — 'nonexistent' is not a column
const _badDirect = users.nonexistent

// Direct access works as ASTExpr
const _directAsExpr2: ASTExpr = users.name

// ══════════════════════════════════════════════════════════════════════
// 22. ALIASED TABLE COLUMN ACCESS
// ══════════════════════════════════════════════════════════════════════

// Aliased tables have direct column access too
const _u1Direct: ASTColumn = u1.name
const _u1DirectId: ASTColumn = u1.id

// Aliased table columns use the alias name
const _u1DirectExpr: ASTExpr = u1.name

// Direct access on aliased tables returns ASTExpr
const _u1ColLegacy: ASTExpr = u1.name

// Direct access on aliased tables works in expressions
const _u1Eq: ASTExpr = eq(u1.name, 'Alice')

// Self-join with direct access
const sjDirect = db
  .select(u1)
  .columns(u1.name, u2.name)
  .innerJoin(u2, eq(u1.id, u2.id))

// ══════════════════════════════════════════════════════════════════════
// 23. EDGE CASE: TABLE WITH COLUMNS NAMED LIKE INTERNAL PROPERTIES
// ══════════════════════════════════════════════════════════════════════

// A table with columns that could collide with old string-keyed internals
const edgeTable = table('edge', {
  name: text('name').notNull(),
  id: serial('id').primaryKey(),
  table: text('table_name'),
})

// These are column accessors, not internal properties
const _edgeName: ASTColumn = edgeTable.name
const _edgeId: ASTColumn = edgeTable.id
const _edgeTable: ASTColumn = edgeTable.table

// Internal properties are accessed via Symbols
type EdgeName = typeof edgeTable[typeof TableName]
const _edgeTableName: EdgeName = 'edge'

// InferSelect still works
type EdgeSelect = InferSelect<typeof edgeTable>
const _edgeRow: EdgeSelect = { name: 'x', id: 1, table: null }

// tableAs works on edge-case tables
const edgeAlias = tableAs(edgeTable, 'e')
const _edgeAliasName: ASTColumn = edgeAlias.name
const _edgeAliasTable: ASTColumn = edgeAlias.table

// ══════════════════════════════════════════════════════════════════════
// 24. SYMBOL-KEYED INTERNAL ACCESS
// ══════════════════════════════════════════════════════════════════════

// TableName returns the literal table name type
type UsersTableName = typeof users[typeof TableName]
const _usersName: UsersTableName = 'users'
// @ts-expect-error — name is 'users', not 'posts'
const _badUsersName: UsersTableName = 'posts' as 'posts'

// Columns returns the columns config
type UsersCols = typeof users[typeof Columns]
type _CheckId = UsersCols['id'] extends ColumnDef<'id', number, true, true> ? true : never
const _checkIdOk: _CheckId = true

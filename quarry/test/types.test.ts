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
  col,
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
  fn,
  alias,
  literal,
  asc,
  desc,
  quarry,
} from '@petradb/quarry'
import type {
  ASTExpr,
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
// 4. col() TYPE SAFETY
// ══════════════════════════════════════════════════════════════════════

const _colId = col(users, 'id')
const _colName = col(users, 'name')
const _colAge = col(users, 'age')
// @ts-expect-error — 'nonexistent' is not a column
const _badCol = col(users, 'nonexistent')
// @ts-expect-error — 'title' is a posts column, not users
const _wrongTable = col(users, 'title')
const _postTitle = col(posts, 'title')
const _postUserId = col(posts, 'userId')

// ══════════════════════════════════════════════════════════════════════
// 5. EXPRESSION COMPOSITION — all operators return ASTExpr-compatible
// ══════════════════════════════════════════════════════════════════════

// Comparison
const _e1: ASTExpr = eq(col(users, 'id'), 1)
const _e2: ASTExpr = ne(col(users, 'id'), 1)
const _e3: ASTExpr = gt(col(users, 'age'), 20)
const _e4: ASTExpr = gte(col(users, 'age'), 20)
const _e5: ASTExpr = lt(col(users, 'age'), 20)
const _e6: ASTExpr = lte(col(users, 'age'), 20)
const _e7: ASTExpr = isDistinctFrom(col(users, 'age'), null)
const _e8: ASTExpr = isNotDistinctFrom(col(users, 'age'), null)

// Pattern matching
const _e9: ASTExpr = like(col(users, 'name'), '%a%')
const _e10: ASTExpr = notLike(col(users, 'name'), '%a%')
const _e11: ASTExpr = ilike(col(users, 'name'), '%a%')
const _e12: ASTExpr = notIlike(col(users, 'name'), '%a%')

// Logical
const _e13: ASTExpr = and(_e1, _e2, _e3)
const _e14: ASTExpr = or(_e1, _e2, _e3)
const _e15: ASTExpr = not(_e1)

// Null checks
const _e16: ASTExpr = isNull(col(users, 'age'))
const _e17: ASTExpr = isNotNull(col(users, 'age'))

// Boolean tests
const _e18: ASTExpr = isTrue(col(users, 'active'))
const _e19: ASTExpr = isNotTrue(col(users, 'active'))
const _e20: ASTExpr = isFalse(col(users, 'active'))
const _e21: ASTExpr = isNotFalse(col(users, 'active'))
const _e22: ASTExpr = isUnknown(col(users, 'active'))
const _e23: ASTExpr = isNotUnknown(col(users, 'active'))

// IN / BETWEEN
const _e24: ASTExpr = inList(col(users, 'name'), ['Alice', 'Bob'])
const _e25: ASTExpr = notInList(col(users, 'name'), ['Alice', 'Bob'])
const _e26: ASTExpr = between(col(users, 'age'), 20, 30)
const _e27: ASTExpr = notBetween(col(users, 'age'), 20, 30)
const _e28: ASTExpr = betweenSymmetric(col(users, 'age'), 30, 20)
const _e29: ASTExpr = notBetweenSymmetric(col(users, 'age'), 30, 20)

// Arithmetic
const _e30: ASTExpr = add(col(users, 'age'), 10)
const _e31: ASTExpr = sub(col(users, 'age'), 10)
const _e32: ASTExpr = mul(col(users, 'age'), 2)
const _e33: ASTExpr = div(col(users, 'age'), 2)
const _e34: ASTExpr = mod(col(users, 'age'), 2)
const _e35: ASTExpr = pow(col(users, 'age'), 2)
const _e36: ASTExpr = neg(col(users, 'age'))

// String
const _e37: ASTExpr = concat(col(users, 'name'), ' suffix')

// Bitwise
const _e38: ASTExpr = bitAnd(col(users, 'age'), 0xFF)
const _e39: ASTExpr = bitOr(col(users, 'age'), 1)
const _e40: ASTExpr = bitXor(col(users, 'age'), 1)
const _e41: ASTExpr = bitNot(col(users, 'age'))
const _e42: ASTExpr = leftShift(col(users, 'age'), 2)
const _e43: ASTExpr = rightShift(col(users, 'age'), 2)

// JSON
const _e44: ASTExpr = jsonGet(col(users, 'name'), 'key')
const _e45: ASTExpr = jsonGetText(col(users, 'name'), 'key')
const _e46: ASTExpr = jsonPath(col(users, 'name'), col(users, 'name'))
const _e47: ASTExpr = jsonPathText(col(users, 'name'), col(users, 'name'))
const _e48: ASTExpr = jsonContains(col(users, 'name'), col(users, 'name'))
const _e49: ASTExpr = jsonContainedBy(col(users, 'name'), col(users, 'name'))
const _e50: ASTExpr = jsonHasKey(col(users, 'name'), 'key')
const _e51: ASTExpr = jsonHasAnyKey(col(users, 'name'), col(users, 'name'))
const _e52: ASTExpr = jsonHasAllKeys(col(users, 'name'), col(users, 'name'))

// Array
const _e53: ASTExpr = arrayOverlap(col(users, 'name'), col(users, 'name'))

// Generic
const _e54: ASTExpr = op(col(users, 'age'), '@@', 'test')
const _e55: ASTExpr = unaryOp('~', col(users, 'age'))

// Aggregates
const _e56: ASTExpr = count()
const _e57: ASTExpr = count(col(users, 'age'))
const _e58: ASTExpr = sum(col(users, 'age'))
const _e59: ASTExpr = avg(col(users, 'age'))
const _e60: ASTExpr = min(col(users, 'age'))
const _e61: ASTExpr = max(col(users, 'age'))
const _e62: ASTExpr = stringAgg(col(users, 'name'), ', ')
const _e63: ASTExpr = arrayAgg(col(users, 'name'))
const _e64: ASTExpr = boolAnd(col(users, 'active'))
const _e65: ASTExpr = boolOr(col(users, 'active'))
const _e66: ASTExpr = jsonAgg(col(users, 'name'))
const _e67: ASTExpr = jsonObjectAgg(col(users, 'name'), col(users, 'age'))

// fn() for arbitrary functions
const _e68: ASTExpr = fn('upper', col(users, 'name'))
const _e69: ASTExpr = fn('coalesce', col(users, 'age'), 0)

// Alias and literal
const _e70: ASTExpr = alias(count(), 'total')
const _e71: ASTExpr = literal('hello')
const _e72: ASTExpr = literal(42)
const _e73: ASTExpr = literal(true)
const _e74: ASTExpr = literal(null)

// Nested composition — all expression types compose with each other
const _complex: ASTExpr = and(
  or(eq(col(users, 'name'), 'A'), like(col(users, 'email'), '%@test%')),
  not(isNull(col(users, 'age'))),
  between(add(col(users, 'age'), 1), 20, 40),
  gt(fn('length', col(users, 'name')), 3),
)

// ══════════════════════════════════════════════════════════════════════
// 6. CASE / CAST / EXISTS — return correct AST types
// ══════════════════════════════════════════════════════════════════════

// caseWhen returns ASTCase
const _case1: ASTCase = caseWhen(
  [{ when: gt(col(users, 'age'), 30), then: literal('old') }],
  'young',
)

// caseWhen without else
const _case2: ASTCase = caseWhen([{ when: isNull(col(users, 'age')), then: literal('unknown') }])

// caseWhen is also ASTExpr (usable in any expression context)
const _case3: ASTExpr = caseWhen([{ when: gt(col(users, 'age'), 30), then: literal('old') }], 'young')

// cast returns ASTCast
const _cast1: ASTCast = cast(col(users, 'age'), 'text')
const _cast2: ASTExpr = cast(col(users, 'age'), 'double')

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

const updateResult = db.update(users).set({ age: 31 }).where(eq(col(users, 'id'), 1)).execute()
type UpdateReturn = Awaited<typeof updateResult>
const _ur: UpdateReturn = { rowCount: 1, rows: [{ id: 1, name: 'A', email: 'a@b.com', age: 31, bio: null, active: true }] }
const _urCount: number = _ur.rowCount
const _urRows: InferSelect<typeof users>[] = _ur.rows

// -- Delete returns { rowCount, rows } --

const deleteResult = db.delete(users).where(eq(col(users, 'id'), 1)).execute()
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
const innerJoinQuery = db.select(users).innerJoin(posts, eq(col(users, 'id'), col(posts, 'userId')))
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
const leftJoinQuery = db.select(users).leftJoin(posts, eq(col(users, 'id'), col(posts, 'userId')))
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
  .innerJoin(posts, eq(col(users, 'id'), col(posts, 'userId')))
  .leftJoin(comments, eq(col(posts, 'id'), col(comments, 'postId')))
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
  .innerJoin(posts, eq(col(users, 'id'), col(posts, 'userId')))
  .where(eq(col(users, 'name'), 'Alice'))
  .orderBy(asc(col(posts, 'title')))
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
  .leftJoin(posts, eq(col(users, 'id'), col(posts, 'userId')))
  .leftJoin(comments, eq(col(posts, 'id'), col(comments, 'postId')))
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

type _IdCol = (typeof users)['_columns']['id']
type _CheckSerial = _IdCol extends ColumnDef<'id', number, true, true> ? true : never
const _checkSerial: _CheckSerial = true

type _NameCol = (typeof users)['_columns']['name']
type _CheckText = _NameCol extends ColumnDef<'name', string, true, false> ? true : never
const _checkText: _CheckText = true

type _AgeCol = (typeof users)['_columns']['age']
type _CheckNullable = _AgeCol extends ColumnDef<'age', number, false, false> ? true : never
const _checkNullable: _CheckNullable = true

type _ActiveCol = (typeof users)['_columns']['active']
type _CheckDefault = _ActiveCol extends ColumnDef<'active', boolean, true, true> ? true : never
const _checkDefault: _CheckDefault = true

// New type ColumnDef checks
type _UuidCol = (typeof allTypes)['_columns']['m']
type _CheckUuid = _UuidCol extends ColumnDef<'m', string, false, false> ? true : never
const _checkUuid: _CheckUuid = true

type _JsonCol = (typeof allTypes)['_columns']['t']
type _CheckJson = _JsonCol extends ColumnDef<'t', unknown, false, false> ? true : never
const _checkJson: _CheckJson = true

type _ByteaCol = (typeof allTypes)['_columns']['u']
type _CheckBytea = _ByteaCol extends ColumnDef<'u', number[], false, false> ? true : never
const _checkBytea: _CheckBytea = true

type _BigserialCol = (typeof allTypes)['_columns']['b']
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

const selectExpr = db.select(users).columns(col(users, 'id')).toExpr()
const _seKind: typeof selectExpr.kind = 'select'
const _seAssign: ASTExpr = selectExpr

// toExpr preserves clauses
const selectExprFull = db
  .select(users)
  .columns(col(users, 'id'))
  .where(gt(col(users, 'age'), 20))
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

const inSubq = inSubquery(col(users, 'id'), selectExpr)
const _inSubqType: ASTInQuery = inSubq
const _inSubqExpr: ASTExpr = inSubq // ASTInQuery is ASTExpr
const _inSubqKind: typeof inSubq.kind = 'inQuery'

// -- notInSubquery() returns ASTInQuery --

const notInSubq = notInSubquery(col(users, 'id'), selectExpr)
const _notInSubqType: ASTInQuery = notInSubq
const _notInSubqExpr: ASTExpr = notInSubq
const _notInSubqKind: typeof notInSubq.kind = 'inQuery'

// -- Subqueries compose with other expressions --

const _subqWhere: ASTExpr = gt(col(users, 'age'), subq)
const _inSubqWhere: ASTExpr = and(inSubq, gt(col(users, 'age'), 20))
const _existsSubq: ASTExpr = exists(selectExpr)

// -- Subquery can be aliased --

const _aliasedSubq: ASTExpr = alias(subq, 'max_val')

// -- Negative: subquery() requires ASTExpr, not random objects --

// @ts-expect-error — subquery requires ASTExpr, not a string
const _badSubq1 = subquery('SELECT 1')

// @ts-expect-error — inSubquery requires ASTExpr for both args
const _badSubq2 = inSubquery('id', selectExpr)

// @ts-expect-error — inSubquery requires ASTExpr for query arg
const _badSubq3 = inSubquery(col(users, 'id'), 'SELECT id FROM users')

// @ts-expect-error — notInSubquery requires ASTExpr for query arg
const _badSubq4 = notInSubquery(col(users, 'id'), 42)

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
  .returning(col(users, 'id'))
  .execute()

// ══════════════════════════════════════════════════════════════════════
// 13. TABLE ALIASES
// ══════════════════════════════════════════════════════════════════════

// -- as() returns a TableDef with the alias name --

const u1 = users.as('u1')
type U1Name = typeof u1._name
const _u1NameCheck: U1Name = 'u1'
// @ts-expect-error — alias name is 'u1', not 'users'
const _u1BadName: U1Name = 'users'

// -- Aliased table preserves column types --

type U1Select = InferSelect<typeof u1>
const _u1Row: U1Select = { id: 1, name: 'Alice', email: 'a@b.com', age: 30, bio: null, active: true }
// @ts-expect-error — name is still required string
const _u1BadRow: U1Select = { id: 1, name: null, email: 'a@b.com', age: null, bio: null, active: true }

// -- col() with aliased table is type-safe --

const _u1Col = col(u1, 'name')
const _u1ColExpr: ASTExpr = _u1Col
// @ts-expect-error — 'nonexistent' is not a column
const _u1BadCol = col(u1, 'nonexistent')

// -- Two aliases have different name types --

const u2 = users.as('u2')
type U2Name = typeof u2._name
const _u2NameCheck: U2Name = 'u2'

// -- Aliased table works with select --

const aliasedSelect = db.select(u1).execute()
type AliasedSelectResult = Awaited<typeof aliasedSelect>
const _asrCheck: AliasedSelectResult = undefined as unknown as InferSelect<typeof u1>[]

// -- Self-join preserves both aliases' types --

const selfJoinQuery = db.select(u1).innerJoin(u2, eq(col(u1, 'id'), col(u2, 'id')))
type SelfJoinResult = Awaited<ReturnType<typeof selfJoinQuery.execute>>[number]
// Both aliases have InferSelect<users> shape
const _sjName: string = undefined as unknown as SelfJoinResult['name']
const _sjAge: number | null = undefined as unknown as SelfJoinResult['age']

// -- Left join with alias makes joined columns nullable --

const leftAliasQuery = db.select(u1).leftJoin(u2, eq(col(u1, 'id'), col(u2, 'id')))
type LeftAliasResult = Awaited<ReturnType<typeof leftAliasQuery.execute>>[number]
const _laName: string | null = undefined as unknown as LeftAliasResult['name'] // nullable from left join

// -- _originalName exists on aliased tables --

const _origName: string | undefined = u1._originalName

// -- as() is chainable (re-aliasing) --

const u3 = u1.as('u3')
type U3Name = typeof u3._name
const _u3NameCheck: U3Name = 'u3'

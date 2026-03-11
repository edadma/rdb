// Compile-time type tests — run with: tsc --noEmit -p test/tsconfig.json
// All @ts-expect-error lines MUST produce errors (if they don't, the test fails)

import {
  table,
  serial,
  text,
  integer,
  boolean,
  col,
  eq,
  gt,
  and,
  or,
  count,
  alias,
  asc,
  desc,
  literal,
  isNull,
  inList,
  between,
  fn,
  like,
} from '@petradb/quarry'
import type { InferSelect, InferInsert, TableDef, ColumnDef } from '@petradb/quarry'

// ── Schema definition ──

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

// ── InferSelect tests ──

type UserSelect = InferSelect<typeof users>

// Positive: correct types compile
const user1: UserSelect = { id: 1, name: 'Alice', email: 'a@b.com', age: 30, bio: 'hello', active: true }
const user2: UserSelect = { id: 2, name: 'Bob', email: 'b@b.com', age: null, bio: null, active: false }

// Positive: nullable fields accept null
const _ageNull: UserSelect['age'] = null
const _bioNull: UserSelect['bio'] = null

// Positive: non-null fields are the correct type
const _id: number = user1.id
const _name: string = user1.name
const _active: boolean = user1.active

// Negative: non-null field cannot be null
// @ts-expect-error — id cannot be null (serial, notNull)
const badUser1: UserSelect = { id: null, name: 'X', email: 'x@x.com', age: null, bio: null, active: true }

// @ts-expect-error — name cannot be null (notNull)
const badUser2: UserSelect = { id: 1, name: null, email: 'x@x.com', age: null, bio: null, active: true }

// @ts-expect-error — active cannot be null (notNull)
const badUser3: UserSelect = { id: 1, name: 'X', email: 'x@x.com', age: null, bio: null, active: null }

// @ts-expect-error — missing required field 'name'
const badUser4: UserSelect = { id: 1, email: 'x@x.com', age: null, bio: null, active: true }

// @ts-expect-error — wrong type for name (number instead of string)
const badUser5: UserSelect = { id: 1, name: 42, email: 'x@x.com', age: null, bio: null, active: true }

// @ts-expect-error — wrong type for active (string instead of boolean)
const badUser6: UserSelect = { id: 1, name: 'X', email: 'x@x.com', age: null, bio: null, active: 'true' }

// ── InferInsert tests ──

type UserInsert = InferInsert<typeof users>

// Positive: minimal insert — only required non-default fields
const ins1: UserInsert = { name: 'Alice', email: 'a@b.com' }

// Positive: with optional fields
const ins2: UserInsert = { name: 'Alice', email: 'a@b.com', age: 30, active: false }

// Positive: serial id is optional (has default)
const ins3: UserInsert = { id: 99, name: 'Manual', email: 'm@b.com' }

// Positive: nullable fields accept null in insert
const ins4: UserInsert = { name: 'N', email: 'n@b.com', age: null, bio: null }

// Negative: missing required field 'name'
// @ts-expect-error — name is required (notNull, no default)
const badIns1: UserInsert = { email: 'a@b.com' }

// Negative: missing required field 'email'
// @ts-expect-error — email is required (notNull, no default)
const badIns2: UserInsert = { name: 'Alice' }

// Negative: wrong type for name
// @ts-expect-error — name must be string
const badIns3: UserInsert = { name: 123, email: 'a@b.com' }

// Negative: active has default but accepts boolean, not string
// @ts-expect-error — active must be boolean
const badIns4: UserInsert = { name: 'X', email: 'x@x.com', active: 'yes' }

// ── col() type safety ──

// Positive: valid column names
const _colId = col(users, 'id')
const _colName = col(users, 'name')
const _colAge = col(users, 'age')

// Negative: invalid column name
// @ts-expect-error — 'nonexistent' is not a column of users
const _badCol = col(users, 'nonexistent')

// Negative: column from wrong table
// @ts-expect-error — 'title' is a posts column, not users
const _wrongTable = col(users, 'title')

// Positive: posts columns work
const _postTitle = col(posts, 'title')
const _postUserId = col(posts, 'userId')

// ── Expression type compatibility ──

// All these should compile — expressions compose freely
const _expr1 = eq(col(users, 'id'), 1)
const _expr2 = gt(col(users, 'age'), 20)
const _expr3 = and(_expr1, _expr2)
const _expr4 = or(_expr1, _expr2)
const _expr5 = isNull(col(users, 'age'))
const _expr6 = inList(col(users, 'name'), ['Alice', 'Bob'])
const _expr7 = between(col(users, 'age'), 20, 30)
const _expr8 = like(col(users, 'name'), '%alice%')
const _expr9 = fn('upper', col(users, 'name'))
const _expr10 = alias(count(), 'total')
const _expr11 = asc(col(users, 'name'))
const _expr12 = desc(col(users, 'id'))

// Literal values auto-convert
const _expr13 = eq(col(users, 'name'), 'Alice') // string literal
const _expr14 = eq(col(users, 'id'), 42) // number literal
const _expr15 = eq(col(users, 'active'), true) // boolean literal
const _expr16 = eq(col(users, 'age'), null) // null literal

// ── Table definition types ──

// Verify table is correctly typed
type UsersTable = typeof users
type _CheckTableDef = UsersTable extends TableDef<'users', any> ? true : never
const _checkTable: _CheckTableDef = true

// Verify column types
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

// ── PostSelect ──

type PostSelect = InferSelect<typeof posts>
const post1: PostSelect = { id: 1, userId: 1, title: 'Hello', body: 'World' }
const post2: PostSelect = { id: 2, userId: 1, title: 'Test', body: null } // body is nullable

// @ts-expect-error — title cannot be null
const badPost: PostSelect = { id: 1, userId: 1, title: null, body: null }

type PostInsert = InferInsert<typeof posts>
const postIns1: PostInsert = { userId: 1, title: 'Hello' } // minimal, body optional
const postIns2: PostInsert = { userId: 1, title: 'Hello', body: 'World' }

// @ts-expect-error — userId is required
const badPostIns: PostInsert = { title: 'Hello' }

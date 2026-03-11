# @petradb/quarry

Type-safe query builder for PetraDB that generates AST objects instead of SQL strings, bypassing the parser entirely.

## Features

- **Schema as source of truth** — one definition drives DDL, queries, and TypeScript types
- **Compile-time type safety** — `InferSelect`, `InferInsert`, typed `col()` references, typed join results
- **AST-based** — builds plain JS objects that map directly to engine internals, no SQL parsing overhead
- **Typed joins** — inner joins preserve nullability, left joins make joined columns nullable

## Install

```bash
npm install @petradb/quarry @petradb/engine
```

## Quick Start

```typescript
import { Session } from '@petradb/engine'
import { quarry, table, serial, text, integer, boolean, col, eq, asc } from '@petradb/quarry'
import type { InferSelect, InferInsert } from '@petradb/quarry'

// Define schema
const users = table('users', {
  id: serial('id').primaryKey(),
  name: text('name').notNull(),
  email: text('email').notNull().unique(),
  age: integer('age'),
  active: boolean('active').notNull().default(true),
})

// Inferred types
type User = InferSelect<typeof users>
//   { id: number, name: string, email: string, age: number | null, active: boolean }

type NewUser = InferInsert<typeof users>
//   { name: string, email: string, age?: number | null, active?: boolean, id?: number }

// Connect
const session = new Session()
const db = quarry(session)

// Create table
await db.createTable(users)

// Insert
const user = await db.insert(users).values({ name: 'Alice', email: 'alice@example.com', age: 30 }).execute()

// Select
const rows = await db.select(users)
  .where(eq(col(users, 'active'), true))
  .orderBy(asc(col(users, 'name')))
  .limit(10)
  .execute()

// Update
await db.update(users)
  .set({ age: 31 })
  .where(eq(col(users, 'name'), 'Alice'))
  .execute()

// Delete
await db.delete(users)
  .where(eq(col(users, 'id'), 1))
  .execute()
```

## Schema Definition

Column types: `serial`, `text`, `integer`, `boolean`

Column modifiers: `.notNull()`, `.default(value)`, `.primaryKey()`, `.unique()`, `.references(table, column)`

```typescript
const posts = table('posts', {
  id: serial('id').primaryKey(),
  userId: integer('user_id').notNull().references('users', 'id'),
  title: text('title').notNull(),
  body: text('body'),
  published: boolean('published').notNull().default(false),
})
```

## Joins

Join result types are inferred automatically:

```typescript
// Inner join — both tables' columns in result, nullability preserved
const rows = await db.select(users)
  .innerJoin(posts, eq(col(users, 'id'), col(posts, 'userId')))
  .execute()
// type: (User & Post)[]

// Left join — joined table's columns become nullable
const rows = await db.select(users)
  .leftJoin(posts, eq(col(users, 'id'), col(posts, 'userId')))
  .execute()
// type: (User & { id: number | null, userId: number | null, title: string | null, ... })[]

// Chained joins
const rows = await db.select(users)
  .innerJoin(posts, eq(col(users, 'id'), col(posts, 'userId')))
  .leftJoin(comments, eq(col(posts, 'id'), col(comments, 'postId')))
  .execute()
// posts columns: non-null, comments columns: nullable
```

## Expressions

```typescript
import { eq, ne, gt, gte, lt, lte, like, ilike } from '@petradb/quarry'
import { and, or, not, isNull, isNotNull } from '@petradb/quarry'
import { inList, notInList, between } from '@petradb/quarry'
import { add, sub, mul, div } from '@petradb/quarry'
import { count, sum, avg, min, max, fn, alias } from '@petradb/quarry'

// Comparisons
eq(col(users, 'name'), 'Alice')
gt(col(users, 'age'), 21)

// Logical
and(eq(col(users, 'active'), true), gt(col(users, 'age'), 18))
or(eq(col(users, 'name'), 'Alice'), eq(col(users, 'name'), 'Bob'))

// Null checks
isNull(col(users, 'age'))

// Collections
inList(col(users, 'name'), ['Alice', 'Bob', 'Charlie'])
between(col(users, 'age'), 18, 65)

// Aggregates with groupBy
db.select(users)
  .columns(col(users, 'active'), alias(count(), 'total'))
  .groupBy(col(users, 'active'))

// Functions
fn('upper', col(users, 'name'))
fn('coalesce', col(users, 'age'), 0)
```

## AST Inspection

Every builder has a `.toAST()` method that returns the raw AST without executing:

```typescript
const ast = db.select(users)
  .where(eq(col(users, 'name'), 'Alice'))
  .toAST()

// { kind: 'query', query: { kind: 'select', exprs: [...], from: [...], where: { kind: 'binary', ... } } }
```

## How It Works

Quarry builds plain JavaScript objects (discriminated unions with a `kind` field) that represent the query AST. When you call `.execute()`, these objects are passed to the engine's `executeAST()` method, which converts them directly into the engine's internal Scala AST — skipping SQL parsing entirely.

```
Schema definition → Builder API → JS AST objects → Engine AST → Rewrite → Execute
                                        ↑ no SQL parser
```

# @petradb/quarry

Type-safe query builder for PetraDB that generates AST objects instead of SQL strings, bypassing the parser entirely.

## Install

```bash
npm install @petradb/quarry @petradb/engine
```

## Quick Start

```typescript
import { Session } from '@petradb/engine'
import { quarry, table, serial, text, integer, boolean, eq, asc } from '@petradb/quarry'
import type { InferSelect, InferInsert } from '@petradb/quarry'

const users = table('users', {
  id: serial('id').primaryKey(),
  name: text('name').notNull(),
  email: text('email').notNull().unique(),
  age: integer('age'),
  active: boolean('active').notNull().default(true),
})

type User = InferSelect<typeof users>
type NewUser = InferInsert<typeof users>

const session = new Session()
const db = quarry(session)

await db.createTable(users)

const [user] = await db.insert(users).values({ name: 'Alice', email: 'alice@example.com', age: 30 }).execute()

const rows = await db.select(users)
  .where(eq(users.active, true))
  .orderBy(asc(users.name))
  .limit(10)
  .execute()

await db.update(users).set({ age: 31 }).where(eq(users.name, 'Alice')).execute()

await db.delete(users).where(eq(users.id, 1)).execute()
```

## Features

- Schema-driven DDL, queries, and TypeScript types from a single definition
- All 21 column types (serial, text, integer, boolean, uuid, timestamp, json, bytea, etc.)
- Inner, left, right, full, and cross joins with correct nullability inference
- Upsert with `onConflictDoNothing` / `onConflictDoUpdate`
- Subqueries (IN, NOT IN, EXISTS, scalar)
- CASE, CAST, aggregate FILTER, NULLS FIRST/LAST, DISTINCT ON
- Statistical aggregates (variance, stddev), bitwise aggregates, EVERY
- INSERT...SELECT, UPDATE...FROM, DELETE...USING
- Table aliases for self-joins
- Transactions with automatic commit/rollback
- AST inspection via `.toAST()` for debugging

## Documentation

Full documentation at [petradb.dev/integrations/quarry](https://petradb.dev/integrations/quarry/).

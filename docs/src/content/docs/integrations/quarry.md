---
title: Quarry
description: Type-safe query builder for PetraDB that generates AST instead of SQL.
---

Quarry is a type-safe query builder for PetraDB that generates AST objects instead of SQL strings, bypassing the parser entirely. Schema definitions serve as a single source of truth for DDL, queries, and compile-time TypeScript types.

## Install

```bash
npm install @petradb/quarry @petradb/engine
```

## Setup

```typescript
import { Session } from "@petradb/engine";
import { quarry } from "@petradb/quarry";

const session = new Session({ storage: "memory" });
const db = quarry(session);
```

### Storage modes

```typescript
// In-memory (default)
new Session({ storage: "memory" });

// File-backed persistent storage
new Session({ storage: "persistent", path: "./mydb.petra" });
```

## Schema definition

Define tables using Quarry's column constructors. The schema drives table creation, query building, and TypeScript type inference:

```typescript
import { table, serial, text, integer, boolean } from "@petradb/quarry";

const users = table("users", {
  id: serial("id").primaryKey(),
  name: text("name").notNull(),
  email: text("email").notNull().unique(),
  age: integer("age"),
  active: boolean("active").notNull().default(true),
});
```

### Column types

| Constructor | SQL type | TypeScript type |
|---|---|---|
| `serial(name)` | `SERIAL` | `number` |
| `text(name)` | `TEXT` | `string` |
| `integer(name)` | `INTEGER` | `number` |
| `boolean(name)` | `BOOLEAN` | `boolean` |

### Column modifiers

| Modifier | Effect |
|---|---|
| `.notNull()` | Column cannot be null; `InferSelect` type excludes `null` |
| `.default(value)` | Column is optional in `InferInsert` |
| `.primaryKey()` | Primary key; implies notNull + hasDefault (auto-increment for serial) |
| `.unique()` | Adds unique constraint |
| `.references(table, column)` | Adds foreign key reference |

### Inferred types

Quarry infers two types from each table definition:

```typescript
import type { InferSelect, InferInsert } from "@petradb/quarry";

type User = InferSelect<typeof users>;
// { id: number, name: string, email: string, age: number | null, active: boolean }

type NewUser = InferInsert<typeof users>;
// { name: string, email: string, age?: number | null, active?: boolean, id?: number }
```

**`InferSelect`** — the row type returned by queries:
- `notNull` columns → non-nullable type
- nullable columns → `type | null`

**`InferInsert`** — the type accepted by `.values()`:
- `notNull` columns without defaults → required
- columns with defaults (`.default()`, `.primaryKey()`, serial) → optional
- nullable columns → optional, accepts `null`

## Create table

```typescript
await db.createTable(users);
```

This generates and executes a `CREATE TABLE` command from the schema definition — no SQL needed.

## Insert

```typescript
// Single row — returns the inserted row with all columns
const user = await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com", age: 30 })
  .execute();
// user.id → auto-generated serial
// user.active → true (default)

// Multiple rows
await db
  .insert(users)
  .values(
    { name: "Bob", email: "bob@example.com", age: 25 },
    { name: "Charlie", email: "charlie@example.com" },
  )
  .execute();
```

Insert requires all `notNull` columns without defaults. Optional fields can be omitted. TypeScript enforces this at compile time.

## Select

```typescript
import { col, eq, gt, asc, desc } from "@petradb/quarry";

// All rows
const allUsers = await db.select(users).execute();

// Where clause
const alice = await db
  .select(users)
  .where(eq(col(users, "name"), "Alice"))
  .execute();

// Specific columns
const names = await db
  .select(users)
  .columns(col(users, "name"), col(users, "email"))
  .execute();

// Order, limit, offset
const page = await db
  .select(users)
  .orderBy(asc(col(users, "name")))
  .limit(10)
  .offset(20)
  .execute();

// Distinct
const statuses = await db
  .select(users)
  .columns(col(users, "active"))
  .distinct()
  .execute();
```

### Column references

The `col()` function creates a type-safe column reference. TypeScript enforces that the column name exists on the table:

```typescript
col(users, "name");  // ✓ compiles
col(users, "title"); // ✗ compile error — 'title' not in users
```

## Expressions

### Comparison

```typescript
import { eq, ne, gt, gte, lt, lte, like, ilike } from "@petradb/quarry";

eq(col(users, "name"), "Alice")     // name = 'Alice'
ne(col(users, "name"), "Bob")       // name != 'Bob'
gt(col(users, "age"), 21)           // age > 21
gte(col(users, "age"), 18)          // age >= 18
lt(col(users, "age"), 65)           // age < 65
lte(col(users, "age"), 30)          // age <= 30
like(col(users, "name"), "A%")      // name LIKE 'A%'
ilike(col(users, "email"), "%@x%")  // email ILIKE '%@x%'
```

### Logical

```typescript
import { and, or, not } from "@petradb/quarry";

and(eq(col(users, "active"), true), gt(col(users, "age"), 18))
or(eq(col(users, "name"), "Alice"), eq(col(users, "name"), "Bob"))
not(eq(col(users, "active"), false))
```

`and()` and `or()` accept any number of arguments:

```typescript
and(cond1, cond2, cond3) // cond1 AND cond2 AND cond3
```

### Null checks

```typescript
import { isNull, isNotNull } from "@petradb/quarry";

isNull(col(users, "age"))     // age IS NULL
isNotNull(col(users, "age"))  // age IS NOT NULL
```

### Collections

```typescript
import { inList, notInList, between } from "@petradb/quarry";

inList(col(users, "name"), ["Alice", "Bob", "Charlie"])  // name IN (...)
notInList(col(users, "id"), [1, 2, 3])                   // id NOT IN (...)
between(col(users, "age"), 18, 65)                       // age BETWEEN 18 AND 65
```

### Arithmetic

```typescript
import { add, sub, mul, div } from "@petradb/quarry";

add(col(users, "age"), 10)  // age + 10
sub(col(users, "age"), 5)   // age - 5
mul(col(users, "age"), 2)   // age * 2
div(col(users, "age"), 3)   // age / 3
```

### Aliases

```typescript
import { alias } from "@petradb/quarry";

alias(add(col(users, "age"), 10), "age_plus_10")
```

### Literals

```typescript
import { literal } from "@petradb/quarry";

literal("hello")  // string
literal(42)        // number
literal(true)      // boolean
literal(null)      // null
```

## Aggregates and grouping

```typescript
import { count, sum, avg, min, max, alias } from "@petradb/quarry";

// Count all rows
const [{ total }] = await db
  .select(users)
  .columns(alias(count(), "total"))
  .execute();

// Group by with aggregate
const stats = await db
  .select(users)
  .columns(col(users, "active"), alias(count(), "cnt"))
  .groupBy(col(users, "active"))
  .execute();

// Having
const popular = await db
  .select(users)
  .columns(col(users, "active"), alias(count(), "cnt"))
  .groupBy(col(users, "active"))
  .having(gt(alias(count(), "cnt"), 5))
  .execute();

// Other aggregates
sum(col(users, "age"))
avg(col(users, "age"))
min(col(users, "age"))
max(col(users, "age"))
```

## Functions

Call any SQL function with `fn()`:

```typescript
import { fn } from "@petradb/quarry";

fn("upper", col(users, "name"))        // UPPER(name)
fn("coalesce", col(users, "age"), 0)   // COALESCE(age, 0)
fn("length", col(users, "name"))       // LENGTH(name)
```

## Joins

Quarry supports inner and left joins with compile-time result typing.

### Inner join

All columns from both tables are included in the result. Nullability is preserved from the original schema:

```typescript
const posts = table("posts", {
  id: serial("id").primaryKey(),
  userId: integer("user_id").notNull(),
  title: text("title").notNull(),
  body: text("body"),
});

const rows = await db
  .select(users)
  .innerJoin(posts, eq(col(users, "id"), col(posts, "userId")))
  .where(eq(col(users, "name"), "Alice"))
  .execute();

// Result type: (InferSelect<users> & InferSelect<posts>)[]
// rows[0].name  → string
// rows[0].title → string
// rows[0].body  → string | null (nullable in posts schema)
```

### Left join

The joined table's columns all become nullable, since unmatched rows produce `null`:

```typescript
const rows = await db
  .select(users)
  .leftJoin(posts, eq(col(users, "id"), col(posts, "userId")))
  .execute();

// Result type: (InferSelect<users> & Nullable<InferSelect<posts>>)[]
// rows[0].name   → string      (base table, not affected)
// rows[0].title  → string | null (left join makes it nullable)
// rows[0].userId → number | null (left join makes it nullable)
```

### Chained joins

Multiple joins accumulate types correctly:

```typescript
const comments = table("comments", {
  id: serial("id").primaryKey(),
  postId: integer("post_id").notNull(),
  content: text("content").notNull(),
});

const rows = await db
  .select(users)
  .innerJoin(posts, eq(col(users, "id"), col(posts, "userId")))
  .leftJoin(comments, eq(col(posts, "id"), col(comments, "postId")))
  .execute();

// posts columns: non-null (inner join)
// comments columns: nullable (left join)
// rows[0].title   → string        (inner join)
// rows[0].content → string | null  (left join)
```

### Join with select columns

```typescript
const rows = await db
  .select(users)
  .columns(col(users, "name"), col(posts, "title"))
  .innerJoin(posts, eq(col(users, "id"), col(posts, "userId")))
  .execute();
```

### Join with aggregates

```typescript
const rows = await db
  .select(users)
  .columns(col(users, "name"), alias(count(), "post_count"))
  .innerJoin(posts, eq(col(users, "id"), col(posts, "userId")))
  .groupBy(col(users, "name"))
  .orderBy(desc(alias(count(), "post_count")))
  .execute();
```

## Update

```typescript
// Update with where
const result = await db
  .update(users)
  .set({ age: 31 })
  .where(eq(col(users, "name"), "Alice"))
  .execute();
// result.rowCount → 1

// Update multiple fields
await db
  .update(users)
  .set({ name: "Alice Smith", age: 32, active: false })
  .where(eq(col(users, "id"), 1))
  .execute();

// Set to null
await db
  .update(users)
  .set({ age: null })
  .where(eq(col(users, "name"), "Bob"))
  .execute();
```

The `.set()` method accepts `Partial<InferSelect<T>>` — TypeScript enforces valid column names and types.

## Delete

```typescript
const result = await db
  .delete(users)
  .where(eq(col(users, "name"), "Alice"))
  .execute();
// result.rowCount → 1
```

## AST inspection

Every builder has a `.toAST()` method that returns the raw AST object without executing it. This is useful for debugging, logging, or building higher-level abstractions:

```typescript
const ast = db
  .select(users)
  .where(eq(col(users, "name"), "Alice"))
  .orderBy(asc(col(users, "id")))
  .limit(10)
  .toAST();

console.log(JSON.stringify(ast, null, 2));
// {
//   "kind": "query",
//   "query": {
//     "kind": "select",
//     "exprs": [{ "kind": "star" }],
//     "from": [{ "kind": "table", "name": "users" }],
//     "where": { "kind": "binary", "left": ..., "op": "=", "right": ... },
//     "orderBy": [{ "expr": ..., "direction": "asc" }],
//     "limit": 10
//   }
// }
```

## How it works

Quarry builds plain JavaScript objects (discriminated unions with a `kind` field) that represent the query AST. When you call `.execute()`, these objects are passed to the engine's `executeAST()` method, which converts them directly into the engine's internal Scala AST — skipping SQL string generation and parsing entirely.

```
Schema → Builder API → JS AST objects → Engine AST → Rewrite → Execute
                              ↑ no SQL parser
```

This gives Quarry the same query capabilities as SQL while eliminating parsing overhead and enabling full compile-time type safety.

## Clean up

```typescript
await session.close();
```

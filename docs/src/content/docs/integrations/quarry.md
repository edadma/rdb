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
| `bigserial(name)` | `BIGSERIAL` | `number` |
| `text(name)` | `TEXT` | `string` |
| `varchar(name, length?)` | `VARCHAR(n)` | `string` |
| `char(name, length?)` | `CHAR(n)` | `string` |
| `integer(name)` | `INTEGER` | `number` |
| `smallint(name)` | `SMALLINT` | `number` |
| `bigint(name)` | `BIGINT` | `number` |
| `doublePrecision(name)` | `DOUBLE` | `number` |
| `real(name)` | `REAL` | `number` |
| `numeric(name, precision?, scale?)` | `NUMERIC(p,s)` | `number` |
| `boolean(name)` | `BOOLEAN` | `boolean` |
| `uuid(name)` | `UUID` | `string` |
| `timestamp(name)` | `TIMESTAMP` | `string` |
| `timestamptz(name)` | `TIMESTAMPTZ` | `string` |
| `date(name)` | `DATE` | `string` |
| `time(name)` | `TIME` | `string` |
| `timetz(name)` | `TIMETZ` | `string` |
| `interval(name)` | `INTERVAL` | `string` |
| `json(name)` | `JSON` | `unknown` |
| `bytea(name)` | `BYTEA` | `number[]` |

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
const [user] = await db
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

### RETURNING

By default, insert returns all columns (`*`). Use `.returning()` to select specific columns:

```typescript
const [{ id }] = await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com" })
  .returning(users.id)
  .execute();
```

### Upsert (ON CONFLICT)

Handle conflicts on insert with `onConflictDoNothing()` or `onConflictDoUpdate()`:

```typescript
// Skip conflicting rows silently
await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com" })
  .onConflictDoNothing()
  .execute();

// Update specific columns when a conflict occurs
await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com", age: 31 })
  .onConflictDoUpdate(["email"], { name: "Alice Updated", age: 31 })
  .execute();
```

The first argument to `onConflictDoUpdate` specifies the conflict columns, the second specifies which columns to update. Both are type-safe — TypeScript enforces that only valid column keys are used.

### INSERT...SELECT

Insert rows from a query instead of literal values:

```typescript
// Archive all active users
const query = db
  .select(users)
  .columns(users.name, users.email)
  .where(eq(users.active, true))
  .toExpr();

await db.insertFrom(archive, query, ["name", "email"]).execute();
```

The second argument is the select query (use `.toExpr()`). The optional third argument specifies which target columns to populate — if omitted, the engine expects the query to produce values for all columns.

```typescript
// Without column list (query must match all target columns)
await db.insertFrom(archive, query).execute();

// With onConflictDoNothing
await db.insertFrom(archive, query, ["name", "email"]).onConflictDoNothing().execute();
```

## Select

```typescript
import { eq, gt, asc, desc } from "@petradb/quarry";

// All rows
const allUsers = await db.select(users).execute();

// Where clause
const alice = await db
  .select(users)
  .where(eq(users.name, "Alice"))
  .execute();

// Specific columns
const names = await db
  .select(users)
  .columns(users.name, users.email)
  .execute();

// Order, limit, offset
const page = await db
  .select(users)
  .orderBy(asc(users.name))
  .limit(10)
  .offset(20)
  .execute();

// Distinct
const statuses = await db
  .select(users)
  .columns(users.active)
  .distinct()
  .execute();

// Distinct on — one row per distinct value of the given columns
const perCategory = await db
  .select(products)
  .distinctOn(products.category)
  .orderBy(asc(products.category), asc(products.price))
  .execute();
// Returns the cheapest product in each category
```

### Column references

Columns are accessed directly as properties on the table object. TypeScript prevents accessing columns that don't exist in the schema:

```typescript
users.name;  // ✓ compiles
users.title; // ✗ compile error — 'title' not in users
```

## Expressions

### Comparison

```typescript
import { eq, ne, gt, gte, lt, lte, like, ilike } from "@petradb/quarry";
import { notLike, notIlike, isDistinctFrom, isNotDistinctFrom } from "@petradb/quarry";

eq(users.name, "Alice")     // name = 'Alice'
ne(users.name, "Bob")       // name != 'Bob'
gt(users.age, 21)           // age > 21
gte(users.age, 18)          // age >= 18
lt(users.age, 65)           // age < 65
lte(users.age, 30)          // age <= 30
like(users.name, "A%")      // name LIKE 'A%'
notLike(users.name, "A%")   // name NOT LIKE 'A%'
ilike(users.email, "%@x%")  // email ILIKE '%@x%'
notIlike(users.email, "%@x%")

// Null-safe comparison
isDistinctFrom(users.age, null)     // age IS DISTINCT FROM NULL
isNotDistinctFrom(users.age, null)  // age IS NOT DISTINCT FROM NULL
```

### Logical

```typescript
import { and, or, not } from "@petradb/quarry";

and(eq(users.active, true), gt(users.age, 18))
or(eq(users.name, "Alice"), eq(users.name, "Bob"))
not(eq(users.active, false))
```

`and()` and `or()` accept any number of arguments:

```typescript
and(cond1, cond2, cond3) // cond1 AND cond2 AND cond3
```

### Null checks

```typescript
import { isNull, isNotNull } from "@petradb/quarry";

isNull(users.age)     // age IS NULL
isNotNull(users.age)  // age IS NOT NULL
```

### Boolean tests

```typescript
import { isTrue, isNotTrue, isFalse, isNotFalse, isUnknown, isNotUnknown } from "@petradb/quarry";

isTrue(users.active)       // active IS TRUE
isNotTrue(users.active)    // active IS NOT TRUE
isFalse(users.active)      // active IS FALSE
isNotFalse(users.active)   // active IS NOT FALSE
isUnknown(users.active)    // active IS UNKNOWN
isNotUnknown(users.active) // active IS NOT UNKNOWN
```

### Collections

```typescript
import { inList, notInList, between, notBetween, betweenSymmetric } from "@petradb/quarry";

inList(users.name, ["Alice", "Bob", "Charlie"])  // name IN (...)
notInList(users.id, [1, 2, 3])                   // id NOT IN (...)
between(users.age, 18, 65)                       // age BETWEEN 18 AND 65
notBetween(users.age, 18, 65)                    // age NOT BETWEEN 18 AND 65
betweenSymmetric(users.age, 65, 18)              // age BETWEEN SYMMETRIC 65 AND 18
notBetweenSymmetric(users.age, 65, 18)           // age NOT BETWEEN SYMMETRIC 65 AND 18
```

### Arithmetic

```typescript
import { add, sub, mul, div, mod, pow, neg } from "@petradb/quarry";

add(users.age, 10)  // age + 10
sub(users.age, 5)   // age - 5
mul(users.age, 2)   // age * 2
div(users.age, 3)   // age / 3
mod(users.age, 2)   // age % 2
pow(users.age, 2)   // age ^ 2
neg(users.age)      // -age
```

### String operators

```typescript
import { concat } from "@petradb/quarry";

concat(users.name, " Jr.")  // name || ' Jr.'
```

### Bitwise operators

```typescript
import { bitAnd, bitOr, bitXor, bitNot, leftShift, rightShift } from "@petradb/quarry";

bitAnd(users.flags, 0xFF)   // flags & 255
bitOr(users.flags, 1)       // flags | 1
bitXor(users.flags, 0xFF)   // flags # 255
bitNot(users.flags)         // ~flags
leftShift(users.flags, 2)   // flags << 2
rightShift(users.flags, 1)  // flags >> 1
```

### JSON operators

```typescript
import { jsonGet, jsonGetText, jsonPath, jsonPathText } from "@petradb/quarry";
import { jsonContains, jsonContainedBy, jsonHasKey, jsonHasAnyKey, jsonHasAllKeys } from "@petradb/quarry";

jsonGet(t.data, "name")       // data -> 'name'
jsonGetText(t.data, "name")   // data ->> 'name'
jsonPath(t.data, path)        // data #> path
jsonPathText(t.data, path)    // data #>> path
jsonContains(t.data, other)   // data @> other
jsonContainedBy(t.data, other) // data <@ other
jsonHasKey(t.data, "key")     // data ? 'key'
jsonHasAnyKey(t.data, keys)   // data ?| keys
jsonHasAllKeys(t.data, keys)  // data ?& keys
```

### Array operators

```typescript
import { arrayOverlap } from "@petradb/quarry";

arrayOverlap(t.tags, t.otherTags)  // tags && otherTags (arrays overlap)
```

### Generic operators

For operators not covered by a named helper, use `op()` and `unaryOp()`:

```typescript
import { op, unaryOp } from "@petradb/quarry";

op(users.age, ">=", 18)       // age >= 18
unaryOp("NOT", eq(users.active, true))
```

### CASE expression

```typescript
import { caseWhen, literal } from "@petradb/quarry";

caseWhen(
  [
    { when: gt(users.age, 60), then: literal("senior") },
    { when: gt(users.age, 18), then: literal("adult") },
  ],
  "minor", // else
)
```

### CAST expression

```typescript
import { cast } from "@petradb/quarry";

cast(users.age, "text")    // CAST(age AS TEXT)
cast(users.age, "double")  // CAST(age AS DOUBLE)
```

### Aliases and literals

```typescript
import { alias, literal } from "@petradb/quarry";

alias(add(users.age, 10), "age_plus_10")

literal("hello")  // string
literal(42)        // number
literal(true)      // boolean
literal(null)      // null
```

## Aggregates and grouping

### Built-in aggregates

```typescript
import { count, sum, avg, min, max, alias } from "@petradb/quarry";
import { stringAgg, arrayAgg, boolAnd, boolOr, jsonAgg, jsonObjectAgg } from "@petradb/quarry";

// Count all rows
const [{ total }] = await db
  .select(users)
  .columns(alias(count(), "total"))
  .execute();

// Group by with aggregate
const stats = await db
  .select(users)
  .columns(users.active, alias(count(), "cnt"))
  .groupBy(users.active)
  .execute();

// Having
const popular = await db
  .select(users)
  .columns(users.active, alias(count(), "cnt"))
  .groupBy(users.active)
  .having(gt(alias(count(), "cnt"), 5))
  .execute();

// Other aggregates
sum(users.age)                              // SUM(age)
avg(users.age)                              // AVG(age)
min(users.age)                              // MIN(age)
max(users.age)                              // MAX(age)
stringAgg(users.name, ", ")                 // STRING_AGG(name, ', ')
arrayAgg(users.name)                        // ARRAY_AGG(name)
boolAnd(users.active)                       // BOOL_AND(active)
boolOr(users.active)                        // BOOL_OR(active)
jsonAgg(users.name)                         // JSON_AGG(name)
jsonObjectAgg(users.name, users.age)        // JSON_OBJECT_AGG(name, age)
```

### Statistical aggregates

```typescript
import { variance, varSamp, varPop, stddev, stddevSamp, stddevPop } from "@petradb/quarry";

variance(emp.salary)   // VARIANCE(salary) — sample variance
varSamp(emp.salary)    // VAR_SAMP(salary) — same as variance
varPop(emp.salary)     // VAR_POP(salary) — population variance
stddev(emp.salary)     // STDDEV(salary) — sample standard deviation
stddevSamp(emp.salary) // STDDEV_SAMP(salary) — same as stddev
stddevPop(emp.salary)  // STDDEV_POP(salary) — population standard deviation
```

### Bitwise aggregates

```typescript
import { bitAndAgg, bitOrAgg, bitXorAgg } from "@petradb/quarry";

bitAndAgg(emp.flags)  // BIT_AND(flags)
bitOrAgg(emp.flags)   // BIT_OR(flags)
bitXorAgg(emp.flags)  // BIT_XOR(flags)
```

### EVERY

```typescript
import { every } from "@petradb/quarry";

every(emp.active)  // EVERY(active) — true when all rows are true
```

### Aggregate FILTER

Restrict which rows an aggregate processes with `filter()`:

```typescript
import { filter } from "@petradb/quarry";

// COUNT(*) FILTER (WHERE salary > 100)
filter(count(), gt(emp.salary, 100))

// SUM(salary) FILTER (WHERE active = true)
filter(sum(emp.salary), eq(emp.active, true))
```

Example with multiple filtered aggregates:

```typescript
const [row] = await db
  .select(employees)
  .columns(
    alias(count(), "total"),
    alias(filter(count(), gt(employees.salary, 100)), "high_earners"),
    alias(filter(sum(employees.salary), eq(employees.active, true)), "active_payroll"),
  )
  .execute();
```

## Functions

Call any SQL function with `fn()`:

```typescript
import { fn } from "@petradb/quarry";

fn("upper", users.name)        // UPPER(name)
fn("coalesce", users.age, 0)   // COALESCE(age, 0)
fn("length", users.name)       // LENGTH(name)
fn("lower", users.email)       // LOWER(email)
fn("abs", users.age)           // ABS(age)
fn("round", users.score, 2)    // ROUND(score, 2)
```

## Joins

Quarry supports inner, left, right, full, and cross joins with compile-time result typing.

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
  .innerJoin(posts, eq(users.id, posts.userId))
  .where(eq(users.name, "Alice"))
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
  .leftJoin(posts, eq(users.id, posts.userId))
  .execute();

// Result type: (InferSelect<users> & Nullable<InferSelect<posts>>)[]
// rows[0].name   → string       (base table, not affected)
// rows[0].title  → string | null (left join makes it nullable)
// rows[0].userId → number | null (left join makes it nullable)
```

### Right join

The base table's columns become nullable, the joined table's columns preserve their original nullability:

```typescript
const rows = await db
  .select(users)
  .rightJoin(posts, eq(users.id, posts.userId))
  .execute();

// Result type: (Nullable<InferSelect<users>> & InferSelect<posts>)[]
// rows[0].name  → string | null (right join makes base table nullable)
// rows[0].title → string        (joined table, not affected)
```

### Full join

Both sides become nullable:

```typescript
const rows = await db
  .select(users)
  .fullJoin(posts, eq(users.id, posts.userId))
  .execute();

// Result type: (Nullable<InferSelect<users>> & Nullable<InferSelect<posts>>)[]
// rows[0].name  → string | null
// rows[0].title → string | null
```

### Cross join

Produces the cartesian product of both tables — no `on` condition:

```typescript
const rows = await db
  .select(users)
  .crossJoin(posts)
  .execute();

// Result type: (InferSelect<users> & InferSelect<posts>)[]
// Every combination of user × post
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
  .innerJoin(posts, eq(users.id, posts.userId))
  .leftJoin(comments, eq(posts.id, comments.postId))
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
  .columns(users.name, posts.title)
  .innerJoin(posts, eq(users.id, posts.userId))
  .execute();
```

### Join with aggregates

```typescript
const rows = await db
  .select(users)
  .columns(users.name, alias(count(), "post_count"))
  .innerJoin(posts, eq(users.id, posts.userId))
  .groupBy(users.name)
  .orderBy(desc(alias(count(), "post_count")))
  .execute();
```

## Table aliases

Use `tableAs()` to create aliased tables for self-joins or when the same table appears multiple times:

```typescript
import { tableAs } from "@petradb/quarry";

const mgr = tableAs(employees, "mgr");
const emp = tableAs(employees, "emp");

const rows = await db
  .select(emp)
  .columns(
    alias(emp.name, "employee"),
    alias(mgr.name, "manager"),
  )
  .leftJoin(mgr, eq(emp.managerId, mgr.id))
  .execute();
```

Aliases are type-safe — `mgr.name` still enforces that `name` exists in the employees schema.

## Subqueries

### IN subquery

```typescript
import { inSubquery, notInSubquery } from "@petradb/quarry";

// Users who have at least one post
const rows = await db
  .select(users)
  .where(
    inSubquery(
      users.id,
      db.select(posts).columns(posts.userId).toExpr(),
    ),
  )
  .execute();

// Users who have NO posts
const rows = await db
  .select(users)
  .where(
    notInSubquery(
      users.id,
      db.select(posts).columns(posts.userId).toExpr(),
    ),
  )
  .execute();
```

### EXISTS subquery

```typescript
import { exists } from "@petradb/quarry";

const rows = await db
  .select(users)
  .where(
    exists(
      db
        .select(posts)
        .columns(literal(1))
        .where(eq(posts.userId, users.id))
        .toExpr(),
    ),
  )
  .execute();
```

### Scalar subquery

Use `subquery()` to wrap a select as a scalar value:

```typescript
import { subquery } from "@petradb/quarry";

// Users older than the average age
const rows = await db
  .select(users)
  .where(
    gt(
      users.age,
      subquery(db.select(users).columns(avg(users.age)).toExpr()),
    ),
  )
  .execute();
```

:::note
Use `.toExpr()` (not `.toAST()`) when embedding a select as a subquery. `.toExpr()` returns the raw `ASTSelect` node, while `.toAST()` wraps it in a `QueryCommand`.
:::

## Ordering

```typescript
import { asc, desc } from "@petradb/quarry";

// Basic ordering
db.select(users).orderBy(asc(users.name))
db.select(users).orderBy(desc(users.age))

// Multiple columns
db.select(users).orderBy(asc(users.name), desc(users.age))

// NULLS FIRST / NULLS LAST
db.select(users).orderBy(asc(users.age, { nulls: "first" }))
db.select(users).orderBy(desc(users.age, { nulls: "last" }))
```

When `nulls` is not specified, the engine uses the default behavior (nulls sort last in ascending order, first in descending order).

## Update

```typescript
// Update with where
const result = await db
  .update(users)
  .set({ age: 31 })
  .where(eq(users.name, "Alice"))
  .execute();
// result.rowCount → 1

// Update multiple fields
await db
  .update(users)
  .set({ name: "Alice Smith", age: 32, active: false })
  .where(eq(users.id, 1))
  .execute();

// Set to null
await db
  .update(users)
  .set({ age: null })
  .where(eq(users.name, "Bob"))
  .execute();
```

The `.set()` method accepts `Partial<InferSelect<T>>` — TypeScript enforces valid column names and types.

### UPDATE...FROM

Join another table to drive updates:

```typescript
const priceUpdates = table("price_updates", {
  id: serial("id").primaryKey(),
  productName: text("product_name").notNull(),
  newPrice: integer("new_price").notNull(),
});

await db
  .update(products)
  .set({ price: 0 }) // set value; use col references in WHERE for conditional logic
  .from(priceUpdates)
  .where(eq(products.name, priceUpdates.productName))
  .execute();
```

`.from()` accepts multiple tables:

```typescript
db.update(t1).set({ ... }).from(t2, t3).where(and(...))
```

### RETURNING

Update and delete support `.returning()` to get back the affected rows:

```typescript
const result = await db
  .update(users)
  .set({ active: false })
  .where(lt(users.age, 18))
  .returning(users.id, users.name)
  .execute();
// result.rows → [{ id: 3, name: "Charlie" }, ...]
```

## Delete

```typescript
const result = await db
  .delete(users)
  .where(eq(users.name, "Alice"))
  .execute();
// result.rowCount → 1
```

### DELETE...USING

Join another table to determine which rows to delete:

```typescript
const deleteList = table("delete_list", {
  id: serial("id").primaryKey(),
  userName: text("user_name").notNull(),
});

await db
  .delete(users)
  .using(deleteList)
  .where(eq(users.name, deleteList.userName))
  .execute();
```

`.using()` accepts multiple tables:

```typescript
db.delete(t1).using(t2, t3).where(and(...))
```

## Transactions

Wrap multiple operations in a transaction with automatic commit/rollback:

```typescript
const result = await db.transaction(async (tx) => {
  const [user] = await tx
    .insert(users)
    .values({ name: "Alice", email: "alice@example.com" })
    .execute();

  await tx
    .insert(posts)
    .values({ userId: user.id, title: "First Post" })
    .execute();

  return user;
});
// If any operation throws, the entire transaction is rolled back
```

The callback receives a `QuarryDB` instance scoped to the transaction. The return value of the callback becomes the return value of `transaction()`, with the type preserved.

## AST inspection

Every builder has a `.toAST()` method that returns the raw AST object without executing it. This is useful for debugging, logging, or building higher-level abstractions:

```typescript
const ast = db
  .select(users)
  .where(eq(users.name, "Alice"))
  .orderBy(asc(users.id))
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

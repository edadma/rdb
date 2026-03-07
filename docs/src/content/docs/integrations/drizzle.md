---
title: Drizzle ORM
description: Using Drizzle ORM with PetraDB.
---

PetraDB provides a [Drizzle ORM](https://orm.drizzle.team) driver via the `@petradb/drizzle` package. It uses `drizzle-orm/pg-proxy` under the hood — Drizzle generates PostgreSQL-dialect SQL, and PetraDB executes it in-process with no wire protocol.

## Install

```bash
npm install @petradb/drizzle drizzle-orm @petradb/engine
```

## Setup

```typescript
import { Session } from "@petradb/engine";
import { drizzle } from "@petradb/drizzle";

const session = new Session({ storage: "memory" });
const db = drizzle(session);
```

### Storage modes

```typescript
// In-memory (default)
new Session({ storage: "memory" })

// File-backed persistent storage
new Session({ storage: "persistent", path: "./mydb.petra" })
```

## Schema definition

Define tables using Drizzle's `pgTable`:

```typescript
import { pgTable, serial, text, integer, boolean } from "drizzle-orm/pg-core";

const users = pgTable("users", {
  id: serial("id").primaryKey(),
  name: text("name").notNull(),
  email: text("email").notNull(),
  age: integer("age"),
  active: boolean("active").default(true),
});
```

Create the table through the session (Drizzle's pg-proxy does not include schema push):

```typescript
await session.execute(`
  CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT NOT NULL,
    age INTEGER,
    active BOOLEAN DEFAULT true
  )
`);
```

## Insert

```typescript
// Single row
await db.insert(users).values({
  name: "Alice",
  email: "alice@example.com",
  age: 30,
});

// Multiple rows
await db.insert(users).values([
  { name: "Bob", email: "bob@example.com", age: 25 },
  { name: "Charlie", email: "charlie@example.com", age: 35 },
]);

// With returning
const [inserted] = await db
  .insert(users)
  .values({ name: "Diana", email: "diana@example.com", age: 28 })
  .returning();
console.log(inserted.id); // auto-generated serial
```

## Select

```typescript
import { eq, gt } from "drizzle-orm";

// All rows
const allUsers = await db.select().from(users);

// Where clause
const alice = await db.select().from(users).where(eq(users.name, "Alice"));

// Conditions
const older = await db.select().from(users).where(gt(users.age, 28));

// Specific columns
const names = await db
  .select({ name: users.name, email: users.email })
  .from(users);

// Limit
const first = await db.select().from(users).limit(1);
```

## Update

```typescript
// Update rows
await db.update(users).set({ age: 31 }).where(eq(users.name, "Alice"));

// With returning
const [updated] = await db
  .update(users)
  .set({ active: false })
  .where(eq(users.name, "Bob"))
  .returning();
```

## Delete

```typescript
// Delete rows
await db.delete(users).where(eq(users.name, "Charlie"));

// With returning
const [deleted] = await db
  .delete(users)
  .where(eq(users.name, "Diana"))
  .returning();
```

## Transactions

Drizzle's `db.transaction()` API is not available with pg-proxy. Use `db.$session` to issue transaction commands directly:

```typescript
// Commit
await db.$session.execute("BEGIN");
await db.insert(users).values({ name: "Eve", email: "eve@example.com", age: 22 });
await db.$session.execute("COMMIT");

// Rollback
await db.$session.execute("BEGIN");
await db.insert(users).values({ name: "Frank", email: "frank@example.com" });
await db.$session.execute("ROLLBACK");
// Frank is not inserted
```

## Type mapping

PetraDB returns native JS types through the proxy — no string coercion needed:

| Drizzle type | PetraDB column | JS type |
|---|---|---|
| `serial()` | `SERIAL` | `number` |
| `integer()` | `INTEGER` | `number` |
| `text()` | `TEXT` | `string` |
| `boolean()` | `BOOLEAN` | `boolean` |
| `numeric()` | `NUMERIC` | `string` |

Nullable columns return `null` when no value is present.

## Clean up

```typescript
await session.close();
```

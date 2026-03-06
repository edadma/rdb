---
title: Knex.js
description: Using Knex.js query builder with PetraDB.
---

PetraDB provides a [Knex.js](https://knexjs.org) dialect via the `@petradb/knex` package. This lets you use Knex's query builder, schema builder, and migrations with PetraDB's embeddable SQL engine.

## Install

```bash
npm install @petradb/knex knex
```

## Setup

```typescript
import Knex from "knex";
import PetraDBClient from "@petradb/knex";

const knex = Knex({
  client: PetraDBClient,
  connection: { storage: "memory" },
  useNullAsDefault: true,
});
```

### Storage modes

```typescript
// In-memory (default)
{ storage: "memory" }

// File-backed persistent storage
{ storage: "persistent", path: "./mydb.petra" }
```

## Schema builder

```typescript
// Create table
await knex.schema.createTable("users", (t) => {
  t.increments("id");
  t.string("name").notNullable();
  t.integer("age");
  t.boolean("active").defaultTo(true);
  t.timestamps(true, true);
});

// Check if table exists
const exists = await knex.schema.hasTable("users");

// Check if column exists
const hasAge = await knex.schema.hasColumn("users", "age");

// Add column
await knex.schema.alterTable("users", (t) => {
  t.string("email");
});

// Create index
await knex.schema.alterTable("users", (t) => {
  t.index(["name"]);
});

// Drop table
await knex.schema.dropTableIfExists("users");
```

## Queries

```typescript
// Insert
await knex("users").insert({ name: "Alice", age: 30 });
await knex("users").insert([
  { name: "Bob", age: 25 },
  { name: "Carol", age: 35 },
]);

// Insert with returning
const [inserted] = await knex("users")
  .insert({ name: "Dave", age: 28 })
  .returning("*");

// Select
const users = await knex("users").where("age", ">", 25);
const first = await knex("users").where("name", "Alice").first();

// Update
const updated = await knex("users")
  .where("name", "Alice")
  .update({ age: 31 });

// Update with returning
const [changed] = await knex("users")
  .where("name", "Alice")
  .update({ age: 32 })
  .returning("*");

// Delete
const deleted = await knex("users").where("active", false).del();

// Aggregates
const [{ count }] = await knex("users").count("* as count");
const [{ max }] = await knex("users").max("age as max");
```

## Joins

```typescript
await knex.schema.createTable("orders", (t) => {
  t.increments("id");
  t.integer("user_id").references("id").inTable("users");
  t.string("product");
  t.decimal("amount", 10, 2);
});

const results = await knex("orders")
  .join("users", "orders.user_id", "users.id")
  .select("users.name", "orders.product", "orders.amount");
```

## Transactions

```typescript
await knex.transaction(async (trx) => {
  await trx("users").insert({ name: "Eve", age: 22 });
  await trx("users").where("name", "Bob").update({ age: 26 });
});
```

## Supported column types

| Knex method | PetraDB type |
|---|---|
| `increments()` | `SERIAL PRIMARY KEY` |
| `bigIncrements()` | `BIGSERIAL PRIMARY KEY` |
| `integer()` | `INTEGER` |
| `bigint()` | `BIGINT` |
| `smallint()` | `SMALLINT` |
| `tinyint()` | `SMALLINT` |
| `float()` | `DOUBLE` |
| `double()` | `DOUBLE` |
| `decimal(p, s)` | `NUMERIC(p, s)` |
| `string(n)` / `varchar(n)` | `VARCHAR(n)` |
| `text()` | `TEXT` |
| `boolean()` | `BOOLEAN` |
| `date()` | `DATE` |
| `timestamp()` | `TIMESTAMP` |
| `uuid()` | `UUID` |
| `json()` | `JSON` |
| `jsonb()` | `JSONB` |
| `binary()` | `BYTEA` |
| `enum()` | `TEXT CHECK (...)` or native `ENUM` |

## Clean up

```typescript
await knex.destroy();
```

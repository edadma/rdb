# @petradb/knex

Knex.js dialect for [PetraDB](https://petradb.dev) — use Knex's query builder, schema builder, and migrations with PetraDB's embeddable SQL engine.

## Install

```bash
npm install @petradb/knex @petradb/engine knex
```

## Usage

```typescript
import Knex from "knex";
import PetraDBClient from "@petradb/knex";

const knex = Knex({
  client: PetraDBClient,
  connection: { storage: "memory" },
  useNullAsDefault: true,
});

// Schema
await knex.schema.createTable("users", (t) => {
  t.increments("id");
  t.string("name");
  t.integer("age");
});

// Insert
await knex("users").insert({ name: "Alice", age: 30 });

// Query
const users = await knex("users").where("age", ">", 25);

// Clean up
await knex.destroy();
```

## Storage Modes

Configure via `connection`:

```typescript
// In-memory (default)
{ storage: "memory" }

// File-backed persistent storage
{ storage: "persistent", path: "./mydb.petra" }
```

## Features

- Full Knex query builder support (select, insert, update, delete, joins, aggregates)
- Schema builder (createTable, dropTable, hasTable, hasColumn, alterTable)
- RETURNING clause support
- Transaction support (BEGIN/COMMIT/ROLLBACK)
- PostgreSQL-style `$1, $2, ...` parameter binding

## Documentation

See [petradb.dev](https://petradb.dev) for full PetraDB documentation.

---
title: AdonisJS Lucid
description: Using AdonisJS Lucid ORM with PetraDB.
---

PetraDB provides an [AdonisJS Lucid](https://lucid.adonisjs.com) driver via the `@petradb/lucid` package. This lets you use Lucid's ORM, migrations, seeders, and query builder with PetraDB's embeddable SQL engine.

## Install

```bash
npm install @petradb/lucid @petradb/knex knex
```

## Setup

Import `@petradb/lucid` before creating any Lucid connections. The import patches Lucid's internals to accept `petradb` as a valid database client.

### AdonisJS app

In your AdonisJS project, add a side-effect import at the top of your database config:

```typescript
// config/database.ts
import '@petradb/lucid'
import { defineConfig } from '@adonisjs/lucid'

export default defineConfig({
  connection: 'petradb',
  connections: {
    petradb: {
      client: 'petradb',
      connection: {
        storage: 'persistent',
        path: './data/app.petra',
      },
      useNullAsDefault: true,
    },
  },
})
```

### Standalone usage

You can use Lucid's `Database` class directly without AdonisJS:

```typescript
import '@petradb/lucid'
import { Database } from '@adonisjs/lucid/database'

const db = new Database({
  connection: 'petradb',
  connections: {
    petradb: {
      client: 'petradb' as any,
      connection: {
        storage: 'memory',
      },
      useNullAsDefault: true,
    },
  },
}, logger, emitter)
```

## Storage modes

Configure via `connection`:

```typescript
// In-memory (default)
{ storage: "memory" }

// File-backed persistent storage
{ storage: "persistent", path: "./mydb.petra" }
```

## Schema builder

```typescript
// Create table
await db.schema.createTable("users", (t) => {
  t.increments("id")
  t.string("name").notNullable()
  t.string("email").unique()
  t.integer("age")
  t.boolean("active").defaultTo(true)
  t.timestamps(true, true)
})

// Check if table/column exists
await db.schema.hasTable("users")
await db.schema.hasColumn("users", "email")

// Add column
await db.schema.alterTable("users", (t) => {
  t.string("bio")
})

// Drop table
await db.schema.dropTableIfExists("users")
```

## Query builder

```typescript
// Insert
await db.table("users").insert({ name: "Alice", age: 30 })

// Insert with returning
const [user] = await db.table("users")
  .insert({ name: "Bob", age: 25 })
  .returning("*")

// Select
const users = await db.from("users").where("age", ">", 25)
const first = await db.from("users").where("name", "Alice").first()

// Update
await db.from("users").where("name", "Alice").update({ age: 31 })

// Delete
await db.from("users").where("active", false).delete()

// Aggregates
const [{ count }] = await db.from("users").count("* as count")
```

## Raw queries

```typescript
const result = await db.rawQuery("SELECT * FROM users WHERE age > ?", [25])
```

## Transactions

```typescript
await db.transaction(async (trx) => {
  await trx.table("users").insert({ name: "Eve", age: 22 })
  await trx.from("users").where("name", "Bob").update({ age: 26 })
})
```

## ORM models

Define models using Lucid's `BaseModel`:

```typescript
import { BaseModel, column } from '@adonisjs/lucid/orm'

class User extends BaseModel {
  @column({ isPrimary: true })
  declare id: number

  @column()
  declare name: string

  @column()
  declare email: string
}

// Create
const user = await User.create({ name: "Alice", email: "alice@example.com" })

// Find
const found = await User.find(user.id)
const all = await User.all()

// Update
found.name = "Alicia"
await found.save()

// Delete
await found.delete()

// Query scopes
const active = await User.query().where("active", true)
```

## Migrations

Create migration files and run them with Lucid's migrator:

```typescript
import { BaseSchema } from '@adonisjs/lucid/schema'

export default class CreateUsersTable extends BaseSchema {
  async up() {
    this.schema.createTable("users", (t) => {
      t.increments("id")
      t.string("name").notNullable()
      t.string("email").unique()
      t.timestamps(true, true)
    })
  }

  async down() {
    this.schema.dropTable("users")
  }
}
```

## Dialect features

The PetraDB dialect supports:

- Schema introspection (`getAllTables`, `getAllViews`, `getAllTypes`)
- `RETURNING` statements
- Table truncation
- Drop all tables/views/types

Not supported:

- Advisory locks (not needed for an embeddable engine)
- Domains

## Clean up

```typescript
await db.manager.closeAll()
```

---
title: Knex.js
description: Utiliser le constructeur de requetes Knex.js avec PetraDB.
---

PetraDB fournit un dialecte [Knex.js](https://knexjs.org) via le package `@petradb/knex`. Cela vous permet d'utiliser le constructeur de requetes, le constructeur de schemas et les migrations de Knex avec le moteur SQL embarquable de PetraDB.

## Installation

```bash
npm install @petradb/knex knex
```

## Configuration

```typescript
import Knex from "knex";
import PetraDBClient from "@petradb/knex";

const knex = Knex({
  client: PetraDBClient,
  connection: { storage: "memory" },
  useNullAsDefault: true,
});
```

### Modes de stockage

```typescript
// En memoire (par defaut)
{ storage: "memory" }

// Stockage persistant sur fichier
{ storage: "persistent", path: "./mydb.petra" }
```

## Constructeur de schemas

```typescript
// Creer une table
await knex.schema.createTable("users", (t) => {
  t.increments("id");
  t.string("name").notNullable();
  t.integer("age");
  t.boolean("active").defaultTo(true);
  t.timestamps(true, true);
});

// Verifier si une table existe
const exists = await knex.schema.hasTable("users");

// Verifier si une colonne existe
const hasAge = await knex.schema.hasColumn("users", "age");

// Ajouter une colonne
await knex.schema.alterTable("users", (t) => {
  t.string("email");
});

// Creer un index
await knex.schema.alterTable("users", (t) => {
  t.index(["name"]);
});

// Supprimer une table
await knex.schema.dropTableIfExists("users");
```

## Requetes

```typescript
// Insertion
await knex("users").insert({ name: "Alice", age: 30 });
await knex("users").insert([
  { name: "Bob", age: 25 },
  { name: "Carol", age: 35 },
]);

// Insertion avec returning
const [inserted] = await knex("users")
  .insert({ name: "Dave", age: 28 })
  .returning("*");

// Selection
const users = await knex("users").where("age", ">", 25);
const first = await knex("users").where("name", "Alice").first();

// Mise a jour
const updated = await knex("users")
  .where("name", "Alice")
  .update({ age: 31 });

// Mise a jour avec returning
const [changed] = await knex("users")
  .where("name", "Alice")
  .update({ age: 32 })
  .returning("*");

// Suppression
const deleted = await knex("users").where("active", false).del();

// Agregats
const [{ count }] = await knex("users").count("* as count");
const [{ max }] = await knex("users").max("age as max");
```

## Jointures

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

## Types de colonnes supportes

| Methode Knex | Type PetraDB |
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
| `enum()` | `TEXT CHECK (...)` ou `ENUM` natif |

## Nettoyage

```typescript
await knex.destroy();
```

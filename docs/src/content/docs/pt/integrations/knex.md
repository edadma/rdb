---
title: Knex.js
description: Usando o construtor de consultas Knex.js com o PetraDB.
---

O PetraDB fornece um dialeto [Knex.js](https://knexjs.org) via o pacote `@petradb/knex`. Isso permite usar o construtor de consultas, construtor de schema e migracoes do Knex com o engine SQL embarcavel do PetraDB.

## Instalacao

```bash
npm install @petradb/knex knex
```

## Configuracao

```typescript
import Knex from "knex";
import PetraDBClient from "@petradb/knex";

const knex = Knex({
  client: PetraDBClient,
  connection: { storage: "memory" },
  useNullAsDefault: true,
});
```

### Modos de armazenamento

```typescript
// Em memoria (padrao)
{ storage: "memory" }

// Armazenamento persistente em arquivo
{ storage: "persistent", path: "./mydb.petra" }
```

## Construtor de schema

```typescript
// Criar tabela
await knex.schema.createTable("users", (t) => {
  t.increments("id");
  t.string("name").notNullable();
  t.integer("age");
  t.boolean("active").defaultTo(true);
  t.timestamps(true, true);
});

// Verificar se tabela existe
const exists = await knex.schema.hasTable("users");

// Verificar se coluna existe
const hasAge = await knex.schema.hasColumn("users", "age");

// Adicionar coluna
await knex.schema.alterTable("users", (t) => {
  t.string("email");
});

// Criar indice
await knex.schema.alterTable("users", (t) => {
  t.index(["name"]);
});

// Excluir tabela
await knex.schema.dropTableIfExists("users");
```

## Consultas

```typescript
// Insert
await knex("users").insert({ name: "Alice", age: 30 });
await knex("users").insert([
  { name: "Bob", age: 25 },
  { name: "Carol", age: 35 },
]);

// Insert com returning
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

// Update com returning
const [changed] = await knex("users")
  .where("name", "Alice")
  .update({ age: 32 })
  .returning("*");

// Delete
const deleted = await knex("users").where("active", false).del();

// Agregacoes
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

## Transacoes

```typescript
await knex.transaction(async (trx) => {
  await trx("users").insert({ name: "Eve", age: 22 });
  await trx("users").where("name", "Bob").update({ age: 26 });
});
```

## Tipos de coluna suportados

| Metodo Knex | Tipo PetraDB |
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
| `enum()` | `TEXT CHECK (...)` ou `ENUM` nativo |

## Limpeza

```typescript
await knex.destroy();
```

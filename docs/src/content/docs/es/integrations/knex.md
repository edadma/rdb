---
title: Knex.js
description: Uso del constructor de consultas Knex.js con PetraDB.
---

PetraDB proporciona un dialecto de [Knex.js](https://knexjs.org) a traves del paquete `@petradb/knex`. Esto te permite usar el constructor de consultas, el constructor de esquemas y las migraciones de Knex con el motor SQL embebible de PetraDB.

## Instalacion

```bash
npm install @petradb/knex knex
```

## Configuracion

```typescript
import Knex from "knex";
import PetraDBClient from "@petradb/knex";

const knex = Knex({
  client: PetraDBClient,
  connection: { storage: "memory" },
  useNullAsDefault: true,
});
```

### Modos de almacenamiento

```typescript
// En memoria (por defecto)
{ storage: "memory" }

// Almacenamiento persistente respaldado por archivo
{ storage: "persistent", path: "./mydb.petra" }
```

## Constructor de esquemas

```typescript
// Crear tabla
await knex.schema.createTable("users", (t) => {
  t.increments("id");
  t.string("name").notNullable();
  t.integer("age");
  t.boolean("active").defaultTo(true);
  t.timestamps(true, true);
});

// Verificar si existe una tabla
const exists = await knex.schema.hasTable("users");

// Verificar si existe una columna
const hasAge = await knex.schema.hasColumn("users", "age");

// Agregar columna
await knex.schema.alterTable("users", (t) => {
  t.string("email");
});

// Crear indice
await knex.schema.alterTable("users", (t) => {
  t.index(["name"]);
});

// Eliminar tabla
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

// Insert con returning
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

// Update con returning
const [changed] = await knex("users")
  .where("name", "Alice")
  .update({ age: 32 })
  .returning("*");

// Delete
const deleted = await knex("users").where("active", false).del();

// Agregados
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

## Transacciones

```typescript
await knex.transaction(async (trx) => {
  await trx("users").insert({ name: "Eve", age: 22 });
  await trx("users").where("name", "Bob").update({ age: 26 });
});
```

## Tipos de columna soportados

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
| `enum()` | `TEXT CHECK (...)` o `ENUM` nativo |

## Limpieza

```typescript
await knex.destroy();
```

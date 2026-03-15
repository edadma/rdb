---
title: Knex.js
description: 将 Knex.js 查询构建器与 PetraDB 一起使用。
---

PetraDB 通过 `@petradb/knex` 包提供 [Knex.js](https://knexjs.org) 方言。这让你可以将 Knex 的查询构建器、模式构建器和迁移与 PetraDB 的可嵌入 SQL 引擎配合使用。

## 安装

```bash
npm install @petradb/knex knex
```

## 设置

```typescript
import Knex from "knex";
import PetraDBClient from "@petradb/knex";

const knex = Knex({
  client: PetraDBClient,
  connection: { storage: "memory" },
  useNullAsDefault: true,
});
```

### 存储模式

```typescript
// 内存模式（默认）
{ storage: "memory" }

// 文件持久化存储
{ storage: "persistent", path: "./mydb.petra" }
```

## 模式构建器

```typescript
// 创建表
await knex.schema.createTable("users", (t) => {
  t.increments("id");
  t.string("name").notNullable();
  t.integer("age");
  t.boolean("active").defaultTo(true);
  t.timestamps(true, true);
});

// 检查表是否存在
const exists = await knex.schema.hasTable("users");

// 检查列是否存在
const hasAge = await knex.schema.hasColumn("users", "age");

// 添加列
await knex.schema.alterTable("users", (t) => {
  t.string("email");
});

// 创建索引
await knex.schema.alterTable("users", (t) => {
  t.index(["name"]);
});

// 删除表
await knex.schema.dropTableIfExists("users");
```

## 查询

```typescript
// 插入
await knex("users").insert({ name: "Alice", age: 30 });
await knex("users").insert([
  { name: "Bob", age: 25 },
  { name: "Carol", age: 35 },
]);

// 带 returning 的插入
const [inserted] = await knex("users")
  .insert({ name: "Dave", age: 28 })
  .returning("*");

// 查询
const users = await knex("users").where("age", ">", 25);
const first = await knex("users").where("name", "Alice").first();

// 更新
const updated = await knex("users")
  .where("name", "Alice")
  .update({ age: 31 });

// 带 returning 的更新
const [changed] = await knex("users")
  .where("name", "Alice")
  .update({ age: 32 })
  .returning("*");

// 删除
const deleted = await knex("users").where("active", false).del();

// 聚合
const [{ count }] = await knex("users").count("* as count");
const [{ max }] = await knex("users").max("age as max");
```

## 连接

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

## 事务

```typescript
await knex.transaction(async (trx) => {
  await trx("users").insert({ name: "Eve", age: 22 });
  await trx("users").where("name", "Bob").update({ age: 26 });
});
```

## 支持的列类型

| Knex 方法 | PetraDB 类型 |
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
| `enum()` | `TEXT CHECK (...)` 或原生 `ENUM` |

## 清理

```typescript
await knex.destroy();
```

---
title: Knex.js
description: PetraDBでKnex.jsクエリビルダーを使用する方法です。
---

PetraDBは`@petradb/knex`パッケージを通じて[Knex.js](https://knexjs.org)ダイアレクトを提供します。これによりKnexのクエリビルダー、スキーマビルダー、マイグレーションをPetraDBの組み込みSQLエンジンで使用できます。

## インストール

```bash
npm install @petradb/knex knex
```

## セットアップ

```typescript
import Knex from "knex";
import PetraDBClient from "@petradb/knex";

const knex = Knex({
  client: PetraDBClient,
  connection: { storage: "memory" },
  useNullAsDefault: true,
});
```

### ストレージモード

```typescript
// インメモリ（デフォルト）
{ storage: "memory" }

// ファイルベースの永続ストレージ
{ storage: "persistent", path: "./mydb.petra" }
```

## スキーマビルダー

```typescript
// テーブル作成
await knex.schema.createTable("users", (t) => {
  t.increments("id");
  t.string("name").notNullable();
  t.integer("age");
  t.boolean("active").defaultTo(true);
  t.timestamps(true, true);
});

// テーブルの存在確認
const exists = await knex.schema.hasTable("users");

// カラムの存在確認
const hasAge = await knex.schema.hasColumn("users", "age");

// カラム追加
await knex.schema.alterTable("users", (t) => {
  t.string("email");
});

// インデックス作成
await knex.schema.alterTable("users", (t) => {
  t.index(["name"]);
});

// テーブル削除
await knex.schema.dropTableIfExists("users");
```

## クエリ

```typescript
// Insert
await knex("users").insert({ name: "Alice", age: 30 });
await knex("users").insert([
  { name: "Bob", age: 25 },
  { name: "Carol", age: 35 },
]);

// returning付きInsert
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

// returning付きUpdate
const [changed] = await knex("users")
  .where("name", "Alice")
  .update({ age: 32 })
  .returning("*");

// Delete
const deleted = await knex("users").where("active", false).del();

// 集計
const [{ count }] = await knex("users").count("* as count");
const [{ max }] = await knex("users").max("age as max");
```

## ジョイン

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

## トランザクション

```typescript
await knex.transaction(async (trx) => {
  await trx("users").insert({ name: "Eve", age: 22 });
  await trx("users").where("name", "Bob").update({ age: 26 });
});
```

## サポートされるカラム型

| Knexメソッド | PetraDB型 |
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
| `enum()` | `TEXT CHECK (...)`またはネイティブ`ENUM` |

## クリーンアップ

```typescript
await knex.destroy();
```

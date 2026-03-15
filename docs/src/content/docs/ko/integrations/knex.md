---
title: Knex.js
description: PetraDB와 함께 Knex.js 쿼리 빌더를 사용하는 방법.
---

PetraDB는 `@petradb/knex` 패키지를 통해 [Knex.js](https://knexjs.org) 방언을 제공합니다. 이를 통해 Knex의 쿼리 빌더, 스키마 빌더, 마이그레이션을 PetraDB의 임베더블 SQL 엔진과 함께 사용할 수 있습니다.

## 설치

```bash
npm install @petradb/knex knex
```

## 설정

```typescript
import Knex from "knex";
import PetraDBClient from "@petradb/knex";

const knex = Knex({
  client: PetraDBClient,
  connection: { storage: "memory" },
  useNullAsDefault: true,
});
```

### 스토리지 모드

```typescript
// 인메모리 (기본값)
{ storage: "memory" }

// 파일 기반 영구 스토리지
{ storage: "persistent", path: "./mydb.petra" }
```

## 스키마 빌더

```typescript
// 테이블 생성
await knex.schema.createTable("users", (t) => {
  t.increments("id");
  t.string("name").notNullable();
  t.integer("age");
  t.boolean("active").defaultTo(true);
  t.timestamps(true, true);
});

// 테이블 존재 여부 확인
const exists = await knex.schema.hasTable("users");

// 컬럼 존재 여부 확인
const hasAge = await knex.schema.hasColumn("users", "age");

// 컬럼 추가
await knex.schema.alterTable("users", (t) => {
  t.string("email");
});

// 인덱스 생성
await knex.schema.alterTable("users", (t) => {
  t.index(["name"]);
});

// 테이블 삭제
await knex.schema.dropTableIfExists("users");
```

## 쿼리

```typescript
// 삽입
await knex("users").insert({ name: "Alice", age: 30 });
await knex("users").insert([
  { name: "Bob", age: 25 },
  { name: "Carol", age: 35 },
]);

// returning과 함께 삽입
const [inserted] = await knex("users")
  .insert({ name: "Dave", age: 28 })
  .returning("*");

// 조회
const users = await knex("users").where("age", ">", 25);
const first = await knex("users").where("name", "Alice").first();

// 수정
const updated = await knex("users")
  .where("name", "Alice")
  .update({ age: 31 });

// returning과 함께 수정
const [changed] = await knex("users")
  .where("name", "Alice")
  .update({ age: 32 })
  .returning("*");

// 삭제
const deleted = await knex("users").where("active", false).del();

// 집계
const [{ count }] = await knex("users").count("* as count");
const [{ max }] = await knex("users").max("age as max");
```

## 조인

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

## 트랜잭션

```typescript
await knex.transaction(async (trx) => {
  await trx("users").insert({ name: "Eve", age: 22 });
  await trx("users").where("name", "Bob").update({ age: 26 });
});
```

## 지원되는 컬럼 타입

| Knex 메서드 | PetraDB 타입 |
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
| `enum()` | `TEXT CHECK (...)` 또는 네이티브 `ENUM` |

## 정리

```typescript
await knex.destroy();
```

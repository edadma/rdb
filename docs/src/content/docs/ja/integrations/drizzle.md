---
title: Drizzle ORM
description: PetraDBでDrizzle ORMを使用する方法です。
---

PetraDBは`@petradb/drizzle`パッケージを通じて[Drizzle ORM](https://orm.drizzle.team)ドライバーを提供します。カスタムPostgreSQLダイアレクトドライバーを実装しており、DrizzleがPostgreSQLダイアレクトのSQLを生成し、PetraDBがワイヤプロトコルなしでインプロセスで実行します。このドライバーは`drizzle-orm/node-postgres`と完全な機能パリティを持ち、`db.transaction()`、すべてのミューテーションでの`returning()`、リレーショナルクエリをサポートします。

## インストール

```bash
npm install @petradb/drizzle drizzle-orm @petradb/engine
```

## セットアップ

```typescript
import { Session } from "@petradb/engine";
import { drizzle } from "@petradb/drizzle";

const session = new Session({ storage: "memory" });
const db = drizzle(session);
```

### ストレージモード

```typescript
// インメモリ（デフォルト）
new Session({ storage: "memory" })

// ファイルベースの永続ストレージ
new Session({ storage: "persistent", path: "./mydb.petra" })
```

## スキーマ定義

Drizzleの`pgTable`を使用してテーブルを定義します。

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

セッション経由でテーブルを作成するか、`drizzle-kit generate` + `migrate()`で[マイグレーション](#マイグレーション)を使用します。

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
// 単一行
await db.insert(users).values({
  name: "Alice",
  email: "alice@example.com",
  age: 30,
});

// 複数行
await db.insert(users).values([
  { name: "Bob", email: "bob@example.com", age: 25 },
  { name: "Charlie", email: "charlie@example.com", age: 35 },
]);

// returning付き
const [inserted] = await db
  .insert(users)
  .values({ name: "Diana", email: "diana@example.com", age: 28 })
  .returning();
console.log(inserted.id); // 自動生成されたserial
```

## Select

```typescript
import { eq, gt } from "drizzle-orm";

// すべての行
const allUsers = await db.select().from(users);

// WHERE句
const alice = await db.select().from(users).where(eq(users.name, "Alice"));

// 条件
const older = await db.select().from(users).where(gt(users.age, 28));

// 特定のカラム
const names = await db
  .select({ name: users.name, email: users.email })
  .from(users);

// Limit
const first = await db.select().from(users).limit(1);
```

## Update

```typescript
// 行の更新
await db.update(users).set({ age: 31 }).where(eq(users.name, "Alice"));

// returning付き
const [updated] = await db
  .update(users)
  .set({ active: false })
  .where(eq(users.name, "Bob"))
  .returning();
```

## Delete

```typescript
// 行の削除
await db.delete(users).where(eq(users.name, "Charlie"));

// returning付き
const [deleted] = await db
  .delete(users)
  .where(eq(users.name, "Diana"))
  .returning();
```

## トランザクション

Drizzleの`db.transaction()` APIで自動コミット/ロールバックを使用します。

```typescript
// 自動コミット
const result = await db.transaction(async (tx) => {
  const [inserted] = await tx
    .insert(users)
    .values({ name: "Eve", email: "eve@example.com", age: 22 })
    .returning();
  return inserted;
});

// エラー時の自動ロールバック
await db.transaction(async (tx) => {
  await tx.insert(users).values({ name: "Frank", email: "frank@example.com" });
  throw new Error("something went wrong");
  // Frankは挿入されません — トランザクションがロールバックされます
});

// 明示的ロールバック
await db.transaction(async (tx) => {
  await tx.insert(users).values({ name: "Grace", email: "grace@example.com" });
  tx.rollback(); // TransactionRollbackErrorをスロー
});
```

`db.$session`を使用して手動トランザクション制御も可能です。

```typescript
await db.$session.execute("BEGIN");
await db.insert(users).values({ name: "Hank", email: "hank@example.com" });
await db.$session.execute("COMMIT");
```

## 型マッピング

PetraDBはネイティブJS型を返すため、文字列変換は不要です。

| Drizzle型 | PetraDBカラム | JS型 |
|---|---|---|
| `serial()` | `SERIAL` | `number` |
| `integer()` | `INTEGER` | `number` |
| `text()` | `TEXT` | `string` |
| `boolean()` | `BOOLEAN` | `boolean` |
| `numeric()` | `NUMERIC` | `string` |

NULLableカラムは値がない場合に`null`を返します。

## マイグレーション

[Drizzle Kit](https://orm.drizzle.team/docs/kit-overview)マイグレーションを`migrate()`関数で適用します。

```typescript
import { migrate } from "@petradb/drizzle";

await migrate(db, { migrationsFolder: "./drizzle" });
```

これは`drizzle-kit generate`で生成されたマイグレーションジャーナルとSQLファイルを読み取り、順番に実行し、適用されたマイグレーションを`drizzle.__drizzle_migrations`テーブル（自動作成）で追跡します。

一般的なワークフロー：

```bash
# スキーマ変更からマイグレーションを生成
npx drizzle-kit generate

# 起動時にマイグレーションを適用
```

```typescript
import { Session } from "@petradb/engine";
import { drizzle, migrate } from "@petradb/drizzle";

const session = new Session({ storage: "memory" });
const db = drizzle(session);

await migrate(db, { migrationsFolder: "./drizzle" });
// テーブルが作成されました — 通常通りdbを使用できます
```

## クリーンアップ

```typescript
await session.close();
```

---
title: Drizzle ORM
description: 将 Drizzle ORM 与 PetraDB 一起使用。
---

PetraDB 通过 `@petradb/drizzle` 包提供 [Drizzle ORM](https://orm.drizzle.team) 驱动。它实现了自定义 PostgreSQL 方言驱动 — Drizzle 生成 PostgreSQL 方言的 SQL，PetraDB 在进程内执行，无需线协议。该驱动与 `drizzle-orm/node-postgres` 完全功能对等，包括 `db.transaction()`、所有变更操作的 `returning()`，以及关系查询。

## 安装

```bash
npm install @petradb/drizzle drizzle-orm @petradb/engine
```

## 设置

```typescript
import { Session } from "@petradb/engine";
import { drizzle } from "@petradb/drizzle";

const session = new Session({ storage: "memory" });
const db = drizzle(session);
```

### 存储模式

```typescript
// 内存模式（默认）
new Session({ storage: "memory" })

// 文件持久化存储
new Session({ storage: "persistent", path: "./mydb.petra" })
```

## 模式定义

使用 Drizzle 的 `pgTable` 定义表：

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

通过 session 创建表，或使用[迁移](#迁移)配合 `drizzle-kit generate` + `migrate()`：

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

## 插入

```typescript
// 单行
await db.insert(users).values({
  name: "Alice",
  email: "alice@example.com",
  age: 30,
});

// 多行
await db.insert(users).values([
  { name: "Bob", email: "bob@example.com", age: 25 },
  { name: "Charlie", email: "charlie@example.com", age: 35 },
]);

// 带 returning
const [inserted] = await db
  .insert(users)
  .values({ name: "Diana", email: "diana@example.com", age: 28 })
  .returning();
console.log(inserted.id); // 自动生成的序列号
```

## 查询

```typescript
import { eq, gt } from "drizzle-orm";

// 所有行
const allUsers = await db.select().from(users);

// WHERE 子句
const alice = await db.select().from(users).where(eq(users.name, "Alice"));

// 条件
const older = await db.select().from(users).where(gt(users.age, 28));

// 指定列
const names = await db
  .select({ name: users.name, email: users.email })
  .from(users);

// 限制
const first = await db.select().from(users).limit(1);
```

## 更新

```typescript
// 更新行
await db.update(users).set({ age: 31 }).where(eq(users.name, "Alice"));

// 带 returning
const [updated] = await db
  .update(users)
  .set({ active: false })
  .where(eq(users.name, "Bob"))
  .returning();
```

## 删除

```typescript
// 删除行
await db.delete(users).where(eq(users.name, "Charlie"));

// 带 returning
const [deleted] = await db
  .delete(users)
  .where(eq(users.name, "Diana"))
  .returning();
```

## 事务

使用 Drizzle 的 `db.transaction()` API 实现自动提交/回滚：

```typescript
// 自动提交
const result = await db.transaction(async (tx) => {
  const [inserted] = await tx
    .insert(users)
    .values({ name: "Eve", email: "eve@example.com", age: 22 })
    .returning();
  return inserted;
});

// 错误时自动回滚
await db.transaction(async (tx) => {
  await tx.insert(users).values({ name: "Frank", email: "frank@example.com" });
  throw new Error("something went wrong");
  // Frank 不会被插入 — 事务已回滚
});

// 显式回滚
await db.transaction(async (tx) => {
  await tx.insert(users).values({ name: "Grace", email: "grace@example.com" });
  tx.rollback(); // 抛出 TransactionRollbackError
});
```

你也可以使用 `db.$session` 进行手动事务控制：

```typescript
await db.$session.execute("BEGIN");
await db.insert(users).values({ name: "Hank", email: "hank@example.com" });
await db.$session.execute("COMMIT");
```

## 类型映射

PetraDB 返回原生 JS 类型 — 无需字符串转换：

| Drizzle 类型 | PetraDB 列 | JS 类型 |
|---|---|---|
| `serial()` | `SERIAL` | `number` |
| `integer()` | `INTEGER` | `number` |
| `text()` | `TEXT` | `string` |
| `boolean()` | `BOOLEAN` | `boolean` |
| `numeric()` | `NUMERIC` | `string` |

可空列在没有值时返回 `null`。

## 迁移

使用 `migrate()` 函数应用 [Drizzle Kit](https://orm.drizzle.team/docs/kit-overview) 迁移：

```typescript
import { migrate } from "@petradb/drizzle";

await migrate(db, { migrationsFolder: "./drizzle" });
```

这会读取由 `drizzle-kit generate` 生成的迁移日志和 SQL 文件，按顺序执行，并在 `drizzle.__drizzle_migrations` 表中跟踪已应用的迁移（自动创建）。

典型工作流：

```bash
# 从模式更改生成迁移
npx drizzle-kit generate

# 启动时应用迁移
```

```typescript
import { Session } from "@petradb/engine";
import { drizzle, migrate } from "@petradb/drizzle";

const session = new Session({ storage: "memory" });
const db = drizzle(session);

await migrate(db, { migrationsFolder: "./drizzle" });
// 表已创建 — 正常使用 db
```

## 清理

```typescript
await session.close();
```

---
title: Drizzle ORM
description: PetraDB와 함께 Drizzle ORM을 사용하는 방법.
---

PetraDB는 `@petradb/drizzle` 패키지를 통해 [Drizzle ORM](https://orm.drizzle.team) 드라이버를 제공합니다. 커스텀 PostgreSQL 방언 드라이버를 구현하여, Drizzle이 PostgreSQL 방언 SQL을 생성하면 PetraDB가 와이어 프로토콜 없이 프로세스 내에서 직접 실행합니다. 이 드라이버는 `db.transaction()`, 모든 변형에서의 `returning()`, 관계형 쿼리를 포함하여 `drizzle-orm/node-postgres`와 완전한 기능 동등성을 제공합니다.

## 설치

```bash
npm install @petradb/drizzle drizzle-orm @petradb/engine
```

## 설정

```typescript
import { Session } from "@petradb/engine";
import { drizzle } from "@petradb/drizzle";

const session = new Session({ storage: "memory" });
const db = drizzle(session);
```

### 스토리지 모드

```typescript
// 인메모리 (기본값)
new Session({ storage: "memory" })

// 파일 기반 영구 스토리지
new Session({ storage: "persistent", path: "./mydb.petra" })
```

## 스키마 정의

Drizzle의 `pgTable`을 사용하여 테이블을 정의합니다:

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

세션을 통해 테이블을 생성하거나, `drizzle-kit generate` + `migrate()`로 [마이그레이션](#마이그레이션)을 사용합니다:

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

## 삽입

```typescript
// 단일 행
await db.insert(users).values({
  name: "Alice",
  email: "alice@example.com",
  age: 30,
});

// 여러 행
await db.insert(users).values([
  { name: "Bob", email: "bob@example.com", age: 25 },
  { name: "Charlie", email: "charlie@example.com", age: 35 },
]);

// returning과 함께
const [inserted] = await db
  .insert(users)
  .values({ name: "Diana", email: "diana@example.com", age: 28 })
  .returning();
console.log(inserted.id); // 자동 생성된 serial
```

## 조회

```typescript
import { eq, gt } from "drizzle-orm";

// 모든 행
const allUsers = await db.select().from(users);

// WHERE 절
const alice = await db.select().from(users).where(eq(users.name, "Alice"));

// 조건
const older = await db.select().from(users).where(gt(users.age, 28));

// 특정 컬럼
const names = await db
  .select({ name: users.name, email: users.email })
  .from(users);

// Limit
const first = await db.select().from(users).limit(1);
```

## 수정

```typescript
// 행 수정
await db.update(users).set({ age: 31 }).where(eq(users.name, "Alice"));

// returning과 함께
const [updated] = await db
  .update(users)
  .set({ active: false })
  .where(eq(users.name, "Bob"))
  .returning();
```

## 삭제

```typescript
// 행 삭제
await db.delete(users).where(eq(users.name, "Charlie"));

// returning과 함께
const [deleted] = await db
  .delete(users)
  .where(eq(users.name, "Diana"))
  .returning();
```

## 트랜잭션

Drizzle의 `db.transaction()` API를 사용하여 자동 커밋/롤백합니다:

```typescript
// 자동 커밋
const result = await db.transaction(async (tx) => {
  const [inserted] = await tx
    .insert(users)
    .values({ name: "Eve", email: "eve@example.com", age: 22 })
    .returning();
  return inserted;
});

// 오류 시 자동 롤백
await db.transaction(async (tx) => {
  await tx.insert(users).values({ name: "Frank", email: "frank@example.com" });
  throw new Error("something went wrong");
  // Frank는 삽입되지 않음 — 트랜잭션이 롤백됨
});

// 명시적 롤백
await db.transaction(async (tx) => {
  await tx.insert(users).values({ name: "Grace", email: "grace@example.com" });
  tx.rollback(); // TransactionRollbackError를 throw
});
```

수동 트랜잭션 제어를 위해 `db.$session`을 사용할 수도 있습니다:

```typescript
await db.$session.execute("BEGIN");
await db.insert(users).values({ name: "Hank", email: "hank@example.com" });
await db.$session.execute("COMMIT");
```

## 타입 매핑

PetraDB는 네이티브 JS 타입을 반환합니다 — 문자열 강제 변환이 필요 없습니다:

| Drizzle 타입 | PetraDB 컬럼 | JS 타입 |
|---|---|---|
| `serial()` | `SERIAL` | `number` |
| `integer()` | `INTEGER` | `number` |
| `text()` | `TEXT` | `string` |
| `boolean()` | `BOOLEAN` | `boolean` |
| `numeric()` | `NUMERIC` | `string` |

NULL 허용 컬럼은 값이 없을 때 `null`을 반환합니다.

## 마이그레이션

`migrate()` 함수로 [Drizzle Kit](https://orm.drizzle.team/docs/kit-overview) 마이그레이션을 적용합니다:

```typescript
import { migrate } from "@petradb/drizzle";

await migrate(db, { migrationsFolder: "./drizzle" });
```

이 함수는 `drizzle-kit generate`가 생성한 마이그레이션 저널과 SQL 파일을 읽고, 순서대로 실행하며, 적용된 마이그레이션을 `drizzle.__drizzle_migrations` 테이블(자동 생성)에 추적합니다.

일반적인 워크플로:

```bash
# 스키마 변경에서 마이그레이션 생성
npx drizzle-kit generate

# 시작 시 마이그레이션 적용
```

```typescript
import { Session } from "@petradb/engine";
import { drizzle, migrate } from "@petradb/drizzle";

const session = new Session({ storage: "memory" });
const db = drizzle(session);

await migrate(db, { migrationsFolder: "./drizzle" });
// 테이블이 생성됨 — db를 정상적으로 사용
```

## 정리

```typescript
await session.close();
```

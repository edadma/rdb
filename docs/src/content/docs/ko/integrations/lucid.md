---
title: Lucid ORM
description: PetraDB와 함께 Lucid ORM을 사용하는 방법.
---

PetraDB는 `@petradb/lucid` 패키지를 통해 [AdonisJS Lucid](https://lucid.adonisjs.com) 드라이버를 제공합니다. 이를 통해 Lucid의 ORM, 마이그레이션, 시더, 쿼리 빌더를 PetraDB의 임베더블 SQL 엔진과 함께 사용할 수 있습니다.

## 설치

```bash
npm install @petradb/lucid @petradb/knex knex
```

## 설정

Lucid 연결을 생성하기 전에 `@petradb/lucid`를 가져옵니다. 이 가져오기는 Lucid 내부를 패치하여 `petradb`를 유효한 데이터베이스 클라이언트로 받아들입니다.

### AdonisJS 앱

AdonisJS 프로젝트에서 데이터베이스 설정 상단에 사이드 이펙트 가져오기를 추가합니다:

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

### 독립 실행형 사용

AdonisJS 없이 Lucid의 `Database` 클래스를 직접 사용할 수 있습니다:

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

## 스토리지 모드

`connection`으로 설정합니다:

```typescript
// 인메모리 (기본값)
{ storage: "memory" }

// 파일 기반 영구 스토리지
{ storage: "persistent", path: "./mydb.petra" }
```

## 스키마 빌더

```typescript
// 테이블 생성
await db.schema.createTable("users", (t) => {
  t.increments("id")
  t.string("name").notNullable()
  t.string("email").unique()
  t.integer("age")
  t.boolean("active").defaultTo(true)
  t.timestamps(true, true)
})

// 테이블/컬럼 존재 여부 확인
await db.schema.hasTable("users")
await db.schema.hasColumn("users", "email")

// 컬럼 추가
await db.schema.alterTable("users", (t) => {
  t.string("bio")
})

// 테이블 삭제
await db.schema.dropTableIfExists("users")
```

## 쿼리 빌더

```typescript
// 삽입
await db.table("users").insert({ name: "Alice", age: 30 })

// returning과 함께 삽입
const [user] = await db.table("users")
  .insert({ name: "Bob", age: 25 })
  .returning("*")

// 조회
const users = await db.from("users").where("age", ">", 25)
const first = await db.from("users").where("name", "Alice").first()

// 수정
await db.from("users").where("name", "Alice").update({ age: 31 })

// 삭제
await db.from("users").where("active", false).delete()

// 집계
const [{ count }] = await db.from("users").count("* as count")
```

## 원시 쿼리

```typescript
const result = await db.rawQuery("SELECT * FROM users WHERE age > ?", [25])
```

## 트랜잭션

```typescript
await db.transaction(async (trx) => {
  await trx.table("users").insert({ name: "Eve", age: 22 })
  await trx.from("users").where("name", "Bob").update({ age: 26 })
})
```

## ORM 모델

Lucid의 `BaseModel`을 사용하여 모델을 정의합니다:

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

// 생성
const user = await User.create({ name: "Alice", email: "alice@example.com" })

// 찾기
const found = await User.find(user.id)
const all = await User.all()

// 수정
found.name = "Alicia"
await found.save()

// 삭제
await found.delete()

// 쿼리 스코프
const active = await User.query().where("active", true)
```

## 마이그레이션

마이그레이션 파일을 작성하고 Lucid의 마이그레이터로 실행합니다:

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

## 방언 기능

PetraDB 방언은 다음을 지원합니다:

- 스키마 인트로스펙션 (`getAllTables`, `getAllViews`, `getAllTypes`)
- `RETURNING` 구문
- 테이블 truncation
- 모든 테이블/뷰/타입 삭제
- 트랜잭션 내 DDL (DML 롤백과 완전히 원자적)

미지원 사항:

- Advisory lock (임베더블 엔진에는 불필요)
- Domain

## 정리

```typescript
await db.manager.closeAll()
```

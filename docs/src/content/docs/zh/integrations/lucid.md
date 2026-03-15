---
title: Lucid ORM
description: 将 Lucid ORM 与 PetraDB 一起使用。
---

PetraDB 通过 `@petradb/lucid` 包提供 [AdonisJS Lucid](https://lucid.adonisjs.com) 驱动。这让你可以将 Lucid 的 ORM、迁移、种子器和查询构建器与 PetraDB 的可嵌入 SQL 引擎配合使用。

## 安装

```bash
npm install @petradb/lucid @petradb/knex knex
```

## 设置

在创建任何 Lucid 连接之前导入 `@petradb/lucid`。该导入会修补 Lucid 内部以接受 `petradb` 作为有效的数据库客户端。

### AdonisJS 应用

在你的 AdonisJS 项目中，在数据库配置文件顶部添加副作用导入：

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

### 独立使用

你可以不依赖 AdonisJS 直接使用 Lucid 的 `Database` 类：

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

## 存储模式

通过 `connection` 配置：

```typescript
// 内存模式（默认）
{ storage: "memory" }

// 文件持久化存储
{ storage: "persistent", path: "./mydb.petra" }
```

## 模式构建器

```typescript
// 创建表
await db.schema.createTable("users", (t) => {
  t.increments("id")
  t.string("name").notNullable()
  t.string("email").unique()
  t.integer("age")
  t.boolean("active").defaultTo(true)
  t.timestamps(true, true)
})

// 检查表/列是否存在
await db.schema.hasTable("users")
await db.schema.hasColumn("users", "email")

// 添加列
await db.schema.alterTable("users", (t) => {
  t.string("bio")
})

// 删除表
await db.schema.dropTableIfExists("users")
```

## 查询构建器

```typescript
// 插入
await db.table("users").insert({ name: "Alice", age: 30 })

// 带 returning 的插入
const [user] = await db.table("users")
  .insert({ name: "Bob", age: 25 })
  .returning("*")

// 查询
const users = await db.from("users").where("age", ">", 25)
const first = await db.from("users").where("name", "Alice").first()

// 更新
await db.from("users").where("name", "Alice").update({ age: 31 })

// 删除
await db.from("users").where("active", false).delete()

// 聚合
const [{ count }] = await db.from("users").count("* as count")
```

## 原始查询

```typescript
const result = await db.rawQuery("SELECT * FROM users WHERE age > ?", [25])
```

## 事务

```typescript
await db.transaction(async (trx) => {
  await trx.table("users").insert({ name: "Eve", age: 22 })
  await trx.from("users").where("name", "Bob").update({ age: 26 })
})
```

## ORM 模型

使用 Lucid 的 `BaseModel` 定义模型：

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

// 创建
const user = await User.create({ name: "Alice", email: "alice@example.com" })

// 查找
const found = await User.find(user.id)
const all = await User.all()

// 更新
found.name = "Alicia"
await found.save()

// 删除
await found.delete()

// 查询作用域
const active = await User.query().where("active", true)
```

## 迁移

创建迁移文件并使用 Lucid 的迁移器运行：

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

## 方言功能

PetraDB 方言支持：

- Schema 自省（`getAllTables`、`getAllViews`、`getAllTypes`）
- `RETURNING` 语句
- 表截断
- 删除所有表/视图/类型
- 事务内的 DDL（与 DML 完全原子回滚）

不支持：

- 咨询锁（可嵌入引擎不需要）
- 域

## 清理

```typescript
await db.manager.closeAll()
```

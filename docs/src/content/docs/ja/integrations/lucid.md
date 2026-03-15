---
title: Lucid ORM
description: PetraDBでLucid ORMを使用する方法です。
---

PetraDBは`@petradb/lucid`パッケージを通じて[AdonisJS Lucid](https://lucid.adonisjs.com)ドライバーを提供します。これによりLucidのORM、マイグレーション、シーダー、クエリビルダーをPetraDBの組み込みSQLエンジンで使用できます。

## インストール

```bash
npm install @petradb/lucid @petradb/knex knex
```

## セットアップ

Lucid接続を作成する前に`@petradb/lucid`をインポートしてください。このインポートによりLucidの内部を修正し、`petradb`を有効なデータベースクライアントとして受け入れるようにします。

### AdonisJSアプリ

AdonisJSプロジェクトでは、データベース設定の先頭にサイドエフェクトインポートを追加します。

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

### スタンドアロン使用

AdonisJSなしでLucidの`Database`クラスを直接使用できます。

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

## ストレージモード

`connection`で設定します。

```typescript
// インメモリ（デフォルト）
{ storage: "memory" }

// ファイルベースの永続ストレージ
{ storage: "persistent", path: "./mydb.petra" }
```

## スキーマビルダー

```typescript
// テーブル作成
await db.schema.createTable("users", (t) => {
  t.increments("id")
  t.string("name").notNullable()
  t.string("email").unique()
  t.integer("age")
  t.boolean("active").defaultTo(true)
  t.timestamps(true, true)
})

// テーブル/カラムの存在確認
await db.schema.hasTable("users")
await db.schema.hasColumn("users", "email")

// カラム追加
await db.schema.alterTable("users", (t) => {
  t.string("bio")
})

// テーブル削除
await db.schema.dropTableIfExists("users")
```

## クエリビルダー

```typescript
// Insert
await db.table("users").insert({ name: "Alice", age: 30 })

// returning付きInsert
const [user] = await db.table("users")
  .insert({ name: "Bob", age: 25 })
  .returning("*")

// Select
const users = await db.from("users").where("age", ">", 25)
const first = await db.from("users").where("name", "Alice").first()

// Update
await db.from("users").where("name", "Alice").update({ age: 31 })

// Delete
await db.from("users").where("active", false).delete()

// 集計
const [{ count }] = await db.from("users").count("* as count")
```

## 生クエリ

```typescript
const result = await db.rawQuery("SELECT * FROM users WHERE age > ?", [25])
```

## トランザクション

```typescript
await db.transaction(async (trx) => {
  await trx.table("users").insert({ name: "Eve", age: 22 })
  await trx.from("users").where("name", "Bob").update({ age: 26 })
})
```

## ORMモデル

Lucidの`BaseModel`を使用してモデルを定義します。

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

// 作成
const user = await User.create({ name: "Alice", email: "alice@example.com" })

// 検索
const found = await User.find(user.id)
const all = await User.all()

// 更新
found.name = "Alicia"
await found.save()

// 削除
await found.delete()

// クエリスコープ
const active = await User.query().where("active", true)
```

## マイグレーション

マイグレーションファイルを作成し、Lucidのマイグレーターで実行します。

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

## ダイアレクト機能

PetraDBダイアレクトがサポートする機能：

- スキーマイントロスペクション（`getAllTables`、`getAllViews`、`getAllTypes`）
- `RETURNING`文
- テーブルのトランケート
- すべてのテーブル/ビュー/型の削除
- トランザクション内のDDL（DMLロールバックと完全にアトミック）

サポートされていない機能：

- アドバイザリーロック（組み込みエンジンでは不要）
- ドメイン

## クリーンアップ

```typescript
await db.manager.closeAll()
```

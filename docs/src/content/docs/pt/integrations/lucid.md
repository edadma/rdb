---
title: Lucid ORM
description: Usando o Lucid ORM com o PetraDB.
---

O PetraDB fornece um driver [AdonisJS Lucid](https://lucid.adonisjs.com) via o pacote `@petradb/lucid`. Isso permite usar o ORM, migracoes, seeders e construtor de consultas do Lucid com o engine SQL embarcavel do PetraDB.

## Instalacao

```bash
npm install @petradb/lucid @petradb/knex knex
```

## Configuracao

Importe `@petradb/lucid` antes de criar qualquer conexao Lucid. A importacao modifica os internos do Lucid para aceitar `petradb` como um cliente de banco de dados valido.

### Aplicacao AdonisJS

No seu projeto AdonisJS, adicione uma importacao de efeito colateral no topo da sua configuracao de banco de dados:

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

### Uso independente

Voce pode usar a classe `Database` do Lucid diretamente sem AdonisJS:

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

## Modos de armazenamento

Configure via `connection`:

```typescript
// Em memoria (padrao)
{ storage: "memory" }

// Armazenamento persistente em arquivo
{ storage: "persistent", path: "./mydb.petra" }
```

## Construtor de schema

```typescript
// Criar tabela
await db.schema.createTable("users", (t) => {
  t.increments("id")
  t.string("name").notNullable()
  t.string("email").unique()
  t.integer("age")
  t.boolean("active").defaultTo(true)
  t.timestamps(true, true)
})

// Verificar se tabela/coluna existe
await db.schema.hasTable("users")
await db.schema.hasColumn("users", "email")

// Adicionar coluna
await db.schema.alterTable("users", (t) => {
  t.string("bio")
})

// Excluir tabela
await db.schema.dropTableIfExists("users")
```

## Construtor de consultas

```typescript
// Insert
await db.table("users").insert({ name: "Alice", age: 30 })

// Insert com returning
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

// Agregacoes
const [{ count }] = await db.from("users").count("* as count")
```

## Consultas brutas

```typescript
const result = await db.rawQuery("SELECT * FROM users WHERE age > ?", [25])
```

## Transacoes

```typescript
await db.transaction(async (trx) => {
  await trx.table("users").insert({ name: "Eve", age: 22 })
  await trx.from("users").where("name", "Bob").update({ age: 26 })
})
```

## Modelos ORM

Defina modelos usando `BaseModel` do Lucid:

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

// Criar
const user = await User.create({ name: "Alice", email: "alice@example.com" })

// Buscar
const found = await User.find(user.id)
const all = await User.all()

// Atualizar
found.name = "Alicia"
await found.save()

// Excluir
await found.delete()

// Escopos de consulta
const active = await User.query().where("active", true)
```

## Migracoes

Crie arquivos de migracao e execute-os com o migrador do Lucid:

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

## Recursos do dialeto

O dialeto PetraDB suporta:

- Introspeccao de schema (`getAllTables`, `getAllViews`, `getAllTypes`)
- Comandos `RETURNING`
- Truncamento de tabelas
- Exclusao de todas as tabelas/views/types
- DDL dentro de transacoes (totalmente atomico com rollback de DML)

Nao suportado:

- Advisory locks (nao necessario para um engine embarcavel)
- Domains

## Limpeza

```typescript
await db.manager.closeAll()
```

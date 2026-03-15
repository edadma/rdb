---
title: Lucid ORM
description: Utiliser Lucid ORM avec PetraDB.
---

PetraDB fournit un pilote [AdonisJS Lucid](https://lucid.adonisjs.com) via le package `@petradb/lucid`. Cela vous permet d'utiliser l'ORM, les migrations, les seeders et le constructeur de requetes de Lucid avec le moteur SQL embarquable de PetraDB.

## Installation

```bash
npm install @petradb/lucid @petradb/knex knex
```

## Configuration

Importez `@petradb/lucid` avant de creer toute connexion Lucid. L'import modifie les composants internes de Lucid pour accepter `petradb` comme client de base de donnees valide.

### Application AdonisJS

Dans votre projet AdonisJS, ajoutez un import a effet de bord en haut de votre configuration de base de donnees :

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

### Utilisation autonome

Vous pouvez utiliser la classe `Database` de Lucid directement sans AdonisJS :

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

## Modes de stockage

Configuration via `connection` :

```typescript
// En memoire (par defaut)
{ storage: "memory" }

// Stockage persistant sur fichier
{ storage: "persistent", path: "./mydb.petra" }
```

## Constructeur de schemas

```typescript
// Creer une table
await db.schema.createTable("users", (t) => {
  t.increments("id")
  t.string("name").notNullable()
  t.string("email").unique()
  t.integer("age")
  t.boolean("active").defaultTo(true)
  t.timestamps(true, true)
})

// Verifier si une table/colonne existe
await db.schema.hasTable("users")
await db.schema.hasColumn("users", "email")

// Ajouter une colonne
await db.schema.alterTable("users", (t) => {
  t.string("bio")
})

// Supprimer une table
await db.schema.dropTableIfExists("users")
```

## Constructeur de requetes

```typescript
// Insertion
await db.table("users").insert({ name: "Alice", age: 30 })

// Insertion avec returning
const [user] = await db.table("users")
  .insert({ name: "Bob", age: 25 })
  .returning("*")

// Selection
const users = await db.from("users").where("age", ">", 25)
const first = await db.from("users").where("name", "Alice").first()

// Mise a jour
await db.from("users").where("name", "Alice").update({ age: 31 })

// Suppression
await db.from("users").where("active", false).delete()

// Agregats
const [{ count }] = await db.from("users").count("* as count")
```

## Requetes brutes

```typescript
const result = await db.rawQuery("SELECT * FROM users WHERE age > ?", [25])
```

## Transactions

```typescript
await db.transaction(async (trx) => {
  await trx.table("users").insert({ name: "Eve", age: 22 })
  await trx.from("users").where("name", "Bob").update({ age: 26 })
})
```

## Modeles ORM

Definissez des modeles en utilisant le `BaseModel` de Lucid :

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

// Creer
const user = await User.create({ name: "Alice", email: "alice@example.com" })

// Trouver
const found = await User.find(user.id)
const all = await User.all()

// Mettre a jour
found.name = "Alicia"
await found.save()

// Supprimer
await found.delete()

// Portees de requete
const active = await User.query().where("active", true)
```

## Migrations

Creez des fichiers de migration et executez-les avec le migrateur Lucid :

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

## Fonctionnalites du dialecte

Le dialecte PetraDB supporte :

- Introspection de schema (`getAllTables`, `getAllViews`, `getAllTypes`)
- Instructions `RETURNING`
- Troncature de tables
- Suppression de toutes les tables/vues/types
- DDL dans les transactions (entierement atomique avec rollback DML)

Non supporte :

- Verrous consultatifs (non necessaires pour un moteur embarquable)
- Domaines

## Nettoyage

```typescript
await db.manager.closeAll()
```

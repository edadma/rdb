---
title: Lucid ORM
description: Uso de Lucid ORM con PetraDB.
---

PetraDB proporciona un driver de [AdonisJS Lucid](https://lucid.adonisjs.com) a traves del paquete `@petradb/lucid`. Esto te permite usar el ORM, migraciones, seeders y constructor de consultas de Lucid con el motor SQL embebible de PetraDB.

## Instalacion

```bash
npm install @petradb/lucid @petradb/knex knex
```

## Configuracion

Importa `@petradb/lucid` antes de crear cualquier conexion Lucid. La importacion parchea los internos de Lucid para aceptar `petradb` como un cliente de base de datos valido.

### Aplicacion AdonisJS

En tu proyecto AdonisJS, agrega una importacion de efecto secundario al inicio de tu configuracion de base de datos:

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

### Uso independiente

Puedes usar la clase `Database` de Lucid directamente sin AdonisJS:

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

## Modos de almacenamiento

Configura mediante `connection`:

```typescript
// En memoria (por defecto)
{ storage: "memory" }

// Almacenamiento persistente respaldado por archivo
{ storage: "persistent", path: "./mydb.petra" }
```

## Constructor de esquemas

```typescript
// Crear tabla
await db.schema.createTable("users", (t) => {
  t.increments("id")
  t.string("name").notNullable()
  t.string("email").unique()
  t.integer("age")
  t.boolean("active").defaultTo(true)
  t.timestamps(true, true)
})

// Verificar si existe tabla/columna
await db.schema.hasTable("users")
await db.schema.hasColumn("users", "email")

// Agregar columna
await db.schema.alterTable("users", (t) => {
  t.string("bio")
})

// Eliminar tabla
await db.schema.dropTableIfExists("users")
```

## Constructor de consultas

```typescript
// Insert
await db.table("users").insert({ name: "Alice", age: 30 })

// Insert con returning
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

// Agregados
const [{ count }] = await db.from("users").count("* as count")
```

## Consultas directas

```typescript
const result = await db.rawQuery("SELECT * FROM users WHERE age > ?", [25])
```

## Transacciones

```typescript
await db.transaction(async (trx) => {
  await trx.table("users").insert({ name: "Eve", age: 22 })
  await trx.from("users").where("name", "Bob").update({ age: 26 })
})
```

## Modelos ORM

Define modelos usando `BaseModel` de Lucid:

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

// Crear
const user = await User.create({ name: "Alice", email: "alice@example.com" })

// Buscar
const found = await User.find(user.id)
const all = await User.all()

// Actualizar
found.name = "Alicia"
await found.save()

// Eliminar
await found.delete()

// Ambitos de consulta
const active = await User.query().where("active", true)
```

## Migraciones

Crea archivos de migracion y ejecutalos con el migrador de Lucid:

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

## Funcionalidades del dialecto

El dialecto PetraDB soporta:

- Introspeccion de esquema (`getAllTables`, `getAllViews`, `getAllTypes`)
- Sentencias `RETURNING`
- Truncamiento de tablas
- Eliminar todas las tablas/vistas/tipos
- DDL dentro de transacciones (completamente atomico con rollback de DML)

No soportado:

- Advisory locks (no necesarios para un motor embebible)
- Dominios

## Limpieza

```typescript
await db.manager.closeAll()
```

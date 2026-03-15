---
title: Drizzle ORM
description: Uso de Drizzle ORM con PetraDB.
---

PetraDB proporciona un driver de [Drizzle ORM](https://orm.drizzle.team) a traves del paquete `@petradb/drizzle`. Implementa un driver de dialecto PostgreSQL personalizado — Drizzle genera SQL con dialecto PostgreSQL, y PetraDB lo ejecuta en proceso sin protocolo de red. El driver tiene paridad completa de funcionalidades con `drizzle-orm/node-postgres`, incluyendo `db.transaction()`, `returning()` en todas las mutaciones y consultas relacionales.

## Instalacion

```bash
npm install @petradb/drizzle drizzle-orm @petradb/engine
```

## Configuracion

```typescript
import { Session } from "@petradb/engine";
import { drizzle } from "@petradb/drizzle";

const session = new Session({ storage: "memory" });
const db = drizzle(session);
```

### Modos de almacenamiento

```typescript
// En memoria (por defecto)
new Session({ storage: "memory" })

// Almacenamiento persistente respaldado por archivo
new Session({ storage: "persistent", path: "./mydb.petra" })
```

## Definicion de esquema

Define tablas usando `pgTable` de Drizzle:

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

Crea la tabla a traves de la sesion, o usa [migraciones](#migraciones) con `drizzle-kit generate` + `migrate()`:

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
// Una sola fila
await db.insert(users).values({
  name: "Alice",
  email: "alice@example.com",
  age: 30,
});

// Multiples filas
await db.insert(users).values([
  { name: "Bob", email: "bob@example.com", age: 25 },
  { name: "Charlie", email: "charlie@example.com", age: 35 },
]);

// Con returning
const [inserted] = await db
  .insert(users)
  .values({ name: "Diana", email: "diana@example.com", age: 28 })
  .returning();
console.log(inserted.id); // serial auto-generado
```

## Select

```typescript
import { eq, gt } from "drizzle-orm";

// Todas las filas
const allUsers = await db.select().from(users);

// Clausula where
const alice = await db.select().from(users).where(eq(users.name, "Alice"));

// Condiciones
const older = await db.select().from(users).where(gt(users.age, 28));

// Columnas especificas
const names = await db
  .select({ name: users.name, email: users.email })
  .from(users);

// Limite
const first = await db.select().from(users).limit(1);
```

## Update

```typescript
// Actualizar filas
await db.update(users).set({ age: 31 }).where(eq(users.name, "Alice"));

// Con returning
const [updated] = await db
  .update(users)
  .set({ active: false })
  .where(eq(users.name, "Bob"))
  .returning();
```

## Delete

```typescript
// Eliminar filas
await db.delete(users).where(eq(users.name, "Charlie"));

// Con returning
const [deleted] = await db
  .delete(users)
  .where(eq(users.name, "Diana"))
  .returning();
```

## Transacciones

Usa la API `db.transaction()` de Drizzle para commit/rollback automatico:

```typescript
// Commit automatico
const result = await db.transaction(async (tx) => {
  const [inserted] = await tx
    .insert(users)
    .values({ name: "Eve", email: "eve@example.com", age: 22 })
    .returning();
  return inserted;
});

// Rollback automatico en caso de error
await db.transaction(async (tx) => {
  await tx.insert(users).values({ name: "Frank", email: "frank@example.com" });
  throw new Error("something went wrong");
  // Frank no se inserta — la transaccion se revierte
});

// Rollback explicito
await db.transaction(async (tx) => {
  await tx.insert(users).values({ name: "Grace", email: "grace@example.com" });
  tx.rollback(); // lanza TransactionRollbackError
});
```

Tambien puedes usar `db.$session` para control manual de transacciones:

```typescript
await db.$session.execute("BEGIN");
await db.insert(users).values({ name: "Hank", email: "hank@example.com" });
await db.$session.execute("COMMIT");
```

## Mapeo de tipos

PetraDB retorna tipos JS nativos — no se necesita coercion de cadenas:

| Tipo Drizzle | Columna PetraDB | Tipo JS |
|---|---|---|
| `serial()` | `SERIAL` | `number` |
| `integer()` | `INTEGER` | `number` |
| `text()` | `TEXT` | `string` |
| `boolean()` | `BOOLEAN` | `boolean` |
| `numeric()` | `NUMERIC` | `string` |

Las columnas anulables retornan `null` cuando no hay valor presente.

## Migraciones

Aplica migraciones de [Drizzle Kit](https://orm.drizzle.team/docs/kit-overview) con la funcion `migrate()`:

```typescript
import { migrate } from "@petradb/drizzle";

await migrate(db, { migrationsFolder: "./drizzle" });
```

Esto lee el journal de migraciones y los archivos SQL generados por `drizzle-kit generate`, los ejecuta en orden y rastrea las migraciones aplicadas en una tabla `drizzle.__drizzle_migrations` (creada automaticamente).

Flujo de trabajo tipico:

```bash
# Generar migraciones a partir de cambios de esquema
npx drizzle-kit generate

# Aplicar migraciones al inicio
```

```typescript
import { Session } from "@petradb/engine";
import { drizzle, migrate } from "@petradb/drizzle";

const session = new Session({ storage: "memory" });
const db = drizzle(session);

await migrate(db, { migrationsFolder: "./drizzle" });
// Las tablas estan creadas — usa db normalmente
```

## Limpieza

```typescript
await session.close();
```

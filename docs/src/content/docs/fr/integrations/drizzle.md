---
title: Drizzle ORM
description: Utiliser Drizzle ORM avec PetraDB.
---

PetraDB fournit un pilote [Drizzle ORM](https://orm.drizzle.team) via le package `@petradb/drizzle`. Il implemente un pilote de dialecte PostgreSQL personnalise -- Drizzle genere du SQL en dialecte PostgreSQL, et PetraDB l'execute en processus sans protocole filaire. Le pilote offre une parite complete des fonctionnalites avec `drizzle-orm/node-postgres`, y compris `db.transaction()`, `returning()` sur toutes les mutations et les requetes relationnelles.

## Installation

```bash
npm install @petradb/drizzle drizzle-orm @petradb/engine
```

## Configuration

```typescript
import { Session } from "@petradb/engine";
import { drizzle } from "@petradb/drizzle";

const session = new Session({ storage: "memory" });
const db = drizzle(session);
```

### Modes de stockage

```typescript
// En memoire (par defaut)
new Session({ storage: "memory" })

// Stockage persistant sur fichier
new Session({ storage: "persistent", path: "./mydb.petra" })
```

## Definition du schema

Definissez les tables en utilisant le `pgTable` de Drizzle :

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

Creez la table via la session, ou utilisez les [migrations](#migrations) avec `drizzle-kit generate` + `migrate()` :

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

## Insertion

```typescript
// Ligne unique
await db.insert(users).values({
  name: "Alice",
  email: "alice@example.com",
  age: 30,
});

// Lignes multiples
await db.insert(users).values([
  { name: "Bob", email: "bob@example.com", age: 25 },
  { name: "Charlie", email: "charlie@example.com", age: 35 },
]);

// Avec returning
const [inserted] = await db
  .insert(users)
  .values({ name: "Diana", email: "diana@example.com", age: 28 })
  .returning();
console.log(inserted.id); // serial auto-genere
```

## Selection

```typescript
import { eq, gt } from "drizzle-orm";

// Toutes les lignes
const allUsers = await db.select().from(users);

// Clause where
const alice = await db.select().from(users).where(eq(users.name, "Alice"));

// Conditions
const older = await db.select().from(users).where(gt(users.age, 28));

// Colonnes specifiques
const names = await db
  .select({ name: users.name, email: users.email })
  .from(users);

// Limite
const first = await db.select().from(users).limit(1);
```

## Mise a jour

```typescript
// Mettre a jour des lignes
await db.update(users).set({ age: 31 }).where(eq(users.name, "Alice"));

// Avec returning
const [updated] = await db
  .update(users)
  .set({ active: false })
  .where(eq(users.name, "Bob"))
  .returning();
```

## Suppression

```typescript
// Supprimer des lignes
await db.delete(users).where(eq(users.name, "Charlie"));

// Avec returning
const [deleted] = await db
  .delete(users)
  .where(eq(users.name, "Diana"))
  .returning();
```

## Transactions

Utilisez l'API `db.transaction()` de Drizzle pour un commit/rollback automatique :

```typescript
// Commit automatique
const result = await db.transaction(async (tx) => {
  const [inserted] = await tx
    .insert(users)
    .values({ name: "Eve", email: "eve@example.com", age: 22 })
    .returning();
  return inserted;
});

// Rollback automatique en cas d'erreur
await db.transaction(async (tx) => {
  await tx.insert(users).values({ name: "Frank", email: "frank@example.com" });
  throw new Error("something went wrong");
  // Frank n'est pas insere — la transaction est annulee
});

// Rollback explicite
await db.transaction(async (tx) => {
  await tx.insert(users).values({ name: "Grace", email: "grace@example.com" });
  tx.rollback(); // lance TransactionRollbackError
});
```

Vous pouvez egalement utiliser `db.$session` pour un controle manuel des transactions :

```typescript
await db.$session.execute("BEGIN");
await db.insert(users).values({ name: "Hank", email: "hank@example.com" });
await db.$session.execute("COMMIT");
```

## Mapping de types

PetraDB retourne des types JS natifs -- pas de coercition de chaines necessaire :

| Type Drizzle | Colonne PetraDB | Type JS |
|---|---|---|
| `serial()` | `SERIAL` | `number` |
| `integer()` | `INTEGER` | `number` |
| `text()` | `TEXT` | `string` |
| `boolean()` | `BOOLEAN` | `boolean` |
| `numeric()` | `NUMERIC` | `string` |

Les colonnes nullables retournent `null` lorsqu'aucune valeur n'est presente.

## Migrations

Appliquez les migrations [Drizzle Kit](https://orm.drizzle.team/docs/kit-overview) avec la fonction `migrate()` :

```typescript
import { migrate } from "@petradb/drizzle";

await migrate(db, { migrationsFolder: "./drizzle" });
```

Cela lit le journal de migration et les fichiers SQL generes par `drizzle-kit generate`, les execute dans l'ordre et suit les migrations appliquees dans une table `drizzle.__drizzle_migrations` (creee automatiquement).

Flux de travail typique :

```bash
# Generer les migrations a partir des changements de schema
npx drizzle-kit generate

# Appliquer les migrations au demarrage
```

```typescript
import { Session } from "@petradb/engine";
import { drizzle, migrate } from "@petradb/drizzle";

const session = new Session({ storage: "memory" });
const db = drizzle(session);

await migrate(db, { migrationsFolder: "./drizzle" });
// Les tables sont maintenant creees — utilisez db normalement
```

## Nettoyage

```typescript
await session.close();
```

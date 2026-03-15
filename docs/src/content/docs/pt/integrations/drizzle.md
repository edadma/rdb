---
title: Drizzle ORM
description: Usando o Drizzle ORM com o PetraDB.
---

O PetraDB fornece um driver [Drizzle ORM](https://orm.drizzle.team) via o pacote `@petradb/drizzle`. Ele implementa um driver de dialeto PostgreSQL personalizado — o Drizzle gera SQL no dialeto PostgreSQL e o PetraDB o executa no processo sem protocolo de rede. O driver tem paridade completa de recursos com `drizzle-orm/node-postgres`, incluindo `db.transaction()`, `returning()` em todas as mutacoes e consultas relacionais.

## Instalacao

```bash
npm install @petradb/drizzle drizzle-orm @petradb/engine
```

## Configuracao

```typescript
import { Session } from "@petradb/engine";
import { drizzle } from "@petradb/drizzle";

const session = new Session({ storage: "memory" });
const db = drizzle(session);
```

### Modos de armazenamento

```typescript
// Em memoria (padrao)
new Session({ storage: "memory" })

// Armazenamento persistente em arquivo
new Session({ storage: "persistent", path: "./mydb.petra" })
```

## Definicao de schema

Defina tabelas usando `pgTable` do Drizzle:

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

Crie a tabela pela sessao ou use [migracoes](#migracoes) com `drizzle-kit generate` + `migrate()`:

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
// Linha unica
await db.insert(users).values({
  name: "Alice",
  email: "alice@example.com",
  age: 30,
});

// Multiplas linhas
await db.insert(users).values([
  { name: "Bob", email: "bob@example.com", age: 25 },
  { name: "Charlie", email: "charlie@example.com", age: 35 },
]);

// Com returning
const [inserted] = await db
  .insert(users)
  .values({ name: "Diana", email: "diana@example.com", age: 28 })
  .returning();
console.log(inserted.id); // serial gerado automaticamente
```

## Select

```typescript
import { eq, gt } from "drizzle-orm";

// Todas as linhas
const allUsers = await db.select().from(users);

// Clausula where
const alice = await db.select().from(users).where(eq(users.name, "Alice"));

// Condicoes
const older = await db.select().from(users).where(gt(users.age, 28));

// Colunas especificas
const names = await db
  .select({ name: users.name, email: users.email })
  .from(users);

// Limite
const first = await db.select().from(users).limit(1);
```

## Update

```typescript
// Atualizar linhas
await db.update(users).set({ age: 31 }).where(eq(users.name, "Alice"));

// Com returning
const [updated] = await db
  .update(users)
  .set({ active: false })
  .where(eq(users.name, "Bob"))
  .returning();
```

## Delete

```typescript
// Excluir linhas
await db.delete(users).where(eq(users.name, "Charlie"));

// Com returning
const [deleted] = await db
  .delete(users)
  .where(eq(users.name, "Diana"))
  .returning();
```

## Transacoes

Use a API `db.transaction()` do Drizzle para commit/rollback automatico:

```typescript
// Commit automatico
const result = await db.transaction(async (tx) => {
  const [inserted] = await tx
    .insert(users)
    .values({ name: "Eve", email: "eve@example.com", age: 22 })
    .returning();
  return inserted;
});

// Rollback automatico em caso de erro
await db.transaction(async (tx) => {
  await tx.insert(users).values({ name: "Frank", email: "frank@example.com" });
  throw new Error("something went wrong");
  // Frank nao e inserido — a transacao sofre rollback
});

// Rollback explicito
await db.transaction(async (tx) => {
  await tx.insert(users).values({ name: "Grace", email: "grace@example.com" });
  tx.rollback(); // lanca TransactionRollbackError
});
```

Voce tambem pode usar `db.$session` para controle manual de transacao:

```typescript
await db.$session.execute("BEGIN");
await db.insert(users).values({ name: "Hank", email: "hank@example.com" });
await db.$session.execute("COMMIT");
```

## Mapeamento de tipos

O PetraDB retorna tipos JS nativos — sem necessidade de coercao de string:

| Tipo Drizzle | Coluna PetraDB | Tipo JS |
|---|---|---|
| `serial()` | `SERIAL` | `number` |
| `integer()` | `INTEGER` | `number` |
| `text()` | `TEXT` | `string` |
| `boolean()` | `BOOLEAN` | `boolean` |
| `numeric()` | `NUMERIC` | `string` |

Colunas nulas retornam `null` quando nenhum valor esta presente.

## Migracoes

Aplique migracoes do [Drizzle Kit](https://orm.drizzle.team/docs/kit-overview) com a funcao `migrate()`:

```typescript
import { migrate } from "@petradb/drizzle";

await migrate(db, { migrationsFolder: "./drizzle" });
```

Isso le o journal de migracao e os arquivos SQL gerados pelo `drizzle-kit generate`, executa-os em ordem e rastreia migracoes aplicadas em uma tabela `drizzle.__drizzle_migrations` (criada automaticamente).

Fluxo de trabalho tipico:

```bash
# Gerar migracoes a partir de alteracoes no schema
npx drizzle-kit generate

# Aplicar migracoes na inicializacao
```

```typescript
import { Session } from "@petradb/engine";
import { drizzle, migrate } from "@petradb/drizzle";

const session = new Session({ storage: "memory" });
const db = drizzle(session);

await migrate(db, { migrationsFolder: "./drizzle" });
// Tabelas agora estao criadas — use o db normalmente
```

## Limpeza

```typescript
await session.close();
```

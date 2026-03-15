---
title: Quarry
description: Construtor de consultas type-safe para PetraDB que gera AST em vez de SQL.
---

Quarry e um construtor de consultas type-safe para PetraDB que gera objetos AST em vez de strings SQL, ignorando completamente o parser. Definicoes de schema servem como fonte unica de verdade para DDL, consultas e tipos TypeScript em tempo de compilacao.

## Instalacao

```bash
npm install @petradb/quarry @petradb/engine
```

## Configuracao

```typescript
import { Session } from "@petradb/engine";
import { quarry } from "@petradb/quarry";

const session = new Session({ storage: "memory" });
const db = quarry(session);
```

### Modos de armazenamento

```typescript
// Em memoria (padrao)
new Session({ storage: "memory" });

// Armazenamento persistente em arquivo
new Session({ storage: "persistent", path: "./mydb.petra" });
```

## Definicao de schema

Defina tabelas usando os construtores de coluna do Quarry. O schema direciona a criacao de tabelas, construcao de consultas e inferencia de tipos TypeScript:

```typescript
import { table, serial, text, integer, boolean } from "@petradb/quarry";

const users = table("users", {
  id: serial("id").primaryKey(),
  name: text("name").notNull(),
  email: text("email").notNull().unique(),
  age: integer("age"),
  active: boolean("active").notNull().default(true),
});
```

### Tipos de coluna

| Construtor | Tipo SQL | Tipo TypeScript |
|---|---|---|
| `serial(name)` | `SERIAL` | `number` |
| `bigserial(name)` | `BIGSERIAL` | `number` |
| `text(name)` | `TEXT` | `string` |
| `varchar(name, length?)` | `VARCHAR(n)` | `string` |
| `char(name, length?)` | `CHAR(n)` | `string` |
| `integer(name)` | `INTEGER` | `number` |
| `smallint(name)` | `SMALLINT` | `number` |
| `bigint(name)` | `BIGINT` | `number` |
| `doublePrecision(name)` | `DOUBLE` | `number` |
| `real(name)` | `REAL` | `number` |
| `numeric(name, precision?, scale?)` | `NUMERIC(p,s)` | `number` |
| `boolean(name)` | `BOOLEAN` | `boolean` |
| `uuid(name)` | `UUID` | `string` |
| `timestamp(name)` | `TIMESTAMP` | `string` |
| `timestamptz(name)` | `TIMESTAMPTZ` | `string` |
| `date(name)` | `DATE` | `string` |
| `time(name)` | `TIME` | `string` |
| `timetz(name)` | `TIMETZ` | `string` |
| `interval(name)` | `INTERVAL` | `string` |
| `json(name)` | `JSON` | `unknown` |
| `bytea(name)` | `BYTEA` | `number[]` |

### Modificadores de coluna

| Modificador | Efeito |
|---|---|
| `.notNull()` | Coluna nao pode ser nula; tipo `InferSelect` exclui `null` |
| `.default(value)` | Coluna e opcional em `InferInsert` |
| `.primaryKey()` | Chave primaria; implica notNull + hasDefault (auto-incremento para serial) |
| `.unique()` | Adiciona restricao unique |
| `.references(table, column)` | Adiciona referencia de chave estrangeira |

### Tipos inferidos

O Quarry infere dois tipos de cada definicao de tabela:

```typescript
import type { InferSelect, InferInsert } from "@petradb/quarry";

type User = InferSelect<typeof users>;
// { id: number, name: string, email: string, age: number | null, active: boolean }

type NewUser = InferInsert<typeof users>;
// { name: string, email: string, age?: number | null, active?: boolean, id?: number }
```

**`InferSelect`** — o tipo de linha retornado por consultas:
- Colunas `notNull` -> tipo nao nulo
- Colunas nulaveis -> `type | null`

**`InferInsert`** — o tipo aceito por `.values()`:
- Colunas `notNull` sem defaults -> obrigatorias
- Colunas com defaults (`.default()`, `.primaryKey()`, serial) -> opcionais
- Colunas nulaveis -> opcionais, aceitam `null`

## Criar tabela

```typescript
await db.createTable(users);
```

Isso gera e executa um comando `CREATE TABLE` a partir da definicao do schema — sem necessidade de SQL.

## Insert

```typescript
// Linha unica — retorna a linha inserida com todas as colunas
const [user] = await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com", age: 30 })
  .execute();
// user.id -> serial gerado automaticamente
// user.active -> true (padrao)

// Multiplas linhas
await db
  .insert(users)
  .values(
    { name: "Bob", email: "bob@example.com", age: 25 },
    { name: "Charlie", email: "charlie@example.com" },
  )
  .execute();
```

Insert requer todas as colunas `notNull` sem defaults. Campos opcionais podem ser omitidos. TypeScript impoe isso em tempo de compilacao.

### RETURNING

Por padrao, insert retorna todas as colunas (`*`). Use `.returning()` para selecionar colunas especificas:

```typescript
const [{ id }] = await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com" })
  .returning(users.id)
  .execute();
```

### Upsert (ON CONFLICT)

Trate conflitos no insert com `onConflictDoNothing()` ou `onConflictDoUpdate()`:

```typescript
// Ignorar linhas conflitantes silenciosamente
await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com" })
  .onConflictDoNothing()
  .execute();

// Atualizar colunas especificas quando um conflito ocorre
await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com", age: 31 })
  .onConflictDoUpdate(["email"], { name: "Alice Updated", age: 31 })
  .execute();
```

O primeiro argumento de `onConflictDoUpdate` especifica as colunas de conflito, o segundo especifica quais colunas atualizar. Ambos sao type-safe — TypeScript impoe que apenas chaves de coluna validas sejam usadas.

### INSERT...SELECT

Insira linhas de uma consulta em vez de valores literais:

```typescript
// Arquivar todos os usuarios ativos
const query = db
  .select(users)
  .columns(users.name, users.email)
  .where(eq(users.active, true))
  .toExpr();

await db.insertFrom(archive, query, ["name", "email"]).execute();
```

O segundo argumento e a consulta select (use `.toExpr()`). O terceiro argumento opcional especifica quais colunas de destino preencher — se omitido, o engine espera que a consulta produza valores para todas as colunas.

```typescript
// Sem lista de colunas (a consulta deve corresponder a todas as colunas de destino)
await db.insertFrom(archive, query).execute();

// Com onConflictDoNothing
await db.insertFrom(archive, query, ["name", "email"]).onConflictDoNothing().execute();
```

## Select

```typescript
import { eq, gt, asc, desc } from "@petradb/quarry";

// Todas as linhas
const allUsers = await db.select(users).execute();

// Clausula where
const alice = await db
  .select(users)
  .where(eq(users.name, "Alice"))
  .execute();

// Colunas especificas
const names = await db
  .select(users)
  .columns(users.name, users.email)
  .execute();

// Ordenacao, limite, offset
const page = await db
  .select(users)
  .orderBy(asc(users.name))
  .limit(10)
  .offset(20)
  .execute();

// Distinct
const statuses = await db
  .select(users)
  .columns(users.active)
  .distinct()
  .execute();

// Distinct on — uma linha por valor distinto das colunas especificadas
const perCategory = await db
  .select(products)
  .distinctOn(products.category)
  .orderBy(asc(products.category), asc(products.price))
  .execute();
// Retorna o produto mais barato de cada categoria
```

### Referencias de coluna

Colunas sao acessadas diretamente como propriedades no objeto da tabela. TypeScript impede o acesso a colunas que nao existem no schema:

```typescript
users.name;  // ✓ compila
users.title; // ✗ erro de compilacao — 'title' nao existe em users
```

## Expressoes

### Comparacao

```typescript
import { eq, ne, gt, gte, lt, lte, like, ilike } from "@petradb/quarry";
import { notLike, notIlike, isDistinctFrom, isNotDistinctFrom } from "@petradb/quarry";

eq(users.name, "Alice")     // name = 'Alice'
ne(users.name, "Bob")       // name != 'Bob'
gt(users.age, 21)           // age > 21
gte(users.age, 18)          // age >= 18
lt(users.age, 65)           // age < 65
lte(users.age, 30)          // age <= 30
like(users.name, "A%")      // name LIKE 'A%'
notLike(users.name, "A%")   // name NOT LIKE 'A%'
ilike(users.email, "%@x%")  // email ILIKE '%@x%'
notIlike(users.email, "%@x%")

// Comparacao null-safe
isDistinctFrom(users.age, null)     // age IS DISTINCT FROM NULL
isNotDistinctFrom(users.age, null)  // age IS NOT DISTINCT FROM NULL
```

### Logicas

```typescript
import { and, or, not } from "@petradb/quarry";

and(eq(users.active, true), gt(users.age, 18))
or(eq(users.name, "Alice"), eq(users.name, "Bob"))
not(eq(users.active, false))
```

`and()` e `or()` aceitam qualquer numero de argumentos:

```typescript
and(cond1, cond2, cond3) // cond1 AND cond2 AND cond3
```

### Verificacoes de null

```typescript
import { isNull, isNotNull } from "@petradb/quarry";

isNull(users.age)     // age IS NULL
isNotNull(users.age)  // age IS NOT NULL
```

### Testes booleanos

```typescript
import { isTrue, isNotTrue, isFalse, isNotFalse, isUnknown, isNotUnknown } from "@petradb/quarry";

isTrue(users.active)       // active IS TRUE
isNotTrue(users.active)    // active IS NOT TRUE
isFalse(users.active)      // active IS FALSE
isNotFalse(users.active)   // active IS NOT FALSE
isUnknown(users.active)    // active IS UNKNOWN
isNotUnknown(users.active) // active IS NOT UNKNOWN
```

### Colecoes

```typescript
import { inList, notInList, between, notBetween, betweenSymmetric } from "@petradb/quarry";

inList(users.name, ["Alice", "Bob", "Charlie"])  // name IN (...)
notInList(users.id, [1, 2, 3])                   // id NOT IN (...)
between(users.age, 18, 65)                       // age BETWEEN 18 AND 65
notBetween(users.age, 18, 65)                    // age NOT BETWEEN 18 AND 65
betweenSymmetric(users.age, 65, 18)              // age BETWEEN SYMMETRIC 65 AND 18
notBetweenSymmetric(users.age, 65, 18)           // age NOT BETWEEN SYMMETRIC 65 AND 18
```

### Aritmetica

```typescript
import { add, sub, mul, div, mod, pow, neg } from "@petradb/quarry";

add(users.age, 10)  // age + 10
sub(users.age, 5)   // age - 5
mul(users.age, 2)   // age * 2
div(users.age, 3)   // age / 3
mod(users.age, 2)   // age % 2
pow(users.age, 2)   // age ^ 2
neg(users.age)      // -age
```

### Operadores de string

```typescript
import { concat } from "@petradb/quarry";

concat(users.name, " Jr.")  // name || ' Jr.'
```

### Operadores bitwise

```typescript
import { bitAnd, bitOr, bitXor, bitNot, leftShift, rightShift } from "@petradb/quarry";

bitAnd(users.flags, 0xFF)   // flags & 255
bitOr(users.flags, 1)       // flags | 1
bitXor(users.flags, 0xFF)   // flags # 255
bitNot(users.flags)         // ~flags
leftShift(users.flags, 2)   // flags << 2
rightShift(users.flags, 1)  // flags >> 1
```

### Operadores JSON

```typescript
import { jsonGet, jsonGetText, jsonPath, jsonPathText } from "@petradb/quarry";
import { jsonContains, jsonContainedBy, jsonHasKey, jsonHasAnyKey, jsonHasAllKeys } from "@petradb/quarry";

jsonGet(t.data, "name")       // data -> 'name'
jsonGetText(t.data, "name")   // data ->> 'name'
jsonPath(t.data, path)        // data #> path
jsonPathText(t.data, path)    // data #>> path
jsonContains(t.data, other)   // data @> other
jsonContainedBy(t.data, other) // data <@ other
jsonHasKey(t.data, "key")     // data ? 'key'
jsonHasAnyKey(t.data, keys)   // data ?| keys
jsonHasAllKeys(t.data, keys)  // data ?& keys
```

### Operadores de array

```typescript
import { arrayOverlap } from "@petradb/quarry";

arrayOverlap(t.tags, t.otherTags)  // tags && otherTags (arrays se sobrepoem)
```

### Operadores genericos

Para operadores nao cobertos por um helper nomeado, use `op()` e `unaryOp()`:

```typescript
import { op, unaryOp } from "@petradb/quarry";

op(users.age, ">=", 18)       // age >= 18
unaryOp("NOT", eq(users.active, true))
```

### Expressao CASE

```typescript
import { caseWhen, literal } from "@petradb/quarry";

caseWhen(
  [
    { when: gt(users.age, 60), then: literal("senior") },
    { when: gt(users.age, 18), then: literal("adult") },
  ],
  "minor", // else
)
```

### Expressao CAST

```typescript
import { cast } from "@petradb/quarry";

cast(users.age, "text")    // CAST(age AS TEXT)
cast(users.age, "double")  // CAST(age AS DOUBLE)
```

### Aliases e literais

```typescript
import { alias, literal } from "@petradb/quarry";

alias(add(users.age, 10), "age_plus_10")

literal("hello")  // string
literal(42)        // number
literal(true)      // boolean
literal(null)      // null
```

## Agregacoes e agrupamento

### Agregacoes embutidas

```typescript
import { count, sum, avg, min, max, alias } from "@petradb/quarry";
import { stringAgg, arrayAgg, boolAnd, boolOr, jsonAgg, jsonObjectAgg } from "@petradb/quarry";

// Contar todas as linhas
const [{ total }] = await db
  .select(users)
  .columns(alias(count(), "total"))
  .execute();

// Agrupar por com agregacao
const stats = await db
  .select(users)
  .columns(users.active, alias(count(), "cnt"))
  .groupBy(users.active)
  .execute();

// Having
const popular = await db
  .select(users)
  .columns(users.active, alias(count(), "cnt"))
  .groupBy(users.active)
  .having(gt(alias(count(), "cnt"), 5))
  .execute();

// Outras agregacoes
sum(users.age)                              // SUM(age)
avg(users.age)                              // AVG(age)
min(users.age)                              // MIN(age)
max(users.age)                              // MAX(age)
stringAgg(users.name, ", ")                 // STRING_AGG(name, ', ')
arrayAgg(users.name)                        // ARRAY_AGG(name)
boolAnd(users.active)                       // BOOL_AND(active)
boolOr(users.active)                        // BOOL_OR(active)
jsonAgg(users.name)                         // JSON_AGG(name)
jsonObjectAgg(users.name, users.age)        // JSON_OBJECT_AGG(name, age)
```

### Agregacoes estatisticas

```typescript
import { variance, varSamp, varPop, stddev, stddevSamp, stddevPop } from "@petradb/quarry";

variance(emp.salary)   // VARIANCE(salary) — variancia amostral
varSamp(emp.salary)    // VAR_SAMP(salary) — igual a variance
varPop(emp.salary)     // VAR_POP(salary) — variancia populacional
stddev(emp.salary)     // STDDEV(salary) — desvio padrao amostral
stddevSamp(emp.salary) // STDDEV_SAMP(salary) — igual a stddev
stddevPop(emp.salary)  // STDDEV_POP(salary) — desvio padrao populacional
```

### Agregacoes bitwise

```typescript
import { bitAndAgg, bitOrAgg, bitXorAgg } from "@petradb/quarry";

bitAndAgg(emp.flags)  // BIT_AND(flags)
bitOrAgg(emp.flags)   // BIT_OR(flags)
bitXorAgg(emp.flags)  // BIT_XOR(flags)
```

### EVERY

```typescript
import { every } from "@petradb/quarry";

every(emp.active)  // EVERY(active) — verdadeiro quando todas as linhas sao verdadeiras
```

### FILTER em agregacoes

Restrinja quais linhas uma agregacao processa com `filter()`:

```typescript
import { filter } from "@petradb/quarry";

// COUNT(*) FILTER (WHERE salary > 100)
filter(count(), gt(emp.salary, 100))

// SUM(salary) FILTER (WHERE active = true)
filter(sum(emp.salary), eq(emp.active, true))
```

Exemplo com multiplas agregacoes filtradas:

```typescript
const [row] = await db
  .select(employees)
  .columns(
    alias(count(), "total"),
    alias(filter(count(), gt(employees.salary, 100)), "high_earners"),
    alias(filter(sum(employees.salary), eq(employees.active, true)), "active_payroll"),
  )
  .execute();
```

## Funcoes

Chame qualquer funcao SQL com `fn()`:

```typescript
import { fn } from "@petradb/quarry";

fn("upper", users.name)        // UPPER(name)
fn("coalesce", users.age, 0)   // COALESCE(age, 0)
fn("length", users.name)       // LENGTH(name)
fn("lower", users.email)       // LOWER(email)
fn("abs", users.age)           // ABS(age)
fn("round", users.score, 2)    // ROUND(score, 2)
```

## Joins

O Quarry suporta joins inner, left, right, full e cross com tipagem de resultado em tempo de compilacao.

### Inner join

Todas as colunas de ambas as tabelas sao incluidas no resultado. A nulabilidade e preservada do schema original:

```typescript
const posts = table("posts", {
  id: serial("id").primaryKey(),
  userId: integer("user_id").notNull(),
  title: text("title").notNull(),
  body: text("body"),
});

const rows = await db
  .select(users)
  .innerJoin(posts, eq(users.id, posts.userId))
  .where(eq(users.name, "Alice"))
  .execute();

// Tipo do resultado: (InferSelect<users> & InferSelect<posts>)[]
// rows[0].name  -> string
// rows[0].title -> string
// rows[0].body  -> string | null (nulavel no schema de posts)
```

### Left join

As colunas da tabela juntada se tornam todas nulaveis, ja que linhas sem correspondencia produzem `null`:

```typescript
const rows = await db
  .select(users)
  .leftJoin(posts, eq(users.id, posts.userId))
  .execute();

// Tipo do resultado: (InferSelect<users> & Nullable<InferSelect<posts>>)[]
// rows[0].name   -> string       (tabela base, nao afetada)
// rows[0].title  -> string | null (left join torna nulavel)
// rows[0].userId -> number | null (left join torna nulavel)
```

### Right join

As colunas da tabela base se tornam nulaveis, as colunas da tabela juntada preservam sua nulabilidade original:

```typescript
const rows = await db
  .select(users)
  .rightJoin(posts, eq(users.id, posts.userId))
  .execute();

// Tipo do resultado: (Nullable<InferSelect<users>> & InferSelect<posts>)[]
// rows[0].name  -> string | null (right join torna tabela base nulavel)
// rows[0].title -> string        (tabela juntada, nao afetada)
```

### Full join

Ambos os lados se tornam nulaveis:

```typescript
const rows = await db
  .select(users)
  .fullJoin(posts, eq(users.id, posts.userId))
  .execute();

// Tipo do resultado: (Nullable<InferSelect<users>> & Nullable<InferSelect<posts>>)[]
// rows[0].name  -> string | null
// rows[0].title -> string | null
```

### Cross join

Produz o produto cartesiano de ambas as tabelas — sem condicao `on`:

```typescript
const rows = await db
  .select(users)
  .crossJoin(posts)
  .execute();

// Tipo do resultado: (InferSelect<users> & InferSelect<posts>)[]
// Cada combinacao de usuario x post
```

### Joins encadeados

Multiplos joins acumulam tipos corretamente:

```typescript
const comments = table("comments", {
  id: serial("id").primaryKey(),
  postId: integer("post_id").notNull(),
  content: text("content").notNull(),
});

const rows = await db
  .select(users)
  .innerJoin(posts, eq(users.id, posts.userId))
  .leftJoin(comments, eq(posts.id, comments.postId))
  .execute();

// colunas de posts: nao nulas (inner join)
// colunas de comments: nulaveis (left join)
// rows[0].title   -> string        (inner join)
// rows[0].content -> string | null  (left join)
```

### Join com selecao de colunas

```typescript
const rows = await db
  .select(users)
  .columns(users.name, posts.title)
  .innerJoin(posts, eq(users.id, posts.userId))
  .execute();
```

### Join com agregacoes

```typescript
const rows = await db
  .select(users)
  .columns(users.name, alias(count(), "post_count"))
  .innerJoin(posts, eq(users.id, posts.userId))
  .groupBy(users.name)
  .orderBy(desc(alias(count(), "post_count")))
  .execute();
```

## Aliases de tabela

Use `tableAs()` para criar tabelas com alias para self-joins ou quando a mesma tabela aparece varias vezes:

```typescript
import { tableAs } from "@petradb/quarry";

const mgr = tableAs(employees, "mgr");
const emp = tableAs(employees, "emp");

const rows = await db
  .select(emp)
  .columns(
    alias(emp.name, "employee"),
    alias(mgr.name, "manager"),
  )
  .leftJoin(mgr, eq(emp.managerId, mgr.id))
  .execute();
```

Aliases sao type-safe — `mgr.name` ainda impoe que `name` exista no schema de employees.

## Subconsultas

### Subconsulta IN

```typescript
import { inSubquery, notInSubquery } from "@petradb/quarry";

// Usuarios que tem pelo menos um post
const rows = await db
  .select(users)
  .where(
    inSubquery(
      users.id,
      db.select(posts).columns(posts.userId).toExpr(),
    ),
  )
  .execute();

// Usuarios que NAO tem posts
const rows = await db
  .select(users)
  .where(
    notInSubquery(
      users.id,
      db.select(posts).columns(posts.userId).toExpr(),
    ),
  )
  .execute();
```

### Subconsulta EXISTS

```typescript
import { exists } from "@petradb/quarry";

const rows = await db
  .select(users)
  .where(
    exists(
      db
        .select(posts)
        .columns(literal(1))
        .where(eq(posts.userId, users.id))
        .toExpr(),
    ),
  )
  .execute();
```

### Subconsulta escalar

Use `subquery()` para encapsular um select como valor escalar:

```typescript
import { subquery } from "@petradb/quarry";

// Usuarios mais velhos que a idade media
const rows = await db
  .select(users)
  .where(
    gt(
      users.age,
      subquery(db.select(users).columns(avg(users.age)).toExpr()),
    ),
  )
  .execute();
```

:::note
Use `.toExpr()` (nao `.toAST()`) ao embutir um select como subconsulta. `.toExpr()` retorna o no `ASTSelect` bruto, enquanto `.toAST()` o encapsula em um `QueryCommand`.
:::

## Ordenacao

```typescript
import { asc, desc } from "@petradb/quarry";

// Ordenacao basica
db.select(users).orderBy(asc(users.name))
db.select(users).orderBy(desc(users.age))

// Multiplas colunas
db.select(users).orderBy(asc(users.name), desc(users.age))

// NULLS FIRST / NULLS LAST
db.select(users).orderBy(asc(users.age, { nulls: "first" }))
db.select(users).orderBy(desc(users.age, { nulls: "last" }))
```

Quando `nulls` nao e especificado, o engine usa o comportamento padrao (nulos vem por ultimo em ordem ascendente, primeiro em ordem descendente).

## Update

```typescript
// Update com where
const result = await db
  .update(users)
  .set({ age: 31 })
  .where(eq(users.name, "Alice"))
  .execute();
// result.rowCount -> 1

// Atualizar multiplos campos
await db
  .update(users)
  .set({ name: "Alice Smith", age: 32, active: false })
  .where(eq(users.id, 1))
  .execute();

// Definir como null
await db
  .update(users)
  .set({ age: null })
  .where(eq(users.name, "Bob"))
  .execute();
```

O metodo `.set()` aceita `Partial<InferSelect<T>>` — TypeScript impoe nomes de coluna e tipos validos.

### UPDATE...FROM

Junte outra tabela para direcionar atualizacoes:

```typescript
const priceUpdates = table("price_updates", {
  id: serial("id").primaryKey(),
  productName: text("product_name").notNull(),
  newPrice: integer("new_price").notNull(),
});

await db
  .update(products)
  .set({ price: 0 }) // definir valor; use referencias de coluna no WHERE para logica condicional
  .from(priceUpdates)
  .where(eq(products.name, priceUpdates.productName))
  .execute();
```

`.from()` aceita multiplas tabelas:

```typescript
db.update(t1).set({ ... }).from(t2, t3).where(and(...))
```

### RETURNING

Update e delete suportam `.returning()` para obter de volta as linhas afetadas:

```typescript
const result = await db
  .update(users)
  .set({ active: false })
  .where(lt(users.age, 18))
  .returning(users.id, users.name)
  .execute();
// result.rows -> [{ id: 3, name: "Charlie" }, ...]
```

## Delete

```typescript
const result = await db
  .delete(users)
  .where(eq(users.name, "Alice"))
  .execute();
// result.rowCount -> 1
```

### DELETE...USING

Junte outra tabela para determinar quais linhas excluir:

```typescript
const deleteList = table("delete_list", {
  id: serial("id").primaryKey(),
  userName: text("user_name").notNull(),
});

await db
  .delete(users)
  .using(deleteList)
  .where(eq(users.name, deleteList.userName))
  .execute();
```

`.using()` aceita multiplas tabelas:

```typescript
db.delete(t1).using(t2, t3).where(and(...))
```

## Transacoes

Encapsule multiplas operacoes em uma transacao com commit/rollback automatico:

```typescript
const result = await db.transaction(async (tx) => {
  const [user] = await tx
    .insert(users)
    .values({ name: "Alice", email: "alice@example.com" })
    .execute();

  await tx
    .insert(posts)
    .values({ userId: user.id, title: "First Post" })
    .execute();

  return user;
});
// Se qualquer operacao lancar erro, toda a transacao sofre rollback
```

O callback recebe uma instancia `QuarryDB` com escopo na transacao. O valor de retorno do callback se torna o valor de retorno de `transaction()`, com o tipo preservado.

## Inspecao de AST

Todo builder tem um metodo `.toAST()` que retorna o objeto AST bruto sem executa-lo. Isso e util para depuracao, logging ou construcao de abstracoes de nivel superior:

```typescript
const ast = db
  .select(users)
  .where(eq(users.name, "Alice"))
  .orderBy(asc(users.id))
  .limit(10)
  .toAST();

console.log(JSON.stringify(ast, null, 2));
// {
//   "kind": "query",
//   "query": {
//     "kind": "select",
//     "exprs": [{ "kind": "star" }],
//     "from": [{ "kind": "table", "name": "users" }],
//     "where": { "kind": "binary", "left": ..., "op": "=", "right": ... },
//     "orderBy": [{ "expr": ..., "direction": "asc" }],
//     "limit": 10
//   }
// }
```

## Como funciona

O Quarry constroi objetos JavaScript simples (unions discriminadas com um campo `kind`) que representam o AST da consulta. Quando voce chama `.execute()`, esses objetos sao passados para o metodo `executeAST()` do engine, que os converte diretamente no AST interno Scala do engine — pulando completamente a geracao e o parsing de strings SQL.

```
Schema -> Builder API -> Objetos AST JS -> AST Engine -> Reescrita -> Execucao
                              ↑ sem parser SQL
```

Isso da ao Quarry as mesmas capacidades de consulta que SQL enquanto elimina overhead de parsing e habilita seguranca de tipo completa em tempo de compilacao.

## Limpeza

```typescript
await session.close();
```

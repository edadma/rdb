---
title: Quarry
description: Constructor de consultas con seguridad de tipos para PetraDB que genera AST en lugar de SQL.
---

Quarry es un constructor de consultas con seguridad de tipos para PetraDB que genera objetos AST en lugar de cadenas SQL, omitiendo el parser por completo. Las definiciones de esquema sirven como fuente unica de verdad para DDL, consultas y tipos TypeScript en tiempo de compilacion.

## Instalacion

```bash
npm install @petradb/quarry @petradb/engine
```

## Configuracion

```typescript
import { Session } from "@petradb/engine";
import { quarry } from "@petradb/quarry";

const session = new Session({ storage: "memory" });
const db = quarry(session);
```

### Modos de almacenamiento

```typescript
// En memoria (por defecto)
new Session({ storage: "memory" });

// Almacenamiento persistente respaldado por archivo
new Session({ storage: "persistent", path: "./mydb.petra" });
```

## Definicion de esquema

Define tablas usando los constructores de columna de Quarry. El esquema impulsa la creacion de tablas, la construccion de consultas y la inferencia de tipos TypeScript:

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

### Tipos de columna

| Constructor | Tipo SQL | Tipo TypeScript |
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

### Modificadores de columna

| Modificador | Efecto |
|---|---|
| `.notNull()` | La columna no puede ser nula; el tipo `InferSelect` excluye `null` |
| `.default(value)` | La columna es opcional en `InferInsert` |
| `.primaryKey()` | Clave primaria; implica notNull + hasDefault (auto-incremento para serial) |
| `.unique()` | Agrega restriccion de unicidad |
| `.references(table, column)` | Agrega referencia de clave foranea |

### Tipos inferidos

Quarry infiere dos tipos de cada definicion de tabla:

```typescript
import type { InferSelect, InferInsert } from "@petradb/quarry";

type User = InferSelect<typeof users>;
// { id: number, name: string, email: string, age: number | null, active: boolean }

type NewUser = InferInsert<typeof users>;
// { name: string, email: string, age?: number | null, active?: boolean, id?: number }
```

**`InferSelect`** — el tipo de fila retornado por consultas:
- Columnas `notNull` → tipo no anulable
- Columnas anulables → `type | null`

**`InferInsert`** — el tipo aceptado por `.values()`:
- Columnas `notNull` sin valores por defecto → requeridas
- Columnas con valores por defecto (`.default()`, `.primaryKey()`, serial) → opcionales
- Columnas anulables → opcionales, aceptan `null`

## Crear tabla

```typescript
await db.createTable(users);
```

Esto genera y ejecuta un comando `CREATE TABLE` desde la definicion del esquema — sin SQL necesario.

## Insert

```typescript
// Una sola fila — retorna la fila insertada con todas las columnas
const [user] = await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com", age: 30 })
  .execute();
// user.id → serial auto-generado
// user.active → true (por defecto)

// Multiples filas
await db
  .insert(users)
  .values(
    { name: "Bob", email: "bob@example.com", age: 25 },
    { name: "Charlie", email: "charlie@example.com" },
  )
  .execute();
```

Insert requiere todas las columnas `notNull` sin valores por defecto. Los campos opcionales pueden omitirse. TypeScript lo aplica en tiempo de compilacion.

### RETURNING

Por defecto, insert retorna todas las columnas (`*`). Usa `.returning()` para seleccionar columnas especificas:

```typescript
const [{ id }] = await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com" })
  .returning(users.id)
  .execute();
```

### Upsert (ON CONFLICT)

Maneja conflictos en insert con `onConflictDoNothing()` u `onConflictDoUpdate()`:

```typescript
// Omitir filas en conflicto silenciosamente
await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com" })
  .onConflictDoNothing()
  .execute();

// Actualizar columnas especificas cuando ocurre un conflicto
await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com", age: 31 })
  .onConflictDoUpdate(["email"], { name: "Alice Updated", age: 31 })
  .execute();
```

El primer argumento de `onConflictDoUpdate` especifica las columnas de conflicto, el segundo especifica que columnas actualizar. Ambos tienen seguridad de tipos — TypeScript asegura que solo se usen claves de columna validas.

### INSERT...SELECT

Insertar filas desde una consulta en lugar de valores literales:

```typescript
// Archivar todos los usuarios activos
const query = db
  .select(users)
  .columns(users.name, users.email)
  .where(eq(users.active, true))
  .toExpr();

await db.insertFrom(archive, query, ["name", "email"]).execute();
```

El segundo argumento es la consulta select (usa `.toExpr()`). El tercer argumento opcional especifica que columnas destino llenar — si se omite, el motor espera que la consulta produzca valores para todas las columnas.

```typescript
// Sin lista de columnas (la consulta debe coincidir con todas las columnas destino)
await db.insertFrom(archive, query).execute();

// Con onConflictDoNothing
await db.insertFrom(archive, query, ["name", "email"]).onConflictDoNothing().execute();
```

## Select

```typescript
import { eq, gt, asc, desc } from "@petradb/quarry";

// Todas las filas
const allUsers = await db.select(users).execute();

// Clausula where
const alice = await db
  .select(users)
  .where(eq(users.name, "Alice"))
  .execute();

// Columnas especificas
const names = await db
  .select(users)
  .columns(users.name, users.email)
  .execute();

// Orden, limite, desplazamiento
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

// Distinct on — una fila por valor distinto de las columnas dadas
const perCategory = await db
  .select(products)
  .distinctOn(products.category)
  .orderBy(asc(products.category), asc(products.price))
  .execute();
// Retorna el producto mas barato en cada categoria
```

### Referencias de columna

Las columnas se acceden directamente como propiedades del objeto tabla. TypeScript previene el acceso a columnas que no existen en el esquema:

```typescript
users.name;  // ✓ compila
users.title; // ✗ error de compilacion — 'title' no esta en users
```

## Expresiones

### Comparacion

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

// Comparacion segura ante nulos
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

`and()` y `or()` aceptan cualquier numero de argumentos:

```typescript
and(cond1, cond2, cond3) // cond1 AND cond2 AND cond3
```

### Verificaciones de nulos

```typescript
import { isNull, isNotNull } from "@petradb/quarry";

isNull(users.age)     // age IS NULL
isNotNull(users.age)  // age IS NOT NULL
```

### Pruebas booleanas

```typescript
import { isTrue, isNotTrue, isFalse, isNotFalse, isUnknown, isNotUnknown } from "@petradb/quarry";

isTrue(users.active)       // active IS TRUE
isNotTrue(users.active)    // active IS NOT TRUE
isFalse(users.active)      // active IS FALSE
isNotFalse(users.active)   // active IS NOT FALSE
isUnknown(users.active)    // active IS UNKNOWN
isNotUnknown(users.active) // active IS NOT UNKNOWN
```

### Colecciones

```typescript
import { inList, notInList, between, notBetween, betweenSymmetric } from "@petradb/quarry";

inList(users.name, ["Alice", "Bob", "Charlie"])  // name IN (...)
notInList(users.id, [1, 2, 3])                   // id NOT IN (...)
between(users.age, 18, 65)                       // age BETWEEN 18 AND 65
notBetween(users.age, 18, 65)                    // age NOT BETWEEN 18 AND 65
betweenSymmetric(users.age, 65, 18)              // age BETWEEN SYMMETRIC 65 AND 18
notBetweenSymmetric(users.age, 65, 18)           // age NOT BETWEEN SYMMETRIC 65 AND 18
```

### Aritmeticas

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

### Operadores de cadena

```typescript
import { concat } from "@petradb/quarry";

concat(users.name, " Jr.")  // name || ' Jr.'
```

### Operadores bit a bit

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

arrayOverlap(t.tags, t.otherTags)  // tags && otherTags (superposicion de arrays)
```

### Operadores genericos

Para operadores no cubiertos por un helper con nombre, usa `op()` y `unaryOp()`:

```typescript
import { op, unaryOp } from "@petradb/quarry";

op(users.age, ">=", 18)       // age >= 18
unaryOp("NOT", eq(users.active, true))
```

### Expresion CASE

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

### Expresion CAST

```typescript
import { cast } from "@petradb/quarry";

cast(users.age, "text")    // CAST(age AS TEXT)
cast(users.age, "double")  // CAST(age AS DOUBLE)
```

### Alias y literales

```typescript
import { alias, literal } from "@petradb/quarry";

alias(add(users.age, 10), "age_plus_10")

literal("hello")  // string
literal(42)        // number
literal(true)      // boolean
literal(null)      // null
```

## Agregados y agrupacion

### Agregados integrados

```typescript
import { count, sum, avg, min, max, alias } from "@petradb/quarry";
import { stringAgg, arrayAgg, boolAnd, boolOr, jsonAgg, jsonObjectAgg } from "@petradb/quarry";

// Contar todas las filas
const [{ total }] = await db
  .select(users)
  .columns(alias(count(), "total"))
  .execute();

// Agrupar por con agregado
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

// Otros agregados
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

### Agregados estadisticos

```typescript
import { variance, varSamp, varPop, stddev, stddevSamp, stddevPop } from "@petradb/quarry";

variance(emp.salary)   // VARIANCE(salary) — varianza muestral
varSamp(emp.salary)    // VAR_SAMP(salary) — igual que variance
varPop(emp.salary)     // VAR_POP(salary) — varianza poblacional
stddev(emp.salary)     // STDDEV(salary) — desviacion estandar muestral
stddevSamp(emp.salary) // STDDEV_SAMP(salary) — igual que stddev
stddevPop(emp.salary)  // STDDEV_POP(salary) — desviacion estandar poblacional
```

### Agregados bit a bit

```typescript
import { bitAndAgg, bitOrAgg, bitXorAgg } from "@petradb/quarry";

bitAndAgg(emp.flags)  // BIT_AND(flags)
bitOrAgg(emp.flags)   // BIT_OR(flags)
bitXorAgg(emp.flags)  // BIT_XOR(flags)
```

### EVERY

```typescript
import { every } from "@petradb/quarry";

every(emp.active)  // EVERY(active) — true cuando todas las filas son true
```

### FILTER en agregados

Restringe que filas procesa un agregado con `filter()`:

```typescript
import { filter } from "@petradb/quarry";

// COUNT(*) FILTER (WHERE salary > 100)
filter(count(), gt(emp.salary, 100))

// SUM(salary) FILTER (WHERE active = true)
filter(sum(emp.salary), eq(emp.active, true))
```

Ejemplo con multiples agregados filtrados:

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

## Funciones

Llama a cualquier funcion SQL con `fn()`:

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

Quarry soporta inner, left, right, full y cross joins con tipado de resultado en tiempo de compilacion.

### Inner join

Todas las columnas de ambas tablas se incluyen en el resultado. La nulabilidad se preserva del esquema original:

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

// Tipo de resultado: (InferSelect<users> & InferSelect<posts>)[]
// rows[0].name  → string
// rows[0].title → string
// rows[0].body  → string | null (anulable en el esquema de posts)
```

### Left join

Las columnas de la tabla unida se vuelven todas anulables, ya que las filas sin coincidencia producen `null`:

```typescript
const rows = await db
  .select(users)
  .leftJoin(posts, eq(users.id, posts.userId))
  .execute();

// Tipo de resultado: (InferSelect<users> & Nullable<InferSelect<posts>>)[]
// rows[0].name   → string       (tabla base, no afectada)
// rows[0].title  → string | null (left join la hace anulable)
// rows[0].userId → number | null (left join la hace anulable)
```

### Right join

Las columnas de la tabla base se vuelven anulables, las columnas de la tabla unida preservan su nulabilidad original:

```typescript
const rows = await db
  .select(users)
  .rightJoin(posts, eq(users.id, posts.userId))
  .execute();

// Tipo de resultado: (Nullable<InferSelect<users>> & InferSelect<posts>)[]
// rows[0].name  → string | null (right join hace la tabla base anulable)
// rows[0].title → string        (tabla unida, no afectada)
```

### Full join

Ambos lados se vuelven anulables:

```typescript
const rows = await db
  .select(users)
  .fullJoin(posts, eq(users.id, posts.userId))
  .execute();

// Tipo de resultado: (Nullable<InferSelect<users>> & Nullable<InferSelect<posts>>)[]
// rows[0].name  → string | null
// rows[0].title → string | null
```

### Cross join

Produce el producto cartesiano de ambas tablas — sin condicion `on`:

```typescript
const rows = await db
  .select(users)
  .crossJoin(posts)
  .execute();

// Tipo de resultado: (InferSelect<users> & InferSelect<posts>)[]
// Cada combinacion de usuario × post
```

### Joins encadenados

Multiples joins acumulan tipos correctamente:

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

// columnas de posts: no nulas (inner join)
// columnas de comments: anulables (left join)
// rows[0].title   → string        (inner join)
// rows[0].content → string | null  (left join)
```

### Join con columnas seleccionadas

```typescript
const rows = await db
  .select(users)
  .columns(users.name, posts.title)
  .innerJoin(posts, eq(users.id, posts.userId))
  .execute();
```

### Join con agregados

```typescript
const rows = await db
  .select(users)
  .columns(users.name, alias(count(), "post_count"))
  .innerJoin(posts, eq(users.id, posts.userId))
  .groupBy(users.name)
  .orderBy(desc(alias(count(), "post_count")))
  .execute();
```

## Alias de tablas

Usa `tableAs()` para crear tablas con alias para self-joins o cuando la misma tabla aparece multiples veces:

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

Los alias tienen seguridad de tipos — `mgr.name` aun asegura que `name` exista en el esquema de employees.

## Subconsultas

### Subconsulta IN

```typescript
import { inSubquery, notInSubquery } from "@petradb/quarry";

// Usuarios que tienen al menos un post
const rows = await db
  .select(users)
  .where(
    inSubquery(
      users.id,
      db.select(posts).columns(posts.userId).toExpr(),
    ),
  )
  .execute();

// Usuarios que NO tienen posts
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

Usa `subquery()` para envolver un select como un valor escalar:

```typescript
import { subquery } from "@petradb/quarry";

// Usuarios mayores que la edad promedio
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
Usa `.toExpr()` (no `.toAST()`) cuando incrustes un select como subconsulta. `.toExpr()` retorna el nodo `ASTSelect` sin envolver, mientras que `.toAST()` lo envuelve en un `QueryCommand`.
:::

## Ordenamiento

```typescript
import { asc, desc } from "@petradb/quarry";

// Ordenamiento basico
db.select(users).orderBy(asc(users.name))
db.select(users).orderBy(desc(users.age))

// Multiples columnas
db.select(users).orderBy(asc(users.name), desc(users.age))

// NULLS FIRST / NULLS LAST
db.select(users).orderBy(asc(users.age, { nulls: "first" }))
db.select(users).orderBy(desc(users.age, { nulls: "last" }))
```

Cuando `nulls` no se especifica, el motor usa el comportamiento por defecto (los nulos se ordenan al final en orden ascendente, primero en orden descendente).

## Update

```typescript
// Update con where
const result = await db
  .update(users)
  .set({ age: 31 })
  .where(eq(users.name, "Alice"))
  .execute();
// result.rowCount → 1

// Actualizar multiples campos
await db
  .update(users)
  .set({ name: "Alice Smith", age: 32, active: false })
  .where(eq(users.id, 1))
  .execute();

// Establecer a null
await db
  .update(users)
  .set({ age: null })
  .where(eq(users.name, "Bob"))
  .execute();
```

El metodo `.set()` acepta `Partial<InferSelect<T>>` — TypeScript asegura nombres de columna y tipos validos.

### UPDATE...FROM

Unir otra tabla para impulsar actualizaciones:

```typescript
const priceUpdates = table("price_updates", {
  id: serial("id").primaryKey(),
  productName: text("product_name").notNull(),
  newPrice: integer("new_price").notNull(),
});

await db
  .update(products)
  .set({ price: 0 }) // establecer valor; usar referencias de columna en WHERE para logica condicional
  .from(priceUpdates)
  .where(eq(products.name, priceUpdates.productName))
  .execute();
```

`.from()` acepta multiples tablas:

```typescript
db.update(t1).set({ ... }).from(t2, t3).where(and(...))
```

### RETURNING

Update y delete soportan `.returning()` para obtener las filas afectadas:

```typescript
const result = await db
  .update(users)
  .set({ active: false })
  .where(lt(users.age, 18))
  .returning(users.id, users.name)
  .execute();
// result.rows → [{ id: 3, name: "Charlie" }, ...]
```

## Delete

```typescript
const result = await db
  .delete(users)
  .where(eq(users.name, "Alice"))
  .execute();
// result.rowCount → 1
```

### DELETE...USING

Unir otra tabla para determinar que filas eliminar:

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

`.using()` acepta multiples tablas:

```typescript
db.delete(t1).using(t2, t3).where(and(...))
```

## Transacciones

Envuelve multiples operaciones en una transaccion con commit/rollback automatico:

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
// Si cualquier operacion lanza una excepcion, toda la transaccion se revierte
```

El callback recibe una instancia `QuarryDB` vinculada a la transaccion. El valor de retorno del callback se convierte en el valor de retorno de `transaction()`, con el tipo preservado.

## Inspeccion del AST

Cada builder tiene un metodo `.toAST()` que retorna el objeto AST sin ejecutarlo. Esto es util para depuracion, registro o construir abstracciones de nivel superior:

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

Quarry construye objetos JavaScript simples (uniones discriminadas con un campo `kind`) que representan el AST de la consulta. Cuando llamas a `.execute()`, estos objetos se pasan al metodo `executeAST()` del motor, que los convierte directamente en el AST interno de Scala del motor — omitiendo la generacion y analisis de cadenas SQL por completo.

```
Esquema → API del Builder → Objetos AST JS → AST del Motor → Reescritura → Ejecucion
                                   ↑ sin parser SQL
```

Esto le da a Quarry las mismas capacidades de consulta que SQL mientras elimina la sobrecarga de analisis y habilita seguridad de tipos completa en tiempo de compilacion.

## Limpieza

```typescript
await session.close();
```

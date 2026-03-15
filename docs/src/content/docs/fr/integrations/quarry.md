---
title: Quarry
description: Constructeur de requetes type-safe pour PetraDB generant un AST au lieu de SQL.
---

Quarry est un constructeur de requetes type-safe pour PetraDB qui genere des objets AST au lieu de chaines SQL, contournant entierement le parseur. Les definitions de schemas servent de source unique de verite pour le DDL, les requetes et les types TypeScript a la compilation.

## Installation

```bash
npm install @petradb/quarry @petradb/engine
```

## Configuration

```typescript
import { Session } from "@petradb/engine";
import { quarry } from "@petradb/quarry";

const session = new Session({ storage: "memory" });
const db = quarry(session);
```

### Modes de stockage

```typescript
// En memoire (par defaut)
new Session({ storage: "memory" });

// Stockage persistant sur fichier
new Session({ storage: "persistent", path: "./mydb.petra" });
```

## Definition du schema

Definissez les tables en utilisant les constructeurs de colonnes de Quarry. Le schema pilote la creation de tables, la construction de requetes et l'inference de types TypeScript :

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

### Types de colonnes

| Constructeur | Type SQL | Type TypeScript |
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

### Modificateurs de colonnes

| Modificateur | Effet |
|---|---|
| `.notNull()` | La colonne ne peut pas etre null ; le type `InferSelect` exclut `null` |
| `.default(value)` | La colonne est optionnelle dans `InferInsert` |
| `.primaryKey()` | Cle primaire ; implique notNull + hasDefault (auto-increment pour serial) |
| `.unique()` | Ajoute une contrainte d'unicite |
| `.references(table, column)` | Ajoute une reference de cle etrangere |

### Types inferes

Quarry infere deux types a partir de chaque definition de table :

```typescript
import type { InferSelect, InferInsert } from "@petradb/quarry";

type User = InferSelect<typeof users>;
// { id: number, name: string, email: string, age: number | null, active: boolean }

type NewUser = InferInsert<typeof users>;
// { name: string, email: string, age?: number | null, active?: boolean, id?: number }
```

**`InferSelect`** -- le type de ligne retourne par les requetes :
- colonnes `notNull` -> type non-nullable
- colonnes nullables -> `type | null`

**`InferInsert`** -- le type accepte par `.values()` :
- colonnes `notNull` sans valeur par defaut -> obligatoire
- colonnes avec valeur par defaut (`.default()`, `.primaryKey()`, serial) -> optionnel
- colonnes nullables -> optionnel, accepte `null`

## Creer une table

```typescript
await db.createTable(users);
```

Cela genere et execute une commande `CREATE TABLE` a partir de la definition du schema -- pas de SQL necessaire.

## Insertion

```typescript
// Ligne unique — retourne la ligne inseree avec toutes les colonnes
const [user] = await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com", age: 30 })
  .execute();
// user.id → serial auto-genere
// user.active → true (valeur par defaut)

// Lignes multiples
await db
  .insert(users)
  .values(
    { name: "Bob", email: "bob@example.com", age: 25 },
    { name: "Charlie", email: "charlie@example.com" },
  )
  .execute();
```

L'insertion requiert toutes les colonnes `notNull` sans valeur par defaut. Les champs optionnels peuvent etre omis. TypeScript impose cela a la compilation.

### RETURNING

Par defaut, l'insertion retourne toutes les colonnes (`*`). Utilisez `.returning()` pour selectionner des colonnes specifiques :

```typescript
const [{ id }] = await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com" })
  .returning(users.id)
  .execute();
```

### Upsert (ON CONFLICT)

Gerez les conflits lors de l'insertion avec `onConflictDoNothing()` ou `onConflictDoUpdate()` :

```typescript
// Ignorer silencieusement les lignes en conflit
await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com" })
  .onConflictDoNothing()
  .execute();

// Mettre a jour des colonnes specifiques en cas de conflit
await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com", age: 31 })
  .onConflictDoUpdate(["email"], { name: "Alice Updated", age: 31 })
  .execute();
```

Le premier argument de `onConflictDoUpdate` specifie les colonnes de conflit, le second specifie les colonnes a mettre a jour. Les deux sont type-safe -- TypeScript impose que seules les cles de colonnes valides soient utilisees.

### INSERT...SELECT

Inserez des lignes a partir d'une requete au lieu de valeurs litterales :

```typescript
// Archiver tous les utilisateurs actifs
const query = db
  .select(users)
  .columns(users.name, users.email)
  .where(eq(users.active, true))
  .toExpr();

await db.insertFrom(archive, query, ["name", "email"]).execute();
```

Le deuxieme argument est la requete de selection (utilisez `.toExpr()`). Le troisieme argument optionnel specifie les colonnes cibles a remplir -- s'il est omis, le moteur attend que la requete produise des valeurs pour toutes les colonnes.

```typescript
// Sans liste de colonnes (la requete doit correspondre a toutes les colonnes cibles)
await db.insertFrom(archive, query).execute();

// Avec onConflictDoNothing
await db.insertFrom(archive, query, ["name", "email"]).onConflictDoNothing().execute();
```

## Selection

```typescript
import { eq, gt, asc, desc } from "@petradb/quarry";

// Toutes les lignes
const allUsers = await db.select(users).execute();

// Clause where
const alice = await db
  .select(users)
  .where(eq(users.name, "Alice"))
  .execute();

// Colonnes specifiques
const names = await db
  .select(users)
  .columns(users.name, users.email)
  .execute();

// Tri, limite, decalage
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

// Distinct on — une ligne par valeur distincte des colonnes donnees
const perCategory = await db
  .select(products)
  .distinctOn(products.category)
  .orderBy(asc(products.category), asc(products.price))
  .execute();
// Retourne le produit le moins cher dans chaque categorie
```

### References de colonnes

Les colonnes sont accessibles directement comme proprietes sur l'objet table. TypeScript empeche l'acces aux colonnes qui n'existent pas dans le schema :

```typescript
users.name;  // ✓ compile
users.title; // ✗ erreur de compilation — 'title' n'existe pas dans users
```

## Expressions

### Comparaison

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

// Comparaison null-safe
isDistinctFrom(users.age, null)     // age IS DISTINCT FROM NULL
isNotDistinctFrom(users.age, null)  // age IS NOT DISTINCT FROM NULL
```

### Logique

```typescript
import { and, or, not } from "@petradb/quarry";

and(eq(users.active, true), gt(users.age, 18))
or(eq(users.name, "Alice"), eq(users.name, "Bob"))
not(eq(users.active, false))
```

`and()` et `or()` acceptent un nombre quelconque d'arguments :

```typescript
and(cond1, cond2, cond3) // cond1 AND cond2 AND cond3
```

### Verification de null

```typescript
import { isNull, isNotNull } from "@petradb/quarry";

isNull(users.age)     // age IS NULL
isNotNull(users.age)  // age IS NOT NULL
```

### Tests booleens

```typescript
import { isTrue, isNotTrue, isFalse, isNotFalse, isUnknown, isNotUnknown } from "@petradb/quarry";

isTrue(users.active)       // active IS TRUE
isNotTrue(users.active)    // active IS NOT TRUE
isFalse(users.active)      // active IS FALSE
isNotFalse(users.active)   // active IS NOT FALSE
isUnknown(users.active)    // active IS UNKNOWN
isNotUnknown(users.active) // active IS NOT UNKNOWN
```

### Collections

```typescript
import { inList, notInList, between, notBetween, betweenSymmetric } from "@petradb/quarry";

inList(users.name, ["Alice", "Bob", "Charlie"])  // name IN (...)
notInList(users.id, [1, 2, 3])                   // id NOT IN (...)
between(users.age, 18, 65)                       // age BETWEEN 18 AND 65
notBetween(users.age, 18, 65)                    // age NOT BETWEEN 18 AND 65
betweenSymmetric(users.age, 65, 18)              // age BETWEEN SYMMETRIC 65 AND 18
notBetweenSymmetric(users.age, 65, 18)           // age NOT BETWEEN SYMMETRIC 65 AND 18
```

### Arithmetique

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

### Operateurs de chaines

```typescript
import { concat } from "@petradb/quarry";

concat(users.name, " Jr.")  // name || ' Jr.'
```

### Operateurs bit a bit

```typescript
import { bitAnd, bitOr, bitXor, bitNot, leftShift, rightShift } from "@petradb/quarry";

bitAnd(users.flags, 0xFF)   // flags & 255
bitOr(users.flags, 1)       // flags | 1
bitXor(users.flags, 0xFF)   // flags # 255
bitNot(users.flags)         // ~flags
leftShift(users.flags, 2)   // flags << 2
rightShift(users.flags, 1)  // flags >> 1
```

### Operateurs JSON

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

### Operateurs de tableaux

```typescript
import { arrayOverlap } from "@petradb/quarry";

arrayOverlap(t.tags, t.otherTags)  // tags && otherTags (les tableaux se chevauchent)
```

### Operateurs generiques

Pour les operateurs non couverts par un helper nomme, utilisez `op()` et `unaryOp()` :

```typescript
import { op, unaryOp } from "@petradb/quarry";

op(users.age, ">=", 18)       // age >= 18
unaryOp("NOT", eq(users.active, true))
```

### Expression CASE

```typescript
import { caseWhen, literal } from "@petradb/quarry";

caseWhen(
  [
    { when: gt(users.age, 60), then: literal("senior") },
    { when: gt(users.age, 18), then: literal("adult") },
  ],
  "minor", // sinon
)
```

### Expression CAST

```typescript
import { cast } from "@petradb/quarry";

cast(users.age, "text")    // CAST(age AS TEXT)
cast(users.age, "double")  // CAST(age AS DOUBLE)
```

### Alias et litteraux

```typescript
import { alias, literal } from "@petradb/quarry";

alias(add(users.age, 10), "age_plus_10")

literal("hello")  // chaine
literal(42)        // nombre
literal(true)      // booleen
literal(null)      // null
```

## Agregats et regroupement

### Agregats integres

```typescript
import { count, sum, avg, min, max, alias } from "@petradb/quarry";
import { stringAgg, arrayAgg, boolAnd, boolOr, jsonAgg, jsonObjectAgg } from "@petradb/quarry";

// Compter toutes les lignes
const [{ total }] = await db
  .select(users)
  .columns(alias(count(), "total"))
  .execute();

// Regrouper avec agregat
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

// Autres agregats
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

### Agregats statistiques

```typescript
import { variance, varSamp, varPop, stddev, stddevSamp, stddevPop } from "@petradb/quarry";

variance(emp.salary)   // VARIANCE(salary) — variance echantillon
varSamp(emp.salary)    // VAR_SAMP(salary) — identique a variance
varPop(emp.salary)     // VAR_POP(salary) — variance de population
stddev(emp.salary)     // STDDEV(salary) — ecart type echantillon
stddevSamp(emp.salary) // STDDEV_SAMP(salary) — identique a stddev
stddevPop(emp.salary)  // STDDEV_POP(salary) — ecart type de population
```

### Agregats bit a bit

```typescript
import { bitAndAgg, bitOrAgg, bitXorAgg } from "@petradb/quarry";

bitAndAgg(emp.flags)  // BIT_AND(flags)
bitOrAgg(emp.flags)   // BIT_OR(flags)
bitXorAgg(emp.flags)  // BIT_XOR(flags)
```

### EVERY

```typescript
import { every } from "@petradb/quarry";

every(emp.active)  // EVERY(active) — vrai quand toutes les lignes sont vraies
```

### FILTER sur les agregats

Restreignez les lignes traitees par un agregat avec `filter()` :

```typescript
import { filter } from "@petradb/quarry";

// COUNT(*) FILTER (WHERE salary > 100)
filter(count(), gt(emp.salary, 100))

// SUM(salary) FILTER (WHERE active = true)
filter(sum(emp.salary), eq(emp.active, true))
```

Exemple avec plusieurs agregats filtres :

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

## Fonctions

Appelez toute fonction SQL avec `fn()` :

```typescript
import { fn } from "@petradb/quarry";

fn("upper", users.name)        // UPPER(name)
fn("coalesce", users.age, 0)   // COALESCE(age, 0)
fn("length", users.name)       // LENGTH(name)
fn("lower", users.email)       // LOWER(email)
fn("abs", users.age)           // ABS(age)
fn("round", users.score, 2)    // ROUND(score, 2)
```

## Jointures

Quarry supporte les jointures inner, left, right, full et cross avec typage des resultats a la compilation.

### Jointure inner

Toutes les colonnes des deux tables sont incluses dans le resultat. La nullabilite est preservee depuis le schema original :

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

// Type du resultat : (InferSelect<users> & InferSelect<posts>)[]
// rows[0].name  → string
// rows[0].title → string
// rows[0].body  → string | null (nullable dans le schema posts)
```

### Jointure left

Les colonnes de la table jointe deviennent toutes nullables, car les lignes non correspondantes produisent `null` :

```typescript
const rows = await db
  .select(users)
  .leftJoin(posts, eq(users.id, posts.userId))
  .execute();

// Type du resultat : (InferSelect<users> & Nullable<InferSelect<posts>>)[]
// rows[0].name   → string       (table de base, non affectee)
// rows[0].title  → string | null (la jointure left la rend nullable)
// rows[0].userId → number | null (la jointure left la rend nullable)
```

### Jointure right

Les colonnes de la table de base deviennent nullables, les colonnes de la table jointe preservent leur nullabilite originale :

```typescript
const rows = await db
  .select(users)
  .rightJoin(posts, eq(users.id, posts.userId))
  .execute();

// Type du resultat : (Nullable<InferSelect<users>> & InferSelect<posts>)[]
// rows[0].name  → string | null (la jointure right rend la table de base nullable)
// rows[0].title → string        (table jointe, non affectee)
```

### Jointure full

Les deux cotes deviennent nullables :

```typescript
const rows = await db
  .select(users)
  .fullJoin(posts, eq(users.id, posts.userId))
  .execute();

// Type du resultat : (Nullable<InferSelect<users>> & Nullable<InferSelect<posts>>)[]
// rows[0].name  → string | null
// rows[0].title → string | null
```

### Jointure cross

Produit le produit cartesien des deux tables -- pas de condition `on` :

```typescript
const rows = await db
  .select(users)
  .crossJoin(posts)
  .execute();

// Type du resultat : (InferSelect<users> & InferSelect<posts>)[]
// Chaque combinaison utilisateur x article
```

### Jointures chainees

Les jointures multiples accumulent les types correctement :

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

// colonnes posts : non-null (jointure inner)
// colonnes comments : nullable (jointure left)
// rows[0].title   → string        (jointure inner)
// rows[0].content → string | null  (jointure left)
```

### Jointure avec selection de colonnes

```typescript
const rows = await db
  .select(users)
  .columns(users.name, posts.title)
  .innerJoin(posts, eq(users.id, posts.userId))
  .execute();
```

### Jointure avec agregats

```typescript
const rows = await db
  .select(users)
  .columns(users.name, alias(count(), "post_count"))
  .innerJoin(posts, eq(users.id, posts.userId))
  .groupBy(users.name)
  .orderBy(desc(alias(count(), "post_count")))
  .execute();
```

## Alias de tables

Utilisez `tableAs()` pour creer des tables avec alias pour les auto-jointures ou quand la meme table apparait plusieurs fois :

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

Les alias sont type-safe -- `mgr.name` impose toujours que `name` existe dans le schema employees.

## Sous-requetes

### Sous-requete IN

```typescript
import { inSubquery, notInSubquery } from "@petradb/quarry";

// Utilisateurs ayant au moins un article
const rows = await db
  .select(users)
  .where(
    inSubquery(
      users.id,
      db.select(posts).columns(posts.userId).toExpr(),
    ),
  )
  .execute();

// Utilisateurs n'ayant AUCUN article
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

### Sous-requete EXISTS

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

### Sous-requete scalaire

Utilisez `subquery()` pour encapsuler une selection comme valeur scalaire :

```typescript
import { subquery } from "@petradb/quarry";

// Utilisateurs plus ages que l'age moyen
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
Utilisez `.toExpr()` (pas `.toAST()`) lors de l'integration d'une selection comme sous-requete. `.toExpr()` retourne le noeud `ASTSelect` brut, tandis que `.toAST()` l'encapsule dans un `QueryCommand`.
:::

## Tri

```typescript
import { asc, desc } from "@petradb/quarry";

// Tri simple
db.select(users).orderBy(asc(users.name))
db.select(users).orderBy(desc(users.age))

// Colonnes multiples
db.select(users).orderBy(asc(users.name), desc(users.age))

// NULLS FIRST / NULLS LAST
db.select(users).orderBy(asc(users.age, { nulls: "first" }))
db.select(users).orderBy(desc(users.age, { nulls: "last" }))
```

Lorsque `nulls` n'est pas specifie, le moteur utilise le comportement par defaut (les null sont tries en dernier en ordre ascendant, en premier en ordre descendant).

## Mise a jour

```typescript
// Mise a jour avec where
const result = await db
  .update(users)
  .set({ age: 31 })
  .where(eq(users.name, "Alice"))
  .execute();
// result.rowCount → 1

// Mettre a jour plusieurs champs
await db
  .update(users)
  .set({ name: "Alice Smith", age: 32, active: false })
  .where(eq(users.id, 1))
  .execute();

// Mettre a null
await db
  .update(users)
  .set({ age: null })
  .where(eq(users.name, "Bob"))
  .execute();
```

La methode `.set()` accepte `Partial<InferSelect<T>>` -- TypeScript impose des noms et types de colonnes valides.

### UPDATE...FROM

Joignez une autre table pour piloter les mises a jour :

```typescript
const priceUpdates = table("price_updates", {
  id: serial("id").primaryKey(),
  productName: text("product_name").notNull(),
  newPrice: integer("new_price").notNull(),
});

await db
  .update(products)
  .set({ price: 0 }) // valeur definie ; utilisez les references de colonnes dans WHERE pour la logique conditionnelle
  .from(priceUpdates)
  .where(eq(products.name, priceUpdates.productName))
  .execute();
```

`.from()` accepte plusieurs tables :

```typescript
db.update(t1).set({ ... }).from(t2, t3).where(and(...))
```

### RETURNING

Update et delete supportent `.returning()` pour recuperer les lignes affectees :

```typescript
const result = await db
  .update(users)
  .set({ active: false })
  .where(lt(users.age, 18))
  .returning(users.id, users.name)
  .execute();
// result.rows → [{ id: 3, name: "Charlie" }, ...]
```

## Suppression

```typescript
const result = await db
  .delete(users)
  .where(eq(users.name, "Alice"))
  .execute();
// result.rowCount → 1
```

### DELETE...USING

Joignez une autre table pour determiner les lignes a supprimer :

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

`.using()` accepte plusieurs tables :

```typescript
db.delete(t1).using(t2, t3).where(and(...))
```

## Transactions

Encapsulez plusieurs operations dans une transaction avec commit/rollback automatique :

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
// Si une operation echoue, la transaction entiere est annulee
```

Le callback recoit une instance `QuarryDB` scopee a la transaction. La valeur de retour du callback devient la valeur de retour de `transaction()`, avec le type preserve.

## Inspection de l'AST

Chaque builder dispose d'une methode `.toAST()` qui retourne l'objet AST brut sans l'executer. Utile pour le debogage, la journalisation ou la construction d'abstractions de niveau superieur :

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

## Comment ca fonctionne

Quarry construit de simples objets JavaScript (unions discriminees avec un champ `kind`) qui representent l'AST de la requete. Lorsque vous appelez `.execute()`, ces objets sont passes a la methode `executeAST()` du moteur, qui les convertit directement dans l'AST interne Scala du moteur -- en evitant entierement la generation et l'analyse de chaines SQL.

```
Schema → API Builder → Objets AST JS → AST Moteur → Reecriture → Execution
                              ↑ pas de parseur SQL
```

Cela donne a Quarry les memes capacites de requete que le SQL tout en eliminant le cout d'analyse et en permettant une securite de type complete a la compilation.

## Nettoyage

```typescript
await session.close();
```

---
title: Quarry
description: SQLの代わりにASTを生成するPetraDB用の型安全なクエリビルダーです。
---

Quarryは、SQL文字列の代わりにASTオブジェクトを生成するPetraDB用の型安全なクエリビルダーで、パーサーを完全にバイパスします。スキーマ定義がDDL、クエリ、コンパイル時TypeScript型の唯一の情報源となります。

## インストール

```bash
npm install @petradb/quarry
```

## セットアップ

```typescript
import { Session } from "@petradb/engine";
import { quarry } from "@petradb/quarry";

const session = new Session({ storage: "memory" });
const db = quarry(session);
```

### ストレージモード

```typescript
// インメモリ（デフォルト）
new Session({ storage: "memory" });

// ファイルベースの永続ストレージ
new Session({ storage: "persistent", path: "./mydb.petra" });
```

## スキーマ定義

Quarryのカラムコンストラクタを使用してテーブルを定義します。スキーマはテーブル作成、クエリ構築、TypeScript型推論を駆動します。

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

### カラム型

| コンストラクタ | SQL型 | TypeScript型 |
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

### カラム修飾子

| 修飾子 | 効果 |
|---|---|
| `.notNull()` | カラムはNULL不可。`InferSelect`型から`null`を除外 |
| `.default(value)` | `InferInsert`でカラムがオプションに |
| `.primaryKey()` | 主キー。notNull + hasDefaultを暗黙的に設定（serialでは自動インクリメント） |
| `.unique()` | ユニーク制約を追加 |
| `.references(table, column)` | 外部キー参照を追加 |

### 推論される型

Quarryは各テーブル定義から2つの型を推論します。

```typescript
import type { InferSelect, InferInsert } from "@petradb/quarry";

type User = InferSelect<typeof users>;
// { id: number, name: string, email: string, age: number | null, active: boolean }

type NewUser = InferInsert<typeof users>;
// { name: string, email: string, age?: number | null, active?: boolean, id?: number }
```

**`InferSelect`** — クエリが返す行の型です。
- `notNull`カラム → 非NULLable型
- NULLableカラム → `type | null`

**`InferInsert`** — `.values()`が受け入れる型です。
- デフォルトのない`notNull`カラム → 必須
- デフォルト付きカラム（`.default()`、`.primaryKey()`、serial） → オプション
- NULLableカラム → オプション、`null`を受け入れ

## テーブル作成

```typescript
await db.createTable(users);
```

スキーマ定義から`CREATE TABLE`コマンドを生成して実行します — SQLは不要です。

## Insert

```typescript
// 単一行 — すべてのカラムを含む挿入された行を返します
const [user] = await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com", age: 30 })
  .execute();
// user.id → 自動生成されたserial
// user.active → true（デフォルト）

// 複数行
await db
  .insert(users)
  .values(
    { name: "Bob", email: "bob@example.com", age: 25 },
    { name: "Charlie", email: "charlie@example.com" },
  )
  .execute();
```

Insertにはデフォルトのない`notNull`カラムがすべて必要です。オプションフィールドは省略できます。TypeScriptがこれをコンパイル時に強制します。

### RETURNING

デフォルトでは、insertはすべてのカラム（`*`）を返します。`.returning()`で特定のカラムを選択します。

```typescript
const [{ id }] = await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com" })
  .returning(users.id)
  .execute();
```

### Upsert（ON CONFLICT）

`onConflictDoNothing()`または`onConflictDoUpdate()`でinsert時のコンフリクトを処理します。

```typescript
// コンフリクト行をサイレントにスキップ
await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com" })
  .onConflictDoNothing()
  .execute();

// コンフリクト発生時に特定のカラムを更新
await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com", age: 31 })
  .onConflictDoUpdate(["email"], { name: "Alice Updated", age: 31 })
  .execute();
```

`onConflictDoUpdate`の最初の引数はコンフリクトカラムを指定し、2番目は更新するカラムを指定します。どちらも型安全で、TypeScriptが有効なカラムキーのみが使用されることを強制します。

### INSERT...SELECT

リテラル値の代わりにクエリから行を挿入します。

```typescript
// すべてのアクティブユーザーをアーカイブ
const query = db
  .select(users)
  .columns(users.name, users.email)
  .where(eq(users.active, true))
  .toExpr();

await db.insertFrom(archive, query, ["name", "email"]).execute();
```

2番目の引数はselectクエリです（`.toExpr()`を使用）。オプションの3番目の引数は、どのターゲットカラムに値を入れるかを指定します — 省略した場合、エンジンはクエリがすべてのカラムの値を生成することを期待します。

```typescript
// カラムリストなし（クエリがすべてのターゲットカラムに一致する必要があります）
await db.insertFrom(archive, query).execute();

// onConflictDoNothing付き
await db.insertFrom(archive, query, ["name", "email"]).onConflictDoNothing().execute();
```

## Select

```typescript
import { eq, gt, asc, desc } from "@petradb/quarry";

// すべての行
const allUsers = await db.select(users).execute();

// WHERE句
const alice = await db
  .select(users)
  .where(eq(users.name, "Alice"))
  .execute();

// 特定のカラム
const names = await db
  .select(users)
  .columns(users.name, users.email)
  .execute();

// ORDER、LIMIT、OFFSET
const page = await db
  .select(users)
  .orderBy(asc(users.name))
  .limit(10)
  .offset(20)
  .execute();

// DISTINCT
const statuses = await db
  .select(users)
  .columns(users.active)
  .distinct()
  .execute();

// DISTINCT ON — 指定カラムの各ユニーク値ごとに1行
const perCategory = await db
  .select(products)
  .distinctOn(products.category)
  .orderBy(asc(products.category), asc(products.price))
  .execute();
// 各カテゴリで最も安い商品を返します
```

### カラム参照

カラムはテーブルオブジェクトのプロパティとして直接アクセスします。TypeScriptはスキーマに存在しないカラムへのアクセスを防止します。

```typescript
users.name;  // ✓ コンパイル可能
users.title; // ✗ コンパイルエラー — 'title'はusersに存在しない
```

## 式

### 比較

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

// NULL安全な比較
isDistinctFrom(users.age, null)     // age IS DISTINCT FROM NULL
isNotDistinctFrom(users.age, null)  // age IS NOT DISTINCT FROM NULL
```

### 論理演算

```typescript
import { and, or, not } from "@petradb/quarry";

and(eq(users.active, true), gt(users.age, 18))
or(eq(users.name, "Alice"), eq(users.name, "Bob"))
not(eq(users.active, false))
```

`and()`と`or()`は任意の数の引数を受け取ります。

```typescript
and(cond1, cond2, cond3) // cond1 AND cond2 AND cond3
```

### NULLチェック

```typescript
import { isNull, isNotNull } from "@petradb/quarry";

isNull(users.age)     // age IS NULL
isNotNull(users.age)  // age IS NOT NULL
```

### ブーリアンテスト

```typescript
import { isTrue, isNotTrue, isFalse, isNotFalse, isUnknown, isNotUnknown } from "@petradb/quarry";

isTrue(users.active)       // active IS TRUE
isNotTrue(users.active)    // active IS NOT TRUE
isFalse(users.active)      // active IS FALSE
isNotFalse(users.active)   // active IS NOT FALSE
isUnknown(users.active)    // active IS UNKNOWN
isNotUnknown(users.active) // active IS NOT UNKNOWN
```

### コレクション

```typescript
import { inList, notInList, between, notBetween, betweenSymmetric } from "@petradb/quarry";

inList(users.name, ["Alice", "Bob", "Charlie"])  // name IN (...)
notInList(users.id, [1, 2, 3])                   // id NOT IN (...)
between(users.age, 18, 65)                       // age BETWEEN 18 AND 65
notBetween(users.age, 18, 65)                    // age NOT BETWEEN 18 AND 65
betweenSymmetric(users.age, 65, 18)              // age BETWEEN SYMMETRIC 65 AND 18
notBetweenSymmetric(users.age, 65, 18)           // age NOT BETWEEN SYMMETRIC 65 AND 18
```

### 算術演算

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

### 文字列演算子

```typescript
import { concat } from "@petradb/quarry";

concat(users.name, " Jr.")  // name || ' Jr.'
```

### ビット演算子

```typescript
import { bitAnd, bitOr, bitXor, bitNot, leftShift, rightShift } from "@petradb/quarry";

bitAnd(users.flags, 0xFF)   // flags & 255
bitOr(users.flags, 1)       // flags | 1
bitXor(users.flags, 0xFF)   // flags # 255
bitNot(users.flags)         // ~flags
leftShift(users.flags, 2)   // flags << 2
rightShift(users.flags, 1)  // flags >> 1
```

### JSON演算子

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

### 配列演算子

```typescript
import { arrayOverlap } from "@petradb/quarry";

arrayOverlap(t.tags, t.otherTags)  // tags && otherTags（配列のオーバーラップ）
```

### 汎用演算子

名前付きヘルパーでカバーされない演算子には`op()`と`unaryOp()`を使用します。

```typescript
import { op, unaryOp } from "@petradb/quarry";

op(users.age, ">=", 18)       // age >= 18
unaryOp("NOT", eq(users.active, true))
```

### CASE式

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

### CAST式

```typescript
import { cast } from "@petradb/quarry";

cast(users.age, "text")    // CAST(age AS TEXT)
cast(users.age, "double")  // CAST(age AS DOUBLE)
```

### エイリアスとリテラル

```typescript
import { alias, literal } from "@petradb/quarry";

alias(add(users.age, 10), "age_plus_10")

literal("hello")  // 文字列
literal(42)        // 数値
literal(true)      // ブーリアン
literal(null)      // null
```

## 集計とグルーピング

### 組み込み集計関数

```typescript
import { count, sum, avg, min, max, alias } from "@petradb/quarry";
import { stringAgg, arrayAgg, boolAnd, boolOr, jsonAgg, jsonObjectAgg } from "@petradb/quarry";

// すべての行をカウント
const [{ total }] = await db
  .select(users)
  .columns(alias(count(), "total"))
  .execute();

// GROUP BYと集計
const stats = await db
  .select(users)
  .columns(users.active, alias(count(), "cnt"))
  .groupBy(users.active)
  .execute();

// HAVING
const popular = await db
  .select(users)
  .columns(users.active, alias(count(), "cnt"))
  .groupBy(users.active)
  .having(gt(alias(count(), "cnt"), 5))
  .execute();

// その他の集計関数
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

### 統計集計

```typescript
import { variance, varSamp, varPop, stddev, stddevSamp, stddevPop } from "@petradb/quarry";

variance(emp.salary)   // VARIANCE(salary) — 標本分散
varSamp(emp.salary)    // VAR_SAMP(salary) — varianceと同じ
varPop(emp.salary)     // VAR_POP(salary) — 母分散
stddev(emp.salary)     // STDDEV(salary) — 標本標準偏差
stddevSamp(emp.salary) // STDDEV_SAMP(salary) — stddevと同じ
stddevPop(emp.salary)  // STDDEV_POP(salary) — 母標準偏差
```

### ビット集計

```typescript
import { bitAndAgg, bitOrAgg, bitXorAgg } from "@petradb/quarry";

bitAndAgg(emp.flags)  // BIT_AND(flags)
bitOrAgg(emp.flags)   // BIT_OR(flags)
bitXorAgg(emp.flags)  // BIT_XOR(flags)
```

### EVERY

```typescript
import { every } from "@petradb/quarry";

every(emp.active)  // EVERY(active) — すべての行がtrueの場合にtrue
```

### 集計FILTER

`filter()`で集計が処理する行を制限します。

```typescript
import { filter } from "@petradb/quarry";

// COUNT(*) FILTER (WHERE salary > 100)
filter(count(), gt(emp.salary, 100))

// SUM(salary) FILTER (WHERE active = true)
filter(sum(emp.salary), eq(emp.active, true))
```

複数のフィルター付き集計の例：

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

## 関数

`fn()`で任意のSQL関数を呼び出します。

```typescript
import { fn } from "@petradb/quarry";

fn("upper", users.name)        // UPPER(name)
fn("coalesce", users.age, 0)   // COALESCE(age, 0)
fn("length", users.name)       // LENGTH(name)
fn("lower", users.email)       // LOWER(email)
fn("abs", users.age)           // ABS(age)
fn("round", users.score, 2)    // ROUND(score, 2)
```

## ジョイン

Quarryはinner、left、right、full、crossジョインをコンパイル時の結果型付きでサポートします。

### Innerジョイン

両テーブルのすべてのカラムが結果に含まれます。NULLabilityは元のスキーマから保持されます。

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

// 結果型: (InferSelect<users> & InferSelect<posts>)[]
// rows[0].name  → string
// rows[0].title → string
// rows[0].body  → string | null (postsスキーマでNULLable)
```

### Leftジョイン

ジョインされたテーブルのカラムはすべてNULLableになります（マッチしない行はnullを生成するため）。

```typescript
const rows = await db
  .select(users)
  .leftJoin(posts, eq(users.id, posts.userId))
  .execute();

// 結果型: (InferSelect<users> & Nullable<InferSelect<posts>>)[]
// rows[0].name   → string       (ベーステーブル、影響なし)
// rows[0].title  → string | null (leftジョインでNULLableに)
// rows[0].userId → number | null (leftジョインでNULLableに)
```

### Rightジョイン

ベーステーブルのカラムがNULLableになり、ジョインされたテーブルのカラムは元のNULLabilityを保持します。

```typescript
const rows = await db
  .select(users)
  .rightJoin(posts, eq(users.id, posts.userId))
  .execute();

// 結果型: (Nullable<InferSelect<users>> & InferSelect<posts>)[]
// rows[0].name  → string | null (rightジョインでベーステーブルがNULLableに)
// rows[0].title → string        (ジョインテーブル、影響なし)
```

### Fullジョイン

両サイドがNULLableになります。

```typescript
const rows = await db
  .select(users)
  .fullJoin(posts, eq(users.id, posts.userId))
  .execute();

// 結果型: (Nullable<InferSelect<users>> & Nullable<InferSelect<posts>>)[]
// rows[0].name  → string | null
// rows[0].title → string | null
```

### Crossジョイン

両テーブルのデカルト積を生成します — `on`条件なし。

```typescript
const rows = await db
  .select(users)
  .crossJoin(posts)
  .execute();

// 結果型: (InferSelect<users> & InferSelect<posts>)[]
// user × postのすべての組み合わせ
```

### チェーンジョイン

複数のジョインは型を正しく蓄積します。

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

// postsカラム: 非NULL (innerジョイン)
// commentsカラム: NULLable (leftジョイン)
// rows[0].title   → string        (innerジョイン)
// rows[0].content → string | null  (leftジョイン)
```

### カラム選択付きジョイン

```typescript
const rows = await db
  .select(users)
  .columns(users.name, posts.title)
  .innerJoin(posts, eq(users.id, posts.userId))
  .execute();
```

### 集計付きジョイン

```typescript
const rows = await db
  .select(users)
  .columns(users.name, alias(count(), "post_count"))
  .innerJoin(posts, eq(users.id, posts.userId))
  .groupBy(users.name)
  .orderBy(desc(alias(count(), "post_count")))
  .execute();
```

## テーブルエイリアス

`tableAs()`でエイリアス付きテーブルを作成し、セルフジョインや同じテーブルが複数回出現する場合に使用します。

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

エイリアスは型安全です — `mgr.name`はemployeesスキーマに`name`が存在することを引き続き強制します。

## サブクエリ

### INサブクエリ

```typescript
import { inSubquery, notInSubquery } from "@petradb/quarry";

// 少なくとも1つの投稿があるユーザー
const rows = await db
  .select(users)
  .where(
    inSubquery(
      users.id,
      db.select(posts).columns(posts.userId).toExpr(),
    ),
  )
  .execute();

// 投稿がないユーザー
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

### EXISTSサブクエリ

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

### スカラーサブクエリ

`subquery()`でselectをスカラー値としてラップします。

```typescript
import { subquery } from "@petradb/quarry";

// 平均年齢より上のユーザー
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
サブクエリとしてselectを埋め込む場合は`.toExpr()`（`.toAST()`ではなく）を使用してください。`.toExpr()`は生の`ASTSelect`ノードを返し、`.toAST()`はそれを`QueryCommand`でラップします。
:::

## ソート

```typescript
import { asc, desc } from "@petradb/quarry";

// 基本的なソート
db.select(users).orderBy(asc(users.name))
db.select(users).orderBy(desc(users.age))

// 複数カラム
db.select(users).orderBy(asc(users.name), desc(users.age))

// NULLS FIRST / NULLS LAST
db.select(users).orderBy(asc(users.age, { nulls: "first" }))
db.select(users).orderBy(desc(users.age, { nulls: "last" }))
```

`nulls`が指定されていない場合、エンジンはデフォルトの動作を使用します（昇順ではNULLが最後、降順ではNULLが最初にソートされます）。

## Update

```typescript
// WHERE付きUpdate
const result = await db
  .update(users)
  .set({ age: 31 })
  .where(eq(users.name, "Alice"))
  .execute();
// result.rowCount → 1

// 複数フィールドの更新
await db
  .update(users)
  .set({ name: "Alice Smith", age: 32, active: false })
  .where(eq(users.id, 1))
  .execute();

// NULLに設定
await db
  .update(users)
  .set({ age: null })
  .where(eq(users.name, "Bob"))
  .execute();
```

`.set()`メソッドは`Partial<InferSelect<T>>`を受け入れます — TypeScriptが有効なカラム名と型を強制します。

### UPDATE...FROM

別のテーブルをジョインして更新を駆動します。

```typescript
const priceUpdates = table("price_updates", {
  id: serial("id").primaryKey(),
  productName: text("product_name").notNull(),
  newPrice: integer("new_price").notNull(),
});

await db
  .update(products)
  .set({ price: 0 }) // 値を設定; 条件ロジックにはWHEREでカラム参照を使用
  .from(priceUpdates)
  .where(eq(products.name, priceUpdates.productName))
  .execute();
```

`.from()`は複数テーブルを受け入れます。

```typescript
db.update(t1).set({ ... }).from(t2, t3).where(and(...))
```

### RETURNING

UpdateとDeleteは`.returning()`で影響を受けた行を取得できます。

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

別のテーブルをジョインして削除する行を決定します。

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

`.using()`は複数テーブルを受け入れます。

```typescript
db.delete(t1).using(t2, t3).where(and(...))
```

## トランザクション

自動コミット/ロールバック付きで複数の操作をトランザクションにラップします。

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
// いずれかの操作がスローした場合、トランザクション全体がロールバックされます
```

コールバックはトランザクションにスコープされた`QuarryDB`インスタンスを受け取ります。コールバックの戻り値が`transaction()`の戻り値になり、型は保持されます。

## AST検査

すべてのビルダーには`.toAST()`メソッドがあり、実行せずに生のASTオブジェクトを返します。デバッグ、ログ記録、または高レベルの抽象化の構築に便利です。

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

## 仕組み

Quarryはクエリのv ASTを表すプレーンなJavaScriptオブジェクト（`kind`フィールドを持つ判別共用体）を構築します。`.execute()`を呼び出すと、これらのオブジェクトはエンジンの`executeAST()`メソッドに渡され、エンジンの内部Scala ASTに直接変換されます — SQL文字列の生成とパースを完全にスキップします。

```
スキーマ → ビルダーAPI → JS ASTオブジェクト → エンジンAST → 書き換え → 実行
                              ↑ SQLパーサーなし
```

これによりQuarryはSQLと同じクエリ機能を持ちながら、パースのオーバーヘッドを排除し、完全なコンパイル時型安全性を実現します。

## クリーンアップ

```typescript
await session.close();
```

---
title: Quarry
description: 为 PetraDB 生成 AST 而非 SQL 的类型安全查询构建器。
---

Quarry 是 PetraDB 的类型安全查询构建器，它生成 AST 对象而非 SQL 字符串，完全绕过解析器。模式定义作为 DDL、查询和编译时 TypeScript 类型的单一事实来源。

## 安装

```bash
npm install @petradb/quarry
```

## 设置

```typescript
import { Session } from "@petradb/engine";
import { quarry } from "@petradb/quarry";

const session = new Session({ storage: "memory" });
const db = quarry(session);
```

### 存储模式

```typescript
// 内存模式（默认）
new Session({ storage: "memory" });

// 文件持久化存储
new Session({ storage: "persistent", path: "./mydb.petra" });
```

## 模式定义

使用 Quarry 的列构造函数定义表。模式驱动表创建、查询构建和 TypeScript 类型推断：

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

### 列类型

| 构造函数 | SQL 类型 | TypeScript 类型 |
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

### 列修饰符

| 修饰符 | 效果 |
|---|---|
| `.notNull()` | 列不能为 null；`InferSelect` 类型排除 `null` |
| `.default(value)` | 列在 `InferInsert` 中为可选 |
| `.primaryKey()` | 主键；隐含 notNull + hasDefault（serial 自增） |
| `.unique()` | 添加唯一约束 |
| `.references(table, column)` | 添加外键引用 |

### 推断类型

Quarry 从每个表定义推断两种类型：

```typescript
import type { InferSelect, InferInsert } from "@petradb/quarry";

type User = InferSelect<typeof users>;
// { id: number, name: string, email: string, age: number | null, active: boolean }

type NewUser = InferInsert<typeof users>;
// { name: string, email: string, age?: number | null, active?: boolean, id?: number }
```

**`InferSelect`** — 查询返回的行类型：
- `notNull` 列 → 非空类型
- 可空列 → `type | null`

**`InferInsert`** — `.values()` 接受的类型：
- 没有默认值的 `notNull` 列 → 必需
- 有默认值的列（`.default()`、`.primaryKey()`、serial） → 可选
- 可空列 → 可选，接受 `null`

## 创建表

```typescript
await db.createTable(users);
```

这会从模式定义生成并执行 `CREATE TABLE` 命令 — 无需 SQL。

## 插入

```typescript
// 单行 — 返回包含所有列的插入行
const [user] = await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com", age: 30 })
  .execute();
// user.id → 自动生成的序列号
// user.active → true（默认值）

// 多行
await db
  .insert(users)
  .values(
    { name: "Bob", email: "bob@example.com", age: 25 },
    { name: "Charlie", email: "charlie@example.com" },
  )
  .execute();
```

插入要求所有没有默认值的 `notNull` 列。可选字段可以省略。TypeScript 在编译时强制执行此规则。

### RETURNING

默认情况下，插入返回所有列（`*`）。使用 `.returning()` 选择特定列：

```typescript
const [{ id }] = await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com" })
  .returning(users.id)
  .execute();
```

### Upsert（ON CONFLICT）

使用 `onConflictDoNothing()` 或 `onConflictDoUpdate()` 处理插入冲突：

```typescript
// 静默跳过冲突行
await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com" })
  .onConflictDoNothing()
  .execute();

// 冲突时更新特定列
await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com", age: 31 })
  .onConflictDoUpdate(["email"], { name: "Alice Updated", age: 31 })
  .execute();
```

`onConflictDoUpdate` 的第一个参数指定冲突列，第二个参数指定要更新的列。两者都是类型安全的 — TypeScript 强制只能使用有效的列键。

### INSERT...SELECT

从查询结果插入行而非字面值：

```typescript
// 归档所有活跃用户
const query = db
  .select(users.name, users.email)
  .from(users)
  .where(eq(users.active, true));

await db.insertFrom(archive, query, ["name", "email"]).execute();
```

第二个参数是一个 select 构建器。可选的第三个参数指定要填充的目标列 — 如果省略，引擎期望查询为所有列生成值。

```typescript
// 不指定列列表（查询必须匹配所有目标列）
await db.insertFrom(archive, query).execute();

// 配合 onConflictDoNothing
await db.insertFrom(archive, query, ["name", "email"]).onConflictDoNothing().execute();
```

## 查询

```typescript
import { eq, gt, asc, desc } from "@petradb/quarry";

// 所有行（SELECT *）
const allUsers = await db.from(users).execute();

// WHERE 子句
const alice = await db
  .from(users)
  .where(eq(users.name, "Alice"))
  .execute();

// 指定列
const names = await db
  .select(users.name, users.email)
  .from(users)
  .execute();

// 排序、限制、偏移
const page = await db
  .from(users)
  .orderBy(asc(users.name))
  .limit(10)
  .offset(20)
  .execute();

// 去重
const statuses = await db
  .select(users.active)
  .from(users)
  .distinct()
  .execute();

// Distinct on — 每个不同值返回一行
const perCategory = await db
  .from(products)
  .distinctOn(products.category)
  .orderBy(asc(products.category), asc(products.price))
  .execute();
// 返回每个分类中最便宜的产品
```

### 列引用

列作为表对象上的属性直接访问。TypeScript 会阻止访问模式中不存在的列：

```typescript
users.name;  // ✓ 编译通过
users.title; // ✗ 编译错误 — 'title' 不在 users 中
```

## 表达式

### 比较

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

// 空值安全比较
isDistinctFrom(users.age, null)     // age IS DISTINCT FROM NULL
isNotDistinctFrom(users.age, null)  // age IS NOT DISTINCT FROM NULL
```

### 逻辑

```typescript
import { and, or, not } from "@petradb/quarry";

and(eq(users.active, true), gt(users.age, 18))
or(eq(users.name, "Alice"), eq(users.name, "Bob"))
not(eq(users.active, false))
```

`and()` 和 `or()` 接受任意数量的参数：

```typescript
and(cond1, cond2, cond3) // cond1 AND cond2 AND cond3
```

### 空值检查

```typescript
import { isNull, isNotNull } from "@petradb/quarry";

isNull(users.age)     // age IS NULL
isNotNull(users.age)  // age IS NOT NULL
```

### 布尔测试

```typescript
import { isTrue, isNotTrue, isFalse, isNotFalse, isUnknown, isNotUnknown } from "@petradb/quarry";

isTrue(users.active)       // active IS TRUE
isNotTrue(users.active)    // active IS NOT TRUE
isFalse(users.active)      // active IS FALSE
isNotFalse(users.active)   // active IS NOT FALSE
isUnknown(users.active)    // active IS UNKNOWN
isNotUnknown(users.active) // active IS NOT UNKNOWN
```

### 集合

```typescript
import { inList, notInList, between, notBetween, betweenSymmetric } from "@petradb/quarry";

inList(users.name, ["Alice", "Bob", "Charlie"])  // name IN (...)
notInList(users.id, [1, 2, 3])                   // id NOT IN (...)
between(users.age, 18, 65)                       // age BETWEEN 18 AND 65
notBetween(users.age, 18, 65)                    // age NOT BETWEEN 18 AND 65
betweenSymmetric(users.age, 65, 18)              // age BETWEEN SYMMETRIC 65 AND 18
notBetweenSymmetric(users.age, 65, 18)           // age NOT BETWEEN SYMMETRIC 65 AND 18
```

### 算术

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

### 字符串运算符

```typescript
import { concat } from "@petradb/quarry";

concat(users.name, " Jr.")  // name || ' Jr.'
```

### 位运算符

```typescript
import { bitAnd, bitOr, bitXor, bitNot, leftShift, rightShift } from "@petradb/quarry";

bitAnd(users.flags, 0xFF)   // flags & 255
bitOr(users.flags, 1)       // flags | 1
bitXor(users.flags, 0xFF)   // flags # 255
bitNot(users.flags)         // ~flags
leftShift(users.flags, 2)   // flags << 2
rightShift(users.flags, 1)  // flags >> 1
```

### JSON 运算符

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

### 数组运算符

```typescript
import { arrayOverlap } from "@petradb/quarry";

arrayOverlap(t.tags, t.otherTags)  // tags && otherTags（数组重叠）
```

### 通用运算符

对于没有命名辅助函数覆盖的运算符，使用 `op()` 和 `unaryOp()`：

```typescript
import { op, unaryOp } from "@petradb/quarry";

op(users.age, ">=", 18)       // age >= 18
unaryOp("NOT", eq(users.active, true))
```

### CASE 表达式

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

### CAST 表达式

```typescript
import { cast } from "@petradb/quarry";

cast(users.age, "text")    // CAST(age AS TEXT)
cast(users.age, "double")  // CAST(age AS DOUBLE)
```

### 别名和字面量

```typescript
import { alias, literal } from "@petradb/quarry";

alias(add(users.age, 10), "age_plus_10")

literal("hello")  // 字符串
literal(42)        // 数字
literal(true)      // 布尔
literal(null)      // null
```

## 聚合和分组

### 内置聚合

```typescript
import { count, sum, avg, min, max, alias } from "@petradb/quarry";
import { stringAgg, arrayAgg, boolAnd, boolOr, jsonAgg, jsonObjectAgg } from "@petradb/quarry";

// 计数所有行
const [{ total }] = await db
  .select(alias(count(), "total"))
  .from(users)
  .execute();

// 分组聚合
const stats = await db
  .select(users.active, alias(count(), "cnt"))
  .from(users)
  .groupBy(users.active)
  .execute();

// HAVING
const popular = await db
  .select(users.active, alias(count(), "cnt"))
  .from(users)
  .groupBy(users.active)
  .having(gt(alias(count(), "cnt"), 5))
  .execute();

// 其他聚合
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

### 统计聚合

```typescript
import { variance, varSamp, varPop, stddev, stddevSamp, stddevPop } from "@petradb/quarry";

variance(emp.salary)   // VARIANCE(salary) — 样本方差
varSamp(emp.salary)    // VAR_SAMP(salary) — 同 variance
varPop(emp.salary)     // VAR_POP(salary) — 总体方差
stddev(emp.salary)     // STDDEV(salary) — 样本标准差
stddevSamp(emp.salary) // STDDEV_SAMP(salary) — 同 stddev
stddevPop(emp.salary)  // STDDEV_POP(salary) — 总体标准差
```

### 位聚合

```typescript
import { bitAndAgg, bitOrAgg, bitXorAgg } from "@petradb/quarry";

bitAndAgg(emp.flags)  // BIT_AND(flags)
bitOrAgg(emp.flags)   // BIT_OR(flags)
bitXorAgg(emp.flags)  // BIT_XOR(flags)
```

### EVERY

```typescript
import { every } from "@petradb/quarry";

every(emp.active)  // EVERY(active) — 当所有行为 true 时返回 true
```

### 聚合 FILTER

使用 `filter()` 限制聚合处理的行：

```typescript
import { filter } from "@petradb/quarry";

// COUNT(*) FILTER (WHERE salary > 100)
filter(count(), gt(emp.salary, 100))

// SUM(salary) FILTER (WHERE active = true)
filter(sum(emp.salary), eq(emp.active, true))
```

多个过滤聚合的示例：

```typescript
const [row] = await db
  .select(
    alias(count(), "total"),
    alias(filter(count(), gt(employees.salary, 100)), "high_earners"),
    alias(filter(sum(employees.salary), eq(employees.active, true)), "active_payroll"),
  )
  .from(employees)
  .execute();
```

## 函数

使用 `fn()` 调用任何 SQL 函数：

```typescript
import { fn } from "@petradb/quarry";

fn("upper", users.name)        // UPPER(name)
fn("coalesce", users.age, 0)   // COALESCE(age, 0)
fn("length", users.name)       // LENGTH(name)
fn("lower", users.email)       // LOWER(email)
fn("abs", users.age)           // ABS(age)
fn("round", users.score, 2)    // ROUND(score, 2)
```

## 连接

Quarry 支持 inner、left、right、full 和 cross 连接，带编译时结果类型。

### Inner 连接

结果包含两个表的所有列。可空性从原始模式保留：

```typescript
const posts = table("posts", {
  id: serial("id").primaryKey(),
  userId: integer("user_id").notNull(),
  title: text("title").notNull(),
  body: text("body"),
});

const rows = await db
  .from(users)
  .innerJoin(posts, eq(users.id, posts.userId))
  .where(eq(users.name, "Alice"))
  .execute();

// 结果类型：(InferSelect<users> & InferSelect<posts>)[]
// rows[0].name  → string
// rows[0].title → string
// rows[0].body  → string | null（posts 模式中可空）
```

### Left 连接

被连接表的所有列变为可空，因为不匹配的行产生 `null`：

```typescript
const rows = await db
  .from(users)
  .leftJoin(posts, eq(users.id, posts.userId))
  .execute();

// 结果类型：(InferSelect<users> & Nullable<InferSelect<posts>>)[]
// rows[0].name   → string       （基表，不受影响）
// rows[0].title  → string | null（left join 使其可空）
// rows[0].userId → number | null（left join 使其可空）
```

### Right 连接

基表的列变为可空，被连接表的列保留其原始可空性：

```typescript
const rows = await db
  .from(users)
  .rightJoin(posts, eq(users.id, posts.userId))
  .execute();

// 结果类型：(Nullable<InferSelect<users>> & InferSelect<posts>)[]
// rows[0].name  → string | null（right join 使基表可空）
// rows[0].title → string       （被连接表，不受影响）
```

### Full 连接

两侧都变为可空：

```typescript
const rows = await db
  .from(users)
  .fullJoin(posts, eq(users.id, posts.userId))
  .execute();

// 结果类型：(Nullable<InferSelect<users>> & Nullable<InferSelect<posts>>)[]
// rows[0].name  → string | null
// rows[0].title → string | null
```

### Cross 连接

生成两个表的笛卡尔积 — 无 `on` 条件：

```typescript
const rows = await db
  .from(users)
  .crossJoin(posts)
  .execute();

// 结果类型：(InferSelect<users> & InferSelect<posts>)[]
// user × post 的每种组合
```

### 链式连接

多个连接正确累积类型：

```typescript
const comments = table("comments", {
  id: serial("id").primaryKey(),
  postId: integer("post_id").notNull(),
  content: text("content").notNull(),
});

const rows = await db
  .from(users)
  .innerJoin(posts, eq(users.id, posts.userId))
  .leftJoin(comments, eq(posts.id, comments.postId))
  .execute();

// posts 列：非空（inner join）
// comments 列：可空（left join）
// rows[0].title   → string        （inner join）
// rows[0].content → string | null  （left join）
```

### 连接与列选择

```typescript
const rows = await db
  .select(users.name, posts.title)
  .from(users)
  .innerJoin(posts, eq(users.id, posts.userId))
  .execute();
```

### 连接与聚合

```typescript
const rows = await db
  .select(users.name, alias(count(), "post_count"))
  .from(users)
  .innerJoin(posts, eq(users.id, posts.userId))
  .groupBy(users.name)
  .orderBy(desc(alias(count(), "post_count")))
  .execute();
```

## 表别名

使用 `tableAs()` 为自连接或同一表多次出现时创建别名表：

```typescript
import { tableAs } from "@petradb/quarry";

const mgr = tableAs(employees, "mgr");
const emp = tableAs(employees, "emp");

const rows = await db
  .select(
    alias(emp.name, "employee"),
    alias(mgr.name, "manager"),
  )
  .from(emp)
  .leftJoin(mgr, eq(emp.managerId, mgr.id))
  .execute();
```

别名是类型安全的 — `mgr.name` 仍然强制 `name` 存在于 employees 模式中。

## 子查询

### IN 子查询

```typescript
import { inSubquery, notInSubquery } from "@petradb/quarry";

// 至少有一篇文章的用户
const rows = await db
  .from(users)
  .where(
    inSubquery(users.id, db.select(posts.userId).from(posts)),
  )
  .execute();

// 没有文章的用户
const rows = await db
  .from(users)
  .where(
    notInSubquery(users.id, db.select(posts.userId).from(posts)),
  )
  .execute();
```

### EXISTS 子查询

```typescript
import { exists } from "@petradb/quarry";

const rows = await db
  .from(users)
  .where(
    exists(
      db.select(literal(1)).from(posts).where(eq(posts.userId, users.id)),
    ),
  )
  .execute();
```

### 标量子查询

使用 `subquery()` 将 select 包装为标量值：

```typescript
import { subquery } from "@petradb/quarry";

// 年龄大于平均年龄的用户
const rows = await db
  .from(users)
  .where(
    gt(users.age, subquery(db.select(avg(users.age)).from(users))),
  )
  .execute();
```

子查询函数（`subquery`、`exists`、`inSubquery`、`notInSubquery`）直接接受任何查询构建器 — 无需中间转换。

## 排序

```typescript
import { asc, desc } from "@petradb/quarry";

// 基本排序
db.from(users).orderBy(asc(users.name))
db.from(users).orderBy(desc(users.age))

// 多列
db.from(users).orderBy(asc(users.name), desc(users.age))

// NULLS FIRST / NULLS LAST
db.from(users).orderBy(asc(users.age, { nulls: "first" }))
db.from(users).orderBy(desc(users.age, { nulls: "last" }))
```

不指定 `nulls` 时，引擎使用默认行为（升序时空值排在最后，降序时空值排在最前）。

## 更新

```typescript
// 带 where 的更新
const result = await db
  .update(users)
  .set({ age: 31 })
  .where(eq(users.name, "Alice"))
  .execute();
// result.rowCount → 1

// 更新多个字段
await db
  .update(users)
  .set({ name: "Alice Smith", age: 32, active: false })
  .where(eq(users.id, 1))
  .execute();

// 设置为 null
await db
  .update(users)
  .set({ age: null })
  .where(eq(users.name, "Bob"))
  .execute();
```

`.set()` 方法接受 `Partial<InferSelect<T>>` — TypeScript 强制有效的列名和类型。

### UPDATE...FROM

连接另一个表来驱动更新：

```typescript
const priceUpdates = table("price_updates", {
  id: serial("id").primaryKey(),
  productName: text("product_name").notNull(),
  newPrice: integer("new_price").notNull(),
});

await db
  .update(products)
  .set({ price: 0 }) // 设置值；在 WHERE 中使用列引用进行条件逻辑
  .from(priceUpdates)
  .where(eq(products.name, priceUpdates.productName))
  .execute();
```

`.from()` 接受多个表：

```typescript
db.update(t1).set({ ... }).from(t2, t3).where(and(...))
```

### RETURNING

update 和 delete 支持 `.returning()` 返回受影响的行：

```typescript
const result = await db
  .update(users)
  .set({ active: false })
  .where(lt(users.age, 18))
  .returning(users.id, users.name)
  .execute();
// result.rows → [{ id: 3, name: "Charlie" }, ...]
```

## 删除

```typescript
const result = await db
  .delete(users)
  .where(eq(users.name, "Alice"))
  .execute();
// result.rowCount → 1
```

### DELETE...USING

连接另一个表来确定要删除的行：

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

`.using()` 接受多个表：

```typescript
db.delete(t1).using(t2, t3).where(and(...))
```

## 事务

使用自动提交/回滚将多个操作包装在事务中：

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
// 如果任何操作抛出异常，整个事务将回滚
```

回调接收一个作用域为该事务的 `QuarryDB` 实例。回调的返回值成为 `transaction()` 的返回值，类型保留。

## AST 检查

每个构建器都有 `.toAST()` 方法，返回原始 AST 对象而不执行。这对调试、日志记录或构建更高级抽象很有用：

```typescript
const ast = db
  .from(users)
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

## 工作原理

Quarry 构建普通的 JavaScript 对象（带有 `kind` 字段的判别联合），表示查询 AST。当你调用 `.execute()` 时，这些对象被传递给引擎的 `executeAST()` 方法，该方法将其直接转换为引擎的内部 Scala AST — 完全跳过 SQL 字符串生成和解析。

```
Schema → Builder API → JS AST objects → Engine AST → Rewrite → Execute
                              ↑ 无 SQL 解析器
```

这使 Quarry 具有与 SQL 相同的查询能力，同时消除了解析开销并实现完整的编译时类型安全。

## 清理

```typescript
await session.close();
```

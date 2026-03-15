---
title: Quarry
description: SQL 대신 AST를 생성하는 PetraDB용 타입 안전 쿼리 빌더.
---

Quarry는 SQL 문자열 대신 AST 객체를 생성하는 PetraDB용 타입 안전 쿼리 빌더로, 파서를 완전히 우회합니다. 스키마 정의가 DDL, 쿼리, 컴파일 타임 TypeScript 타입의 단일 소스로 사용됩니다.

## 설치

```bash
npm install @petradb/quarry
```

## 설정

```typescript
import { Session } from "@petradb/engine";
import { quarry } from "@petradb/quarry";

const session = new Session({ storage: "memory" });
const db = quarry(session);
```

### 스토리지 모드

```typescript
// 인메모리 (기본값)
new Session({ storage: "memory" });

// 파일 기반 영구 스토리지
new Session({ storage: "persistent", path: "./mydb.petra" });
```

## 스키마 정의

Quarry의 컬럼 생성자를 사용하여 테이블을 정의합니다. 스키마는 테이블 생성, 쿼리 빌딩, TypeScript 타입 추론을 구동합니다:

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

### 컬럼 타입

| 생성자 | SQL 타입 | TypeScript 타입 |
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

### 컬럼 수정자

| 수정자 | 효과 |
|---|---|
| `.notNull()` | 컬럼에 null 불가; `InferSelect` 타입에서 `null` 제외 |
| `.default(value)` | `InferInsert`에서 컬럼이 선택적 |
| `.primaryKey()` | 기본 키; notNull + hasDefault 암시 (serial의 경우 자동 증가) |
| `.unique()` | 유니크 제약 조건 추가 |
| `.references(table, column)` | 외래 키 참조 추가 |

### 추론된 타입

Quarry는 각 테이블 정의에서 두 가지 타입을 추론합니다:

```typescript
import type { InferSelect, InferInsert } from "@petradb/quarry";

type User = InferSelect<typeof users>;
// { id: number, name: string, email: string, age: number | null, active: boolean }

type NewUser = InferInsert<typeof users>;
// { name: string, email: string, age?: number | null, active?: boolean, id?: number }
```

**`InferSelect`** — 쿼리가 반환하는 행 타입:
- `notNull` 컬럼 → null 불가 타입
- null 허용 컬럼 → `type | null`

**`InferInsert`** — `.values()`가 받는 타입:
- 기본값 없는 `notNull` 컬럼 → 필수
- 기본값이 있는 컬럼 (`.default()`, `.primaryKey()`, serial) → 선택적
- null 허용 컬럼 → 선택적, `null` 허용

## 테이블 생성

```typescript
await db.createTable(users);
```

스키마 정의에서 `CREATE TABLE` 명령을 생성하고 실행합니다 — SQL이 필요 없습니다.

## 삽입

```typescript
// 단일 행 — 모든 컬럼이 포함된 삽입된 행을 반환
const [user] = await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com", age: 30 })
  .execute();
// user.id → 자동 생성된 serial
// user.active → true (기본값)

// 여러 행
await db
  .insert(users)
  .values(
    { name: "Bob", email: "bob@example.com", age: 25 },
    { name: "Charlie", email: "charlie@example.com" },
  )
  .execute();
```

삽입은 기본값이 없는 모든 `notNull` 컬럼을 필요로 합니다. 선택적 필드는 생략할 수 있습니다. TypeScript가 이를 컴파일 타임에 강제합니다.

### RETURNING

기본적으로 삽입은 모든 컬럼(`*`)을 반환합니다. `.returning()`을 사용하여 특정 컬럼을 선택합니다:

```typescript
const [{ id }] = await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com" })
  .returning(users.id)
  .execute();
```

### Upsert (ON CONFLICT)

`onConflictDoNothing()` 또는 `onConflictDoUpdate()`로 삽입 시 충돌을 처리합니다:

```typescript
// 충돌하는 행을 조용히 건너뛰기
await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com" })
  .onConflictDoNothing()
  .execute();

// 충돌 시 특정 컬럼 업데이트
await db
  .insert(users)
  .values({ name: "Alice", email: "alice@example.com", age: 31 })
  .onConflictDoUpdate(["email"], { name: "Alice Updated", age: 31 })
  .execute();
```

`onConflictDoUpdate`의 첫 번째 인수는 충돌 컬럼을, 두 번째는 업데이트할 컬럼을 지정합니다. 둘 다 타입 안전합니다 — TypeScript가 유효한 컬럼 키만 사용되도록 강제합니다.

### INSERT...SELECT

리터럴 값 대신 쿼리에서 행을 삽입합니다:

```typescript
// 모든 활성 사용자를 아카이브
const query = db
  .select(users)
  .columns(users.name, users.email)
  .where(eq(users.active, true))
  .toExpr();

await db.insertFrom(archive, query, ["name", "email"]).execute();
```

두 번째 인수는 select 쿼리입니다(`.toExpr()` 사용). 선택적 세 번째 인수는 채울 대상 컬럼을 지정합니다 — 생략하면 엔진은 쿼리가 모든 컬럼의 값을 생성할 것으로 기대합니다.

```typescript
// 컬럼 목록 없이 (쿼리가 모든 대상 컬럼과 일치해야 함)
await db.insertFrom(archive, query).execute();

// onConflictDoNothing과 함께
await db.insertFrom(archive, query, ["name", "email"]).onConflictDoNothing().execute();
```

## 조회

```typescript
import { eq, gt, asc, desc } from "@petradb/quarry";

// 모든 행
const allUsers = await db.select(users).execute();

// WHERE 절
const alice = await db
  .select(users)
  .where(eq(users.name, "Alice"))
  .execute();

// 특정 컬럼
const names = await db
  .select(users)
  .columns(users.name, users.email)
  .execute();

// 정렬, limit, offset
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

// Distinct on — 주어진 컬럼의 고유값당 하나의 행
const perCategory = await db
  .select(products)
  .distinctOn(products.category)
  .orderBy(asc(products.category), asc(products.price))
  .execute();
// 각 카테고리에서 가장 저렴한 상품을 반환
```

### 컬럼 참조

컬럼은 테이블 객체의 속성으로 직접 접근합니다. TypeScript가 스키마에 없는 컬럼 접근을 방지합니다:

```typescript
users.name;  // ✓ 컴파일됨
users.title; // ✗ 컴파일 오류 — 'title'이 users에 없음
```

## 표현식

### 비교

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

// NULL 안전 비교
isDistinctFrom(users.age, null)     // age IS DISTINCT FROM NULL
isNotDistinctFrom(users.age, null)  // age IS NOT DISTINCT FROM NULL
```

### 논리

```typescript
import { and, or, not } from "@petradb/quarry";

and(eq(users.active, true), gt(users.age, 18))
or(eq(users.name, "Alice"), eq(users.name, "Bob"))
not(eq(users.active, false))
```

`and()`와 `or()`은 여러 개의 인수를 받습니다:

```typescript
and(cond1, cond2, cond3) // cond1 AND cond2 AND cond3
```

### NULL 검사

```typescript
import { isNull, isNotNull } from "@petradb/quarry";

isNull(users.age)     // age IS NULL
isNotNull(users.age)  // age IS NOT NULL
```

### 부울 테스트

```typescript
import { isTrue, isNotTrue, isFalse, isNotFalse, isUnknown, isNotUnknown } from "@petradb/quarry";

isTrue(users.active)       // active IS TRUE
isNotTrue(users.active)    // active IS NOT TRUE
isFalse(users.active)      // active IS FALSE
isNotFalse(users.active)   // active IS NOT FALSE
isUnknown(users.active)    // active IS UNKNOWN
isNotUnknown(users.active) // active IS NOT UNKNOWN
```

### 컬렉션

```typescript
import { inList, notInList, between, notBetween, betweenSymmetric } from "@petradb/quarry";

inList(users.name, ["Alice", "Bob", "Charlie"])  // name IN (...)
notInList(users.id, [1, 2, 3])                   // id NOT IN (...)
between(users.age, 18, 65)                       // age BETWEEN 18 AND 65
notBetween(users.age, 18, 65)                    // age NOT BETWEEN 18 AND 65
betweenSymmetric(users.age, 65, 18)              // age BETWEEN SYMMETRIC 65 AND 18
notBetweenSymmetric(users.age, 65, 18)           // age NOT BETWEEN SYMMETRIC 65 AND 18
```

### 산술

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

### 문자열 연산자

```typescript
import { concat } from "@petradb/quarry";

concat(users.name, " Jr.")  // name || ' Jr.'
```

### 비트 연산자

```typescript
import { bitAnd, bitOr, bitXor, bitNot, leftShift, rightShift } from "@petradb/quarry";

bitAnd(users.flags, 0xFF)   // flags & 255
bitOr(users.flags, 1)       // flags | 1
bitXor(users.flags, 0xFF)   // flags # 255
bitNot(users.flags)         // ~flags
leftShift(users.flags, 2)   // flags << 2
rightShift(users.flags, 1)  // flags >> 1
```

### JSON 연산자

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

### 배열 연산자

```typescript
import { arrayOverlap } from "@petradb/quarry";

arrayOverlap(t.tags, t.otherTags)  // tags && otherTags (배열 겹침)
```

### 일반 연산자

이름이 있는 헬퍼에서 다루지 않는 연산자에는 `op()`과 `unaryOp()`을 사용합니다:

```typescript
import { op, unaryOp } from "@petradb/quarry";

op(users.age, ">=", 18)       // age >= 18
unaryOp("NOT", eq(users.active, true))
```

### CASE 표현식

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

### CAST 표현식

```typescript
import { cast } from "@petradb/quarry";

cast(users.age, "text")    // CAST(age AS TEXT)
cast(users.age, "double")  // CAST(age AS DOUBLE)
```

### 별칭과 리터럴

```typescript
import { alias, literal } from "@petradb/quarry";

alias(add(users.age, 10), "age_plus_10")

literal("hello")  // 문자열
literal(42)        // 숫자
literal(true)      // 부울
literal(null)      // null
```

## 집계와 그룹화

### 내장 집계

```typescript
import { count, sum, avg, min, max, alias } from "@petradb/quarry";
import { stringAgg, arrayAgg, boolAnd, boolOr, jsonAgg, jsonObjectAgg } from "@petradb/quarry";

// 모든 행 수 세기
const [{ total }] = await db
  .select(users)
  .columns(alias(count(), "total"))
  .execute();

// 집계와 함께 그룹화
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

// 기타 집계
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

### 통계 집계

```typescript
import { variance, varSamp, varPop, stddev, stddevSamp, stddevPop } from "@petradb/quarry";

variance(emp.salary)   // VARIANCE(salary) — 표본 분산
varSamp(emp.salary)    // VAR_SAMP(salary) — variance와 동일
varPop(emp.salary)     // VAR_POP(salary) — 모집단 분산
stddev(emp.salary)     // STDDEV(salary) — 표본 표준편차
stddevSamp(emp.salary) // STDDEV_SAMP(salary) — stddev와 동일
stddevPop(emp.salary)  // STDDEV_POP(salary) — 모집단 표준편차
```

### 비트 집계

```typescript
import { bitAndAgg, bitOrAgg, bitXorAgg } from "@petradb/quarry";

bitAndAgg(emp.flags)  // BIT_AND(flags)
bitOrAgg(emp.flags)   // BIT_OR(flags)
bitXorAgg(emp.flags)  // BIT_XOR(flags)
```

### EVERY

```typescript
import { every } from "@petradb/quarry";

every(emp.active)  // EVERY(active) — 모든 행이 true일 때 true
```

### 집계 FILTER

`filter()`로 집계가 처리하는 행을 제한합니다:

```typescript
import { filter } from "@petradb/quarry";

// COUNT(*) FILTER (WHERE salary > 100)
filter(count(), gt(emp.salary, 100))

// SUM(salary) FILTER (WHERE active = true)
filter(sum(emp.salary), eq(emp.active, true))
```

여러 필터된 집계를 사용하는 예제:

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

## 함수

`fn()`으로 모든 SQL 함수를 호출합니다:

```typescript
import { fn } from "@petradb/quarry";

fn("upper", users.name)        // UPPER(name)
fn("coalesce", users.age, 0)   // COALESCE(age, 0)
fn("length", users.name)       // LENGTH(name)
fn("lower", users.email)       // LOWER(email)
fn("abs", users.age)           // ABS(age)
fn("round", users.score, 2)    // ROUND(score, 2)
```

## 조인

Quarry는 컴파일 타임 결과 타이핑과 함께 inner, left, right, full, cross 조인을 지원합니다.

### Inner 조인

두 테이블의 모든 컬럼이 결과에 포함됩니다. 원래 스키마의 NULL 허용 여부가 유지됩니다:

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

// 결과 타입: (InferSelect<users> & InferSelect<posts>)[]
// rows[0].name  → string
// rows[0].title → string
// rows[0].body  → string | null (posts 스키마에서 null 허용)
```

### Left 조인

조인된 테이블의 모든 컬럼이 null 허용이 됩니다. 매칭되지 않는 행이 `null`을 생성하기 때문입니다:

```typescript
const rows = await db
  .select(users)
  .leftJoin(posts, eq(users.id, posts.userId))
  .execute();

// 결과 타입: (InferSelect<users> & Nullable<InferSelect<posts>>)[]
// rows[0].name   → string       (기본 테이블, 영향 없음)
// rows[0].title  → string | null (left join으로 null 허용)
// rows[0].userId → number | null (left join으로 null 허용)
```

### Right 조인

기본 테이블의 컬럼이 null 허용이 되고, 조인된 테이블의 컬럼은 원래의 null 허용 여부를 유지합니다:

```typescript
const rows = await db
  .select(users)
  .rightJoin(posts, eq(users.id, posts.userId))
  .execute();

// 결과 타입: (Nullable<InferSelect<users>> & InferSelect<posts>)[]
// rows[0].name  → string | null (right join으로 기본 테이블 null 허용)
// rows[0].title → string        (조인된 테이블, 영향 없음)
```

### Full 조인

양쪽 모두 null 허용이 됩니다:

```typescript
const rows = await db
  .select(users)
  .fullJoin(posts, eq(users.id, posts.userId))
  .execute();

// 결과 타입: (Nullable<InferSelect<users>> & Nullable<InferSelect<posts>>)[]
// rows[0].name  → string | null
// rows[0].title → string | null
```

### Cross 조인

두 테이블의 데카르트 곱을 생성합니다 — `on` 조건 없음:

```typescript
const rows = await db
  .select(users)
  .crossJoin(posts)
  .execute();

// 결과 타입: (InferSelect<users> & InferSelect<posts>)[]
// 사용자 × 게시물의 모든 조합
```

### 연쇄 조인

여러 조인이 타입을 올바르게 누적합니다:

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

// posts 컬럼: non-null (inner join)
// comments 컬럼: nullable (left join)
// rows[0].title   → string        (inner join)
// rows[0].content → string | null  (left join)
```

### 컬럼 선택과 조인

```typescript
const rows = await db
  .select(users)
  .columns(users.name, posts.title)
  .innerJoin(posts, eq(users.id, posts.userId))
  .execute();
```

### 집계와 조인

```typescript
const rows = await db
  .select(users)
  .columns(users.name, alias(count(), "post_count"))
  .innerJoin(posts, eq(users.id, posts.userId))
  .groupBy(users.name)
  .orderBy(desc(alias(count(), "post_count")))
  .execute();
```

## 테이블 별칭

셀프 조인이나 같은 테이블이 여러 번 나타날 때 `tableAs()`를 사용하여 별칭이 지정된 테이블을 생성합니다:

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

별칭은 타입 안전합니다 — `mgr.name`은 여전히 employees 스키마에 `name`이 존재하는지 강제합니다.

## 서브쿼리

### IN 서브쿼리

```typescript
import { inSubquery, notInSubquery } from "@petradb/quarry";

// 게시물이 하나 이상 있는 사용자
const rows = await db
  .select(users)
  .where(
    inSubquery(
      users.id,
      db.select(posts).columns(posts.userId).toExpr(),
    ),
  )
  .execute();

// 게시물이 없는 사용자
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

### EXISTS 서브쿼리

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

### 스칼라 서브쿼리

`subquery()`를 사용하여 select를 스칼라 값으로 래핑합니다:

```typescript
import { subquery } from "@petradb/quarry";

// 평균 나이보다 나이가 많은 사용자
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
서브쿼리로 select를 임베딩할 때는 `.toAST()`가 아닌 `.toExpr()`을 사용합니다. `.toExpr()`은 원시 `ASTSelect` 노드를 반환하고, `.toAST()`는 이를 `QueryCommand`로 래핑합니다.
:::

## 정렬

```typescript
import { asc, desc } from "@petradb/quarry";

// 기본 정렬
db.select(users).orderBy(asc(users.name))
db.select(users).orderBy(desc(users.age))

// 여러 컬럼
db.select(users).orderBy(asc(users.name), desc(users.age))

// NULLS FIRST / NULLS LAST
db.select(users).orderBy(asc(users.age, { nulls: "first" }))
db.select(users).orderBy(desc(users.age, { nulls: "last" }))
```

`nulls`를 지정하지 않으면 엔진이 기본 동작을 사용합니다(오름차순에서 null이 마지막, 내림차순에서 null이 처음).

## 수정

```typescript
// WHERE와 함께 수정
const result = await db
  .update(users)
  .set({ age: 31 })
  .where(eq(users.name, "Alice"))
  .execute();
// result.rowCount → 1

// 여러 필드 수정
await db
  .update(users)
  .set({ name: "Alice Smith", age: 32, active: false })
  .where(eq(users.id, 1))
  .execute();

// null로 설정
await db
  .update(users)
  .set({ age: null })
  .where(eq(users.name, "Bob"))
  .execute();
```

`.set()` 메서드는 `Partial<InferSelect<T>>`를 받습니다 — TypeScript가 유효한 컬럼 이름과 타입을 강제합니다.

### UPDATE...FROM

다른 테이블을 조인하여 업데이트를 구동합니다:

```typescript
const priceUpdates = table("price_updates", {
  id: serial("id").primaryKey(),
  productName: text("product_name").notNull(),
  newPrice: integer("new_price").notNull(),
});

await db
  .update(products)
  .set({ price: 0 }) // 값 설정; 조건부 로직에는 WHERE의 컬럼 참조 사용
  .from(priceUpdates)
  .where(eq(products.name, priceUpdates.productName))
  .execute();
```

`.from()`은 여러 테이블을 받습니다:

```typescript
db.update(t1).set({ ... }).from(t2, t3).where(and(...))
```

### RETURNING

수정과 삭제는 `.returning()`으로 영향받은 행을 돌려받습니다:

```typescript
const result = await db
  .update(users)
  .set({ active: false })
  .where(lt(users.age, 18))
  .returning(users.id, users.name)
  .execute();
// result.rows → [{ id: 3, name: "Charlie" }, ...]
```

## 삭제

```typescript
const result = await db
  .delete(users)
  .where(eq(users.name, "Alice"))
  .execute();
// result.rowCount → 1
```

### DELETE...USING

다른 테이블을 조인하여 삭제할 행을 결정합니다:

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

`.using()`은 여러 테이블을 받습니다:

```typescript
db.delete(t1).using(t2, t3).where(and(...))
```

## 트랜잭션

자동 커밋/롤백과 함께 여러 작업을 트랜잭션에 래핑합니다:

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
// 어떤 작업이든 throw하면 전체 트랜잭션이 롤백됨
```

콜백은 트랜잭션에 스코프된 `QuarryDB` 인스턴스를 받습니다. 콜백의 반환 값이 타입이 보존된 채 `transaction()`의 반환 값이 됩니다.

## AST 검사

모든 빌더에는 실행 없이 원시 AST 객체를 반환하는 `.toAST()` 메서드가 있습니다. 디버깅, 로깅, 상위 수준 추상화 구축에 유용합니다:

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

## 작동 원리

Quarry는 쿼리 AST를 나타내는 일반 JavaScript 객체(`kind` 필드가 있는 구별된 유니온)를 빌드합니다. `.execute()`를 호출하면 이 객체가 엔진의 `executeAST()` 메서드에 전달되어, SQL 문자열 생성과 파싱을 완전히 건너뛰고 엔진의 내부 Scala AST로 직접 변환됩니다.

```
스키마 → 빌더 API → JS AST 객체 → 엔진 AST → 재작성 → 실행
                          ↑ SQL 파서 없음
```

이를 통해 Quarry는 파싱 오버헤드를 제거하고 완전한 컴파일 타임 타입 안전성을 가능하게 하면서 SQL과 동일한 쿼리 기능을 제공합니다.

## 정리

```typescript
await session.close();
```

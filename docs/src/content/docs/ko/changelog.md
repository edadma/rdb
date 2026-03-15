---
title: 변경 이력
---

## v1.5-20260315

### PL/pgSQL - 저장 프로시저, 함수, 트리거

DO 블록, 저장 함수, 저장 프로시저에서의 완전한 절차적 언어 지원:

- **DO 블록** - DECLARE, BEGIN...END를 사용하는 익명 PL/pgSQL 블록
- **저장 함수** - `CREATE FUNCTION name(params) RETURNS type AS $$ ... $$ LANGUAGE plpgsql`, 모든 SQL 표현식에서 호출 가능
- **저장 프로시저** - `CREATE PROCEDURE name(params) AS $$ ... $$ LANGUAGE plpgsql`, `CALL`로 호출
- **트리거** - `CREATE TRIGGER name BEFORE|AFTER INSERT|UPDATE|DELETE ON table FOR EACH ROW EXECUTE FUNCTION func()`
  - BEFORE 트리거는 NULL을 반환하여 작업을 취소할 수 있습니다
  - AFTER 트리거는 작업 이후에 실행됩니다
  - INSERT, UPDATE, DELETE, COPY FROM에 대해 트리거가 실행됩니다
  - TG_OP, TG_TABLE_NAME, OLD, NEW 변수 사용 가능
- **제어 흐름** - IF/ELSIF/ELSE, WHILE LOOP, FOR 범위/쿼리 LOOP, RETURN, RAISE NOTICE/EXCEPTION, PERFORM, EXCEPTION WHEN
- **영속성** - 함수, 프로시저, 트리거는 PersistentDB와 TextDB에서 닫기/다시 열기 후에도 유지됩니다
- **OR REPLACE** - 기존 함수와 프로시저를 덮어쓸 수 있습니다

### 사용자 정의 네이티브 함수

호스트 언어 콜백을 SQL 함수로 등록하여 쿼리, 트리거, 프로시저에서 호출할 수 있습니다:

- **Scala** - `db.registerScalarFunction("name", { args => result })`
- **JavaScript** - `session.registerFunction("name", (args) => result)`
- **C** - `petradb_create_function(db, "name", nargs, user_data, callback)` (SQLite 스타일의 타입 값/컨텍스트 API 사용)

### C 라이브러리 및 커서 API

- **네이티브 공유 라이브러리** - `@exported` C 호출 가능 함수가 포함된 Scala Native을 통한 `libpetradb-engine.so`
- **SQLite 스타일 C API** - `petradb_open`, `petradb_exec`, `petradb_prepare/step/finalize`, 타입별 컬럼 접근자
- **사용자 정의 함수** - `petradb_value_int/double/text`, `petradb_result_int/double/text/null/error`, `petradb_user_data`
- **커서 API** - 지연 로우별 반복을 위한 `session.openCursor(sql)`, `step()`, 타입별 컬럼 접근자, `fetch(n)`, `move(n)`, 매개변수화된 쿼리
- **C 헤더** - 전체 API 문서가 포함된 `petradb.h`
- **C 테스트 스위트** - 67개 테스트
- **Rust FFI 테스트** - 크로스 언어 상호운용성을 증명하는 38개 테스트

### 가상 테이블

- **확장 가능 프레임워크** - `CREATE VIRTUAL TABLE name USING module(args)`, 읽기 전용, SHOW TABLES에 표시
- **내장 CSV 모듈** - 헤더/구분자 옵션이 있는 `CREATE VIRTUAL TABLE t USING csv('file.csv')`
- **커스텀 모듈** - Scala API에서 `db.registerVirtualTableModule("name", module)`

### csv_file() 테이블 함수

가져오기 없이 CSV 파일을 직접 쿼리합니다:

```sql
SELECT * FROM csv_file('data.csv');
SELECT e.name, d.dept FROM csv_file('employees.csv') e
  JOIN csv_file('departments.csv') d ON e.dept_id = d.id;
```

### 고급 인덱스

- **부분 인덱스** - `CREATE INDEX ... WHERE condition` - 조건에 맞는 행만 인덱싱
- **표현식 인덱스** - `CREATE INDEX ... ON table ((expr))` - `lower(email)`과 같은 계산된 값을 인덱싱
- **결합** - 부분 인덱스 + 표현식 인덱스를 함께 사용 가능

### 윈도우 함수

- **FIRST_VALUE(expr)** - 윈도우 프레임의 첫 번째 행 값
- **LAST_VALUE(expr)** - 윈도우 프레임의 마지막 행 값
- **NTH_VALUE(expr, n)** - 프레임의 n번째 행 값

### DELETE ... USING

PostgreSQL 구문과 일치하는 다중 테이블 삭제:

```sql
DELETE FROM orders USING customers
WHERE orders.customer_id = customers.id AND customers.status = 'inactive';
```

### Quarry - 타입 안전 AST 쿼리 빌더

새로운 `@petradb/quarry` 패키지: SQL 문자열이 아닌 AST 객체를 생성하는 타입 안전 쿼리 빌더:

- 21가지 컬럼 타입의 스키마 정의
- 완전한 CRUD: 타입 안전 컬럼 참조가 포함된 select, insert, update, delete
- 조인: 타입이 지정된 결과가 있는 inner, left, right, full outer, cross
- 표현식: 50개 이상의 연산자, 집계, CASE/CAST/EXISTS, 서브쿼리
- Upsert: `onConflictDoNothing()`, `onConflictDoUpdate()`
- 셀프 조인을 위한 테이블 별칭
- 트랜잭션, RETURNING, DISTINCT ON
- 모든 기능에 대한 컴파일 타임 타입 테스트
- 집합 연산: UNION, INTERSECT, EXCEPT
- 윈도우 함수, CTE, 명명된 스칼라 헬퍼

### 버그 수정

- **ByteaValue** - BYTEA 컬럼에 대한 `ARRAY[...]`가 이제 `ArrayValue` 대신 `ByteaValue`를 올바르게 생성합니다
- **JS/Client 결과 타입** - 비완전 매치 크래시를 방지하기 위해 누락된 PL/pgSQL 결과 타입 핸들러(DoBlockResult, CreateFunctionResult 등)를 추가했습니다
- **코덱** - 클라이언트/서버 통신을 위한 모든 새로운 결과 타입의 직렬화를 추가했습니다
- **llms.txt** - `type` 필드를 `command` 필드로 수정하고, 모든 결과 타입과 기능을 업데이트했습니다

### 모듈 이름 변경

- **shared → common** - 공유 타입 모듈의 이름을 `petradb-shared`에서 `petradb-common`으로 변경했습니다

### 문서

- 새로운 **PL/pgSQL** 레퍼런스 페이지 (트리거, 함수, 프로시저, 제어 흐름)
- 새로운 **C API** 레퍼런스 페이지 (완전한 SQLite 스타일 인터페이스)
- Java (JDBC)와 C를 위한 새로운 **시작하기** 가이드
- DDL 문서 업데이트: 부분/표현식 인덱스, CHECK 제약 조건, 트리거, 저장 루틴
- DML 문서 업데이트: DELETE...USING, csv_file(), 가상 테이블
- JS/Scala API 문서 업데이트: registerFunction, 결과 타입
- 랜딩 페이지: 4개의 시작하기 버튼 (JS, Java, Scala, C)
- 모든 최신 기능이 포함된 llms.txt 재작성

### 버전 업데이트

| 컴포넌트 | Maven Central | npm |
|-----------|---------------|-----|
| common | 1.5.0 | — |
| engine | 1.5.0 | @petradb/engine 1.5.0 |
| client | 1.5.0 | @petradb/client 1.5.0 |
| server | 1.5.0 | @petradb/server 1.5.0 |
| cli | 1.5.0 | @petradb/cli 1.5.0 |
| jdbc | 1.5.0 | — |
| drizzle | — | @petradb/drizzle 1.5.0 |
| knex | — | @petradb/knex 1.5.0 |
| lucid | — | @petradb/lucid 1.5.0 |
| quarry | — | @petradb/quarry 1.5.0 |

## v1.4-20260314

### 버그 수정 및 개선

- **상관 IN 서브쿼리 수정** - 인덱싱된 테이블에서의 상관 `IN (SELECT ...)`가 이제 올바르게 작동합니다
- **정규화된 컬럼 해석** - 복잡한 조인에서의 모호한 컬럼 참조 수정
- **빈 IN/ANY 처리** - `IN ()`과 `= ANY('{}')` 에지 케이스 해결
- **외래 키 CASCADE 정리** - `DROP TABLE ... CASCADE`가 이제 자식 테이블의 FK 제약 조건을 올바르게 제거합니다
- **DROP TABLE IF EXISTS ... CASCADE** - `IF EXISTS`와 `CASCADE`를 결합해도 더 이상 파서 오류가 발생하지 않습니다
- **배열 리터럴 캐스팅** - `'{1,2,3}'::integer[]` PostgreSQL 배열 리터럴 구문 지원
- **매개변수화된 쿼리 수정** - 서브쿼리와 상관 경로에 대한 매개변수 바인딩 개선

### 버전 업데이트

| 컴포넌트 | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.4.2 | — |
| engine | 1.4.9 | @petradb/engine 1.4.3 |
| client | 1.4.2 | @petradb/client 1.4.2 |
| server | — | @petradb/server 1.4.3 |
| cli | — | @petradb/cli 1.4.3 |
| jdbc | 1.4.3 | — |
| drizzle | — | @petradb/drizzle 1.4.3 |

## v1.4-20260312

### 공통 테이블 표현식 (CTE)

`WITH`와 `WITH RECURSIVE`를 사용한 완전한 CTE 지원.

**비재귀 CTE** - 가독성과 재사용을 위한 명명된 서브쿼리:

```sql
WITH active_orders AS (
  SELECT * FROM orders WHERE status = 'active'
)
SELECT customer_id, SUM(amount)
FROM active_orders
GROUP BY customer_id;
```

하나의 쿼리에서 여러 CTE를 정의할 수 있으며, 뒤의 CTE는 앞의 CTE를 참조할 수 있습니다. 컬럼 별칭이 지원됩니다: `WITH t(x, y) AS (...)`. 동일한 이름의 테이블이 있으면 CTE가 이를 가립니다.

**재귀 CTE** - 계층적 데이터 및 그래프 데이터를 위한 반복 쿼리:

```sql
WITH RECURSIVE descendants(id, name, depth) AS (
  SELECT id, name, 0 FROM employees WHERE manager_id IS NULL
  UNION ALL
  SELECT e.id, e.name, d.depth + 1
  FROM employees e INNER JOIN descendants d ON e.manager_id = d.id
)
SELECT name, depth FROM descendants ORDER BY depth, name;
```

`UNION ALL`(중복 유지)과 `UNION`(중복 제거) 모두 지원됩니다. 안전 제한으로 최대 1000회 반복합니다.

### 윈도우 함수

세 가지 카테고리의 완전한 윈도우 함수 지원:

**순위 함수** - `PARTITION BY`와 `ORDER BY`를 사용하는 `ROW_NUMBER()`, `RANK()`, `DENSE_RANK()`:

```sql
SELECT name, department, salary,
  RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dept_rank
FROM employees;
```

**값 함수** - 설정 가능한 오프셋과 기본값이 있는 `LAG()`, `LEAD()`, `NTILE()`:

```sql
SELECT name, salary,
  LAG(salary, 1, 0) OVER (ORDER BY salary) AS prev_salary,
  NTILE(4) OVER (ORDER BY salary) AS quartile
FROM employees;
```

**집계 윈도우 함수** - 프레임 사양을 포함한 모든 집계 함수(`SUM`, `COUNT`, `AVG`, `MIN`, `MAX` 등)와 `OVER()`:

```sql
SELECT name, salary,
  SUM(salary) OVER (ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_total,
  AVG(salary) OVER (PARTITION BY department) AS dept_avg
FROM employees;
```

프레임 경계: `UNBOUNDED PRECEDING`, `UNBOUNDED FOLLOWING`, `CURRENT ROW`, `N PRECEDING`, `N FOLLOWING`. 프레임 절이 없으면 집계 윈도우 함수는 전체 파티션에 대해 계산합니다.

### 집계 FILTER 절

그룹화된 쿼리와 윈도우 함수 모두에서 집계 함수에 대한 `FILTER (WHERE ...)`:

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE status = 'active') AS active_count,
  SUM(amount) FILTER (WHERE amount > 100) OVER (ORDER BY created_at
    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_high_value
FROM orders;
```

### 해시 조인 최적화

인덱스가 없는 등가 조인은 이제 크로스 프로덕트 대신 해시 조인 전략을 사용하여 조인 복잡도를 O(n×m)에서 O(n+m)로 줄입니다. INNER, LEFT, RIGHT, FULL 조인에 적용됩니다. 인덱스가 사용 가능한 경우 인덱스 중첩 루프 조인이 우선됩니다. 비등가 조인 조건은 여전히 크로스 프로덕트로 대체됩니다. `EXPLAIN` 출력에서 `Hash Join`, `Hash Left Join`, `Hash Right Join`, `Hash Full Join`으로 표시됩니다.

### 생성 컬럼

`GENERATED ALWAYS AS (expr) STORED` 계산 컬럼:

```sql
CREATE TABLE products (
  price NUMERIC,
  tax_rate NUMERIC DEFAULT 0.08,
  total NUMERIC GENERATED ALWAYS AS (price * (1 + tax_rate)) STORED
);
```

생성 컬럼은 INSERT와 UPDATE 시 재계산됩니다. 직접 설정할 수 없습니다.

### ORDER BY NULL 정렬

`ORDER BY`는 이제 SQL 표준 NULL 정렬을 기본값으로 사용합니다: `ASC` → `NULLS LAST`, `DESC` → `NULLS FIRST`. 명시적 `NULLS FIRST` / `NULLS LAST` 재정의가 지원됩니다.

### 시퀀스 지원

완전한 PostgreSQL 호환 시퀀스 지원. 옵션이 포함된 `CREATE SEQUENCE`와 `DROP SEQUENCE`(`INCREMENT BY`, `START WITH`, `MINVALUE`, `MAXVALUE`, `CYCLE`, `IF NOT EXISTS` / `IF EXISTS`). 시퀀스 함수: `nextval()`, `currval()`, `setval()`, `lastval()`.

`SERIAL`, `SMALLSERIAL`, `BIGSERIAL` 컬럼은 이제 PostgreSQL 동작과 일치하는 백업 시퀀스(`<table>_<column>_seq` 이름)를 생성합니다. `DROP TABLE`은 소유된 시퀀스를 연쇄적으로 삭제합니다. `TRUNCATE`는 백업 시퀀스를 재설정합니다. 시퀀스 상태는 완전히 트랜잭션 처리됩니다 — `ROLLBACK`은 시퀀스 카운터를 복원합니다. 영구 데이터베이스는 카탈로그에 시퀀스 상태를 직렬화합니다.

새로운 SQL 명령: `SHOW SEQUENCES`, `SHOW INDEXES` (모든 테이블의 모든 인덱스).

CLI: 새로운 `\ds` (시퀀스 목록)와 `\di` (인덱스 목록) 메타 명령.

### CREATE INDEX USING 절

`CREATE INDEX ... USING btree` 구문이 이제 허용됩니다(btree가 유일하게 지원되는 방법). 이는 PostgreSQL이 생성한 DDL 및 ORM과의 호환성을 개선합니다.

### 버그 수정

- `ORDER BY`와 NULL 값: 비교자가 이제 두 값이 모두 NULL일 때 0을 반환하여, 여러 정렬 키에서의 비결정적 정렬 결과를 수정합니다
- `ORDER BY` 별칭 해석: SELECT 별칭(예: `SELECT x AS y ... ORDER BY y`)이 이제 비그룹화 쿼리에서 윈도우 함수 유무와 관계없이 올바르게 해석됩니다
- `Type.convert()` null 처리: 타입 변환(예: UPDATE SET의 준비된 구문 매개변수)을 통해 전달되는 NULL 값이 이제 타입의 텍스트 표현으로 변환되지 않고 NULL로 보존됩니다. 13가지 타입에서 수정: TEXT, VARCHAR, CHAR, UUID, TIMESTAMP, DATE, TIME, TIMETZ, INTERVAL, TIMESTAMPTZ, BYTEA, JSON, ENUM
- ORDER BY 파서의 nulls 절에 대한 완전 매치 경고
- 동기 throw에 대한 플레이그라운드 터미널의 조용한 오류

### 버전 업데이트

모든 컴포넌트가 1.4.1로 업데이트되었습니다:

| 컴포넌트 | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.4.1 | — |
| engine | 1.4.1 | @petradb/engine 1.4.1 |
| client | 1.4.1 | @petradb/client 1.4.1 |
| server | 1.4.1 | @petradb/server 1.4.1 |
| cli | 1.4.1 | @petradb/cli 1.4.1 |
| jdbc | 1.4.1 | — |
| knex | — | @petradb/knex 1.4.0 |
| lucid | — | @petradb/lucid 1.4.0 |
| drizzle | — | @petradb/drizzle 1.4.1 |

## v1.3-20260309

### 트랜잭션 DDL

DDL 문(CREATE TABLE, CREATE INDEX, DROP TABLE 등)이 이제 트랜잭션 내에서 완전히 지원되며 DML과 함께 원자적으로 롤백됩니다. DDL과 DML은 단일 BEGIN/COMMIT 블록 내에서 자유롭게 혼합할 수 있습니다. MemoryDB와 PersistentDB 모두 BEGIN 시점에 전체 카탈로그 스냅샷을 캡처하고 ROLLBACK 시 복원합니다.

### Drizzle 관계형 쿼리

Drizzle ORM 관계형 쿼리에 대한 완전한 지원(`db.query.*.findMany()`, `db.query.*.findFirst()`). `json_build_array`와 `json_build_object` 스칼라 함수를 추가하고, LATERAL 서브쿼리에서의 매개변수 치환을 수정했습니다.

### 버전 업데이트

| 컴포넌트 | Maven Central | npm |
|-----------|---------------|-----|
| engine | 1.3.1 | @petradb/engine 1.3.3 |
| server | 1.3.1 | @petradb/server 1.3.1 |
| cli | 1.3.1 | @petradb/cli 1.3.1 |
| jdbc | 1.3.1 | — |
| drizzle | — | @petradb/drizzle 1.3.1 |

## v1.3-20260308

### 스키마 지원

PostgreSQL 스타일 스키마 네임스페이스. 모든 데이터베이스는 기본적으로 `public` 스키마를 가지며, 한정되지 않은 테이블 이름은 `public`으로 해석됩니다. 스키마 한정 이름(`schema.table`)은 모든 DDL 및 DML 문에서 작동합니다 — CREATE TABLE, INSERT, UPDATE, DELETE, SELECT, ALTER TABLE, DROP TABLE, TRUNCATE, CREATE INDEX, COPY.

```sql
CREATE SCHEMA inventory;
CREATE TABLE inventory.products (id SERIAL PRIMARY KEY, name TEXT);
INSERT INTO inventory.products (name) VALUES ('Widget');
SELECT * FROM inventory.products;
```

### information_schema 가상 테이블

`information_schema.schemata`, `information_schema.tables`, `information_schema.columns`를 쿼리할 수 있게 되었습니다. 스키마 한정 테이블은 올바른 `table_schema`를 보고합니다. 이 뷰들은 데이터베이스 메타데이터에서 동적으로 생성됩니다.

### Drizzle ORM 마이그레이션

`@petradb/drizzle`의 새로운 `migrate()` 함수가 Drizzle Kit 마이그레이션 파일을 적용합니다. `meta/_journal.json`을 읽고 SQL 마이그레이션 파일을 순서대로 실행하며, 적용된 마이그레이션을 `drizzle.__drizzle_migrations`에 추적합니다.

```typescript
import { migrate } from "@petradb/drizzle";
await migrate(db, { migrationsFolder: "./drizzle" });
```

### JDBC 메타데이터 개선

`DatabaseMetaData.getColumns()`가 이제 컬럼 타입과 정밀도/스케일 선언에 기반한 정확한 `COLUMN_SIZE`, `DECIMAL_DIGITS`, `CHAR_OCTET_LENGTH` 값을 반환합니다.

### 버전 업데이트

모든 컴포넌트가 1.3.0으로 업데이트되었습니다:

| 컴포넌트 | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.3.0 | — |
| engine | 1.3.0 | @petradb/engine 1.3.0 |
| client | 1.3.0 | @petradb/client 1.3.0 |
| server | 1.3.0 | @petradb/server 1.3.0 |
| cli | 1.3.0 | @petradb/cli 1.3.0 |
| jdbc | 1.3.0 | — |
| knex | — | @petradb/knex 1.3.0 |
| lucid | — | @petradb/lucid 1.3.0 |
| drizzle | — | @petradb/drizzle 1.3.0 |

## v1.2-20260308

### Drizzle ORM 드라이버 재작성

`@petradb/drizzle`가 `drizzle-orm/pg-proxy` 래퍼에서 `PgSession`/`PgPreparedQuery`/`PgTransaction`을 직접 확장하는 커스텀 PostgreSQL 방언 드라이버로 재작성되었습니다. 이를 통해 `drizzle-orm/node-postgres`와 완전한 기능 동등성을 제공합니다:

- 자동 커밋/롤백이 포함된 `db.transaction()`
- 명시적 롤백을 위한 `tx.rollback()`
- insert/update/delete에서의 `returning()` (부분 컬럼 선택 포함)
- 관계형 쿼리 지원 (엔진의 `json_build_array`/`json_agg` 지원 보류 중)

### 타입 강제 변환: 텍스트 매개변수에서 NUMERIC 컬럼으로

`NumericType.convert`가 이제 `TextValue`를 받아 `BigDecimal`로 파싱합니다. 이는 `IntegerType`, `BigintType`, `SmallintType`, `DoubleType`의 기존 강제 변환 동작과 일치합니다. 이를 통해 숫자 값을 텍스트로 전송하는 ORM의 매개변수화된 INSERT/UPDATE가 수정됩니다(표준 PostgreSQL 와이어 프로토콜 동작).

### 3값 NULL 로직

NULL 처리를 위한 완전한 SQL 3값 로직:

- 비교 연산자(`=`, `!=`, `<`, `>`, `<=`, `>=`)는 어느 한 피연산자가 NULL이면 NULL을 반환합니다
- `AND`/`OR`은 적절한 3값 진리표를 구현합니다 (예: `FALSE AND NULL` → `FALSE`, `TRUE OR NULL` → `TRUE`)
- `IN`/`NOT IN`은 NULL을 올바르게 전파합니다 (예: `3 NOT IN (1, 2, NULL)` → unknown)
- 산술(`+`, `-`, `*`, `/`, `%`)과 문자열 연결(`||`)은 NULL을 전파합니다
- `LIKE`는 NULL 피연산자를 처리합니다

### 통합 표현식 문법

SQL 파서의 별도 `expression`과 `booleanExpression` 계층이 단일 표현식 구문으로 통합되었습니다. 부울 연산자(`AND`, `OR`, `NOT`)는 이제 우선순위 체인의 일반 연산자입니다. 이를 통해 표현식이 유효한 모든 곳에서 부울 표현식을 사용할 수 있습니다 (예: `SELECT a > 5 AND b < 10`).

### 즉시 컬럼 참조 검증

`WHERE`, `GROUP BY`, `HAVING`, `ORDER BY`의 컬럼 참조가 이제 쿼리 계획 구성 시점에 즉시 검증되어, 빈 테이블이나 단일 행 정렬에서도 존재하지 않는 컬럼을 감지합니다. 이전에는 잘못된 참조가 행별 평가 시점에만 감지되어, 빈 테이블에 대한 쿼리가 조용히 성공했습니다.

### 버그 수정

- UPDATE 시 NOT NULL 제약 조건이 적용되지 않음
- UNIQUE 제약 조건이 여러 NULL을 거부함 (SQL 표준: NULL은 고유함)
- INSERT 컬럼 목록의 중복 컬럼이 감지되지 않음
- 빈 테이블에서 `SUM`/`AVG`/`MIN`/`MAX`가 NULL 대신 0을 반환함
- `LIKE '_'`가 빈 문자열과 일치함
- `LIMIT 0`이 오류를 발생시킴

### 버전 업데이트

| 컴포넌트 | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | — |
| engine | 1.2.9 | @petradb/engine 1.2.16 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | 1.2.6 | @petradb/server 1.2.9 |
| cli | 1.2.8 | @petradb/cli 1.2.9 |
| jdbc | 1.2.13 | — |
| knex | — | @petradb/knex 1.2.2 |
| lucid | — | @petradb/lucid 1.2.1 |
| drizzle | — | @petradb/drizzle 1.2.2 |

## v1.2-20260307

### SQL: INSERT VALUES에서의 `DEFAULT` 키워드

`INSERT INTO t (id, name) VALUES (DEFAULT, 'Alice')`가 이제 작동합니다. SQL 표준 `DEFAULT` 키워드는 이전에 파서에 의해 거부되어, serial이나 기본값이 있는 컬럼에 대해 명시적으로 `DEFAULT`를 전달하는 ORM 생성 INSERT 문을 중단시켰습니다.

### Drizzle ORM 통합

새로운 `@petradb/drizzle` 패키지가 커스텀 PostgreSQL 방언 구현을 가진 [Drizzle ORM](https://orm.drizzle.team) 드라이버를 제공합니다. `pgTable`을 사용한 스키마 정의, insert/select/update/delete, returning 절, 자동 커밋/롤백이 포함된 `db.transaction()`, 타입 안전 쿼리를 지원합니다. `drizzle-orm/node-postgres`와 완전한 기능 동등성을 제공합니다.

### 종속 패키지 버전 업데이트

Engine, server, cli, jdbc가 DEFAULT 키워드 수정을 반영하여 업데이트되었습니다.

| 컴포넌트 | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | — |
| engine | 1.2.7 | @petradb/engine 1.2.14 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | — | @petradb/server 1.2.7 |
| cli | — | @petradb/cli 1.2.7 |
| jdbc | 1.2.11 | — |
| knex | — | @petradb/knex 1.2.2 |
| lucid | — | @petradb/lucid 1.2.1 |
| drizzle | — | @petradb/drizzle 1.2.0 |

## v1.2-20260306

### JS API: `close()`가 `Promise<void>`를 반환
`Session.close()`가 이제 `void` 대신 `Promise<void>`를 반환하여, 상호 교환성을 위해 client 모듈의 API와 일치합니다.

### 타임스탬프 파싱
`parseTimestamp`가 이제 `Z` 접미사, `+/-HH:MM` 오프셋, 밀리초, 시간대 정보가 포함된 공백 구분 타임스탬프를 처리합니다. `TIMESTAMP` 컬럼을 위해 시간대를 `LocalDateTime`으로 제거합니다.

### JS 파사드 완전성
`toJS`와 `typeString`이 이제 `DateValue`, `TimeValue`, `TimestampTZValue`, `TimeTZValue`, `IntervalValue`, `ByteaValue`를 처리합니다.

### SQL: 한정된 스타 (`table.*`)
`SELECT t.*` 구문이 이제 조인과 혼합 표현식을 포함한 쿼리에서 작동합니다.

### 비교에서의 타입 강제 변환
- `NumberValue`와 `TextValue`가 이제 타입 간 비교를 할 수 있습니다 (텍스트 매개변수 vs 숫자 컬럼 및 그 반대)
- `TimestampValue`가 이제 텍스트를 타임스탬프로 파싱하여 `TextValue`와 비교할 수 있습니다

### Knex 드라이버: Date 바인딩
`_sanitizeBindings`가 JS `Date` 객체를 엔진에 전달하기 전에 ISO 문자열로 변환하여, `Date.toString()` 형식에서의 `DateTimeParseException`을 방지합니다.

| 컴포넌트 | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | — |
| engine | 1.2.6 | @petradb/engine 1.2.13 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | — | @petradb/server 1.2.5 |
| cli | — | @petradb/cli 1.2.5 |
| jdbc | 1.2.10 | — |
| knex | — | @petradb/knex 1.2.2 |
| lucid | — | @petradb/lucid 1.2.1 |

## v1.2-20260305

### JDBC 드라이버
- Fat jar 배포 — `io.github.edadma:petradb-jdbc`가 이제 Maven Central의 단일 자체 포함 jar입니다
- 깔끔한 연결 URL — `jdbc:petradb:memory`, `jdbc:petradb:file:/path`, `jdbc:petradb://host:port`
- ServiceLoader 자동 검색 — `Class.forName` 없이 `DriverManager.getConnection()`이 작동합니다
- 하드코딩된 메타데이터 버전 문자열 수정

### JS/TS 엔진 (`@petradb/engine`)
- JS 파사드에 `CreateViewResult`, `DropViewResult`, `ExplainResult`, `CopyResult` 추가
- TypeScript 타입 정의에 `ExplainResult`와 `CopyResult` 추가

### 문서
- 전체 예제가 포함된 새로운 Knex.js 가이드
- JDBC 문서: Maven/Gradle/sbt 설치 스니펫 추가, 포트 번호 수정

### 인프라
- `petradb-shared`가 이제 Maven Central에 배포 가능
- npm, Scala, JDBC 아티팩트를 포함하는 배포 후 스모크 테스트 스크립트

| 컴포넌트 | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.1 | — |
| engine | 1.2.2 | @petradb/engine 1.2.5 |
| client | 1.2.3 | @petradb/client 1.2.3 |
| server | — | @petradb/server 1.2.3 |
| cli | — | @petradb/cli 1.2.3 |
| jdbc | 1.2.6 | — |
| knex | — | @petradb/knex 1.2.0 |

## v1.2.2

### 엔진 서브패키지 재구조화
- 엔진이 `io.github.edadma.petradb.engine` 서브패키지로 이동
- 엔진과 클라이언트 모두가 확장하는 새로운 공유 `Session` 트레이트

### CLI 클라이언트 지원
- 원격 PetraDB 서버에 연결: `petradb --host localhost --port 5480`
- 인증을 위한 `--user`와 `--password` 플래그
- SQL을 통해 네트워크에서 메타 명령 작동

### SQL
- 뷰 이름과 정의를 반환하는 `SHOW VIEWS` 명령

### Knex 방언
- PetraDB와 함께 Knex.js 쿼리 빌더를 사용하기 위한 `@petradb/knex` 방언 어댑터

### 수정 사항
- 클라이언트 npm 배포 수정
- CLI npm 배포 수정
- 하드코딩된 JDBC 메타데이터 버전 문자열 수정
- JVM, JS, Native에서 1013개 이상의 테스트 통과

## v1.2

### JDBC 드라이버
- Maven Central에 `petradb-jdbc`로 배포
- DBeaver를 위한 `getGeneratedKeys()`, `addBatch()`/`executeBatch()`, FK/인덱스 메타데이터
- 파일 모드(임베디드)와 서버 모드(네트워크) 연결

### SQL 엔진
- CSV 가져오기/내보내기를 위한 `COPY FROM/TO`
- `CREATE TEMP TABLE`, `CREATE/DROP VIEW`
- `SHOW FOREIGN KEYS`/`SHOW INDEXES` 인트로스펙션
- 등가 조인을 위한 인덱스 중첩 루프 조인 최적화
- 파서를 fastparse로 마이그레이션

### 서버
- TOML 설정으로 CORS 지원
- 설정 가능한 `max_sessions`, 기본 포트 5480
- Node.js HTTP 백엔드를 사용한 JS 서버 플랫폼

### 클라이언트
- JS 파사드가 포함된 새로운 `@petradb/client` npm 패키지
- Promise를 반환하는 `connect()`/`execute()`/`close()`가 있는 `Session` 클래스

### CLI
- `\timing`, `\copy` 명령
- Native에서의 영구 히스토리

### 빌드
- Scala 3.8.2, sbt 1.12.4
- JVM, JS, Native에서 1000개 테스트 통과

## v1.1.0

### TextDB - 사람이 편집 가능한 텍스트 파일 영속성
데이터베이스를 `.ptxt` 텍스트 파일로 영속하는 새로운 스토리지 백엔드. 열 때 메모리에 로드하고 변경 후마다 파일을 다시 작성합니다.

### Upsert - `ON CONFLICT DO UPDATE`
`EXCLUDED` 의사 테이블을 사용한 삽입 또는 업데이트 의미론.

### 개선된 예외 계층
타입이 지정된 예외 클래스가 일반 `problem()` 호출을 대체합니다.

### ALTER TABLE 중앙화
`DB.alterTable()`이 이제 모든 ALTER TABLE 디스패치를 중앙화합니다.

## v1.0.1

- `@petradb/engine`에서 `ConnectSQL`을 `Session`으로 이름 변경
- `Promise<ExecuteResult[]>`를 반환하는 비동기 `execute()` API
- 네트워크 사용을 위한 새로운 `@petradb/client` 패키지
- 엔진과 서버 간의 응답 형식 정렬

## v1.0.0

첫 번째 안정 릴리스.

- 크로스 플랫폼 SQL 엔진 (JVM, JavaScript, Native)
- PostgreSQL 호환 구문
- 인메모리 및 영구(충돌 안전) 스토리지
- DDL, DML, 조인, 서브쿼리, 집계, 트랜잭션
- JSONB 연산자, 배열 타입, CHECK 제약 조건
- 879개 통과 테스트

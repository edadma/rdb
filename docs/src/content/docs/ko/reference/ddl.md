---
title: DDL
description: 데이터 정의 언어 — CREATE, ALTER, DROP, TRUNCATE 문.
---

## 스키마

PetraDB는 PostgreSQL 스타일 스키마 네임스페이스를 지원합니다. 모든 데이터베이스는 기본적으로 `public` 스키마를 가집니다. 한정되지 않은 테이블 이름은 `public`으로 해석됩니다.

### CREATE SCHEMA

```sql
CREATE SCHEMA inventory;
CREATE SCHEMA IF NOT EXISTS inventory;
```

### 스키마 한정 테이블

모든 DDL 또는 DML 문에서 `schema.table` 구문을 사용합니다:

```sql
CREATE TABLE inventory.products (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  price NUMERIC(10,2)
);

INSERT INTO inventory.products (name, price) VALUES ('Widget', 9.99);
SELECT * FROM inventory.products;
```

다른 스키마에 같은 이름의 테이블이 존재할 수 있습니다:

```sql
CREATE SCHEMA staging;
CREATE TABLE staging.products (id SERIAL, name TEXT);
CREATE TABLE public.products (id SERIAL, name TEXT);
-- 이들은 별개의 테이블입니다
```

### information_schema

PetraDB는 데이터베이스 구조를 조사하기 위한 `information_schema` 가상 테이블을 제공합니다:

```sql
SELECT * FROM information_schema.schemata;
SELECT * FROM information_schema.tables;
SELECT * FROM information_schema.columns WHERE table_name = 'products';
```

사용 가능한 뷰: `schemata`, `tables`, `columns`.

## 테이블

### CREATE TABLE

```sql
CREATE TABLE orders (
  id UUID DEFAULT gen_random_uuid(),
  customer_name TEXT NOT NULL,
  amount NUMERIC(10,2),
  status order_status,
  tags INT[],
  metadata JSON,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS orders (...);
```

### 외래 키

```sql
CREATE TABLE line_items (
  id SERIAL,
  order_id UUID REFERENCES orders (id) ON DELETE CASCADE ON UPDATE CASCADE,
  product TEXT NOT NULL
);
```

### 생성 컬럼

다른 컬럼에서 자동으로 파생되는 계산 컬럼:

```sql
CREATE TABLE products (
  price NUMERIC,
  tax_rate NUMERIC DEFAULT 0.08,
  total NUMERIC GENERATED ALWAYS AS (price * (1 + tax_rate)) STORED
);

INSERT INTO products (price) VALUES (100);
SELECT * FROM products;
-- price: 100, tax_rate: 0.08, total: 108
```

생성 컬럼은 INSERT와 UPDATE 시 재계산됩니다. 직접 작성할 수 없습니다.

### ALTER TABLE

컬럼 추가, 삭제, 이름 변경:

```sql
ALTER TABLE orders ADD COLUMN notes TEXT;
ALTER TABLE orders DROP COLUMN notes;
ALTER TABLE orders RENAME COLUMN amount TO total;
ALTER TABLE orders RENAME TO purchases;
```

컬럼 속성 변경:

```sql
ALTER TABLE orders ALTER COLUMN notes SET NOT NULL;
ALTER TABLE orders ALTER COLUMN notes DROP NOT NULL;
ALTER TABLE orders ALTER COLUMN notes SET DEFAULT 'none';
ALTER TABLE orders ALTER COLUMN notes DROP DEFAULT;
ALTER TABLE orders ALTER COLUMN amount SET DATA TYPE NUMERIC(10,2);
```

제약 조건 추가 및 삭제:

```sql
ALTER TABLE orders ADD CONSTRAINT chk_amount CHECK (amount > 0);
ALTER TABLE orders ADD CONSTRAINT uq_email UNIQUE (email);
ALTER TABLE orders ADD CONSTRAINT fk_customer
  FOREIGN KEY (customer_id) REFERENCES customers (id) ON DELETE CASCADE;
ALTER TABLE orders DROP CONSTRAINT chk_amount;
```

### CHECK 제약 조건

컬럼 값에 대한 조건을 강제합니다:

```sql
CREATE TABLE products (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  price NUMERIC CHECK (price >= 0),
  quantity INT,
  CONSTRAINT positive_qty CHECK (quantity >= 0)
);
```

CHECK 제약 조건은 INSERT와 UPDATE 시 강제됩니다.

## 트리거

전체 문서는 [PL/pgSQL — 트리거](/reference/plpgsql/#트리거)를 참고하세요.

```sql
CREATE TRIGGER trg_audit AFTER INSERT ON orders
  FOR EACH ROW EXECUTE FUNCTION audit_log();

DROP TRIGGER trg_audit ON orders;
DROP TRIGGER IF EXISTS trg_audit ON orders;
```

### TRUNCATE TABLE

모든 행을 제거하고 serial 시퀀스를 재설정합니다:

```sql
TRUNCATE TABLE orders;
```

### DROP TABLE

```sql
DROP TABLE orders;
DROP TABLE IF EXISTS orders;
```

## 뷰

### CREATE VIEW

쿼리를 기반으로 하는 명명된 뷰를 생성합니다. 기존 뷰를 덮어쓰려면 `OR REPLACE`를 사용합니다:

```sql
CREATE VIEW active_orders AS
  SELECT * FROM orders WHERE status = 'pending';

CREATE OR REPLACE VIEW active_orders AS
  SELECT * FROM orders WHERE status != 'delivered';
```

### DROP VIEW

```sql
DROP VIEW active_orders;
DROP VIEW IF EXISTS active_orders;
```

## 커스텀 타입

### CREATE TYPE

열거 타입을 정의합니다:

```sql
CREATE TYPE order_status AS ENUM ('pending', 'shipped', 'delivered');
```

### DROP TYPE

```sql
DROP TYPE order_status CASCADE;
```

## 시퀀스

시퀀스는 순차적 숫자 값을 생성하는 명명된 카운터입니다. 주로 기본 키 생성에 사용됩니다.

### CREATE SEQUENCE

```sql
CREATE SEQUENCE order_seq;
CREATE SEQUENCE order_seq START WITH 100 INCREMENT BY 10;
CREATE SEQUENCE IF NOT EXISTS order_seq;
```

옵션:

| 옵션 | 기본값 | 설명 |
|--------|---------|-------------|
| `INCREMENT BY n` | 1 | 스텝 크기 |
| `START WITH n` | 1 | 초기값 |
| `MINVALUE n` / `NO MINVALUE` | 1 | 최소값 |
| `MAXVALUE n` / `NO MAXVALUE` | 4611686018427387903 | 최대값 |
| `CYCLE` / `NO CYCLE` | `NO CYCLE` | 한계에서 순환 여부 |

### DROP SEQUENCE

```sql
DROP SEQUENCE order_seq;
DROP SEQUENCE IF EXISTS order_seq;
```

### SERIAL과 시퀀스

`SERIAL`, `SMALLSERIAL`, `BIGSERIAL` 컬럼은 `<table>_<column>_seq`라는 이름의 백업 시퀀스를 자동으로 생성합니다. 이는 PostgreSQL 동작과 일치합니다:

```sql
CREATE TABLE orders (id SERIAL PRIMARY KEY, name TEXT);
-- "orders_id_seq" 시퀀스를 암묵적으로 생성

SELECT nextval('orders_id_seq');   -- 작동함
SELECT currval('orders_id_seq');   -- INSERT 또는 nextval 후 작동함
```

테이블을 삭제하면 소유된 시퀀스도 연쇄적으로 삭제됩니다. `TRUNCATE`는 백업 시퀀스를 시작값으로 재설정합니다.

### 시퀀스 함수

| 함수 | 설명 |
|----------|-------------|
| `nextval('seq_name')` | 다음 값으로 진행하고 반환 |
| `currval('seq_name')` | 현재 값 반환 (세션에서 먼저 nextval 호출 필요) |
| `setval('seq_name', value)` | 현재 값 설정; 다음 nextval은 value + increment를 반환 |
| `setval('seq_name', value, false)` | 현재 값 설정; 다음 nextval은 value를 반환 |
| `lastval()` | 이 세션에서 어떤 시퀀스든 마지막 값 반환 |

## 저장 함수와 프로시저

전체 문서는 [PL/pgSQL](/reference/plpgsql/)을 참고하세요.

```sql
CREATE FUNCTION double(x INT) RETURNS INT AS $$
BEGIN RETURN x * 2; END $$ LANGUAGE plpgsql;

CREATE PROCEDURE reset_counts() AS $$
BEGIN UPDATE counters SET val = 0; END $$ LANGUAGE plpgsql;

DROP FUNCTION double;
DROP PROCEDURE IF EXISTS reset_counts;
```

## 인덱스

```sql
CREATE INDEX idx_orders_status ON orders (status);
CREATE UNIQUE INDEX idx_orders_email ON orders (email);
DROP INDEX idx_orders_status;
```

### 부분 인덱스

조건에 맞는 행만 인덱싱하여 인덱스를 더 작고 빠르게 만듭니다:

```sql
CREATE INDEX idx_active_orders ON orders (customer_id) WHERE status = 'active';
CREATE UNIQUE INDEX idx_unique_active_email ON users (email) WHERE active = true;
```

쿼리 플래너는 쿼리의 `WHERE` 절에 인덱스 조건이 포함된 경우에만 부분 인덱스를 사용합니다.

### 표현식 인덱스

원시 컬럼 대신 계산된 값에 대한 인덱스:

```sql
CREATE INDEX idx_lower_email ON users ((lower(email)));
CREATE UNIQUE INDEX idx_lower_name ON users ((lower(name)));
```

표현식은 괄호로 묶어야 합니다. 플래너는 `WHERE lower(email) = 'alice@test.com'`을 인덱스에 자동으로 매칭합니다.

부분 인덱스와 표현식 인덱스를 결합할 수 있습니다:

```sql
CREATE INDEX idx_active_lower ON users ((lower(name))) WHERE status = 'active';
```

## SHOW 명령

데이터베이스 메타데이터를 조사합니다:

```sql
SHOW TABLES;
SHOW VIEWS;
SHOW SEQUENCES;
SHOW COLUMNS orders;
SHOW PRIMARY KEY orders;
SHOW FOREIGN KEYS orders;
SHOW INDEXES orders;
SHOW INDEXES;              -- 모든 테이블의 모든 인덱스
```

### SHOW VIEWS 출력

| 컬럼 | 타입 | 설명 |
|--------|------|-------------|
| `view_name` | TEXT | 뷰 이름 |
| `definition` | TEXT | 뷰를 정의하는 SQL 쿼리 |

### SHOW COLUMNS 출력

| 컬럼 | 타입 | 설명 |
|--------|------|-------------|
| `name` | TEXT | 컬럼 이름 |
| `type` | TEXT | 데이터 타입 |
| `required` | BOOLEAN | NOT NULL 제약 조건 |
| `indexed` | BOOLEAN | 인덱스 존재 여부 |
| `unique` | BOOLEAN | 유니크 제약 조건 존재 여부 |
| `fk_table` | TEXT | 외래 키 대상 테이블 |
| `fk_column` | TEXT | 외래 키 대상 컬럼 |
| `fk_on_delete` | TEXT | ON DELETE 동작 |
| `fk_on_update` | TEXT | ON UPDATE 동작 |
| `default_value` | TEXT | 기본값 표현식 |

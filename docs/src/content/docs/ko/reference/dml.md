---
title: DML
description: 데이터 조작 언어 — INSERT, UPDATE, DELETE, COPY 문.
---

## INSERT

```sql
INSERT INTO orders (customer_name, amount)
VALUES ('Alice Smith', 149.99);
```

### RETURNING

삽입된 행의 값을 반환합니다:

```sql
INSERT INTO orders (customer_name, amount)
VALUES ('Bob Johnson', 75.50)
RETURNING id;

INSERT INTO orders (customer_name, amount)
VALUES ('Carol', 200.00)
RETURNING *;
```

### 쿼리에서 삽입

```sql
INSERT INTO archive (customer_name, amount)
SELECT customer_name, amount FROM orders WHERE status = 'delivered';
```

### INSERT ... ON CONFLICT (Upsert)

충돌하는 행 건너뛰기:

```sql
INSERT INTO users (email, name)
VALUES ('alice@example.com', 'Alice')
ON CONFLICT DO NOTHING;
```

특정 컬럼에 대한 충돌 시 업데이트:

```sql
INSERT INTO users (email, name)
VALUES ('alice@example.com', 'Alice Updated')
ON CONFLICT (email) DO UPDATE SET name = 'Alice Updated';
```

RETURNING과 결합 가능:

```sql
INSERT INTO users (email, name)
VALUES ('alice@example.com', 'Alice')
ON CONFLICT (email) DO UPDATE SET name = 'Alice'
RETURNING *;
```

## UPDATE

```sql
UPDATE orders SET status = 'shipped' WHERE amount > 100;
```

### VALUES에서 대량 업데이트

PostgreSQL 스타일 `UPDATE ... FROM`:

```sql
UPDATE orders
  SET status = d.status
  FROM (VALUES ('ord-1', 'shipped'), ('ord-2', 'delivered'))
       AS d (id, status)
  WHERE orders.id = d.id;
```

### RETURNING이 있는 UPDATE

```sql
UPDATE orders SET status = 'shipped'
WHERE id = 42
RETURNING id, status;
```

## DELETE

```sql
DELETE FROM orders WHERE status = 'delivered';
```

### DELETE ... USING

삭제할 행을 결정하기 위해 다른 테이블을 조인합니다:

```sql
DELETE FROM orders
USING customers
WHERE orders.customer_id = customers.id
  AND customers.status = 'inactive';
```

여러 USING 테이블:

```sql
DELETE FROM order_items
USING products, categories
WHERE order_items.product_id = products.id
  AND products.category_id = categories.id
  AND categories.name = 'discontinued';
```

### RETURNING이 있는 DELETE

```sql
DELETE FROM orders WHERE status = 'delivered'
RETURNING *;
```

## COPY

CSV 형식으로 데이터를 대량 가져오기 및 내보내기합니다.

### COPY FROM

CSV 파일에서 행을 가져옵니다:

```sql
COPY orders FROM 'data/orders.csv';
COPY orders FROM 'data/orders.csv' WITH (HEADER);
COPY orders FROM 'data/orders.csv' WITH (HEADER, DELIMITER '|');
COPY orders (customer_name, amount) FROM 'data/partial.csv' WITH (HEADER);
```

### COPY TO

테이블이나 쿼리를 CSV 파일로 내보냅니다:

```sql
COPY orders TO 'export/orders.csv';
COPY orders TO 'export/orders.csv' WITH (HEADER);
COPY (SELECT * FROM orders WHERE status = 'pending') TO 'export/pending.csv' WITH (HEADER);
```

### 옵션

| 옵션 | 설명 |
|--------|-------------|
| `HEADER` | 첫 번째 행이 헤더 (가져오기 시 건너뜀, 내보내기 시 작성) |
| `DELIMITER 'c'` | 필드 구분 문자 (기본값: `,`) |

## csv_file() - CSV 파일 직접 쿼리

가져오기 없이 CSV 파일을 가상 테이블로 쿼리합니다:

```sql
SELECT * FROM csv_file('data/sales.csv');
SELECT name, age::int FROM csv_file('data/people.csv') WHERE age::int > 25;
```

모든 값은 `TEXT`로 반환됩니다 — `::type`으로 캐스팅합니다. `WHERE`, `ORDER BY`, `LIMIT`, `JOIN`, 집계를 지원합니다.

옵션:

```sql
csv_file('path')                        -- 헤더 포함 (기본값)
csv_file('path', false)                 -- 헤더 없음 (컬럼 이름 column1, column2, ...)
csv_file('path', true, '|')            -- 커스텀 구분자
```

CSV 파일끼리 또는 데이터베이스 테이블과 조인:

```sql
SELECT e.name, d.department
FROM csv_file('employees.csv') e
JOIN csv_file('departments.csv') d ON e.dept_id = d.id;
```

## 가상 테이블

`CREATE VIRTUAL TABLE`을 사용하여 외부 데이터 소스를 쿼리 가능한 테이블로 등록합니다:

```sql
CREATE VIRTUAL TABLE sales USING csv('data/sales.csv');
CREATE VIRTUAL TABLE sales USING csv('data/sales.csv', 'no_header', '|');

SELECT * FROM sales WHERE amount::int > 100;
DROP TABLE sales;
```

가상 테이블은 `SHOW TABLES`에 표시되며 `SELECT`, `WHERE`, `JOIN`, `ORDER BY`, 집계를 지원합니다. 읽기 전용으로, `INSERT`, `UPDATE`, `DELETE`는 지원되지 않습니다.

내장 `csv` 모듈이 기본으로 등록됩니다. 커스텀 모듈은 Scala API를 통해 등록할 수 있습니다:

```scala
db.registerVirtualTableModule("mymodule", new VirtualTableModule { ... })
```

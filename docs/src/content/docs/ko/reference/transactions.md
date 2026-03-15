---
title: 트랜잭션
description: 트랜잭션과 준비된 구문.
---

## 트랜잭션

원자성을 보장하기 위해 여러 문을 트랜잭션으로 래핑합니다:

```sql
BEGIN;
INSERT INTO accounts (name, balance) VALUES ('Alice', 1000);
UPDATE accounts SET balance = balance - 100 WHERE name = 'Alice';
COMMIT;
```

오류 시 롤백:

```sql
BEGIN;
UPDATE accounts SET balance = balance - 9999 WHERE name = 'Alice';
ROLLBACK;
```

### 트랜잭션 내 DDL

DDL 문(CREATE TABLE, CREATE INDEX, DROP TABLE 등)은 트랜잭션 내에서 완전히 지원되며 ROLLBACK 시 DML과 함께 원자적으로 롤백됩니다. DDL과 DML은 단일 트랜잭션 내에서 자유롭게 혼합할 수 있습니다.

```sql
BEGIN;
CREATE TABLE orders (id SERIAL, product TEXT, qty INTEGER);
INSERT INTO orders (product, qty) VALUES ('Widget', 10);
CREATE INDEX idx_orders_product ON orders (product);
COMMIT;  -- 테이블, 데이터, 인덱스가 모두 원자적으로 커밋됨
```

트랜잭션이 롤백되면 테이블, 데이터, 인덱스가 모두 취소됩니다.

## 준비된 구문

`$1`, `$2`, ... 플레이스홀더로 매개변수화된 구문을 생성합니다:

```sql
PREPARE get_user AS SELECT * FROM users WHERE id = $1;
EXECUTE get_user(42);
DEALLOCATE get_user;
```

매개변수화된 삽입:

```sql
PREPARE add_user AS INSERT INTO users (name, email) VALUES ($1, $2);
EXECUTE add_user('Alice', 'alice@example.com');
```

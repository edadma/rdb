---
title: PL/pgSQL
description: 절차적 언어 — DO 블록, 저장 함수, 저장 프로시저.
---

PetraDB는 제어 흐름 로직, 루프, 조건부 SQL 실행을 작성하기 위한 PostgreSQL의 절차적 언어인 PL/pgSQL을 지원합니다.

## DO 블록

저장되지 않고 즉시 실행되는 익명 블록:

```sql
DO $$
DECLARE
  i INT;
  total INT := 0;
BEGIN
  FOR i IN 1..10 LOOP
    total := total + i;
  END LOOP;
  INSERT INTO results (sum) VALUES (total);
END $$;
```

## 저장 함수

함수는 값을 반환하며 모든 SQL 표현식에서 호출할 수 있습니다:

```sql
CREATE FUNCTION factorial(n INT) RETURNS INT AS $$
DECLARE
  result INT := 1;
  i INT := 1;
BEGIN
  WHILE i <= n LOOP
    result := result * i;
    i := i + 1;
  END LOOP;
  RETURN result;
END $$ LANGUAGE plpgsql;

SELECT factorial(5);  -- 120
```

기존 함수를 덮어쓰려면 `CREATE OR REPLACE FUNCTION`을 사용합니다.

### 쿼리에서 함수 사용

함수는 표현식이 허용되는 모든 곳에서 작동합니다:

```sql
SELECT name, classify(score) AS grade FROM students;
SELECT * FROM orders WHERE is_valid(status);
INSERT INTO logs VALUES (format_msg(code, detail));
```

## 저장 프로시저

프로시저는 동작을 수행하며 `CALL`로 호출합니다:

```sql
CREATE PROCEDURE seed_users(n INT) AS $$
DECLARE
  i INT;
BEGIN
  FOR i IN 1..n LOOP
    INSERT INTO users (name) VALUES ('User ' || i::TEXT);
  END LOOP;
END $$ LANGUAGE plpgsql;

CALL seed_users(100);
```

기존 프로시저를 덮어쓰려면 `CREATE OR REPLACE PROCEDURE`를 사용합니다.

## DROP

```sql
DROP FUNCTION factorial;
DROP FUNCTION IF EXISTS factorial;
DROP PROCEDURE seed_users;
DROP PROCEDURE IF EXISTS seed_users;
```

## 블록 구조

모든 PL/pgSQL 블록(DO 블록, 함수 본문, 프로시저 본문)은 동일한 구조를 공유합니다:

```
[DECLARE
  variable_name type [:= default_value];
  ...]
BEGIN
  statements...
[EXCEPTION
  WHEN condition THEN
    statements...]
END
```

## 변수

타입과 선택적 기본값으로 변수를 선언합니다:

```sql
DECLARE
  x INT := 0;
  name TEXT;
  total NUMERIC := 100.50;
```

기본값이 없는 변수는 `NULL`로 초기화됩니다. 대입은 `:=`를 사용합니다:

```sql
x := x + 1;
name := 'Alice';
total := (SELECT SUM(amount) FROM orders);
```

변수는 블록 내의 모든 SQL 문에서 사용할 수 있습니다 — `VALUES`, `WHERE`, `SET` 등에서.

## 제어 흐름

### IF / ELSIF / ELSE

```sql
IF amount > 1000 THEN
  INSERT INTO vip_orders VALUES (order_id);
ELSIF amount > 100 THEN
  INSERT INTO normal_orders VALUES (order_id);
ELSE
  INSERT INTO small_orders VALUES (order_id);
END IF;
```

### WHILE 루프

```sql
WHILE balance > 0 LOOP
  balance := balance - payment;
  month := month + 1;
END LOOP;
```

안전 제한으로 최대 10,000회 반복합니다.

### FOR 범위 루프

```sql
FOR i IN 1..10 LOOP
  INSERT INTO numbers VALUES (i);
END LOOP;
```

루프 변수는 `DECLARE`에서 선언되어야 합니다. 양쪽 경계가 모두 포함됩니다.

### FOR 쿼리 루프

쿼리 결과를 반복합니다:

```sql
FOR name IN SELECT name FROM users ORDER BY id LOOP
  INSERT INTO greetings VALUES ('Hello, ' || name);
END LOOP;
```

단일 컬럼 쿼리의 경우 변수가 스칼라 값을 받습니다. 다중 컬럼 쿼리의 경우 변수가 레코드를 받습니다.

## RETURN

블록을 종료하거나 함수에서 값을 반환합니다:

```sql
-- 함수에서:
RETURN x * 2;

-- DO 블록이나 프로시저에서 (조기 종료):
RETURN;
```

## RAISE

메시지를 출력하거나 오류를 발생시킵니다:

```sql
RAISE NOTICE 'Processing row %', row_id;
RAISE EXCEPTION 'Invalid input: %', value;
```

`%` 플레이스홀더는 인수 값으로 순서대로 치환됩니다. `RAISE EXCEPTION`은 실행을 중단합니다.

## PERFORM

쿼리를 실행하고 결과를 버립니다:

```sql
PERFORM SELECT notify_user(user_id);
```

## 예외 처리

블록 내에서 오류를 잡습니다:

```sql
DO $$
BEGIN
  CREATE TABLE t (id INT);
EXCEPTION
  WHEN duplicate_object THEN
    NULL;  -- 테이블이 이미 존재, 무시
END $$;
```

예외 조건:
- `duplicate_object` — 테이블/타입/제약 조건이 이미 존재
- `unique_violation` — 유니크 제약 조건 위반
- `others` — 모든 예외를 잡음

## NULL 문

아무것도 하지 않는 문으로, 주로 예외 핸들러에서 사용됩니다:

```sql
EXCEPTION
  WHEN others THEN NULL;
```

## 트리거

트리거는 행이 삽입, 수정, 삭제될 때 자동으로 함수를 실행합니다:

```sql
CREATE FUNCTION audit_changes() RETURNS INT AS $$
BEGIN
  INSERT INTO audit_log VALUES (tg_op || ' on ' || tg_table_name);
  RETURN 0;
END $$ LANGUAGE plpgsql;

CREATE TRIGGER trg_audit AFTER INSERT ON orders
  FOR EACH ROW EXECUTE FUNCTION audit_changes();

CREATE TRIGGER trg_audit_del AFTER DELETE ON orders
  FOR EACH ROW EXECUTE FUNCTION audit_changes();
```

### 구문

```sql
CREATE TRIGGER name BEFORE|AFTER INSERT|UPDATE|DELETE
  ON table FOR EACH ROW EXECUTE FUNCTION function_name();

DROP TRIGGER name ON table;
DROP TRIGGER IF EXISTS name ON table;
```

### 타이밍

- **BEFORE** 트리거는 작업 전에 실행됩니다. `NULL`을 반환하면 행 작업이 취소됩니다. non-NULL 값을 반환하면 진행됩니다.
- **AFTER** 트리거는 작업 후에 실행됩니다. 반환 값은 무시됩니다.

### 특수 변수

트리거 함수에서 접근할 수 있는 변수:

| 변수 | 설명 |
|----------|-------------|
| `tg_op` | 작업 이름: `'INSERT'`, `'UPDATE'`, 또는 `'DELETE'` |
| `tg_table_name` | 트리거를 발생시킨 테이블 이름 |
| `OLD` | 작업 전의 행 (UPDATE, DELETE) |
| `NEW` | 작업 후의 행 (INSERT, UPDATE) |

### 이벤트

트리거가 실행되는 경우:
- `INSERT` — `COPY FROM`으로 삽입된 행 포함
- `UPDATE` — 수정된 행마다 실행
- `DELETE` — 삭제된 행마다 실행

### 가드 트리거 예제

```sql
CREATE FUNCTION prevent_delete() RETURNS INT AS $$
BEGIN
  RETURN NULL;  -- DELETE 취소
END $$ LANGUAGE plpgsql;

CREATE TRIGGER trg_protect BEFORE DELETE ON important_data
  FOR EACH ROW EXECUTE FUNCTION prevent_delete();
```

### 네이티브 함수 콜백

등록된 네이티브 함수(Scala, JavaScript, 또는 C)는 트리거 함수에서 호출 가능하며, 외부 시스템과의 통합을 가능하게 합니다:

```sql
-- notify_webhook()이 네이티브 함수로 등록되었다고 가정
CREATE FUNCTION on_order() RETURNS INT AS $$
DECLARE dummy INT;
BEGIN
  dummy := notify_webhook(tg_op);
  RETURN 0;
END $$ LANGUAGE plpgsql;

CREATE TRIGGER trg AFTER INSERT ON orders
  FOR EACH ROW EXECUTE FUNCTION on_order();
```

네이티브 함수 등록에 대해서는 [Scala API](/reference/api-scala/), [JavaScript API](/reference/api-javascript/), [C API](/reference/api-c/)를 참고하세요.

## 영속성

저장 함수, 프로시저, 트리거는 인메모리(세션 수명)와 영구 스토리지(닫기/다시 열기 후 유지) 모두에서 데이터베이스 재시작 시 유지됩니다. 소스 SQL이 저장되어 데이터베이스 열기 시 재실행됩니다.

## 합성

함수는 다른 함수를 호출할 수 있습니다. 프로시저는 함수를 호출할 수 있습니다:

```sql
CREATE FUNCTION double(x INT) RETURNS INT AS $$
BEGIN RETURN x * 2; END $$ LANGUAGE plpgsql;

CREATE FUNCTION quadruple(x INT) RETURNS INT AS $$
BEGIN RETURN double(double(x)); END $$ LANGUAGE plpgsql;

SELECT quadruple(5);  -- 20
```

---
title: 쿼리
description: SELECT 문, 조인, 서브쿼리, 집합 연산 등.
---

## 기본 쿼리

```sql
SELECT * FROM orders
WHERE amount > 50
ORDER BY amount DESC
LIMIT 10 OFFSET 5;
```

## 집계

```sql
SELECT status, COUNT(*), AVG(amount), SUM(amount)
FROM orders
GROUP BY status
HAVING COUNT(*) > 5;
```

### 집계 FILTER

같은 쿼리의 다른 집계에 영향을 주지 않고 집계별 필터를 적용합니다:

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE status = 'active') AS active,
  SUM(amount) FILTER (WHERE amount > 100) AS high_value_total
FROM orders;
```

## 윈도우 함수

윈도우 함수는 `GROUP BY`처럼 행을 축소하지 않고, 현재 행에 관련된 행 집합에 대해 값을 계산합니다.

### 순위 함수

```sql
SELECT name, department, salary,
  ROW_NUMBER() OVER (ORDER BY salary DESC) AS row_num,
  RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dept_rank,
  DENSE_RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dense_rank
FROM employees;
```

### 오프셋 함수

```sql
SELECT name, salary,
  LAG(salary, 1) OVER (ORDER BY salary) AS prev_salary,
  LEAD(salary, 1, 0) OVER (ORDER BY salary) AS next_salary,
  NTILE(4) OVER (ORDER BY salary) AS quartile
FROM employees;
```

`LAG(expr [, offset [, default]])`과 `LEAD(expr [, offset [, default]])`는 선택적 오프셋(기본값 1)과 기본값(기본값 NULL)을 받습니다.

### 값 함수

```sql
SELECT name, department, salary,
  FIRST_VALUE(name) OVER (PARTITION BY department ORDER BY salary) AS lowest_paid,
  LAST_VALUE(name) OVER (PARTITION BY department ORDER BY salary
    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS highest_paid,
  NTH_VALUE(name, 2) OVER (PARTITION BY department ORDER BY salary
    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS second_lowest
FROM employees;
```

| 함수 | 설명 |
|----------|-------------|
| `FIRST_VALUE(expr)` | 윈도우 프레임의 첫 번째 행에서의 `expr` 값 |
| `LAST_VALUE(expr)` | 윈도우 프레임의 마지막 행에서의 `expr` 값 |
| `NTH_VALUE(expr, n)` | 프레임의 n번째 행에서의 `expr` 값 (1부터 시작), 해당 행이 없으면 NULL |

기본 프레임은 `ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW`입니다. `LAST_VALUE`와 `NTH_VALUE`의 경우, 전체 파티션을 보려면 일반적으로 `ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING`이 필요합니다.

### 집계 윈도우 함수

모든 집계 함수를 `OVER()`와 함께 사용할 수 있습니다:

```sql
SELECT name, department, salary,
  SUM(salary) OVER (PARTITION BY department) AS dept_total,
  AVG(salary) OVER () AS overall_avg,
  COUNT(*) OVER (PARTITION BY department) AS dept_size
FROM employees;
```

### 프레임 사양

파티션 내에서 집계 윈도우 함수에 기여하는 행을 제어합니다:

```sql
SELECT name, salary,
  SUM(salary) OVER (ORDER BY salary
    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_total,
  AVG(salary) OVER (ORDER BY salary
    ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) AS moving_avg
FROM employees;
```

지원되는 프레임 경계:
- `UNBOUNDED PRECEDING` / `UNBOUNDED FOLLOWING` — 파티션 시작/끝
- `CURRENT ROW` — 현재 행
- `N PRECEDING` / `N FOLLOWING` — 현재에서 N행 앞/뒤

프레임 절이 없으면 집계 윈도우 함수는 전체 파티션에 대해 계산합니다.

### 윈도우 함수와 FILTER

`FILTER` 절은 집계 윈도우 함수와 함께 작동합니다:

```sql
SELECT name, salary,
  SUM(salary) FILTER (WHERE salary > 70000)
    OVER (ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
    AS running_high_earner_total
FROM employees;
```

## 조인

INNER, LEFT, RIGHT, FULL, CROSS 조인:

```sql
SELECT o.id, o.amount, c.name
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id;
```

## 서브쿼리와 EXISTS

```sql
SELECT * FROM customers c
WHERE EXISTS (
  SELECT 1 FROM orders o
  WHERE o.customer_id = c.id AND o.amount > 100
);
```

## LATERAL 조인

FROM에서의 상관 서브쿼리:

```sql
SELECT c.name, recent.amount
FROM customers c,
LATERAL (
  SELECT amount FROM orders
  WHERE customer_id = c.id
  ORDER BY created_at DESC LIMIT 1
) AS recent;
```

## 소스로서의 VALUES

```sql
SELECT * FROM (VALUES (1, 'a'), (2, 'b')) AS t (id, name);
```

## 집합 연산

```sql
SELECT name FROM customers
UNION
SELECT name FROM suppliers;
```

`UNION`, `UNION ALL`, `INTERSECT`, `EXCEPT`가 지원됩니다.

## 공통 테이블 표현식 (WITH)

CTE는 메인 쿼리에서 참조할 수 있는 명명된 서브쿼리를 정의하여 가독성과 재사용성을 개선합니다:

```sql
WITH active_users AS (
  SELECT id, name FROM users WHERE active = TRUE
),
user_orders AS (
  SELECT u.name, COUNT(*) AS order_count
  FROM active_users u
  INNER JOIN orders o ON u.id = o.user_id
  GROUP BY u.name
)
SELECT name, order_count FROM user_orders ORDER BY order_count DESC;
```

컬럼 별칭을 지정할 수 있습니다: `WITH t(x, y) AS (SELECT 1, 2)`.

뒤의 CTE는 앞의 CTE를 참조할 수 있습니다. CTE 이름은 같은 이름의 테이블을 가립니다.

### 재귀 CTE

`WITH RECURSIVE`는 계층적 데이터, 그래프 순회, 시리즈 생성을 위한 반복 쿼리를 가능하게 합니다:

```sql
-- 숫자 시리즈 생성
WITH RECURSIVE nums(n) AS (
  SELECT 1
  UNION ALL
  SELECT n + 1 FROM nums WHERE n < 10
)
SELECT n FROM nums;

-- 트리 순회
WITH RECURSIVE tree(id, name, depth) AS (
  SELECT id, name, 0 FROM categories WHERE parent_id IS NULL
  UNION ALL
  SELECT c.id, c.name, t.depth + 1
  FROM categories c INNER JOIN tree t ON c.parent_id = t.id
)
SELECT name, depth FROM tree ORDER BY depth, name;
```

재귀 CTE 본문은 앵커 쿼리(비재귀 기본 케이스)와 재귀 쿼리(CTE 이름을 참조)의 `UNION ALL` 또는 `UNION`이어야 합니다. 재귀 쿼리가 새 행을 생성하지 않거나 1000번 반복 후 실행이 멈춥니다.

## CASE 표현식

```sql
SELECT name,
  CASE WHEN amount > 100 THEN 'high' ELSE 'low' END AS tier
FROM orders;
```

## 패턴 매칭

```sql
SELECT * FROM products WHERE name LIKE '%phone%';
SELECT * FROM products WHERE name ILIKE '%Phone%';   -- 대소문자 구분 없음
```

## BETWEEN, IN, ANY

```sql
SELECT * FROM orders WHERE amount BETWEEN 10 AND 100;
SELECT * FROM orders WHERE status IN ('pending', 'shipped');
SELECT * FROM orders WHERE status = ANY(ARRAY['pending', 'shipped']);
```

## OVERLAPS

두 날짜/시간 범위가 겹치는지 테스트합니다:

```sql
SELECT (DATE '2024-01-01', DATE '2024-01-31')
  OVERLAPS (DATE '2024-01-15', DATE '2024-02-15');
```

## DISTINCT

```sql
SELECT DISTINCT category FROM products;
```

### DISTINCT ON

주어진 표현식의 고유 값당 하나의 행을 반환합니다. 각 그룹의 첫 번째 행(`ORDER BY`에 따라)이 유지됩니다:

```sql
SELECT DISTINCT ON (department) department, name, salary
FROM employees
ORDER BY department, salary DESC;
```

부서별 최고 급여를 받는 직원을 반환합니다.

## EXPLAIN

쿼리 실행 계획을 보여줍니다:

```sql
EXPLAIN SELECT * FROM orders WHERE status = 'pending';
```

## ARRAY 생성자

```sql
SELECT ARRAY[1, 2, 3];
```

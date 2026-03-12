---
title: Queries
description: SELECT statements, joins, subqueries, set operations, and more.
---

## Basic Queries

```sql
SELECT * FROM orders
WHERE amount > 50
ORDER BY amount DESC
LIMIT 10 OFFSET 5;
```

## Aggregations

```sql
SELECT status, COUNT(*), AVG(amount), SUM(amount)
FROM orders
GROUP BY status
HAVING COUNT(*) > 5;
```

### Aggregate FILTER

Apply a per-aggregate filter without affecting other aggregates in the same query:

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE status = 'active') AS active,
  SUM(amount) FILTER (WHERE amount > 100) AS high_value_total
FROM orders;
```

## Window Functions

Window functions compute values across a set of rows related to the current row, without collapsing rows like `GROUP BY`.

### Ranking functions

```sql
SELECT name, department, salary,
  ROW_NUMBER() OVER (ORDER BY salary DESC) AS row_num,
  RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dept_rank,
  DENSE_RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dense_rank
FROM employees;
```

### Value functions

```sql
SELECT name, salary,
  LAG(salary, 1) OVER (ORDER BY salary) AS prev_salary,
  LEAD(salary, 1, 0) OVER (ORDER BY salary) AS next_salary,
  NTILE(4) OVER (ORDER BY salary) AS quartile
FROM employees;
```

`LAG(expr [, offset [, default]])` and `LEAD(expr [, offset [, default]])` accept an optional offset (default 1) and default value (default NULL).

### Aggregate window functions

Any aggregate function can be used with `OVER()`:

```sql
SELECT name, department, salary,
  SUM(salary) OVER (PARTITION BY department) AS dept_total,
  AVG(salary) OVER () AS overall_avg,
  COUNT(*) OVER (PARTITION BY department) AS dept_size
FROM employees;
```

### Frame specifications

Control which rows within the partition contribute to an aggregate window function:

```sql
SELECT name, salary,
  SUM(salary) OVER (ORDER BY salary
    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_total,
  AVG(salary) OVER (ORDER BY salary
    ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) AS moving_avg
FROM employees;
```

Supported frame bounds:
- `UNBOUNDED PRECEDING` / `UNBOUNDED FOLLOWING` — partition start/end
- `CURRENT ROW` — the current row
- `N PRECEDING` / `N FOLLOWING` — N rows before/after current

Without a frame clause, aggregate window functions compute over the entire partition.

### FILTER with window functions

The `FILTER` clause works with aggregate window functions:

```sql
SELECT name, salary,
  SUM(salary) FILTER (WHERE salary > 70000)
    OVER (ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
    AS running_high_earner_total
FROM employees;
```

## Joins

INNER, LEFT, RIGHT, FULL, and CROSS joins:

```sql
SELECT o.id, o.amount, c.name
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id;
```

## Subqueries and EXISTS

```sql
SELECT * FROM customers c
WHERE EXISTS (
  SELECT 1 FROM orders o
  WHERE o.customer_id = c.id AND o.amount > 100
);
```

## LATERAL Joins

Correlated subqueries in FROM:

```sql
SELECT c.name, recent.amount
FROM customers c,
LATERAL (
  SELECT amount FROM orders
  WHERE customer_id = c.id
  ORDER BY created_at DESC LIMIT 1
) AS recent;
```

## VALUES as a Source

```sql
SELECT * FROM (VALUES (1, 'a'), (2, 'b')) AS t (id, name);
```

## Set Operations

```sql
SELECT name FROM customers
UNION
SELECT name FROM suppliers;
```

`UNION`, `UNION ALL`, `INTERSECT`, and `EXCEPT` are supported.

## Common Table Expressions (WITH)

CTEs define named subqueries that can be referenced in the main query, improving readability and enabling reuse:

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

Column aliases can be specified: `WITH t(x, y) AS (SELECT 1, 2)`.

Later CTEs can reference earlier CTEs. A CTE name shadows any table with the same name.

### Recursive CTEs

`WITH RECURSIVE` enables iterative queries for hierarchical data, graph traversal, and series generation:

```sql
-- Generate a number series
WITH RECURSIVE nums(n) AS (
  SELECT 1
  UNION ALL
  SELECT n + 1 FROM nums WHERE n < 10
)
SELECT n FROM nums;

-- Tree traversal
WITH RECURSIVE tree(id, name, depth) AS (
  SELECT id, name, 0 FROM categories WHERE parent_id IS NULL
  UNION ALL
  SELECT c.id, c.name, t.depth + 1
  FROM categories c INNER JOIN tree t ON c.parent_id = t.id
)
SELECT name, depth FROM tree ORDER BY depth, name;
```

The recursive CTE body must be a `UNION ALL` or `UNION` of an anchor query (non-recursive base case) and a recursive query (references the CTE name). Execution stops when the recursive query produces no new rows, or after 1000 iterations.

## CASE Expressions

```sql
SELECT name,
  CASE WHEN amount > 100 THEN 'high' ELSE 'low' END AS tier
FROM orders;
```

## Pattern Matching

```sql
SELECT * FROM products WHERE name LIKE '%phone%';
SELECT * FROM products WHERE name ILIKE '%Phone%';   -- case-insensitive
```

## BETWEEN, IN, ANY

```sql
SELECT * FROM orders WHERE amount BETWEEN 10 AND 100;
SELECT * FROM orders WHERE status IN ('pending', 'shipped');
SELECT * FROM orders WHERE status = ANY(ARRAY['pending', 'shipped']);
```

## OVERLAPS

Test whether two date/time ranges overlap:

```sql
SELECT (DATE '2024-01-01', DATE '2024-01-31')
  OVERLAPS (DATE '2024-01-15', DATE '2024-02-15');
```

## DISTINCT

```sql
SELECT DISTINCT category FROM products;
```

## EXPLAIN

Show the query execution plan:

```sql
EXPLAIN SELECT * FROM orders WHERE status = 'pending';
```

## ARRAY Constructor

```sql
SELECT ARRAY[1, 2, 3];
```

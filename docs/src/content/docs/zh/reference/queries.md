---
title: 查询
description: SELECT 语句、连接、子查询、集合运算等。
---

## 基本查询

```sql
SELECT * FROM orders
WHERE amount > 50
ORDER BY amount DESC
LIMIT 10 OFFSET 5;
```

## 聚合

```sql
SELECT status, COUNT(*), AVG(amount), SUM(amount)
FROM orders
GROUP BY status
HAVING COUNT(*) > 5;
```

### 聚合 FILTER

对每个聚合应用过滤器，而不影响同一查询中的其他聚合：

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE status = 'active') AS active,
  SUM(amount) FILTER (WHERE amount > 100) AS high_value_total
FROM orders;
```

## 窗口函数

窗口函数基于与当前行相关的一组行计算值，而不像 `GROUP BY` 那样折叠行。

### 排名函数

```sql
SELECT name, department, salary,
  ROW_NUMBER() OVER (ORDER BY salary DESC) AS row_num,
  RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dept_rank,
  DENSE_RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dense_rank
FROM employees;
```

### 偏移函数

```sql
SELECT name, salary,
  LAG(salary, 1) OVER (ORDER BY salary) AS prev_salary,
  LEAD(salary, 1, 0) OVER (ORDER BY salary) AS next_salary,
  NTILE(4) OVER (ORDER BY salary) AS quartile
FROM employees;
```

`LAG(expr [, offset [, default]])` 和 `LEAD(expr [, offset [, default]])` 接受可选的偏移量（默认 1）和默认值（默认 NULL）。

### 值函数

```sql
SELECT name, department, salary,
  FIRST_VALUE(name) OVER (PARTITION BY department ORDER BY salary) AS lowest_paid,
  LAST_VALUE(name) OVER (PARTITION BY department ORDER BY salary
    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS highest_paid,
  NTH_VALUE(name, 2) OVER (PARTITION BY department ORDER BY salary
    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS second_lowest
FROM employees;
```

| 函数 | 描述 |
|----------|-------------|
| `FIRST_VALUE(expr)` | 窗口帧第一行的 `expr` 值 |
| `LAST_VALUE(expr)` | 窗口帧最后一行的 `expr` 值 |
| `NTH_VALUE(expr, n)` | 窗口帧第 n 行的 `expr` 值（从 1 开始），如果不存在则为 NULL |

默认帧是 `ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW`。对于 `LAST_VALUE` 和 `NTH_VALUE`，通常需要 `ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING` 以查看整个分区。

### 聚合窗口函数

任何聚合函数都可以配合 `OVER()` 使用：

```sql
SELECT name, department, salary,
  SUM(salary) OVER (PARTITION BY department) AS dept_total,
  AVG(salary) OVER () AS overall_avg,
  COUNT(*) OVER (PARTITION BY department) AS dept_size
FROM employees;
```

### 帧规范

控制分区内哪些行参与聚合窗口函数：

```sql
SELECT name, salary,
  SUM(salary) OVER (ORDER BY salary
    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_total,
  AVG(salary) OVER (ORDER BY salary
    ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) AS moving_avg
FROM employees;
```

支持的帧边界：
- `UNBOUNDED PRECEDING` / `UNBOUNDED FOLLOWING` — 分区的开始/结束
- `CURRENT ROW` — 当前行
- `N PRECEDING` / `N FOLLOWING` — 当前行之前/之后 N 行

不指定帧子句时，聚合窗口函数在整个分区上计算。

### FILTER 与窗口函数

`FILTER` 子句可以与聚合窗口函数配合使用：

```sql
SELECT name, salary,
  SUM(salary) FILTER (WHERE salary > 70000)
    OVER (ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
    AS running_high_earner_total
FROM employees;
```

## 连接

INNER、LEFT、RIGHT、FULL 和 CROSS 连接：

```sql
SELECT o.id, o.amount, c.name
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id;
```

## 子查询和 EXISTS

```sql
SELECT * FROM customers c
WHERE EXISTS (
  SELECT 1 FROM orders o
  WHERE o.customer_id = c.id AND o.amount > 100
);
```

## LATERAL 连接

FROM 中的关联子查询：

```sql
SELECT c.name, recent.amount
FROM customers c,
LATERAL (
  SELECT amount FROM orders
  WHERE customer_id = c.id
  ORDER BY created_at DESC LIMIT 1
) AS recent;
```

## VALUES 作为数据源

```sql
SELECT * FROM (VALUES (1, 'a'), (2, 'b')) AS t (id, name);
```

## 集合运算

```sql
SELECT name FROM customers
UNION
SELECT name FROM suppliers;
```

支持 `UNION`、`UNION ALL`、`INTERSECT` 和 `EXCEPT`。

## 公用表表达式（WITH）

CTE 定义可在主查询中引用的命名子查询，提高可读性并实现复用：

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

可以指定列别名：`WITH t(x, y) AS (SELECT 1, 2)`。

后续 CTE 可以引用前面的 CTE。CTE 名称会遮蔽同名的表。

### 递归 CTE

`WITH RECURSIVE` 实现用于层次结构数据、图遍历和系列生成的迭代查询：

```sql
-- 生成数字系列
WITH RECURSIVE nums(n) AS (
  SELECT 1
  UNION ALL
  SELECT n + 1 FROM nums WHERE n < 10
)
SELECT n FROM nums;

-- 树遍历
WITH RECURSIVE tree(id, name, depth) AS (
  SELECT id, name, 0 FROM categories WHERE parent_id IS NULL
  UNION ALL
  SELECT c.id, c.name, t.depth + 1
  FROM categories c INNER JOIN tree t ON c.parent_id = t.id
)
SELECT name, depth FROM tree ORDER BY depth, name;
```

递归 CTE 主体必须是锚查询（非递归基本情况）和递归查询（引用 CTE 名称）的 `UNION ALL` 或 `UNION`。当递归查询不再产生新行或达到 1000 次迭代后停止执行。

## CASE 表达式

```sql
SELECT name,
  CASE WHEN amount > 100 THEN 'high' ELSE 'low' END AS tier
FROM orders;
```

## 模式匹配

```sql
SELECT * FROM products WHERE name LIKE '%phone%';
SELECT * FROM products WHERE name ILIKE '%Phone%';   -- 大小写不敏感
```

## BETWEEN、IN、ANY

```sql
SELECT * FROM orders WHERE amount BETWEEN 10 AND 100;
SELECT * FROM orders WHERE status IN ('pending', 'shipped');
SELECT * FROM orders WHERE status = ANY(ARRAY['pending', 'shipped']);
```

## OVERLAPS

测试两个日期/时间范围是否重叠：

```sql
SELECT (DATE '2024-01-01', DATE '2024-01-31')
  OVERLAPS (DATE '2024-01-15', DATE '2024-02-15');
```

## DISTINCT

```sql
SELECT DISTINCT category FROM products;
```

### DISTINCT ON

返回给定表达式的每个不同值一行。保留每组的第一行（根据 `ORDER BY`）：

```sql
SELECT DISTINCT ON (department) department, name, salary
FROM employees
ORDER BY department, salary DESC;
```

这将返回每个部门薪资最高的员工。

## EXPLAIN

显示查询执行计划：

```sql
EXPLAIN SELECT * FROM orders WHERE status = 'pending';
```

## ARRAY 构造函数

```sql
SELECT ARRAY[1, 2, 3];
```

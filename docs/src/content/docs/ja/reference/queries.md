---
title: クエリ
description: SELECT文、ジョイン、サブクエリ、集合演算などです。
---

## 基本的なクエリ

```sql
SELECT * FROM orders
WHERE amount > 50
ORDER BY amount DESC
LIMIT 10 OFFSET 5;
```

## 集計

```sql
SELECT status, COUNT(*), AVG(amount), SUM(amount)
FROM orders
GROUP BY status
HAVING COUNT(*) > 5;
```

### 集約FILTER

同じクエリ内の他の集計に影響を与えずに、集計ごとのフィルターを適用します。

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE status = 'active') AS active,
  SUM(amount) FILTER (WHERE amount > 100) AS high_value_total
FROM orders;
```

## ウィンドウ関数

ウィンドウ関数は、`GROUP BY`のように行を集約せずに、現在の行に関連する行のセットに対して値を計算します。

### ランキング関数

```sql
SELECT name, department, salary,
  ROW_NUMBER() OVER (ORDER BY salary DESC) AS row_num,
  RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dept_rank,
  DENSE_RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dense_rank
FROM employees;
```

### オフセット関数

```sql
SELECT name, salary,
  LAG(salary, 1) OVER (ORDER BY salary) AS prev_salary,
  LEAD(salary, 1, 0) OVER (ORDER BY salary) AS next_salary,
  NTILE(4) OVER (ORDER BY salary) AS quartile
FROM employees;
```

`LAG(expr [, offset [, default]])`と`LEAD(expr [, offset [, default]])`はオプションのオフセット（デフォルト1）とデフォルト値（デフォルトNULL）を受け取ります。

### 値関数

```sql
SELECT name, department, salary,
  FIRST_VALUE(name) OVER (PARTITION BY department ORDER BY salary) AS lowest_paid,
  LAST_VALUE(name) OVER (PARTITION BY department ORDER BY salary
    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS highest_paid,
  NTH_VALUE(name, 2) OVER (PARTITION BY department ORDER BY salary
    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS second_lowest
FROM employees;
```

| 関数 | 説明 |
|----------|-------------|
| `FIRST_VALUE(expr)` | ウィンドウフレームの最初の行の`expr`の値 |
| `LAST_VALUE(expr)` | ウィンドウフレームの最後の行の`expr`の値 |
| `NTH_VALUE(expr, n)` | フレームのn番目の行（1ベース）の`expr`の値、該当行がなければNULL |

デフォルトのフレームは`ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW`です。`LAST_VALUE`と`NTH_VALUE`では、パーティション全体を見るために通常`ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING`を指定します。

### 集約ウィンドウ関数

任意の集約関数を`OVER()`で使用できます。

```sql
SELECT name, department, salary,
  SUM(salary) OVER (PARTITION BY department) AS dept_total,
  AVG(salary) OVER () AS overall_avg,
  COUNT(*) OVER (PARTITION BY department) AS dept_size
FROM employees;
```

### フレーム指定

パーティション内のどの行が集約ウィンドウ関数に寄与するかを制御します。

```sql
SELECT name, salary,
  SUM(salary) OVER (ORDER BY salary
    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_total,
  AVG(salary) OVER (ORDER BY salary
    ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) AS moving_avg
FROM employees;
```

サポートされるフレーム境界：
- `UNBOUNDED PRECEDING` / `UNBOUNDED FOLLOWING` — パーティションの開始/終了
- `CURRENT ROW` — 現在の行
- `N PRECEDING` / `N FOLLOWING` — 現在の行からN行前/後

フレーム句がない場合、集約ウィンドウ関数はパーティション全体で計算されます。

### ウィンドウ関数でのFILTER

`FILTER`句は集約ウィンドウ関数で動作します。

```sql
SELECT name, salary,
  SUM(salary) FILTER (WHERE salary > 70000)
    OVER (ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
    AS running_high_earner_total
FROM employees;
```

## ジョイン

INNER、LEFT、RIGHT、FULL、CROSSジョインをサポートします。

```sql
SELECT o.id, o.amount, c.name
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id;
```

## サブクエリとEXISTS

```sql
SELECT * FROM customers c
WHERE EXISTS (
  SELECT 1 FROM orders o
  WHERE o.customer_id = c.id AND o.amount > 100
);
```

## LATERALジョイン

FROM内の相関サブクエリです。

```sql
SELECT c.name, recent.amount
FROM customers c,
LATERAL (
  SELECT amount FROM orders
  WHERE customer_id = c.id
  ORDER BY created_at DESC LIMIT 1
) AS recent;
```

## ソースとしてのVALUES

```sql
SELECT * FROM (VALUES (1, 'a'), (2, 'b')) AS t (id, name);
```

## 集合演算

```sql
SELECT name FROM customers
UNION
SELECT name FROM suppliers;
```

`UNION`、`UNION ALL`、`INTERSECT`、`EXCEPT`がサポートされています。

## 共通テーブル式（WITH）

CTEはメインクエリで参照できる名前付きサブクエリを定義し、可読性と再利用を向上させます。

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

カラムエイリアスを指定できます：`WITH t(x, y) AS (SELECT 1, 2)`。

後のCTEは先のCTEを参照できます。CTE名は同名のテーブルをシャドウします。

### 再帰CTE

`WITH RECURSIVE`は階層データ、グラフ走査、系列生成のための反復クエリを可能にします。

```sql
-- 数列の生成
WITH RECURSIVE nums(n) AS (
  SELECT 1
  UNION ALL
  SELECT n + 1 FROM nums WHERE n < 10
)
SELECT n FROM nums;

-- ツリー走査
WITH RECURSIVE tree(id, name, depth) AS (
  SELECT id, name, 0 FROM categories WHERE parent_id IS NULL
  UNION ALL
  SELECT c.id, c.name, t.depth + 1
  FROM categories c INNER JOIN tree t ON c.parent_id = t.id
)
SELECT name, depth FROM tree ORDER BY depth, name;
```

再帰CTEの本体は、アンカークエリ（非再帰的なベースケース）と再帰クエリ（CTE名を参照）の`UNION ALL`または`UNION`である必要があります。再帰クエリが新しい行を生成しなくなるか、1000回の反復後に実行が停止します。

## CASE式

```sql
SELECT name,
  CASE WHEN amount > 100 THEN 'high' ELSE 'low' END AS tier
FROM orders;
```

## パターンマッチング

```sql
SELECT * FROM products WHERE name LIKE '%phone%';
SELECT * FROM products WHERE name ILIKE '%Phone%';   -- 大文字小文字を区別しない
```

## BETWEEN、IN、ANY

```sql
SELECT * FROM orders WHERE amount BETWEEN 10 AND 100;
SELECT * FROM orders WHERE status IN ('pending', 'shipped');
SELECT * FROM orders WHERE status = ANY(ARRAY['pending', 'shipped']);
```

## OVERLAPS

2つの日付/時刻範囲が重なるかテストします。

```sql
SELECT (DATE '2024-01-01', DATE '2024-01-31')
  OVERLAPS (DATE '2024-01-15', DATE '2024-02-15');
```

## DISTINCT

```sql
SELECT DISTINCT category FROM products;
```

### DISTINCT ON

指定された式の各ユニーク値ごとに1行を返します。各グループの最初の行（`ORDER BY`に従って）が保持されます。

```sql
SELECT DISTINCT ON (department) department, name, salary
FROM employees
ORDER BY department, salary DESC;
```

これは各部署で最も給与の高い従業員を返します。

## EXPLAIN

クエリ実行プランを表示します。

```sql
EXPLAIN SELECT * FROM orders WHERE status = 'pending';
```

## ARRAYコンストラクタ

```sql
SELECT ARRAY[1, 2, 3];
```

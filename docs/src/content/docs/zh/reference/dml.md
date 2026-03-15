---
title: DML
description: 数据操作语言 — INSERT、UPDATE、DELETE 和 COPY 语句。
---

## INSERT

```sql
INSERT INTO orders (customer_name, amount)
VALUES ('Alice Smith', 149.99);
```

### RETURNING

返回插入行的值：

```sql
INSERT INTO orders (customer_name, amount)
VALUES ('Bob Johnson', 75.50)
RETURNING id;

INSERT INTO orders (customer_name, amount)
VALUES ('Carol', 200.00)
RETURNING *;
```

### 从查询插入

```sql
INSERT INTO archive (customer_name, amount)
SELECT customer_name, amount FROM orders WHERE status = 'delivered';
```

### INSERT ... ON CONFLICT（Upsert）

跳过冲突行：

```sql
INSERT INTO users (email, name)
VALUES ('alice@example.com', 'Alice')
ON CONFLICT DO NOTHING;
```

冲突时更新指定列：

```sql
INSERT INTO users (email, name)
VALUES ('alice@example.com', 'Alice Updated')
ON CONFLICT (email) DO UPDATE SET name = 'Alice Updated';
```

可以与 RETURNING 结合使用：

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

### 从 VALUES 批量更新

PostgreSQL 风格的 `UPDATE ... FROM`：

```sql
UPDATE orders
  SET status = d.status
  FROM (VALUES ('ord-1', 'shipped'), ('ord-2', 'delivered'))
       AS d (id, status)
  WHERE orders.id = d.id;
```

### 带 RETURNING 的 UPDATE

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

连接其他表来确定要删除的行：

```sql
DELETE FROM orders
USING customers
WHERE orders.customer_id = customers.id
  AND customers.status = 'inactive';
```

多个 USING 表：

```sql
DELETE FROM order_items
USING products, categories
WHERE order_items.product_id = products.id
  AND products.category_id = categories.id
  AND categories.name = 'discontinued';
```

### 带 RETURNING 的 DELETE

```sql
DELETE FROM orders WHERE status = 'delivered'
RETURNING *;
```

## COPY

以 CSV 格式批量导入和导出数据。

### COPY FROM

从 CSV 文件导入行：

```sql
COPY orders FROM 'data/orders.csv';
COPY orders FROM 'data/orders.csv' WITH (HEADER);
COPY orders FROM 'data/orders.csv' WITH (HEADER, DELIMITER '|');
COPY orders (customer_name, amount) FROM 'data/partial.csv' WITH (HEADER);
```

### COPY TO

将表或查询导出为 CSV 文件：

```sql
COPY orders TO 'export/orders.csv';
COPY orders TO 'export/orders.csv' WITH (HEADER);
COPY (SELECT * FROM orders WHERE status = 'pending') TO 'export/pending.csv' WITH (HEADER);
```

### 选项

| 选项 | 描述 |
|--------|-------------|
| `HEADER` | 第一行为表头（导入时跳过，导出时写入） |
| `DELIMITER 'c'` | 字段分隔符（默认：`,`） |

## csv_file() — 直接查询 CSV 文件

将 CSV 文件作为虚拟表查询，无需导入：

```sql
SELECT * FROM csv_file('data/sales.csv');
SELECT name, age::int FROM csv_file('data/people.csv') WHERE age::int > 25;
```

所有值作为 `TEXT` 返回 — 使用 `::type` 进行转换。支持 `WHERE`、`ORDER BY`、`LIMIT`、`JOIN` 和聚合。

选项：

```sql
csv_file('path')                        -- 带表头（默认）
csv_file('path', false)                 -- 无表头（列命名为 column1、column2...）
csv_file('path', true, '|')            -- 自定义分隔符
```

将 CSV 文件相互连接或与数据库表连接：

```sql
SELECT e.name, d.department
FROM csv_file('employees.csv') e
JOIN csv_file('departments.csv') d ON e.dept_id = d.id;
```

## 虚拟表

使用 `CREATE VIRTUAL TABLE` 将外部数据源注册为可查询的表：

```sql
CREATE VIRTUAL TABLE sales USING csv('data/sales.csv');
CREATE VIRTUAL TABLE sales USING csv('data/sales.csv', 'no_header', '|');

SELECT * FROM sales WHERE amount::int > 100;
DROP TABLE sales;
```

虚拟表出现在 `SHOW TABLES` 中，支持 `SELECT`、`WHERE`、`JOIN`、`ORDER BY` 和聚合。它们是只读的 — 不支持 `INSERT`、`UPDATE` 和 `DELETE`。

内置的 `csv` 模块默认已注册。自定义模块可通过 Scala API 注册：

```scala
db.registerVirtualTableModule("mymodule", new VirtualTableModule { ... })
```

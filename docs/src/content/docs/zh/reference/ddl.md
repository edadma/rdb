---
title: DDL
description: 数据定义语言 — CREATE、ALTER、DROP 和 TRUNCATE 语句。
---

## Schema

PetraDB 支持 PostgreSQL 风格的 schema 命名空间。每个数据库默认有一个 `public` schema。未限定的表名解析到 `public`。

### CREATE SCHEMA

```sql
CREATE SCHEMA inventory;
CREATE SCHEMA IF NOT EXISTS inventory;
```

### Schema 限定表

在任何 DDL 或 DML 语句中使用 `schema.table` 语法：

```sql
CREATE TABLE inventory.products (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  price NUMERIC(10,2)
);

INSERT INTO inventory.products (name, price) VALUES ('Widget', 9.99);
SELECT * FROM inventory.products;
```

不同 schema 中可以存在同名的表：

```sql
CREATE SCHEMA staging;
CREATE TABLE staging.products (id SERIAL, name TEXT);
CREATE TABLE public.products (id SERIAL, name TEXT);
-- 这是两个不同的表
```

### information_schema

PetraDB 提供 `information_schema` 虚拟表用于自省数据库结构：

```sql
SELECT * FROM information_schema.schemata;
SELECT * FROM information_schema.tables;
SELECT * FROM information_schema.columns WHERE table_name = 'products';
```

可用视图：`schemata`、`tables`、`columns`。

## 表

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

### 外键

```sql
CREATE TABLE line_items (
  id SERIAL,
  order_id UUID REFERENCES orders (id) ON DELETE CASCADE ON UPDATE CASCADE,
  product TEXT NOT NULL
);
```

### 生成列

从其他列自动派生的计算列：

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

生成列在 INSERT 和 UPDATE 时重新计算，不能直接写入。

### ALTER TABLE

添加、删除或重命名列：

```sql
ALTER TABLE orders ADD COLUMN notes TEXT;
ALTER TABLE orders DROP COLUMN notes;
ALTER TABLE orders RENAME COLUMN amount TO total;
ALTER TABLE orders RENAME TO purchases;
```

修改列属性：

```sql
ALTER TABLE orders ALTER COLUMN notes SET NOT NULL;
ALTER TABLE orders ALTER COLUMN notes DROP NOT NULL;
ALTER TABLE orders ALTER COLUMN notes SET DEFAULT 'none';
ALTER TABLE orders ALTER COLUMN notes DROP DEFAULT;
ALTER TABLE orders ALTER COLUMN amount SET DATA TYPE NUMERIC(10,2);
```

添加和删除约束：

```sql
ALTER TABLE orders ADD CONSTRAINT chk_amount CHECK (amount > 0);
ALTER TABLE orders ADD CONSTRAINT uq_email UNIQUE (email);
ALTER TABLE orders ADD CONSTRAINT fk_customer
  FOREIGN KEY (customer_id) REFERENCES customers (id) ON DELETE CASCADE;
ALTER TABLE orders DROP CONSTRAINT chk_amount;
```

### CHECK 约束

对列值强制条件：

```sql
CREATE TABLE products (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  price NUMERIC CHECK (price >= 0),
  quantity INT,
  CONSTRAINT positive_qty CHECK (quantity >= 0)
);
```

CHECK 约束在 INSERT 和 UPDATE 时执行。

## 触发器

完整文档请参阅 [PL/pgSQL — 触发器](/reference/plpgsql/#触发器)。

```sql
CREATE TRIGGER trg_audit AFTER INSERT ON orders
  FOR EACH ROW EXECUTE FUNCTION audit_log();

DROP TRIGGER trg_audit ON orders;
DROP TRIGGER IF EXISTS trg_audit ON orders;
```

### TRUNCATE TABLE

删除所有行并重置 serial 序列：

```sql
TRUNCATE TABLE orders;
```

### DROP TABLE

```sql
DROP TABLE orders;
DROP TABLE IF EXISTS orders;
```

## 视图

### CREATE VIEW

创建由查询支持的命名视图。使用 `OR REPLACE` 覆盖已有视图：

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

## 自定义类型

### CREATE TYPE

定义枚举类型：

```sql
CREATE TYPE order_status AS ENUM ('pending', 'shipped', 'delivered');
```

### DROP TYPE

```sql
DROP TYPE order_status CASCADE;
```

## 序列

序列是生成连续数值的命名计数器。通常用于主键生成。

### CREATE SEQUENCE

```sql
CREATE SEQUENCE order_seq;
CREATE SEQUENCE order_seq START WITH 100 INCREMENT BY 10;
CREATE SEQUENCE IF NOT EXISTS order_seq;
```

选项：

| 选项 | 默认值 | 描述 |
|--------|---------|-------------|
| `INCREMENT BY n` | 1 | 步长 |
| `START WITH n` | 1 | 初始值 |
| `MINVALUE n` / `NO MINVALUE` | 1 | 最小值 |
| `MAXVALUE n` / `NO MAXVALUE` | 4611686018427387903 | 最大值 |
| `CYCLE` / `NO CYCLE` | `NO CYCLE` | 是否在到达限制时循环 |

### DROP SEQUENCE

```sql
DROP SEQUENCE order_seq;
DROP SEQUENCE IF EXISTS order_seq;
```

### SERIAL 和序列

`SERIAL`、`SMALLSERIAL` 和 `BIGSERIAL` 列自动创建名为 `<table>_<column>_seq` 的后备序列。这与 PostgreSQL 行为一致：

```sql
CREATE TABLE orders (id SERIAL PRIMARY KEY, name TEXT);
-- 隐式创建序列 "orders_id_seq"

SELECT nextval('orders_id_seq');   -- 可用
SELECT currval('orders_id_seq');   -- INSERT 或 nextval 后可用
```

删除表会级联删除其拥有的序列。`TRUNCATE` 将后备序列重置为起始值。

### 序列函数

| 函数 | 描述 |
|----------|-------------|
| `nextval('seq_name')` | 推进并返回下一个值 |
| `currval('seq_name')` | 返回当前值（会话中必须先调用 nextval） |
| `setval('seq_name', value)` | 设置当前值；下一个 nextval 返回 value + increment |
| `setval('seq_name', value, false)` | 设置当前值；下一个 nextval 返回 value |
| `lastval()` | 返回本会话中任何序列的最后一个值 |

## 存储函数和过程

完整文档请参阅 [PL/pgSQL](/reference/plpgsql/)。

```sql
CREATE FUNCTION double(x INT) RETURNS INT AS $$
BEGIN RETURN x * 2; END $$ LANGUAGE plpgsql;

CREATE PROCEDURE reset_counts() AS $$
BEGIN UPDATE counters SET val = 0; END $$ LANGUAGE plpgsql;

DROP FUNCTION double;
DROP PROCEDURE IF EXISTS reset_counts;
```

## 索引

```sql
CREATE INDEX idx_orders_status ON orders (status);
CREATE UNIQUE INDEX idx_orders_email ON orders (email);
DROP INDEX idx_orders_status;
```

### 部分索引

仅索引满足条件的行，使索引更小更快：

```sql
CREATE INDEX idx_active_orders ON orders (customer_id) WHERE status = 'active';
CREATE UNIQUE INDEX idx_unique_active_email ON users (email) WHERE active = true;
```

查询规划器仅在查询的 `WHERE` 子句包含索引条件时才使用部分索引。

### 表达式索引

对计算值而非原始列建索引：

```sql
CREATE INDEX idx_lower_email ON users ((lower(email)));
CREATE UNIQUE INDEX idx_lower_name ON users ((lower(name)));
```

表达式必须用括号括起来。规划器会自动将 `WHERE lower(email) = 'alice@test.com'` 匹配到索引。

部分索引和表达式索引可以组合使用：

```sql
CREATE INDEX idx_active_lower ON users ((lower(name))) WHERE status = 'active';
```

## SHOW 命令

自省数据库元数据：

```sql
SHOW TABLES;
SHOW VIEWS;
SHOW SEQUENCES;
SHOW COLUMNS orders;
SHOW PRIMARY KEY orders;
SHOW FOREIGN KEYS orders;
SHOW INDEXES orders;
SHOW INDEXES;              -- 所有表的所有索引
```

### SHOW VIEWS 输出

| 列 | 类型 | 描述 |
|--------|------|-------------|
| `view_name` | TEXT | 视图名称 |
| `definition` | TEXT | 定义视图的 SQL 查询 |

### SHOW COLUMNS 输出

| 列 | 类型 | 描述 |
|--------|------|-------------|
| `name` | TEXT | 列名 |
| `type` | TEXT | 数据类型 |
| `required` | BOOLEAN | NOT NULL 约束 |
| `indexed` | BOOLEAN | 是否有索引 |
| `unique` | BOOLEAN | 是否有唯一约束 |
| `fk_table` | TEXT | 外键目标表 |
| `fk_column` | TEXT | 外键目标列 |
| `fk_on_delete` | TEXT | ON DELETE 动作 |
| `fk_on_update` | TEXT | ON UPDATE 动作 |
| `default_value` | TEXT | 默认表达式 |

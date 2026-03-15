---
title: 事务
description: 事务和预处理语句。
---

## 事务

将多个语句包装在事务中以确保原子性：

```sql
BEGIN;
INSERT INTO accounts (name, balance) VALUES ('Alice', 1000);
UPDATE accounts SET balance = balance - 100 WHERE name = 'Alice';
COMMIT;
```

出错时回滚：

```sql
BEGIN;
UPDATE accounts SET balance = balance - 9999 WHERE name = 'Alice';
ROLLBACK;
```

### 事务中的 DDL

DDL 语句（CREATE TABLE、CREATE INDEX、DROP TABLE 等）完全支持在事务内执行，在 ROLLBACK 时与任何 DML 一起原子回滚。DDL 和 DML 可以在同一事务中自由交错使用。

```sql
BEGIN;
CREATE TABLE orders (id SERIAL, product TEXT, qty INTEGER);
INSERT INTO orders (product, qty) VALUES ('Widget', 10);
CREATE INDEX idx_orders_product ON orders (product);
COMMIT;  -- 表、数据和索引全部原子提交
```

如果事务回滚，表、数据和索引都会被撤销。

## 预处理语句

使用 `$1`、`$2`... 占位符创建参数化语句：

```sql
PREPARE get_user AS SELECT * FROM users WHERE id = $1;
EXECUTE get_user(42);
DEALLOCATE get_user;
```

参数化插入：

```sql
PREPARE add_user AS INSERT INTO users (name, email) VALUES ($1, $2);
EXECUTE add_user('Alice', 'alice@example.com');
```

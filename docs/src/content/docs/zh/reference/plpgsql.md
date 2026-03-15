---
title: PL/pgSQL
description: 过程语言 — DO 块、存储函数和存储过程。
---

PetraDB 支持 PL/pgSQL，PostgreSQL 的过程语言，用于编写控制流逻辑、循环和条件 SQL 执行。

## DO 块

立即执行而不被存储的匿名块：

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

## 存储函数

函数返回一个值，可以在任何 SQL 表达式中调用：

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

使用 `CREATE OR REPLACE FUNCTION` 覆盖已有函数。

### 在查询中使用函数

函数可以在表达式允许的任何地方使用：

```sql
SELECT name, classify(score) AS grade FROM students;
SELECT * FROM orders WHERE is_valid(status);
INSERT INTO logs VALUES (format_msg(code, detail));
```

## 存储过程

过程执行操作，通过 `CALL` 调用：

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

使用 `CREATE OR REPLACE PROCEDURE` 覆盖已有过程。

## DROP

```sql
DROP FUNCTION factorial;
DROP FUNCTION IF EXISTS factorial;
DROP PROCEDURE seed_users;
DROP PROCEDURE IF EXISTS seed_users;
```

## 块结构

所有 PL/pgSQL 块（DO 块、函数体、过程体）共享相同的结构：

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

## 变量

使用类型和可选默认值声明变量：

```sql
DECLARE
  x INT := 0;
  name TEXT;
  total NUMERIC := 100.50;
```

没有默认值的变量初始化为 `NULL`。赋值使用 `:=`：

```sql
x := x + 1;
name := 'Alice';
total := (SELECT SUM(amount) FROM orders);
```

变量可以在块内的任何 SQL 语句中使用 — 在 `VALUES`、`WHERE`、`SET` 等中。

## 控制流

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

### WHILE 循环

```sql
WHILE balance > 0 LOOP
  balance := balance - payment;
  month := month + 1;
END LOOP;
```

安全限制最多 10,000 次迭代。

### FOR 范围循环

```sql
FOR i IN 1..10 LOOP
  INSERT INTO numbers VALUES (i);
END LOOP;
```

循环变量必须在 `DECLARE` 中声明。两个边界都包含在内。

### FOR 查询循环

遍历查询结果：

```sql
FOR name IN SELECT name FROM users ORDER BY id LOOP
  INSERT INTO greetings VALUES ('Hello, ' || name);
END LOOP;
```

对于单列查询，变量接收标量值。对于多列查询，变量接收记录。

## RETURN

退出块或从函数返回值：

```sql
-- 在函数中：
RETURN x * 2;

-- 在 DO 块或过程中（提前退出）：
RETURN;
```

## RAISE

打印消息或抛出错误：

```sql
RAISE NOTICE 'Processing row %', row_id;
RAISE EXCEPTION 'Invalid input: %', value;
```

`%` 占位符按顺序被参数值替换。`RAISE EXCEPTION` 中止执行。

## PERFORM

执行查询并丢弃结果：

```sql
PERFORM SELECT notify_user(user_id);
```

## 异常处理

在块内捕获错误：

```sql
DO $$
BEGIN
  CREATE TABLE t (id INT);
EXCEPTION
  WHEN duplicate_object THEN
    NULL;  -- 表已存在，忽略
END $$;
```

异常条件：
- `duplicate_object` — 表/类型/约束已存在
- `unique_violation` — 唯一约束违反
- `others` — 捕获任何异常

## NULL 语句

空操作语句，常用于异常处理器中：

```sql
EXCEPTION
  WHEN others THEN NULL;
```

## 触发器

触发器在行被插入、更新或删除时自动执行函数：

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

### 语法

```sql
CREATE TRIGGER name BEFORE|AFTER INSERT|UPDATE|DELETE
  ON table FOR EACH ROW EXECUTE FUNCTION function_name();

DROP TRIGGER name ON table;
DROP TRIGGER IF EXISTS name ON table;
```

### 时机

- **BEFORE** 触发器在操作之前运行。返回 `NULL` 取消行操作。返回任何非 NULL 值继续。
- **AFTER** 触发器在操作之后运行。返回值被忽略。

### 特殊变量

触发器函数可以访问：

| 变量 | 描述 |
|----------|-------------|
| `tg_op` | 操作名称：`'INSERT'`、`'UPDATE'` 或 `'DELETE'` |
| `tg_table_name` | 触发触发器的表名 |
| `OLD` | 操作前的行（UPDATE、DELETE） |
| `NEW` | 操作后的行（INSERT、UPDATE） |

### 事件

触发器在以下操作时触发：
- `INSERT` — 包括通过 `COPY FROM` 插入的行
- `UPDATE` — 每个更新的行触发
- `DELETE` — 每个删除的行触发

### 保护触发器示例

```sql
CREATE FUNCTION prevent_delete() RETURNS INT AS $$
BEGIN
  RETURN NULL;  -- 取消 DELETE
END $$ LANGUAGE plpgsql;

CREATE TRIGGER trg_protect BEFORE DELETE ON important_data
  FOR EACH ROW EXECUTE FUNCTION prevent_delete();
```

### 原生函数回调

注册的原生函数（Scala、JavaScript 或 C）可以从触发器函数中调用，实现与外部系统的集成：

```sql
-- 假设 notify_webhook() 已注册为原生函数
CREATE FUNCTION on_order() RETURNS INT AS $$
DECLARE dummy INT;
BEGIN
  dummy := notify_webhook(tg_op);
  RETURN 0;
END $$ LANGUAGE plpgsql;

CREATE TRIGGER trg AFTER INSERT ON orders
  FOR EACH ROW EXECUTE FUNCTION on_order();
```

注册原生函数请参阅 [Scala API](/reference/api-scala/)、[JavaScript API](/reference/api-javascript/) 和 [C API](/reference/api-c/)。

## 持久化

存储函数、过程和触发器在数据库重启后保持不变，适用于内存存储（会话生命周期）和持久化存储（关闭/重新打开后保留）。源 SQL 被存储并在数据库打开时重新执行。

## 组合

函数可以调用其他函数。过程可以调用函数：

```sql
CREATE FUNCTION double(x INT) RETURNS INT AS $$
BEGIN RETURN x * 2; END $$ LANGUAGE plpgsql;

CREATE FUNCTION quadruple(x INT) RETURNS INT AS $$
BEGIN RETURN double(double(x)); END $$ LANGUAGE plpgsql;

SELECT quadruple(5);  -- 20
```

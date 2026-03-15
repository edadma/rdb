---
title: 数据类型
description: PetraDB 支持的 SQL 数据类型。
---

PetraDB 遵循 PostgreSQL 的 SQL 语法、标识符处理和类型转换约定。

## SQL 兼容性

- **大小写不敏感的关键字** — `SELECT`、`select` 和 `Select` 等价
- **未加引号的标识符折叠** — 未加引号的标识符折叠为小写（`CREATE TABLE Users` → 表名 `users`）
- **双引号标识符** — 保留大小写（`"MixedCase"` 保持原样）
- **字符串转义** — 双单引号（`'it''s'`）和 E-字符串（`E'it\'s'`）
- **运算符** — `!=` 和 `<>` 都表示不等于

## 支持的类型

| 类型 | 描述 |
|------|-------------|
| `SMALLINT` | 16 位整数（-32768 到 32767） |
| `INT` / `INTEGER` | 32 位整数 |
| `BIGINT` | 64 位整数 |
| `SMALLSERIAL` | 自增 16 位整数（创建后备序列） |
| `SERIAL` | 自增 32 位整数（创建后备序列） |
| `BIGSERIAL` | 自增 64 位整数（创建后备序列） |
| `DOUBLE` / `FLOAT` / `REAL` | 双精度浮点数 |
| `NUMERIC(p,s)` / `DECIMAL(p,s)` | 定精度小数 |
| `TEXT` | 变长字符串 |
| `CHAR(n)` | 定长字符串（右补空格） |
| `VARCHAR(n)` | 变长字符串（最大 n 个字符，不补充） |
| `BOOLEAN` | 真/假 |
| `DATE` | 日历日期（`yyyy-MM-dd`） |
| `TIME` | 时间（`HH:mm:ss`） |
| `TIMESTAMP` | 日期和时间 |
| `TIMESTAMP WITH TIME ZONE` | 带时区偏移的日期和时间 |
| `INTERVAL` | 持续时间（ISO 8601 或 `N days N hours N minutes N seconds`） |
| `UUID` | 通用唯一标识符 |
| `JSON` / `JSONB` | 结构化 JSON 对象和数组 |
| `BYTEA` | 二进制数据 |
| `ENUM` | 自定义枚举类型（通过 `CREATE TYPE ... AS ENUM`） |
| `INT[]`、`TEXT[]` 等 | 类型化数组（任何基本类型加 `[]` 后缀） |

## 类型转换

使用 `::` 运算符或 `CAST(expr AS type)` 在类型之间转换：

```sql
SELECT '2024-06-15'::DATE;
SELECT CAST('14:30:00' AS TIME);
SELECT '2 hours 30 minutes'::INTERVAL;
SELECT CAST(val AS TEXT);
SELECT '42'::INT;
SELECT 1::BOOLEAN;
SELECT EXTRACT(year FROM created_at);
```

## 日期/时间算术

```sql
SELECT '2024-01-01'::DATE + 10;                        -- 添加天数
SELECT '2024-01-15'::DATE - '2024-01-10'::DATE;        -- 两日期之间的天数
SELECT now() + '2 hours'::INTERVAL;                     -- 时间戳 + 间隔
SELECT now() - '30 minutes'::INTERVAL;                  -- 时间戳 - 间隔
SELECT '1 hour'::INTERVAL * 3;                          -- 缩放间隔
SELECT EXTRACT(year FROM now());                        -- 提取字段
SELECT date_trunc('month', now());                      -- 截断
```

## 约束

```sql
PRIMARY KEY (id)
UNIQUE (email)
NOT NULL
DEFAULT value
FOREIGN KEY (col) REFERENCES other_table (col) ON DELETE CASCADE ON UPDATE CASCADE
```

---
title: CLI
description: PetraDB 的交互式 SQL shell 和命令行工具。
---

## 安装

```bash
npm install -g @petradb/cli
```

## 使用方法

```bash
petradb [OPTIONS] [path]
```

如果提供了路径，CLI 会在该位置打开（或创建）持久化数据库。不提供路径时使用内存数据库。

### 选项

| 选项 | 描述 |
|--------|-------------|
| `-m`、`--memory` | 使用内存数据库（即使提供了路径） |
| `-e`、`--execute` | 执行 SQL 并退出（可重复） |
| `-f`、`--file` | 执行 SQL 文件并退出（可重复） |
| `--stdin` | 从标准输入读取 SQL 并退出 |
| `--host` | 连接到远程 PetraDB 服务器 |
| `--port` | 服务器端口（默认：5480） |
| `--user` | 认证用户名 |
| `--password` | 认证密码 |

### 示例

```bash
# 启动交互式内存会话
petradb

# 打开或创建持久化数据库
petradb mydata.db

# 非交互式运行 SQL
petradb mydata.db -e "SELECT * FROM users"

# 执行文件后执行查询
petradb mydata.db -f schema.sql -e "SELECT count(*) FROM users"

# 从标准输入传入 SQL
echo "SELECT 1 + 1" | petradb --stdin

# 连接到远程服务器
petradb --host myserver.example.com --port 5480

# 带认证连接
petradb --host localhost --user admin --password secret
```

## 数据库模式

文件扩展名决定存储模式：

| 扩展名 | 模式 | 描述 |
|-----------|------|-------------|
| `.petra`（或其他） | 持久化 | 通过写时复制页面的防崩溃持久化存储 |
| `.ptxt` | 文本 | 人类可读文件，每次更改后重写 |
| *（无路径）* | 内存 | 数据仅在会话期间存在 |

## 导出

`dump` 子命令将持久化数据库的完整模式和数据打印为 SQL 语句。

```bash
petradb dump mydata.db
```

输出包括 `CREATE TYPE`、`CREATE TABLE`、`INSERT INTO` 和 `CREATE VIEW` 语句，可从头重建数据库。

## 交互式 REPL

不使用 `-e`、`-f` 或 `--stdin` 启动时，CLI 进入带有 readline 历史记录的交互式 SQL shell。

输入以 `;` 结尾的 SQL 来执行。支持多行输入 — 在以 `;` 结束之前会显示续行提示（`  -> `）。

### 元命令

| 命令 | 描述 |
|---------|-------------|
| `\dt` | 列出所有表 |
| `\dv` | 列出所有视图 |
| `\ds` | 列出所有序列 |
| `\di` | 列出所有索引 |
| `\d <name>` | 描述表或视图（列、类型、可空性） |
| `\i <file>` | 从文件执行 SQL |
| `\dump` | 打印完整的模式和数据为 SQL |
| `\copy <args>` | 运行 `COPY` 语句 |
| `\timing` | 开关查询计时 |
| `\q` | 退出 |

### 会话示例

```
$ petradb
PetraDB — interactive SQL shell
Type \q to quit, \dt to list tables, \d <table> to describe a table.

petra> CREATE TABLE cities (name TEXT, population INT);
CREATE TABLE
petra> INSERT INTO cities (name, population) VALUES ('Oslo', 709037);
INSERT 0 1
petra> SELECT * FROM cities;
 name | population
------+------------
 Oslo |     709037
(1 row)
petra> \dt
 Table
--------
 cities
(1 row)
petra> \d cities
Table "cities"
 Column     | Type | Nullable
------------+------+----------
 name       | TEXT | null
 population | INT  | null
petra> \timing
Timing is on.
petra> SELECT count(*) FROM cities;
 count
-------
     1
(1 row)
Time: 0.002 s
petra> \q
```

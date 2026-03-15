---
title: 更新日志
---

## v1.5-20260315

### PL/pgSQL — 存储过程、函数和触发器

完整的过程语言支持，包括 DO 块、存储函数和存储过程：

- **DO 块** — 带有 DECLARE、BEGIN...END 的匿名 PL/pgSQL 块
- **存储函数** — `CREATE FUNCTION name(params) RETURNS type AS $$ ... $$ LANGUAGE plpgsql`，可在任何 SQL 表达式中调用
- **存储过程** — `CREATE PROCEDURE name(params) AS $$ ... $$ LANGUAGE plpgsql`，通过 `CALL` 调用
- **触发器** — `CREATE TRIGGER name BEFORE|AFTER INSERT|UPDATE|DELETE ON table FOR EACH ROW EXECUTE FUNCTION func()`
  - BEFORE 触发器可以通过返回 NULL 取消操作
  - AFTER 触发器在操作完成后触发
  - 触发器可在 INSERT、UPDATE、DELETE 和 COPY FROM 时触发
  - 可使用 TG_OP、TG_TABLE_NAME、OLD、NEW 变量
- **控制流** — IF/ELSIF/ELSE、WHILE LOOP、FOR range/query LOOP、RETURN、RAISE NOTICE/EXCEPTION、PERFORM、EXCEPTION WHEN
- **持久化** — 函数、过程和触发器在 PersistentDB 和 TextDB 上关闭/重新打开后仍然保留
- **OR REPLACE** — 覆盖已有的函数和过程

### 用户自定义原生函数

将宿主语言的回调注册为 SQL 函数，可在查询、触发器和过程中调用：

- **Scala** — `db.registerScalarFunction("name", { args => result })`
- **JavaScript** — `session.registerFunction("name", (args) => result)`
- **C** — `petradb_create_function(db, "name", nargs, user_data, callback)`，配合 SQLite 风格的类型化 value/context API

### C 库和游标 API

- **原生共享库** — 通过 Scala Native 提供 `libpetradb-engine.so`，使用 `@exported` 导出 C 可调用函数
- **SQLite 风格 C API** — `petradb_open`、`petradb_exec`、`petradb_prepare/step/finalize`，类型化列访问器
- **用户自定义函数** — `petradb_value_int/double/text`、`petradb_result_int/double/text/null/error`、`petradb_user_data`
- **游标 API** — `session.openCursor(sql)`，支持 `step()` 逐行惰性迭代、类型化列访问器、`fetch(n)`、`move(n)`、参数化查询
- **C 头文件** — `petradb.h`，包含完整 API 文档
- **C 测试套件** — 67 个测试
- **Rust FFI 测试** — 38 个测试，验证跨语言互操作性

### 虚拟表

- **可扩展框架** — `CREATE VIRTUAL TABLE name USING module(args)`，只读，显示在 SHOW TABLES 中
- **内置 CSV 模块** — `CREATE VIRTUAL TABLE t USING csv('file.csv')`，支持表头/分隔符选项
- **自定义模块** — 通过 Scala API `db.registerVirtualTableModule("name", module)` 注册

### csv_file() 表函数

无需导入即可直接查询 CSV 文件：

```sql
SELECT * FROM csv_file('data.csv');
SELECT e.name, d.dept FROM csv_file('employees.csv') e
  JOIN csv_file('departments.csv') d ON e.dept_id = d.id;
```

### 高级索引

- **部分索引** — `CREATE INDEX ... WHERE condition` — 仅索引满足谓词条件的行
- **表达式索引** — `CREATE INDEX ... ON table ((expr))` — 索引计算值，如 `lower(email)`
- **组合使用** — 部分索引和表达式索引可以一起使用

### 窗口函数

- **FIRST_VALUE(expr)** — 窗口帧第一行的值
- **LAST_VALUE(expr)** — 窗口帧最后一行的值
- **NTH_VALUE(expr, n)** — 窗口帧第 n 行的值

### DELETE ... USING

多表删除，匹配 PostgreSQL 语法：

```sql
DELETE FROM orders USING customers
WHERE orders.customer_id = customers.id AND customers.status = 'inactive';
```

### Quarry — 类型安全的 AST 查询构建器

新增 `@petradb/quarry` 包：生成 AST 对象（而非 SQL 字符串）的类型安全查询构建器：

- 支持 21 种列类型的模式定义
- 完整 CRUD：select、insert、update、delete，带类型安全的列引用
- 连接：inner、left、right、full outer、cross，带类型化结果
- 表达式：50+ 运算符、聚合、CASE/CAST/EXISTS、子查询
- Upsert：`onConflictDoNothing()`、`onConflictDoUpdate()`
- 自连接的表别名
- 事务、RETURNING、DISTINCT ON
- 所有功能的编译时类型测试
- 集合运算：UNION、INTERSECT、EXCEPT
- 窗口函数、CTE、命名标量辅助函数

### 错误修复

- **ByteaValue** — `ARRAY[...]` 插入 BYTEA 列现在正确产生 `ByteaValue` 而不是 `ArrayValue`
- **JS/Client 结果类型** — 添加了缺失的 PL/pgSQL 结果类型处理器（DoBlockResult、CreateFunctionResult 等），防止非穷举匹配崩溃
- **编解码器** — 为所有新结果类型添加了客户端/服务器通信的序列化支持
- **llms.txt** — 将 `type` 字段修复为 `command` 字段，更新了所有结果类型和功能

### 模块重命名

- **shared → common** — 将共享类型模块从 `petradb-shared` 重命名为 `petradb-common`

### 文档

- 新增 **PL/pgSQL** 参考页面（触发器、函数、过程、控制流）
- 新增 **C API** 参考页面（完整的 SQLite 风格接口）
- 新增 Java (JDBC) 和 C 的**入门指南**
- 更新 DDL 文档：部分/表达式索引、CHECK 约束、触发器、存储例程
- 更新 DML 文档：DELETE...USING、csv_file()、虚拟表
- 更新 JS/Scala API 文档：registerFunction、结果类型
- 着陆页：四个入门按钮（JS、Java、Scala、C）
- 重写 llms.txt，包含所有当前功能

### 版本更新

| 组件 | Maven Central | npm |
|-----------|---------------|-----|
| common | 1.5.0 | — |
| engine | 1.5.0 | @petradb/engine 1.5.0 |
| client | 1.5.0 | @petradb/client 1.5.0 |
| server | 1.5.0 | @petradb/server 1.5.0 |
| cli | 1.5.0 | @petradb/cli 1.5.0 |
| jdbc | 1.5.0 | — |
| drizzle | — | @petradb/drizzle 1.5.0 |
| knex | — | @petradb/knex 1.5.0 |
| lucid | — | @petradb/lucid 1.5.0 |
| quarry | — | @petradb/quarry 1.5.0 |

## v1.4-20260314

### 错误修复与改进

- **关联 IN 子查询修复** — 带有索引表的关联 `IN (SELECT ...)` 现在可以正确工作
- **限定列解析** — 修复了复杂连接中的歧义列引用
- **空 IN/ANY 处理** — 解决了 `IN ()` 和 `= ANY('{}')` 边界情况
- **外键 CASCADE 清理** — `DROP TABLE ... CASCADE` 现在正确移除子表上的外键约束
- **DROP TABLE IF EXISTS ... CASCADE** — `IF EXISTS` 与 `CASCADE` 组合不再导致解析错误
- **数组字面量转换** — 支持 `'{1,2,3}'::integer[]` PostgreSQL 数组字面量语法
- **参数化查询修复** — 改进了子查询和关联路径的参数绑定

### 版本更新

| 组件 | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.4.2 | — |
| engine | 1.4.9 | @petradb/engine 1.4.3 |
| client | 1.4.2 | @petradb/client 1.4.2 |
| server | — | @petradb/server 1.4.3 |
| cli | — | @petradb/cli 1.4.3 |
| jdbc | 1.4.3 | — |
| drizzle | — | @petradb/drizzle 1.4.3 |

## v1.4-20260312

### 公用表表达式（CTE）

完整的 CTE 支持，包括 `WITH` 和 `WITH RECURSIVE`。

**非递归 CTE** — 提高可读性和复用性的命名子查询：

```sql
WITH active_orders AS (
  SELECT * FROM orders WHERE status = 'active'
)
SELECT customer_id, SUM(amount)
FROM active_orders
GROUP BY customer_id;
```

可以在单个查询中定义多个 CTE，后续 CTE 可以引用前面的 CTE。支持列别名：`WITH t(x, y) AS (...)`。如果 CTE 名称与表名相同，CTE 会遮蔽表名。

**递归 CTE** — 用于层次结构和图数据的迭代查询：

```sql
WITH RECURSIVE descendants(id, name, depth) AS (
  SELECT id, name, 0 FROM employees WHERE manager_id IS NULL
  UNION ALL
  SELECT e.id, e.name, d.depth + 1
  FROM employees e INNER JOIN descendants d ON e.manager_id = d.id
)
SELECT name, depth FROM descendants ORDER BY depth, name;
```

同时支持 `UNION ALL`（保留重复）和 `UNION`（去重）。安全限制最多 1000 次迭代。

### 窗口函数

完整的窗口函数支持，分为三类：

**排名函数** — `ROW_NUMBER()`、`RANK()`、`DENSE_RANK()`，配合 `PARTITION BY` 和 `ORDER BY`：

```sql
SELECT name, department, salary,
  RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dept_rank
FROM employees;
```

**值函数** — `LAG()`、`LEAD()`、`NTILE()`，支持可配置的偏移量和默认值：

```sql
SELECT name, salary,
  LAG(salary, 1, 0) OVER (ORDER BY salary) AS prev_salary,
  NTILE(4) OVER (ORDER BY salary) AS quartile
FROM employees;
```

**聚合窗口函数** — 任何聚合函数（`SUM`、`COUNT`、`AVG`、`MIN`、`MAX` 等）配合 `OVER()`，包括帧规范：

```sql
SELECT name, salary,
  SUM(salary) OVER (ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_total,
  AVG(salary) OVER (PARTITION BY department) AS dept_avg
FROM employees;
```

帧边界：`UNBOUNDED PRECEDING`、`UNBOUNDED FOLLOWING`、`CURRENT ROW`、`N PRECEDING`、`N FOLLOWING`。不指定帧子句时，聚合窗口函数在整个分区上计算。

### 聚合 FILTER 子句

在聚合函数上使用 `FILTER (WHERE ...)`，适用于分组查询和窗口函数：

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE status = 'active') AS active_count,
  SUM(amount) FILTER (WHERE amount > 100) OVER (ORDER BY created_at
    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_high_value
FROM orders;
```

### 哈希连接优化

无索引的等值连接现在使用哈希连接策略，而不是笛卡尔积，将连接复杂度从 O(n×m) 降低到 O(n+m)。适用于 INNER、LEFT、RIGHT 和 FULL 连接。当索引可用时，仍优先使用索引嵌套循环连接。非等值连接条件仍回退到笛卡尔积。在 `EXPLAIN` 输出中显示为 `Hash Join`、`Hash Left Join`、`Hash Right Join`、`Hash Full Join`。

### 生成列

`GENERATED ALWAYS AS (expr) STORED` 计算列：

```sql
CREATE TABLE products (
  price NUMERIC,
  tax_rate NUMERIC DEFAULT 0.08,
  total NUMERIC GENERATED ALWAYS AS (price * (1 + tax_rate)) STORED
);
```

生成列在 INSERT 和 UPDATE 时重新计算，不能直接设置。

### ORDER BY 空值排序

`ORDER BY` 现在默认使用 SQL 标准的空值排序：`ASC` → `NULLS LAST`，`DESC` → `NULLS FIRST`。支持显式的 `NULLS FIRST` / `NULLS LAST` 覆盖。

### 序列支持

完全兼容 PostgreSQL 的序列支持。`CREATE SEQUENCE` 和 `DROP SEQUENCE` 支持选项（`INCREMENT BY`、`START WITH`、`MINVALUE`、`MAXVALUE`、`CYCLE`、`IF NOT EXISTS` / `IF EXISTS`）。序列函数：`nextval()`、`currval()`、`setval()`、`lastval()`。

`SERIAL`、`SMALLSERIAL` 和 `BIGSERIAL` 列现在创建后备序列（名为 `<table>_<column>_seq`），匹配 PostgreSQL 行为。`DROP TABLE` 级联删除所拥有的序列。`TRUNCATE` 重置后备序列。序列状态完全支持事务 — `ROLLBACK` 恢复序列计数器。持久数据库将序列状态序列化到目录中。

新增 SQL 命令：`SHOW SEQUENCES`、`SHOW INDEXES`（所有表的所有索引）。

CLI：新增 `\ds`（列出序列）和 `\di`（列出索引）元命令。

### CREATE INDEX USING 子句

`CREATE INDEX ... USING btree` 语法现已支持（btree 是唯一支持的方法）。这提高了与 PostgreSQL 生成的 DDL 和 ORM 的兼容性。

### 错误修复

- `ORDER BY` 与 NULL 值：比较器在两个值都为 NULL 时现在返回 0，修复了多排序键时的非确定性排序结果
- `ORDER BY` 别名解析：SELECT 别名（例如 `SELECT x AS y ... ORDER BY y`）现在在非分组查询中正确解析，无论是否有窗口函数
- `Type.convert()` 空值处理：通过类型转换传递的 NULL 值（例如通过 UPDATE SET 中的预处理语句参数）现在保留为 NULL，而不是被转换为类型的文本表示。修复了 13 种类型：TEXT、VARCHAR、CHAR、UUID、TIMESTAMP、DATE、TIME、TIMETZ、INTERVAL、TIMESTAMPTZ、BYTEA、JSON、ENUM
- ORDER BY 解析器中 nulls 子句的穷举匹配警告
- playground 终端中同步抛出的静默错误

### 版本更新

所有组件升级到 1.4.1：

| 组件 | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.4.1 | — |
| engine | 1.4.1 | @petradb/engine 1.4.1 |
| client | 1.4.1 | @petradb/client 1.4.1 |
| server | 1.4.1 | @petradb/server 1.4.1 |
| cli | 1.4.1 | @petradb/cli 1.4.1 |
| jdbc | 1.4.1 | — |
| knex | — | @petradb/knex 1.4.0 |
| lucid | — | @petradb/lucid 1.4.0 |
| drizzle | — | @petradb/drizzle 1.4.1 |

## v1.3-20260309

### 事务性 DDL

DDL 语句（CREATE TABLE、CREATE INDEX、DROP TABLE 等）现在完全支持在事务内执行，并与 DML 一起原子回滚。DDL 和 DML 可以在同一个 BEGIN/COMMIT 块中自由交错使用。MemoryDB 和 PersistentDB 都会在 BEGIN 时捕获完整的目录快照，并在 ROLLBACK 时恢复。

### Drizzle 关系查询

完全支持 Drizzle ORM 关系查询（`db.query.*.findMany()`、`db.query.*.findFirst()`）。添加了 `json_build_array` 和 `json_build_object` 标量函数，修复了 LATERAL 子查询中的参数替换。

### 版本更新

| 组件 | Maven Central | npm |
|-----------|---------------|-----|
| engine | 1.3.1 | @petradb/engine 1.3.3 |
| server | 1.3.1 | @petradb/server 1.3.1 |
| cli | 1.3.1 | @petradb/cli 1.3.1 |
| jdbc | 1.3.1 | — |
| drizzle | — | @petradb/drizzle 1.3.1 |

## v1.3-20260308

### Schema 支持

PostgreSQL 风格的 schema 命名空间。每个数据库默认有一个 `public` schema；未限定的表名解析到 `public`。Schema 限定名称（`schema.table`）适用于所有 DDL 和 DML 语句 — CREATE TABLE、INSERT、UPDATE、DELETE、SELECT、ALTER TABLE、DROP TABLE、TRUNCATE、CREATE INDEX 和 COPY。

```sql
CREATE SCHEMA inventory;
CREATE TABLE inventory.products (id SERIAL PRIMARY KEY, name TEXT);
INSERT INTO inventory.products (name) VALUES ('Widget');
SELECT * FROM inventory.products;
```

### information_schema 虚拟表

`information_schema.schemata`、`information_schema.tables` 和 `information_schema.columns` 现在可以查询。Schema 限定的表报告正确的 `table_schema`。这些视图从数据库元数据动态生成。

### Drizzle ORM 迁移

`@petradb/drizzle` 中新增 `migrate()` 函数，用于应用 Drizzle Kit 迁移文件。读取 `meta/_journal.json` 并按顺序执行 SQL 迁移文件，在 `drizzle.__drizzle_migrations` 中跟踪已应用的迁移。

```typescript
import { migrate } from "@petradb/drizzle";
await migrate(db, { migrationsFolder: "./drizzle" });
```

### JDBC 元数据改进

`DatabaseMetaData.getColumns()` 现在根据列类型和精度/标度声明返回准确的 `COLUMN_SIZE`、`DECIMAL_DIGITS` 和 `CHAR_OCTET_LENGTH` 值。

### 版本更新

所有组件升级到 1.3.0：

| 组件 | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.3.0 | — |
| engine | 1.3.0 | @petradb/engine 1.3.0 |
| client | 1.3.0 | @petradb/client 1.3.0 |
| server | 1.3.0 | @petradb/server 1.3.0 |
| cli | 1.3.0 | @petradb/cli 1.3.0 |
| jdbc | 1.3.0 | — |
| knex | — | @petradb/knex 1.3.0 |
| lucid | — | @petradb/lucid 1.3.0 |
| drizzle | — | @petradb/drizzle 1.3.0 |

## v1.2-20260308

### Drizzle ORM 驱动重写

`@petradb/drizzle` 从 `drizzle-orm/pg-proxy` 包装器重写为自定义 PostgreSQL 方言驱动，直接扩展 `PgSession`/`PgPreparedQuery`/`PgTransaction`。这使其与 `drizzle-orm/node-postgres` 完全功能对等：

- `db.transaction()`，支持自动提交/回滚
- `tx.rollback()` 显式回滚
- `returning()` 支持所有变更操作（包括部分列选择）
- 关系查询支持（待引擎支持 `json_build_array`/`json_agg`）

### 类型强制：文本参数到 NUMERIC 列

`NumericType.convert` 现在接受 `TextValue` 并将其解析为 `BigDecimal`，与 `IntegerType`、`BigintType`、`SmallintType` 和 `DoubleType` 的现有强制行为一致。这修复了通过 ORM 进行的参数化 INSERT/UPDATE，ORM 会将数值作为文本发送（标准 PostgreSQL 线协议行为）。

### 三值 NULL 逻辑

完整的 SQL 三值 NULL 逻辑处理：

- 比较运算符（`=`、`!=`、`<`、`>`、`<=`、`>=`）在任一操作数为 NULL 时返回 NULL
- `AND`/`OR` 实现正确的三值真值表（例如 `FALSE AND NULL` → `FALSE`，`TRUE OR NULL` → `TRUE`）
- `IN`/`NOT IN` 正确传播 NULL（例如 `3 NOT IN (1, 2, NULL)` → unknown）
- 算术运算（`+`、`-`、`*`、`/`、`%`）和字符串连接（`||`）传播 NULL
- `LIKE` 处理 NULL 操作数

### 统一表达式语法

SQL 解析器中分离的 `expression` 和 `booleanExpression` 层次结构已合并为单一表达式语法。布尔运算符（`AND`、`OR`、`NOT`）现在是优先级链中的常规运算符。这允许布尔表达式出现在任何表达式有效的位置（例如 `SELECT a > 5 AND b < 10`）。

### 急切列引用验证

`WHERE`、`GROUP BY`、`HAVING` 和 `ORDER BY` 中的列引用现在在查询计划构建时急切验证，即使在空表或单行排序上也能捕获不存在的列。之前，错误的引用只在每行求值时检测，所以对空表的查询会静默成功。

### 错误修复

- UPDATE 时不强制执行 NOT NULL 约束
- UNIQUE 约束拒绝多个 NULL（SQL 标准：NULL 是不同的）
- INSERT 列列表中的重复列未检测到
- 空表上的 `SUM`/`AVG`/`MIN`/`MAX` 返回 0 而不是 NULL
- `LIKE '_'` 匹配空字符串
- `LIMIT 0` 抛出错误

### 版本更新

| 组件 | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | — |
| engine | 1.2.9 | @petradb/engine 1.2.16 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | 1.2.6 | @petradb/server 1.2.9 |
| cli | 1.2.8 | @petradb/cli 1.2.9 |
| jdbc | 1.2.13 | — |
| knex | — | @petradb/knex 1.2.2 |
| lucid | — | @petradb/lucid 1.2.1 |
| drizzle | — | @petradb/drizzle 1.2.2 |

## v1.2-20260307

### SQL：INSERT VALUES 中的 `DEFAULT` 关键字

`INSERT INTO t (id, name) VALUES (DEFAULT, 'Alice')` 现在可以工作。SQL 标准的 `DEFAULT` 关键字之前被解析器拒绝，导致 ORM 生成的 INSERT 语句在显式传递 `DEFAULT` 给 serial 或带默认值的列时无法执行。

### Drizzle ORM 集成

新增 `@petradb/drizzle` 包，提供带有自定义 PostgreSQL 方言实现的 [Drizzle ORM](https://orm.drizzle.team) 驱动。支持使用 `pgTable` 的模式定义、insert/select/update/delete、returning 子句、`db.transaction()` 自动提交/回滚，以及类型安全查询。与 `drizzle-orm/node-postgres` 完全功能对等。

### 依赖包版本更新

Engine、server、cli 和 jdbc 已升级以包含 DEFAULT 关键字修复。

| 组件 | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | — |
| engine | 1.2.7 | @petradb/engine 1.2.14 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | — | @petradb/server 1.2.7 |
| cli | — | @petradb/cli 1.2.7 |
| jdbc | 1.2.11 | — |
| knex | — | @petradb/knex 1.2.2 |
| lucid | — | @petradb/lucid 1.2.1 |
| drizzle | — | @petradb/drizzle 1.2.0 |

## v1.2-20260306

### JS API：`close()` 返回 `Promise<void>`
`Session.close()` 现在返回 `Promise<void>` 而不是 `void`，与 client 模块的 API 匹配以便互换使用。

### 时间戳解析
`parseTimestamp` 现在处理 `Z` 后缀、`+/-HH:MM` 偏移量、毫秒和带时区信息的空格分隔时间戳。对于 `TIMESTAMP` 列，去除时区为 `LocalDateTime`。

### JS 外观层完善
`toJS` 和 `typeString` 现在处理 `DateValue`、`TimeValue`、`TimestampTZValue`、`TimeTZValue`、`IntervalValue` 和 `ByteaValue`。

### SQL：限定星号（`table.*`）
`SELECT t.*` 语法现在可以在查询中工作，包括连接和混合表达式。

### 比较中的类型强制
- `NumberValue` 和 `TextValue` 现在可以跨类型比较（文本参数 vs 数值列，反之亦然）
- `TimestampValue` 现在可以通过将文本解析为时间戳来与 `TextValue` 比较

### Knex 驱动：Date 绑定
`_sanitizeBindings` 在传递给引擎之前将 JS `Date` 对象转换为 ISO 字符串，防止 `Date.toString()` 格式导致的 `DateTimeParseException`。

| 组件 | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | — |
| engine | 1.2.6 | @petradb/engine 1.2.13 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | — | @petradb/server 1.2.5 |
| cli | — | @petradb/cli 1.2.5 |
| jdbc | 1.2.10 | — |
| knex | — | @petradb/knex 1.2.2 |
| lucid | — | @petradb/lucid 1.2.1 |

## v1.2-20260305

### JDBC 驱动
- Fat jar 发布 — `io.github.edadma:petradb-jdbc` 现在是 Maven Central 上的单一自包含 jar
- 简洁的连接 URL — `jdbc:petradb:memory`、`jdbc:petradb:file:/path`、`jdbc:petradb://host:port`
- ServiceLoader 自动发现 — `DriverManager.getConnection()` 无需 `Class.forName` 即可工作
- 修复了硬编码的元数据版本字符串

### JS/TS 引擎（`@petradb/engine`）
- 在 JS 外观层中添加了 `CreateViewResult`、`DropViewResult`、`ExplainResult`、`CopyResult`
- 在 TypeScript 类型定义中添加了 `ExplainResult` 和 `CopyResult`

### 文档
- 新增 Knex.js 指南，包含完整示例
- JDBC 文档：添加了 Maven/Gradle/sbt 安装代码片段，修正了端口号

### 基础设施
- `petradb-shared` 现在可以发布到 Maven Central
- 发布后冒烟测试脚本，覆盖 npm、Scala 和 JDBC 制品

| 组件 | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.1 | — |
| engine | 1.2.2 | @petradb/engine 1.2.5 |
| client | 1.2.3 | @petradb/client 1.2.3 |
| server | — | @petradb/server 1.2.3 |
| cli | — | @petradb/cli 1.2.3 |
| jdbc | 1.2.6 | — |
| knex | — | @petradb/knex 1.2.0 |

## v1.2.2

### 引擎子包重构
- 引擎移至 `io.github.edadma.petradb.engine` 子包
- 新增共享 `Session` trait，由 engine 和 client 共同扩展

### CLI 客户端支持
- 连接到远程 PetraDB 服务器：`petradb --host localhost --port 5480`
- `--user` 和 `--password` 标志用于认证
- 元命令通过 SQL 在网络上工作

### SQL
- `SHOW VIEWS` 命令返回视图名称和定义

### Knex 方言
- `@petradb/knex` 方言适配器，用于将 Knex.js 查询构建器与 PetraDB 配合使用

### 修复
- 修复客户端 npm 发布
- 修复 CLI npm 发布
- 修复硬编码的 JDBC 元数据版本字符串
- 在 JVM、JS 和 Native 上通过 1013+ 测试

## v1.2

### JDBC 驱动
- 作为 `petradb-jdbc` 发布到 Maven Central
- `getGeneratedKeys()`、`addBatch()`/`executeBatch()`、用于 DBeaver 的外键/索引元数据
- 文件模式（嵌入式）和服务器模式（网络）连接

### SQL 引擎
- `COPY FROM/TO` 用于 CSV 导入/导出
- `CREATE TEMP TABLE`、`CREATE/DROP VIEW`
- `SHOW FOREIGN KEYS`/`SHOW INDEXES` 自省
- 等值连接的索引嵌套循环连接优化
- 解析器迁移到 fastparse

### 服务器
- CORS 支持，配合 TOML 配置
- 可配置的 `max_sessions`，默认端口 5480
- 使用 Node.js HTTP 后端的 JS 服务器平台

### 客户端
- 新增 `@petradb/client` npm 包，带 JS 外观层
- `Session` 类，提供返回 Promise 的 `connect()`/`execute()`/`close()`

### CLI
- `\timing`、`\copy` 命令
- Native 上的持久历史记录

### 构建
- Scala 3.8.2、sbt 1.12.4
- 在 JVM、JS 和 Native 上通过 1000 个测试

## v1.1.0

### TextDB — 人类可编辑的文本文件持久化
一种新的存储后端，将数据库持久化为 `.ptxt` 文本文件。打开时加载到内存，每次更改后重写文件。

### Upsert — `ON CONFLICT DO UPDATE`
使用 `EXCLUDED` 伪表的插入或更新语义。

### 改进的异常层次结构
类型化异常类替代了通用的 `problem()` 调用。

### ALTER TABLE 集中化
`DB.alterTable()` 现在集中调度所有 ALTER TABLE 操作。

## v1.0.1

- 在 `@petradb/engine` 中将 `ConnectSQL` 重命名为 `Session`
- 异步 `execute()` API，返回 `Promise<ExecuteResult[]>`
- 新增 `@petradb/client` 包用于网络使用
- 统一了 engine 和 server 之间的响应格式

## v1.0.0

首个稳定版本。

- 跨平台 SQL 引擎（JVM、JavaScript、Native）
- 兼容 PostgreSQL 语法
- 内存和持久化（防崩溃）存储
- DDL、DML、连接、子查询、聚合、事务
- JSONB 运算符、数组类型、CHECK 约束
- 879 个通过的测试

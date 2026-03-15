---
title: JavaScript / TypeScript
description: PetraDB 的 JavaScript 和 TypeScript API 参考。
---

## 安装

```bash
npm install @petradb/engine
```

## Session

### `new Session(options?)`

创建新的数据库实例。

| 选项 | 类型 | 默认值 | 描述 |
|--------|------|---------|-------------|
| `rowMode` | `'object' \| 'array'` | `'object'` | SELECT 结果的默认行格式 |
| `storage` | `'memory' \| 'persistent' \| 'text'` | `'memory'` | 存储后端 |
| `path` | `string` | — | 文件路径（persistent 和 text 模式必需） |
| `pageSize` | `number` | `4096` | 页面大小（字节，仅 persistent 模式） |

```javascript
// 内存模式（默认）
const db = new Session();

// 防崩溃持久化存储（Node.js）
const db = new Session({ storage: 'persistent', path: './mydb' });

// 人类可读文本文件（Node.js）
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

对于持久化存储，PetraDB 会自动检测是创建新文件还是打开已有文件。

### `db.close()`

释放文件句柄。返回 `Promise<void>`。persistent 和 text 存储必需。对内存数据库是空操作（但仍然是异步的）。

```javascript
await db.close();
```

### `db.execute(sql, options?)`

执行一个或多个以 `;` 分隔的 SQL 语句。返回解析为结果数组的 Promise。

| 选项 | 类型 | 默认值 | 描述 |
|--------|------|---------|-------------|
| `rowMode` | `'object' \| 'array'` | 构造函数默认值 | 本次调用的行格式 |

```javascript
const [{ rows, fields }] = await db.execute('SELECT * FROM users');
```

### `db.prepare(sql)`

使用 `$1`、`$2`... 参数占位符创建预处理语句。返回带有 `execute(params, options?)` 方法的语句对象。

```javascript
const stmt = db.prepare('SELECT * FROM users WHERE id = $1');
const [{ rows }] = await stmt.execute([42]);

// 带选项
const [{ rows }] = await stmt.execute([42], { rowMode: 'array' });
```

### `db.registerFunction(name, callback)`

注册可从 SQL、触发器和存储过程中调用的原生 JavaScript 函数：

```javascript
db.registerFunction('my_double', (args) => args[0] * 2);

// 现在可以在 SQL 中使用：
const [{ rows }] = await db.execute('SELECT my_double(21) AS val');
// rows[0].val === 42
```

回调接收 JavaScript 值数组（数字、字符串、布尔值或 `null`），应返回一个 JavaScript 值。

## TypeScript 接口

```typescript
interface SessionOptions {
  rowMode?: 'object' | 'array';
  storage?: 'memory' | 'persistent' | 'text';
  path?: string;
  pageSize?: number;
}

interface ExecuteOptions {
  rowMode?: 'object' | 'array';
}

interface PreparedStatement {
  execute(params?: any[], options?: ExecuteOptions): Promise<ExecuteResult[]>
}

class Session {
  constructor(options?: SessionOptions)
  execute(sql: string, options?: ExecuteOptions): Promise<ExecuteResult[]>
  prepare(sql: string): PreparedStatement
  close(): Promise<void>
}
```

## 结果类型

每个结果都有一个用于判别的 `command` 字段：

```typescript
// DDL
{ command: 'create table', table: string }
{ command: 'drop table', table: string }
{ command: 'create type', type: string }
{ command: 'drop type', type: string }
{ command: 'create index', index: string }
{ command: 'drop index', index: string }
{ command: 'truncate table', table: string }
{ command: 'alter table' }
{ command: 'create view', view: string }
{ command: 'drop view', view: string }
{ command: 'create sequence', sequence: string }
{ command: 'drop sequence', sequence: string }
{ command: 'create schema', schema: string }
{ command: 'create trigger', trigger: string }
{ command: 'drop trigger', trigger: string }

// PL/pgSQL
{ command: 'do' }
{ command: 'create function', function: string }
{ command: 'drop function', function: string }
{ command: 'create procedure', procedure: string }
{ command: 'drop procedure', procedure: string }
{ command: 'call' }

// 查询计划
{ command: 'explain', plan: string }

// DML
{ command: 'insert', result: Record<string, any>, rows: T[], fields: FieldInfo[] }
{ command: 'select', rows: T[], fields: { name: string, dataType: string }[] }
{ command: 'update', rowCount: number }
{ command: 'delete', rowCount: number }
{ command: 'copy', rowCount: number }

// 事务
{ command: 'begin' }
{ command: 'commit' }
{ command: 'rollback' }

// 预处理语句
{ command: 'prepare', name: string }
{ command: 'deallocate', name: string }
```

## 值映射

| SQL 类型 | JavaScript 类型 |
|----------|----------------|
| INT, BIGINT, DOUBLE, NUMERIC | `number` |
| TEXT, CHAR, VARCHAR | `string` |
| BOOLEAN | `boolean` |
| UUID | `string` |
| TIMESTAMP | `Date` |
| ENUM | `string`（标签） |
| JSON array | `Array` |
| JSON object | `Object` |
| NULL | `null` |

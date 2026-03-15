---
title: JavaScript / TypeScript 使用指南
description: 从 JavaScript 和 TypeScript 使用 PetraDB。
---

## 创建数据库

每个 `Session` 实例是一个完全隔离的数据库。默认在内存中运行，但你可以选择持久化或文本存储。

```javascript
import { Session } from '@petradb/engine';

// 内存模式（默认）
const db = new Session();
```

## 存储模式

### 内存模式（默认）

数据存储在内存中，进程退出时丢失。适用于所有环境：Node.js、Deno、Bun 和浏览器。

```javascript
const db = new Session();
// 或显式指定：
const db = new Session({ storage: 'memory' });
```

### 持久化模式（Node.js）

单个二进制文件的防崩溃持久化存储，使用写时复制页面和双缓冲头。如果文件存在则打开；否则创建新数据库。

```javascript
const db = new Session({ storage: 'persistent', path: './mydb' });

// 可选：设置页面大小（默认 4096）
const db = new Session({ storage: 'persistent', path: './mydb', pageSize: 8192 });
```

### 文本模式（Node.js）

将数据存储为人类可读的文本文件（`.ptxt`）。适用于调试、版本控制或手动编辑数据。

```javascript
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

### 关闭

使用持久化或文本存储时，调用 `await db.close()` 释放文件句柄。对于内存数据库，`close()` 是空操作。

```javascript
const db = new Session({ storage: 'persistent', path: './mydb' });
// ... 使用数据库 ...
await db.close();
```

## 执行 SQL

使用 `db.execute(sql)` 运行一个或多个以分号分隔的 SQL 语句。返回一个解析为结果对象数组的 Promise。

```javascript
await db.execute(`
  CREATE TABLE users (
    id SERIAL,
    name TEXT NOT NULL,
    email TEXT,
    PRIMARY KEY (id)
  )
`);

await db.execute("INSERT INTO users (name, email) VALUES ('Alice', 'alice@example.com')");
await db.execute("INSERT INTO users (name, email) VALUES ('Bob', 'bob@example.com')");

const [{ rows, fields }] = await db.execute('SELECT * FROM users');
// rows:   [{ id: 1, name: 'Alice', email: 'alice@example.com' }, ...]
// fields: [{ name: 'id', dataType: 'serial' }, { name: 'name', dataType: 'text' }, ...]
```

## 行模式

默认情况下，SELECT 行作为以列名为键的对象返回。使用 `rowMode: 'array'` 返回位置数组。

```javascript
// 为所有查询设置默认值
const db = new Session({ rowMode: 'array' });

// 或按次覆盖
const [{ rows }] = await db.execute('SELECT id, name FROM users', { rowMode: 'array' });
// rows: [[1, 'Alice'], [2, 'Bob']]
```

## 预处理语句

使用 `db.prepare(sql)` 配合 `$1`、`$2`... 参数占位符。返回一个带有 `execute(params, options?)` 方法的语句对象。

```javascript
const stmt = db.prepare('SELECT * FROM users WHERE id = $1');
const [{ rows }] = await stmt.execute([42]);

// 带选项
const [{ rows }] = await stmt.execute([42], { rowMode: 'array' });
```

SQL 级别的 `PREPARE` / `EXECUTE` / `DEALLOCATE` 也受支持 — 详见[事务参考](/reference/transactions/)。

完整的结果类型、值映射和 TypeScript 接口请参阅 [JavaScript API 参考](/reference/api-javascript/)。

## TypeScript

包含完整的类型定义。使用判别联合来缩窄结果类型：

```typescript
import { Session, ExecuteResult } from '@petradb/engine';

const db = new Session();
const results: ExecuteResult[] = await db.execute('SELECT * FROM users');

for (const result of results) {
  if (result.command === 'select') {
    // 此处 result.rows 和 result.fields 已类型化
  }
}
```

## 完整示例

```javascript
import { Session } from '@petradb/engine';

const db = new Session();

await db.execute(`
  CREATE TYPE status AS ENUM ('active', 'inactive');
  CREATE TABLE products (
    id SERIAL,
    name TEXT NOT NULL,
    price NUMERIC(10,2),
    status status DEFAULT 'active',
    tags JSON,
    created_at TIMESTAMP,
    PRIMARY KEY (id)
  )
`);

await db.execute(`
  INSERT INTO products (name, price, tags, created_at) VALUES
    ('Laptop', 999.99, '["electronics", "computers"]', '2025-01-15 10:30:00');
  INSERT INTO products (name, price, tags, created_at) VALUES
    ('Coffee', 4.50, '["food", "organic"]', '2025-01-16 08:00:00')
`);

const [{ rows }] = await db.execute(`
  SELECT name, price FROM products
  WHERE price > 10
  ORDER BY price DESC
`);

console.log(rows); // [{ name: 'Laptop', price: 999.99 }]
```

## 错误处理

```javascript
async function safeExecute(db, sql) {
  try {
    return await db.execute(sql);
  } catch (error) {
    console.error('SQL Error:', error.message);
    return null;
  }
}
```

## 平台说明

- 内存模式适用于所有环境：Node.js、Deno、Bun 和浏览器（需要打包工具）
- 持久化和文本存储需要 Node.js（它们使用文件系统）
- 不需要外部依赖或原生模块
- 包含 TypeScript 定义
- 在浏览器中处理大数据集时建议使用 Web Workers

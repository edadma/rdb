---
title: JavaScript / TypeScript 入门
description: 安装 PetraDB 并从 JavaScript 或 TypeScript 运行你的第一个 SQL 查询。
---

## 安装

```bash
npm install @petradb/engine
```

无需原生模块，无需安装后脚本 — 纯 JavaScript。

## 运行你的第一个查询

```javascript
import { Session } from '@petradb/engine';

const db = new Session();

await db.execute(`
  CREATE TABLE users (
    id SERIAL,
    name TEXT NOT NULL,
    email TEXT
  );

  INSERT INTO users (name, email) VALUES
    ('Alice', 'alice@example.com'),
    ('Bob', 'bob@example.com');
`);

const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows);
// [
//   { id: 1, name: 'Alice', email: 'alice@example.com' },
//   { id: 2, name: 'Bob', email: 'bob@example.com' }
// ]
```

就这么简单 — 一个完整的兼容 PostgreSQL 的 SQL 数据库，内存运行，无需配置。每个 `Session` 是一个隔离的数据库实例。

## 运行环境

引擎是纯 JavaScript 实现，没有任何原生依赖，因此可以在任何 JavaScript 运行的地方使用：Node.js、Deno、Bun，以及直接在浏览器中运行。你可以[立即在 playground 中尝试](/playground/)。

## 持久化存储

当你需要数据在重启后保留，可以向构造函数传递 `storage` 选项：

```javascript
// 单文件防崩溃持久化存储（Node.js）
const db = new Session({ storage: 'persistent', path: './mydb' });

// 人类可读的文本文件（Node.js）
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

对于持久化数据库，PetraDB 会自动检测是创建新文件还是打开已有文件。使用完毕后调用 `await db.close()` 释放文件句柄。

对于多进程或网络访问，可将 PetraDB 作为[服务器](/guides/server/)运行，并使用[客户端](/guides/client/)库连接。

## 下一步

[JavaScript / TypeScript 指南](/guides/javascript/)涵盖了存储模式、行模式、预处理语句和完整 API。另请参阅 [CLI](/guides/cli/)、[服务器](/guides/server/)和[客户端](/guides/client/)指南。

---
title: JavaScript / TypeScript
description: JavaScript and TypeScript API reference for PetraDB.
---

## Installation

```bash
npm install @petradb/engine
```

## Session

### `new Session(options?)`

Creates a new database instance.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `rowMode` | `'object' \| 'array'` | `'object'` | Default row format for SELECT results |
| `storage` | `'memory' \| 'persistent' \| 'text'` | `'memory'` | Storage backend |
| `path` | `string` | — | File path (required for persistent and text) |
| `pageSize` | `number` | `4096` | Page size in bytes (persistent only) |

```javascript
// In-memory (default)
const db = new Session();

// Crash-safe persistent storage (Node.js)
const db = new Session({ storage: 'persistent', path: './mydb' });

// Human-readable text file (Node.js)
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

For persistent storage, PetraDB auto-detects whether to create a new file or open an existing one.

### `db.close()`

Releases file handles. Returns a `Promise<void>`. Required for persistent and text storage. No-op (but still async) for memory databases.

```javascript
await db.close();
```

### `db.execute(sql, options?)`

Executes one or more SQL statements separated by `;`. Returns a promise that resolves to an array of results.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `rowMode` | `'object' \| 'array'` | constructor default | Row format for this call |

```javascript
const [{ rows, fields }] = await db.execute('SELECT * FROM users');
```

### `db.prepare(sql)`

Creates a prepared statement with `$1`, `$2`, ... parameter placeholders. Returns a statement object with an `execute(params, options?)` method.

```javascript
const stmt = db.prepare('SELECT * FROM users WHERE id = $1');
const [{ rows }] = await stmt.execute([42]);

// With options
const [{ rows }] = await stmt.execute([42], { rowMode: 'array' });
```

### `db.registerFunction(name, callback)`

Register a native JavaScript function callable from SQL, triggers, and stored procedures:

```javascript
db.registerFunction('my_double', (args) => args[0] * 2);

// Now usable in SQL:
const [{ rows }] = await db.execute('SELECT my_double(21) AS val');
// rows[0].val === 42
```

The callback receives an array of JavaScript values (numbers, strings, booleans, or `null`) and should return a JavaScript value.

## TypeScript Interfaces

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

## Result Types

Every result has a `command` field for discrimination:

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

// Query plan
{ command: 'explain', plan: string }

// DML
{ command: 'insert', result: Record<string, any>, rows: T[], fields: FieldInfo[] }
{ command: 'select', rows: T[], fields: { name: string, dataType: string }[] }
{ command: 'update', rowCount: number }
{ command: 'delete', rowCount: number }
{ command: 'copy', rowCount: number }

// Transactions
{ command: 'begin' }
{ command: 'commit' }
{ command: 'rollback' }

// Prepared statements
{ command: 'prepare', name: string }
{ command: 'deallocate', name: string }
```

## Value Mapping

| SQL Type | JavaScript Type |
|----------|----------------|
| INT, BIGINT, DOUBLE, NUMERIC | `number` |
| TEXT, CHAR, VARCHAR | `string` |
| BOOLEAN | `boolean` |
| UUID | `string` |
| TIMESTAMP | `Date` |
| ENUM | `string` (label) |
| JSON array | `Array` |
| JSON object | `Object` |
| NULL | `null` |

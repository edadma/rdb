---
title: JavaScript / TypeScript Usage
description: Using PetraDB from JavaScript and TypeScript.
---

## Installation

```bash
npm install @petradb/engine
```

## Creating a Database

Each `Session` instance is a fully isolated in-memory database.

```javascript
import { Session } from '@petradb/engine';

const db = new Session();
```

## Executing SQL

Use `db.execute(sql)` to run one or more semicolon-separated SQL statements. It returns a promise that resolves to an array of result objects.

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

## Row Modes

By default, SELECT rows are returned as objects keyed by column name. Use `rowMode: 'array'` for positional arrays instead.

```javascript
// Set default for all queries
const db = new Session({ rowMode: 'array' });

// Or override per call
const [{ rows }] = await db.execute('SELECT id, name FROM users', { rowMode: 'array' });
// rows: [[1, 'Alice'], [2, 'Bob']]
```

## Prepared Statements

Use `db.prepare(sql)` with `$1`, `$2`, ... parameter placeholders. Returns a statement object with an `execute(params, options?)` method.

```javascript
const stmt = db.prepare('SELECT * FROM users WHERE id = $1');
const [{ rows }] = await stmt.execute([42]);

// With options
const [{ rows }] = await stmt.execute([42], { rowMode: 'array' });
```

You can also use `PREPARE` / `EXECUTE` / `DEALLOCATE` via SQL:

```sql
PREPARE get_user AS SELECT * FROM users WHERE id = $1;
EXECUTE get_user(42);
DEALLOCATE get_user;
```

## Result Types

Every result has a `command` field for easy discrimination:

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

// DML
{ command: 'insert', result: Record<string, any>, rows: T[], fields: FieldInfo[] }
{ command: 'select', rows: T[], fields: { name: string, dataType: string }[] }
{ command: 'update', rowCount: number }
{ command: 'delete', rowCount: number }

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

## TypeScript

Full type definitions are included. Use discriminated unions to narrow result types:

```typescript
import { Session, ExecuteResult } from '@petradb/engine';

const db = new Session();
const results: ExecuteResult[] = await db.execute('SELECT * FROM users');

for (const result of results) {
  if (result.command === 'select') {
    // result.rows and result.fields are typed here
  }
}
```

## Full Example

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

## Error Handling

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

## Platform Notes

- Works in browsers with bundlers (webpack, Vite, Rollup, etc.)
- No external dependencies or native modules required
- TypeScript definitions included
- Consider Web Workers for large datasets in browsers

# @edadma/rdb

[![npm version](https://badge.fury.io/js/%40edadma%2Frdb.svg)](https://www.npmjs.com/package/@edadma/rdb)

A lightweight, in-memory SQL database for JavaScript and TypeScript. No native dependencies, no external services — just import and query.

## Installation

```bash
npm install @edadma/rdb
```

## Quick Start

```javascript
import { ConnectSQL } from '@edadma/rdb';

const db = new ConnectSQL();

db.execute(`
  CREATE TABLE users (
    id SERIAL,
    name TEXT NOT NULL,
    email TEXT
  )
`);

db.execute("INSERT INTO users (name, email) VALUES ('Alice', 'alice@example.com')");
db.execute("INSERT INTO users (name, email) VALUES ('Bob', 'bob@example.com')");

const [{ rows, fields }] = db.execute('SELECT * FROM users');
// rows:   [{ id: 1, name: 'Alice', email: 'alice@example.com' }, ...]
// fields: [{ name: 'id', dataType: 'serial' }, { name: 'name', dataType: 'text' }, ...]
```

## Row Modes

By default, SELECT rows are returned as objects keyed by column name. Use `rowMode: 'array'` for positional arrays instead.

```javascript
// Set default for all queries
const db = new ConnectSQL({ rowMode: 'array' });

// Or override per call
const [{ rows }] = db.execute('SELECT id, name FROM users', { rowMode: 'array' });
// rows: [[1, 'Alice'], [2, 'Bob']]
```

## Supported SQL

### Data Types

| Type | Description |
|------|-------------|
| `INT` / `INTEGER` | 32-bit integer |
| `SERIAL` | Auto-incrementing 32-bit integer |
| `BIGINT` | 64-bit integer |
| `BIGSERIAL` | Auto-incrementing 64-bit integer |
| `DOUBLE` | Double-precision float |
| `NUMERIC(p, s)` | Fixed-precision decimal |
| `TEXT` | Variable-length string |
| `BOOLEAN` | True/false |
| `TIMESTAMP` | Date and time |
| `UUID` | UUID (use `DEFAULT gen_random_uuid()`) |
| `JSON` | JSON objects and arrays |
| `ENUM` | Custom enumerated types |

### Operations

```sql
-- DDL
CREATE TABLE, DROP TABLE, ALTER TABLE (ADD/DROP/ALTER COLUMN)
CREATE TYPE ... AS ENUM, DROP TYPE

-- DML
INSERT INTO ... VALUES, UPDATE ... SET ... WHERE, DELETE FROM ... WHERE

-- Queries
SELECT, SELECT DISTINCT, WHERE, ORDER BY, LIMIT, OFFSET
GROUP BY, HAVING, JOIN (INNER/LEFT/RIGHT/FULL)
Subqueries, EXISTS, CTEs (WITH), CASE expressions
LIKE, ILIKE, IS NULL, COALESCE
UNION, INTERSECT, EXCEPT
```

### Aggregate Functions

`COUNT`, `SUM`, `AVG`, `MIN`, `MAX`

### Built-in Functions

`gen_random_uuid()`, `CURRENT_TIMESTAMP`, `COALESCE`, `UPPER`, `LOWER`, `LENGTH`, `SUBSTRING`, `TRIM`, `ABS`, `ROUND`, `CEIL`, `FLOOR`

## API

### `new ConnectSQL(options?)`

Creates a new database instance. Each instance is fully isolated.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `rowMode` | `'object' \| 'array'` | `'object'` | Default row format for SELECT results |

### `db.execute(sql, options?)`

Executes one or more SQL statements separated by `;`. Returns an array of results.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `rowMode` | `'object' \| 'array'` | constructor default | Row format for this call |

### Result Types

Every result has a `command` field for easy discrimination:

```typescript
// DDL
{ command: 'create table', table: string }
{ command: 'drop table', table: string }
{ command: 'create type', type: string }
{ command: 'drop type', type: string }
{ command: 'drop index', index: string }
{ command: 'alter table' }

// INSERT — result contains generated/default values
{ command: 'insert', result: Record<string, any> }

// SELECT
{ command: 'select', rows: T[], fields: { name: string, dataType: string }[] }

// UPDATE / DELETE
{ command: 'update', rows: number }
{ command: 'delete', rows: number }
```

### Value Mapping

| SQL Type | JavaScript Type |
|----------|----------------|
| INT, BIGINT, DOUBLE, NUMERIC | `number` |
| TEXT | `string` |
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
import { ConnectSQL, ExecuteResult } from '@edadma/rdb';

const db = new ConnectSQL();
const results: ExecuteResult[] = db.execute('SELECT * FROM users');

for (const result of results) {
  if (result.command === 'select') {
    // result.rows and result.fields are typed here
  }
}
```

## Example

```javascript
import { ConnectSQL } from '@edadma/rdb';

const db = new ConnectSQL();

db.execute(`
  CREATE TYPE status AS ENUM ('active', 'inactive');
  CREATE TABLE products (
    id SERIAL,
    name TEXT NOT NULL,
    price NUMERIC(10,2),
    status status DEFAULT 'active',
    tags JSON,
    created_at TIMESTAMP
  )
`);

db.execute(`
  INSERT INTO products (name, price, tags, created_at) VALUES
    ('Laptop', 999.99, '["electronics", "computers"]', '2025-01-15 10:30:00');
  INSERT INTO products (name, price, tags, created_at) VALUES
    ('Coffee', 4.50, '["food", "organic"]', '2025-01-16 08:00:00')
`);

const [{ rows }] = db.execute(`
  SELECT name, price FROM products
  WHERE price > 10
  ORDER BY price DESC
`);

console.log(rows); // [{ name: 'Laptop', price: 999.99 }]
```

## License

[ISC](https://opensource.org/licenses/ISC)

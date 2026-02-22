A lightweight, in-memory SQL database for JavaScript and TypeScript. No native dependencies, no external services — just import and query. Follows PostgreSQL conventions for SQL syntax, identifier handling, and type casting.

## Installation

```bash
npm install @petradb/engine
```

## Quick Start

```javascript
import { Session } from '@petradb/engine';

const db = new Session();

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

## SQL Compatibility

PetraDB follows PostgreSQL conventions:

- **Case-insensitive keywords** — `SELECT`, `select`, and `Select` are equivalent
- **Unquoted identifier folding** — identifiers fold to lowercase (`CREATE TABLE Users` → table name `users`)
- **Double-quoted identifiers** — preserve case (`"MixedCase"` stays as-is)
- **String escaping** — doubled single quotes (`'it''s'`) and E-strings (`E'it\'s'`)
- **Operators** — both `!=` and `<>` for not-equal
- **Type casting** — `::` operator and `CAST(expr AS type)`

## Supported SQL

### Data Types

| Type | Description |
|------|-------------|
| `SMALLINT` | 16-bit integer |
| `INT` / `INTEGER` | 32-bit integer |
| `BIGINT` | 64-bit integer |
| `SMALLSERIAL` / `SERIAL` / `BIGSERIAL` | Auto-incrementing integers |
| `DOUBLE` / `FLOAT` / `REAL` | Double-precision float |
| `NUMERIC(p, s)` / `DECIMAL(p, s)` | Fixed-precision decimal |
| `TEXT` | Variable-length string |
| `CHAR(n)` | Fixed-length string |
| `VARCHAR(n)` | Variable-length string (max n chars) |
| `BOOLEAN` | True/false |
| `DATE` / `TIME` / `TIMESTAMP` | Date and time types |
| `TIMESTAMP WITH TIME ZONE` | Timezone-aware timestamp |
| `INTERVAL` | Duration |
| `UUID` | UUID (use `DEFAULT gen_random_uuid()`) |
| `JSON` / `JSONB` | JSON objects and arrays |
| `BYTEA` | Binary data |
| `ENUM` | Custom enumerated types |
| `INT[]`, `TEXT[]`, etc. | Typed arrays |

### DDL

```sql
CREATE TABLE, CREATE TABLE IF NOT EXISTS
DROP TABLE, DROP TABLE IF EXISTS
ALTER TABLE (ADD/DROP/RENAME COLUMN, ADD CONSTRAINT, etc.)
CREATE TYPE ... AS ENUM, DROP TYPE
CREATE INDEX, CREATE UNIQUE INDEX, DROP INDEX
TRUNCATE TABLE
```

### DML

```sql
INSERT INTO ... VALUES
INSERT INTO ... SELECT         -- insert from a query
UPDATE ... SET ... WHERE
UPDATE ... SET ... FROM ...    -- bulk update with join semantics
DELETE FROM ... WHERE
TRUNCATE TABLE                 -- fast table reset, resets serial sequences
```

### Queries

```sql
SELECT, SELECT DISTINCT, WHERE, ORDER BY, LIMIT, OFFSET
GROUP BY, HAVING
JOIN (INNER/LEFT/RIGHT/FULL/CROSS)
LATERAL joins                  -- correlated subqueries in FROM
Subqueries, EXISTS, IN, = ANY(...)
VALUES as standalone query and FROM source
Column aliases: AS alias (col1, col2, ...)
CASE expressions, BETWEEN, LIKE, ILIKE
UNION, INTERSECT, EXCEPT
OVERLAPS                       -- date/time range overlap test
CAST(expr AS type), expr::type
```

### Constraints

```sql
PRIMARY KEY, UNIQUE, NOT NULL, DEFAULT
FOREIGN KEY ... REFERENCES ... ON DELETE/UPDATE (CASCADE, SET NULL, RESTRICT)
```

### Transactions

```sql
BEGIN, COMMIT, ROLLBACK
```

### Prepared Statements

```sql
PREPARE name AS statement      -- with $1, $2, ... parameters
EXECUTE name(arg1, arg2, ...)
DEALLOCATE name
```

### Aggregate Functions

`COUNT`, `SUM`, `AVG`, `MIN`, `MAX`, `string_agg`, `array_agg`, `bool_and`, `bool_or`, `every`, `variance`, `var_samp`, `var_pop`, `stddev`, `stddev_samp`, `stddev_pop`

### Built-in Functions

**Text:** `lower`, `upper`, `initcap`, `length`, `trim`, `ltrim`, `rtrim`, `btrim`, `substring`, `left`, `right`, `lpad`, `rpad`, `replace`, `translate`, `concat`, `concat_ws`, `repeat`, `reverse`, `position`, `split_part`, `ascii`, `chr`, `regexp_replace`, `regexp_match`, `starts_with`, `ends_with`

**Numeric:** `abs`, `ceil`, `floor`, `round`, `trunc`, `sign`, `mod`, `%`, `power`, `sqrt`, `cbrt`, `exp`, `ln`, `log10`, `log`, `pi`, `degrees`, `radians`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2`, `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh`, `div`, `factorial`, `gcd`, `lcm`, `random`, `greatest`, `least`, `&` (AND), `|` (OR), `#` (XOR), `~` (NOT), `<<` (shift left), `>>` (shift right)

**Date/Time:** `now`, `current_date`, `current_time`, `date_part`, `EXTRACT`, `date_trunc`, `make_date`, `make_time`, `make_timestamp`, `make_interval`, `age`, `to_char`, `to_date`, `to_timestamp`, `to_number`, `isfinite`

**Array:** `array_length`, `array_append`, `array_prepend`, `array_concat`, `array_cat`, `array_slice`, `array_remove`, `array_replace`, `array_position`, `array_distinct`, `array_lower`, `array_upper`, `array_ndims`, `cardinality`, `string_to_array`, `array_to_string`

**Other:** `coalesce`, `nullif`, `typeof`, `gen_random_uuid`, `octet_length`, `encode`, `decode`

## API

### `new Session(options?)`

Creates a new database instance. Each instance is fully isolated.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `rowMode` | `'object' \| 'array'` | `'object'` | Default row format for SELECT results |

### `db.execute(sql, options?)`

Executes one or more SQL statements separated by `;`. Returns a promise that resolves to an array of results.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `rowMode` | `'object' \| 'array'` | constructor default | Row format for this call |

### `db.prepare(sql)`

Creates a prepared statement with `$1`, `$2`, ... parameter placeholders. Returns a statement object with an `execute(params, options?)` method.

```javascript
const stmt = db.prepare('SELECT * FROM users WHERE id = $1');
const [{ rows }] = await stmt.execute([42]);

// With options
const [{ rows }] = await stmt.execute([42], { rowMode: 'array' });
```

### Result Types

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

### Value Mapping

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

## Example

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

## License

[ISC](https://opensource.org/licenses/ISC)

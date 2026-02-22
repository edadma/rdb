# PetraDB

![Maven Central](https://img.shields.io/maven-central/v/io.github.edadma/petradb-engine_sjs1_3)
[![Last Commit](https://img.shields.io/github/last-commit/edadma/petradb)](https://github.com/edadma/petradb/commits)
![GitHub](https://img.shields.io/github/license/edadma/petradb)
![Scala Version](https://img.shields.io/badge/Scala-3.8.1-blue.svg)
![ScalaJS Version](https://img.shields.io/badge/Scala.js-1.20.2-blue.svg)
![Scala Native Version](https://img.shields.io/badge/Scala_Native-0.5.10-blue.svg)

A cross-platform relational database implementation written in Scala that compiles to JVM, JavaScript, and Native platforms. PetraDB provides a full SQL interface with support for tables, queries, joins, aggregations, and more — with both in-memory and persistent storage backends.

## Overview

PetraDB is designed to provide a lightweight, embeddable SQL database for applications that need relational data operations without the overhead of a full database server. It's particularly useful for:

- **Testing and development** - Quick setup without external database dependencies
- **Client-side applications** - Running SQL queries in web browsers or Node.js
- **Data processing** - In-memory analytics and transformations
- **Embedded systems** - Native compilation for resource-constrained environments
- **Persistent storage** - Crash-safe durable storage backed by [stow](https://github.com/edadma/stow)

## Installation

### JavaScript/Node.js

```bash
npm install @edadma/petradb
```

### Scala (SBT)

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "0.1.7"
```

## Basic Usage

### JavaScript/TypeScript

```javascript
import { ConnectSQL } from '@edadma/petradb';

const db = new ConnectSQL();

// Create a table
db.execute(`
  CREATE TABLE users (
    id SERIAL,
    name TEXT NOT NULL,
    email TEXT,
    created_at TIMESTAMP
  )
`);

// Insert data
db.execute(`
  INSERT INTO users (name, email, created_at)
  VALUES ('John Doe', 'john@example.com', CURRENT_TIMESTAMP)
`);

// Query data — rows are objects by default
const [{ rows, fields }] = db.execute('SELECT * FROM users');
console.log(fields); // [{ name: 'id', dataType: 'serial' }, ...]
console.log(rows);   // [{ id: 1, name: 'John Doe', ... }]

// Or use array mode
const arrayDb = new ConnectSQL({ rowMode: 'array' });
// Can also override per-call: db.execute(sql, { rowMode: 'array' })
```

### Scala (In-Memory)

```scala
import io.github.edadma.petradb.*

given Session = new MemoryDB().connect()

val results = executeSQL("""
  CREATE TABLE products (
    id SERIAL,
    name TEXT NOT NULL,
    price NUMERIC(10,2),
    category TEXT
  );

  INSERT INTO products (name, price, category) VALUES
    ('Laptop', 999.99, 'Electronics'),
    ('Coffee', 4.50, 'Food'),
    ('Book', 19.99, 'Education');

  SELECT category, COUNT(*), AVG(price)
  FROM products
  GROUP BY category
  ORDER BY category;
""")

results.foreach(println)
```

### Scala (Persistent)

```scala
import io.github.edadma.petradb.*

// Create a new persistent database
val db = PersistentDB.create("mydata.db", 4096)
given Session = db.connect()

executeSQL("""
  CREATE TABLE users (
    id SERIAL,
    name TEXT NOT NULL,
    email TEXT,
    PRIMARY KEY (id)
  );

  INSERT INTO users (name, email) VALUES ('Alice', 'alice@example.com');
""")

db.close()

// Reopen existing database — all tables, data, and auto-increment state are restored
val db2 = PersistentDB.open("mydata.db")
given Session = db2.connect()

val results = executeSQL("SELECT * FROM users")
results.foreach(println)

db2.close()
```

Persistent databases use crash-safe atomic writes via [stow](https://github.com/edadma/stow), with copy-on-write pages and double-buffered headers. All DDL and DML operations are durable — the catalog (table definitions, enum types, auto-increment state) and row data are persisted automatically.

## Supported SQL Features

### Data Types

| Type | Description |
|------|-------------|
| `SMALLINT` | 16-bit integer (-32768 to 32767) |
| `INT` / `INTEGER` | 32-bit integer |
| `BIGINT` | 64-bit integer |
| `SMALLSERIAL` | Auto-incrementing 16-bit integer |
| `SERIAL` | Auto-incrementing 32-bit integer |
| `BIGSERIAL` | Auto-incrementing 64-bit integer |
| `DOUBLE` / `FLOAT` / `REAL` | Double-precision floating point |
| `NUMERIC(p,s)` / `DECIMAL(p,s)` | Fixed-precision decimal |
| `TEXT` | Variable-length string |
| `CHAR(n)` | Fixed-length string (right-padded with spaces) |
| `VARCHAR(n)` | Variable-length string (max n characters, no padding) |
| `BOOLEAN` | True/false |
| `DATE` | Calendar date (`yyyy-MM-dd`) |
| `TIME` | Time of day (`HH:mm:ss`) |
| `TIMESTAMP` | Date and time |
| `TIMESTAMP WITH TIME ZONE` | Date and time with timezone offset |
| `INTERVAL` | Duration (ISO 8601 or `N days N hours N minutes N seconds`) |
| `UUID` | Universally unique identifier |
| `JSON` / `JSONB` | Structured JSON objects and arrays |
| `BYTEA` | Binary data |
| `ENUM` | Custom enumerated types (via `CREATE TYPE ... AS ENUM`) |
| `INT[]`, `TEXT[]`, etc. | Typed arrays (any base type with `[]` suffix) |

### SQL Compatibility

PetraDB follows PostgreSQL conventions:

- **Case-insensitive keywords** — `SELECT`, `select`, and `Select` are equivalent
- **Unquoted identifier folding** — unquoted identifiers fold to lowercase (`CREATE TABLE Users` → table name `users`)
- **Double-quoted identifiers** — preserve case (`"MixedCase"` stays as-is)
- **String escaping** — doubled single quotes (`'it''s'`) and E-strings (`E'it\'s'`)
- **Operators** — both `!=` and `<>` for not-equal

### Type Casting

Use the `::` operator or `CAST(expr AS type)` to convert between types:

```sql
SELECT '2024-06-15'::DATE;
SELECT CAST('14:30:00' AS TIME);
SELECT '2 hours 30 minutes'::INTERVAL;
SELECT CAST(val AS TEXT);
SELECT '42'::INT;
SELECT 1::BOOLEAN;
SELECT EXTRACT(year FROM created_at);
```

### DDL (Data Definition Language)

```sql
CREATE TYPE order_status AS ENUM ('pending', 'shipped', 'delivered');

CREATE TABLE orders (
  id UUID DEFAULT gen_random_uuid(),
  customer_name TEXT NOT NULL,
  amount NUMERIC(10,2),
  status order_status,
  tags INT[],
  metadata JSON,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS orders (...);

-- Foreign key constraints
CREATE TABLE line_items (
  id SERIAL,
  order_id UUID REFERENCES orders (id) ON DELETE CASCADE ON UPDATE CASCADE,
  product TEXT NOT NULL
);

-- Indexes
CREATE INDEX idx_orders_status ON orders (status);
CREATE UNIQUE INDEX idx_orders_email ON orders (email);

ALTER TABLE orders ADD COLUMN notes TEXT;
ALTER TABLE orders DROP COLUMN notes;
ALTER TABLE orders RENAME COLUMN amount TO total;
ALTER TABLE orders RENAME TO purchases;

TRUNCATE TABLE orders;

DROP TABLE orders;
DROP TABLE IF EXISTS orders;
DROP INDEX idx_orders_status;
DROP TYPE order_status CASCADE;
```

### DML (Data Manipulation Language)

```sql
INSERT INTO orders (customer_name, amount)
VALUES ('Alice Smith', 149.99);

INSERT INTO orders (customer_name, amount)
VALUES ('Bob Johnson', 75.50)
RETURNING id;

-- Insert from a query
INSERT INTO archive (customer_name, amount)
SELECT customer_name, amount FROM orders WHERE status = 'delivered';

UPDATE orders SET status = 'shipped' WHERE amount > 100;

-- Bulk update from a values list (PostgreSQL UPDATE...FROM)
UPDATE orders
  SET status = d.status
  FROM (VALUES ('ord-1', 'shipped'), ('ord-2', 'delivered'))
       AS d (id, status)
  WHERE orders.id = d.id;

DELETE FROM orders WHERE status = 'delivered';

TRUNCATE TABLE orders;   -- fast table reset, resets serial sequences
```

### Queries

```sql
-- Filtering, sorting, pagination
SELECT * FROM orders
WHERE amount > 50
ORDER BY amount DESC
LIMIT 10 OFFSET 5;

-- Aggregations
SELECT status, COUNT(*), AVG(amount), SUM(amount)
FROM orders
GROUP BY status
HAVING COUNT(*) > 5;

-- Joins (INNER, LEFT, RIGHT, FULL, CROSS)
SELECT o.id, o.amount, c.name
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id;

-- Subqueries and EXISTS
SELECT * FROM customers c
WHERE EXISTS (
  SELECT 1 FROM orders o
  WHERE o.customer_id = c.id AND o.amount > 100
);

-- LATERAL joins (correlated subqueries in FROM)
SELECT c.name, recent.amount
FROM customers c,
LATERAL (SELECT amount FROM orders WHERE customer_id = c.id ORDER BY created_at DESC LIMIT 1) AS recent;

-- VALUES as a source with column aliases
SELECT * FROM (VALUES (1, 'a'), (2, 'b')) AS t (id, name);

-- Set operations
SELECT name FROM customers
UNION
SELECT name FROM suppliers;

-- CASE expressions
SELECT name,
  CASE WHEN amount > 100 THEN 'high' ELSE 'low' END AS tier
FROM orders;

-- Pattern matching
SELECT * FROM products WHERE name LIKE '%phone%';
SELECT * FROM products WHERE name ILIKE '%Phone%';

-- BETWEEN, IN, ANY
SELECT * FROM orders WHERE amount BETWEEN 10 AND 100;
SELECT * FROM orders WHERE status IN ('pending', 'shipped');
SELECT * FROM orders WHERE status = ANY(ARRAY['pending', 'shipped']);

-- OVERLAPS (date/time range overlap test)
SELECT (DATE '2024-01-01', DATE '2024-01-31') OVERLAPS (DATE '2024-01-15', DATE '2024-02-15');

-- DISTINCT
SELECT DISTINCT category FROM products;

-- ARRAY constructor
SELECT ARRAY[1, 2, 3];
```

### Date/Time Arithmetic

```sql
-- Date arithmetic
SELECT '2024-01-01'::DATE + 10;                        -- add days
SELECT '2024-01-15'::DATE - '2024-01-10'::DATE;        -- days between
SELECT now() + '2 hours'::INTERVAL;                     -- timestamp + interval
SELECT now() - '30 minutes'::INTERVAL;                  -- timestamp - interval
SELECT '1 hour'::INTERVAL * 3;                          -- scale interval
SELECT EXTRACT(year FROM now());                        -- extract field
SELECT date_trunc('month', now());                      -- truncate
```

### Scalar Functions

#### Text
| Function | Description |
|----------|-------------|
| `lower(text)` | Convert to lowercase |
| `upper(text)` | Convert to uppercase |
| `initcap(text)` | Capitalize each word |
| `length(text)` / `char_length(text)` | String length |
| `trim(text)` / `ltrim(text)` / `rtrim(text)` | Trim whitespace |
| `substring(text, start [, len])` | Extract substring |
| `left(text, n)` / `right(text, n)` | First/last n characters |
| `lpad(text, len [, pad])` / `rpad(text, len [, pad])` | Pad string |
| `replace(text, from, to)` | Replace occurrences |
| `concat(a, b)` / `concat_ws(sep, ...)` | Concatenate (with separator) |
| `repeat(text, n)` | Repeat string |
| `reverse(text)` | Reverse string |
| `position(substr, text)` | Find substring position (1-based) |
| `split_part(text, delim, n)` | Split and get nth part |
| `ascii(text)` / `chr(int)` | Character/code point conversion |
| `regexp_replace(text, pat, repl [, flags])` | Regex replace (`'g'` for global) |
| `regexp_match(text, pattern)` | First regex match as array |
| `starts_with(text, prefix)` | True if text starts with prefix |
| `ends_with(text, suffix)` | True if text ends with suffix |
| `translate(text, from, to)` | Character-by-character substitution |
| `btrim(text [, chars])` | Trim characters from both ends |

#### Numeric
| Function | Description |
|----------|-------------|
| `abs(x)` | Absolute value |
| `ceil(x)` / `floor(x)` | Round up/down |
| `round(x [, digits])` / `trunc(x [, digits])` | Round/truncate |
| `sign(x)` | Sign (-1, 0, 1) |
| `mod(x, y)` | Modulo |
| `power(x, y)` / `sqrt(x)` | Power/square root |
| `exp(x)` / `ln(x)` / `log10(x)` / `log(base, x)` | Exponential/logarithm |
| `pi()` | Pi constant |
| `degrees(rad)` / `radians(deg)` | Angle conversion |
| `cbrt(x)` | Cube root |
| `div(x, y)` | Integer division |
| `factorial(n)` | Factorial |
| `gcd(a, b)` / `lcm(a, b)` | Greatest common divisor / least common multiple |
| `sin` / `cos` / `tan` / `asin` / `acos` / `atan` / `atan2` | Trigonometry |
| `sinh` / `cosh` / `tanh` / `asinh` / `acosh` / `atanh` | Hyperbolic trigonometry |
| `random()` | Random number [0, 1) |
| `x % y` | Modulo operator |
| `x & y` | Bitwise AND |
| `x \| y` | Bitwise OR |
| `x # y` | Bitwise XOR |
| `~x` | Bitwise NOT |
| `x << n` | Bit shift left |
| `x >> n` | Bit shift right |
| `greatest(a, b, ...)` / `least(a, b, ...)` | Max/min of values |

#### Date/Time
| Function | Description |
|----------|-------------|
| `now()` | Current timestamp (UTC) |
| `current_date()` | Current date (UTC) |
| `current_time()` | Current time (UTC) |
| `date_part(field, source)` | Extract field from date/time |
| `EXTRACT(field FROM source)` | SQL standard extract |
| `date_trunc(field, source)` | Truncate to precision (year/quarter/month/week/day/hour/minute/second) |
| `make_date(y, m, d)` / `make_time(h, m, s)` | Construct date/time |
| `make_timestamp(y, mo, d, h, mi, s)` | Construct timestamp |
| `make_interval(days [, hours [, mins [, secs]]])` | Construct interval |
| `to_number(text, format)` | Parse numeric string |
| `isfinite(date\|timestamp)` | True (no infinities in Java time) |
| `age(ts1, ts2)` / `age(ts)` | Interval between timestamps |
| `to_char(value, format)` | Format as text |
| `to_date(text, format)` / `to_timestamp(text, format)` | Parse with format |

#### Array
| Function | Description |
|----------|-------------|
| `array_length(arr)` | Number of elements |
| `array_append(arr, val)` / `array_prepend(val, arr)` | Add element |
| `array_concat(arr1, arr2)` | Concatenate arrays |
| `array_slice(arr, start [, end])` | Slice array |
| `array_remove(arr, val)` | Remove all occurrences |
| `array_position(arr, val)` | Find element position (1-based) |
| `array_distinct(arr)` | Remove duplicates |
| `array_cat(arr1, arr2)` | Concatenate arrays (alias for `array_concat`) |
| `array_replace(arr, old, new)` | Replace matching elements |
| `array_lower(arr, dim)` / `array_upper(arr, dim)` | Array bounds (1-based) |
| `array_ndims(arr)` | Number of dimensions (always 1) |
| `cardinality(arr)` | Number of elements |
| `string_to_array(text, delim)` | Split string to array |
| `array_to_string(arr, sep)` | Join array to string |

#### Other
| Function | Description |
|----------|-------------|
| `coalesce(a, b, ...)` | First non-null value |
| `nullif(a, b)` | NULL if a = b |
| `typeof(value)` | Type name as text |
| `gen_random_uuid()` | Generate UUID v4 |
| `octet_length(bytea)` | Byte count |
| `encode(bytea, format)` / `decode(text, format)` | Binary encoding (hex, base64) |

### Aggregate Functions

| Function | Description |
|----------|-------------|
| `COUNT(*)` / `COUNT(expr)` | Count rows |
| `SUM(expr)` | Sum of values |
| `AVG(expr)` | Average |
| `MIN(expr)` / `MAX(expr)` | Minimum/maximum |
| `string_agg(text, separator)` | Concatenate with separator |
| `array_agg(expr)` | Collect values into array |
| `bool_and(expr)` / `bool_or(expr)` / `every(expr)` | Logical AND/OR across rows |
| `variance(expr)` / `var_samp(expr)` | Sample variance |
| `var_pop(expr)` | Population variance |
| `stddev(expr)` / `stddev_samp(expr)` | Sample standard deviation |
| `stddev_pop(expr)` | Population standard deviation |

### Transactions

```sql
BEGIN;
INSERT INTO accounts (name, balance) VALUES ('Alice', 1000);
UPDATE accounts SET balance = balance - 100 WHERE name = 'Alice';
COMMIT;

-- Or roll back on error
BEGIN;
UPDATE accounts SET balance = balance - 9999 WHERE name = 'Alice';
ROLLBACK;
```

### Prepared Statements

```sql
PREPARE get_user AS SELECT * FROM users WHERE id = $1;
EXECUTE get_user(42);
DEALLOCATE get_user;

-- Parameterized inserts
PREPARE add_user AS INSERT INTO users (name, email) VALUES ($1, $2);
EXECUTE add_user('Alice', 'alice@example.com');
```

## API Reference

### JavaScript/TypeScript API

```typescript
interface ConnectSQLOptions {
  rowMode?: 'object' | 'array';  // default: 'object'
}

interface ExecuteOptions {
  rowMode?: 'object' | 'array';  // overrides constructor default
}

class ConnectSQL {
  constructor(options?: ConnectSQLOptions)
  execute(sql: string, options?: ExecuteOptions): ExecuteResult[]
}
```

#### Result Types

```javascript
// CREATE TABLE / DROP TABLE
{ command: "create table", table: "table_name" }
{ command: "drop table", table: "table_name" }

// CREATE TYPE / DROP TYPE
{ command: "create type", type: "type_name" }
{ command: "drop type", type: "type_name" }

// DROP INDEX
{ command: "drop index", index: "index_name" }

// ALTER TABLE
{ command: "alter table" }

// INSERT — result contains generated/default column values
{ command: "insert", result: { id: 1, uuid_col: "..." } }

// SELECT — object mode (default)
{ command: "select", rows: [{ id: 1, name: "Alice" }, ...], fields: [{ name: "id", dataType: "serial" }, ...] }

// SELECT — array mode
{ command: "select", rows: [[1, "Alice"], ...], fields: [{ name: "id", dataType: "serial" }, ...] }

// UPDATE / DELETE
{ command: "update", rows: 3 }
{ command: "delete", rows: 1 }
```

### Scala API

```scala
// In-memory database
given Session = new MemoryDB().connect()

// Persistent database — create new or reopen existing
val db = PersistentDB.create("path/to/db", pageSize = 4096)
given Session = db.connect()

// Reopen existing
val db = PersistentDB.open("path/to/db")
given Session = db.connect()

// Execute SQL and get results
val results: Seq[Result] = executeSQL("SELECT * FROM users")

// Result types
sealed trait Result
case class QueryResult(table: TableValue) extends Result
case class InsertResult(obj: Map[String, Value], table: TableValue) extends Result
case class CreateTableResult(table: String) extends Result
case class DropTableResult(table: String) extends Result
case class TruncateResult(table: String) extends Result
case class UpdateResult(rows: Int) extends Result
case class DeleteResult(rows: Int) extends Result
```

## Testing

The project includes comprehensive test suites:

```bash
# Run tests for all platforms
sbt test

# Run JavaScript tests only
sbt engineJS/test

# Run JVM tests only
sbt engineJVM/test

# Run Native tests only
sbt engineNative/test
```

## Contributing

Contributions are welcome! Please follow these guidelines:

1. **Fork and Clone** - Fork the repository and clone your fork
2. **Create Branch** - Create a feature branch for your changes
3. **Write Tests** - Add tests for new functionality
4. **Submit PR** - Create a pull request with a clear description

### Development Setup

```bash
git clone https://github.com/edadma/petradb.git
cd petradb
sbt compile
sbt test
```

## License

[ISC License](LICENSE) - see LICENSE file for details.

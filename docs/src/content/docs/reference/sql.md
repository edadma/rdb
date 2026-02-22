---
title: SQL Reference
description: Complete SQL feature reference for PetraDB.
---

PetraDB follows PostgreSQL conventions for SQL syntax, identifier handling, and type casting.

## SQL Compatibility

- **Case-insensitive keywords** — `SELECT`, `select`, and `Select` are equivalent
- **Unquoted identifier folding** — unquoted identifiers fold to lowercase (`CREATE TABLE Users` → table name `users`)
- **Double-quoted identifiers** — preserve case (`"MixedCase"` stays as-is)
- **String escaping** — doubled single quotes (`'it''s'`) and E-strings (`E'it\'s'`)
- **Operators** — both `!=` and `<>` for not-equal

## Data Types

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

## Type Casting

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

## DDL (Data Definition Language)

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

## DML (Data Manipulation Language)

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

## Queries

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

## Date/Time Arithmetic

```sql
SELECT '2024-01-01'::DATE + 10;                        -- add days
SELECT '2024-01-15'::DATE - '2024-01-10'::DATE;        -- days between
SELECT now() + '2 hours'::INTERVAL;                     -- timestamp + interval
SELECT now() - '30 minutes'::INTERVAL;                  -- timestamp - interval
SELECT '1 hour'::INTERVAL * 3;                          -- scale interval
SELECT EXTRACT(year FROM now());                        -- extract field
SELECT date_trunc('month', now());                      -- truncate
```

## Transactions

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

## Prepared Statements

```sql
PREPARE get_user AS SELECT * FROM users WHERE id = $1;
EXECUTE get_user(42);
DEALLOCATE get_user;

-- Parameterized inserts
PREPARE add_user AS INSERT INTO users (name, email) VALUES ($1, $2);
EXECUTE add_user('Alice', 'alice@example.com');
```

## Constraints

```sql
PRIMARY KEY (id)
UNIQUE (email)
NOT NULL
DEFAULT value
FOREIGN KEY (col) REFERENCES other_table (col) ON DELETE CASCADE ON UPDATE CASCADE
```

## Scalar Functions

### Text

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

### Numeric

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

### Date/Time

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

### Array

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

### Other

| Function | Description |
|----------|-------------|
| `coalesce(a, b, ...)` | First non-null value |
| `nullif(a, b)` | NULL if a = b |
| `typeof(value)` | Type name as text |
| `gen_random_uuid()` | Generate UUID v4 |
| `octet_length(bytea)` | Byte count |
| `encode(bytea, format)` / `decode(text, format)` | Binary encoding (hex, base64) |

## Aggregate Functions

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

---
title: Data Types
description: SQL data types supported by PetraDB.
---

PetraDB follows PostgreSQL conventions for SQL syntax, identifier handling, and type casting.

## SQL Compatibility

- **Case-insensitive keywords** — `SELECT`, `select`, and `Select` are equivalent
- **Unquoted identifier folding** — unquoted identifiers fold to lowercase (`CREATE TABLE Users` → table name `users`)
- **Double-quoted identifiers** — preserve case (`"MixedCase"` stays as-is)
- **String escaping** — doubled single quotes (`'it''s'`) and E-strings (`E'it\'s'`)
- **Operators** — both `!=` and `<>` for not-equal

## Supported Types

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

## Constraints

```sql
PRIMARY KEY (id)
UNIQUE (email)
NOT NULL
DEFAULT value
FOREIGN KEY (col) REFERENCES other_table (col) ON DELETE CASCADE ON UPDATE CASCADE
```

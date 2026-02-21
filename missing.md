# rdb vs PostgreSQL — Missing Features

## Data Types

| Category | rdb has | Missing |
|---|---|---|
| Integer | smallint, integer, bigint | — |
| Serial | smallserial, serial, bigserial | — |
| Float | double (float8) | real (float4) |
| Exact numeric | numeric(p,s) | money |
| Boolean | boolean | — |
| Text | text, char(n), varchar(n) | — |
| Binary | bytea | — |
| Date/Time | date, time, timestamp, timestamptz, interval | timetz |
| UUID | uuid | — |
| JSON | json, jsonb (stored as parsed/binary internally) | — |
| Array | type[] | — |
| Enum | CREATE TYPE ... AS ENUM | — |
| Not in rdb | — | bit/varbit, inet/cidr/macaddr, point/line/box/circle/polygon/path, tsquery/tsvector, xml, range types (int4range, daterange, ...), composite types, domains |

## Operators

| Category | rdb has | Missing |
|---|---|---|
| Arithmetic | `+ - * / %` | `^` (power), `|/` (sqrt), `||/` (cbrt) |
| Comparison | `= != <> < > <= >=` | — |
| Logical | AND, OR, NOT | — |
| String | `||` (concat) | — |
| Pattern | LIKE, ILIKE, NOT LIKE, NOT ILIKE | SIMILAR TO, `~` `~*` `!~` `!~*` (regex operators) |
| Null test | IS NULL, IS NOT NULL | IS DISTINCT FROM, IS NOT DISTINCT FROM |
| Boolean test | IS TRUE, IS FALSE, IS UNKNOWN | — |
| Range | BETWEEN, NOT BETWEEN, OVERLAPS | BETWEEN SYMMETRIC |
| Set | IN, NOT IN, EXISTS, ANY/SOME, ALL | — |
| JSON | `->` `->>` `#>` `#>>` `@>` `<@` `?` `?\|` `?&` `\|\|` (merge) | — |
| Array | `@>` `<@` `&&` (overlap) `\|\|` (concat) | — |
| Cast | CAST(x AS type), `::` | — |
| Bitwise | `&` `|` `#` `~` `<<` `>>` | — |
| Time zone | — | AT TIME ZONE |

## Scalar Functions

### String — have

lower, upper, length, trim, ltrim, rtrim, btrim, substring, substr, left, right, replace, translate, concat, concat_ws, repeat, position, lpad, rpad, initcap, char_length, character_length, ascii, chr, starts_with, ends_with, reverse, split_part, regexp_replace, regexp_match, quote_literal, quote_ident

### String — missing

- overlay(string PLACING string FROM int FOR int)
- format(formatstr, ...)
- md5(string)
- regexp_matches(string, pattern) — global version returning set of arrays
- regexp_split_to_table(string, pattern)

### Math — have

abs, ceil, ceiling, floor, round, trunc, sign, mod, power, sqrt, cbrt, exp, ln, log, log10, pi, degrees, radians, sin, cos, tan, asin, acos, atan, atan2, sinh, cosh, tanh, asinh, acosh, atanh, div, factorial, gcd, lcm, random

### Math — missing

- width_bucket(value, low, high, count)
- scale(numeric)
- setseed(seed)

### Date/Time — have

now, current_date, current_time, clock_timestamp, date_part, EXTRACT, date_trunc, age, make_date, make_time, make_timestamp, make_interval, to_char, to_date, to_timestamp, to_number, isfinite

### Date/Time — missing

- make_timestamptz(year, month, day, hour, min, sec, tz)
- statement_timestamp()
- justify_days(interval), justify_hours(interval), justify_interval(interval)

### Conditional — have

coalesce, nullif, greatest, least, CASE

### Array — have

array_length, array_slice, array_append, array_prepend, array_concat, array_cat, array_remove, array_replace, array_position, array_distinct, array_lower, array_upper, array_ndims, cardinality, string_to_array, array_to_string, regexp_split_to_array

### Array — missing

- array_dims(array)
- array_fill(value, dimensions)
- unnest(array) — set-returning

### Bytea — have

octet_length, encode, decode

### Bytea — missing

- get_byte(bytea, offset)
- set_byte(bytea, offset, value)
- sha256(bytea), sha512(bytea)

### UUID — have

gen_random_uuid

### JSON — have

jsonb_typeof, json_typeof, jsonb_array_length, jsonb_object_keys, jsonb_keys, jsonb_extract_path, jsonb_extract_path_text, jsonb_set, jsonb_insert, jsonb_strip_nulls, jsonb_pretty, jsonb_build_object, jsonb_build_array, to_jsonb, to_json

### JSON — missing

- jsonb_each(json), jsonb_each_text(json) — set-returning
- jsonb_to_record(json) — expands to record type
- jsonb_array_elements(json) — set-returning
- jsonb_path_query(json, jsonpath) — SQL/JSON path

### Set-returning functions — missing

- generate_series(start, stop, step)
- generate_subscripts(array, dim)

## Aggregate Functions

### Have

count, sum, avg, min, max, string_agg, array_agg, bool_and, bool_or, every, bit_and, bit_or, bit_xor, variance, var_samp, var_pop, stddev, stddev_samp, stddev_pop, json_agg, jsonb_agg, json_object_agg, jsonb_object_agg

### Missing
- corr, covar_pop, covar_samp
- regr_avgx, regr_avgy, regr_count, regr_intercept, regr_r2, regr_slope, regr_sxx, regr_sxy, regr_syy
- percentile_cont, percentile_disc, mode (ordered-set aggregates)
- xmlagg

## Window Functions — none implemented

PostgreSQL provides:

- row_number(), rank(), dense_rank(), ntile(n)
- lag(value, offset, default), lead(value, offset, default)
- first_value(value), last_value(value), nth_value(value, n)
- cume_dist(), percent_rank()
- All aggregate functions usable as window functions via OVER(PARTITION BY ... ORDER BY ...)
- Window frame clauses: ROWS/RANGE/GROUPS BETWEEN ... AND ...

## DDL & SQL Features

| Feature | rdb | Missing |
|---|---|---|
| CREATE/DROP TABLE | yes (IF NOT EXISTS/IF EXISTS, CASCADE) | — |
| ALTER TABLE | add/drop/alter column, add/drop constraint, rename | — |
| Indexes | CREATE/DROP INDEX, UNIQUE | partial indexes, expression indexes, GIN, GiST, BRIN |
| Foreign keys | CASCADE, RESTRICT, SET NULL, NO ACTION | SET DEFAULT |
| PREPARE/EXECUTE | yes | — |
| BEGIN/COMMIT/ROLLBACK | yes | SAVEPOINT, RELEASE SAVEPOINT, ROLLBACK TO |
| TRUNCATE | yes | — |
| UNION/INTERSECT/EXCEPT | yes | — |
| LATERAL | yes | — |
| RETURNING | INSERT only | UPDATE RETURNING, DELETE RETURNING |
| CTEs | **no** | WITH, WITH RECURSIVE |
| Views | **no** | CREATE VIEW, materialized views |
| Schemas | **no** | CREATE SCHEMA, search_path |
| UPSERT | **no** | ON CONFLICT DO UPDATE/NOTHING |
| COPY | **no** | COPY TO/FROM |
| Permissions | **no** | GRANT, REVOKE, roles |
| Sequences | **no** (serials built-in) | CREATE SEQUENCE, nextval, currval, setval |
| Stored procs | **no** | CREATE FUNCTION, PL/pgSQL |
| Triggers | **no** | CREATE TRIGGER |
| LISTEN/NOTIFY | **no** | LISTEN, NOTIFY |
| Cursors | **no** | DECLARE, FETCH, CLOSE |
| EXPLAIN | **no** | EXPLAIN, EXPLAIN ANALYZE |
| Temp tables | **no** | CREATE TEMP TABLE |
| GENERATED columns | **no** | GENERATED ALWAYS AS (expr) |
| CHECK constraints | yes | — |
| EXCLUDE constraints | **no** | EXCLUDE USING |

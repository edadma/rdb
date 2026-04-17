---
title: 更新日志
---

## v1.5-20260417

### SQL Compatibility Fixes

Four fixes to improve PostgreSQL compatibility:

- **`type` as column name** — removed `type` from the reserved words list so it can be used as an unquoted column name, matching PostgreSQL behavior
- **Self-referential foreign keys** — `CREATE TABLE` with a column that `REFERENCES` the same table no longer fails; FK validation is deferred until after the table is created
- **Text-to-timestamp coercion** — text values (e.g., ISO-8601 strings from query parameters) can now be compared to `TIMESTAMP`, `TIMESTAMPTZ`, and `DATE` columns without explicit casts
- **Scalar subquery expressions** — parenthesized subqueries like `(SELECT count(*) FROM ...) = 0` now work in `WHERE` clauses and other expression positions

### Version Bumps

| Component | Maven Central | npm |
|-----------|---------------|-----|
| common | 1.5.2 | — |
| engine | 1.5.2 | @petradb/engine 1.5.2 |
| cli | — | @petradb/cli 1.5.2 |
| server | — | @petradb/server 1.5.2 |
| jdbc | 1.5.2 | — |

## v1.5-20260320

### Quarry — API Redesign

**Select API** — reads like SQL now:

```typescript
// Old
db.select(users).columns(users.name).where(eq(users.name, "Alice"))

// New
db.select(users.name).from(users).where(eq(users.name, "Alice"))
db.from(users).where(eq(users.name, "Alice"))  // SELECT *
```

- `db.select(...exprs)` takes column expressions, returns `FromableBuilder`
- `.from(table)` provides the table and returns `SelectBuilder`
- `db.from(table)` is shorthand for `db.select().from(table)` (SELECT \*)
- Old `db.select(table).columns(...)` pattern removed

**Builder-based subqueries** — no more raw AST:

```typescript
// Old — required .toExpr() to extract raw AST
const sub = db.select(posts).columns(posts.userId).toExpr()
inSubquery(users.id, sub)

// New — pass builders directly
inSubquery(users.id, db.select(posts.userId).from(posts))
```

- `subquery()`, `exists()`, `inSubquery()`, `notInSubquery()` accept `QueryBuilder` (SelectBuilder or SetOperationBuilder)
- `withCTE()` accepts builders for CTE queries and main query, returns `QueryBuilder`
- `insertFrom()` and `executeQuery()` accept builders directly
- `toExpr()` marked `@internal` — no longer part of the public API
- `ToCreateAST` removed from public exports

### Version Bumps

| Component | Version |
|-----------|---------|
| quarry | @petradb/quarry 1.5.2 |

## v1.5-20260317

### Bug Fixes

- **JSON parser escape sequences** — fixed backslash escape handling (`\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\r`, `\t`, `\uXXXX`) in JSON string values
- **JSON top-level values** — the JSON parser now accepts all JSON value types (strings, numbers, booleans, null) at the top level, not just objects and arrays
- **JSON path operators** — `#>` and `#>>` now correctly handle text array literals as path arguments
- **JSON containment operators** — `@>` and `<@` now work correctly with text JSON literals (not just parsed JSON objects)
- **Persistent DB auto-create** — `petradb_open_persistent` (C API) now auto-creates new databases instead of failing

### Python Package

- New `petradb` Python package with bundled native library (Linux x86_64)
- SQLite-style API: `Database`, `Cursor`, context managers, parameterized queries
- Persistent and in-memory modes
- Published to PyPI

### FFI Tests

- Go FFI test suite for the C library
- Python FFI test suite for the C library

### Tooling

- Quarry smoke test added to post-publish verification
- Internationalization: 5 languages + Korean translations for docs site
- `publish.sh` fix: uses `npx -p typescript tsc` to avoid wrong `tsc` package

### Version Bumps

All components bumped to 1.5.1 (Python to 1.5.2):

| Component | Maven Central | npm | PyPI |
|-----------|---------------|-----|------|
| common | 1.5.1 | — | — |
| engine | 1.5.1 | @petradb/engine 1.5.1 | — |
| client | 1.5.1 | @petradb/client 1.5.1 | — |
| server | — | @petradb/server 1.5.1 | — |
| cli | — | @petradb/cli 1.5.1 | — |
| jdbc | 1.5.1 | — | — |
| drizzle | — | @petradb/drizzle 1.5.1 | — |
| knex | — | @petradb/knex 1.5.1 | — |
| lucid | — | @petradb/lucid 1.5.1 | — |
| quarry | — | @petradb/quarry 1.5.1 | — |
| python | — | — | petradb 1.5.2 |

## v1.5-20260315

### PL/pgSQL — Stored Procedures, Functions, and Triggers

Full procedural language support in DO blocks, stored functions, and stored procedures:

- **DO blocks** — anonymous PL/pgSQL blocks with DECLARE, BEGIN...END
- **Stored functions** — `CREATE FUNCTION name(params) RETURNS type AS $$ ... $$ LANGUAGE plpgsql`, callable in any SQL expression
- **Stored procedures** — `CREATE PROCEDURE name(params) AS $$ ... $$ LANGUAGE plpgsql`, invoked with `CALL`
- **Triggers** — `CREATE TRIGGER name BEFORE|AFTER INSERT|UPDATE|DELETE ON table FOR EACH ROW EXECUTE FUNCTION func()`
  - BEFORE triggers can cancel operations by returning NULL
  - AFTER triggers fire after the operation
  - Triggers fire for INSERT, UPDATE, DELETE, and COPY FROM
  - TG_OP, TG_TABLE_NAME, OLD, NEW variables available
- **Control flow** — IF/ELSIF/ELSE, WHILE LOOP, FOR range/query LOOP, RETURN, RAISE NOTICE/EXCEPTION, PERFORM, EXCEPTION WHEN
- **Persistence** — functions, procedures, and triggers survive close/reopen on PersistentDB and TextDB
- **OR REPLACE** — overwrite existing functions and procedures

### User-Defined Native Functions

Register host-language callbacks as SQL functions, callable from queries, triggers, and procedures:

- **Scala** — `db.registerScalarFunction("name", { args => result })`
- **JavaScript** — `session.registerFunction("name", (args) => result)`
- **C** — `petradb_create_function(db, "name", nargs, user_data, callback)` with SQLite-style typed value/context API

### C Library and Cursor API

- **Native shared library** — `libpetradb-engine.so` via Scala Native with `@exported` C-callable functions
- **SQLite-style C API** — `petradb_open`, `petradb_exec`, `petradb_prepare/step/finalize`, typed column accessors
- **User-defined functions** — `petradb_value_int/double/text`, `petradb_result_int/double/text/null/error`, `petradb_user_data`
- **Cursor API** — `session.openCursor(sql)` for lazy row-by-row iteration with `step()`, typed column accessors, `fetch(n)`, `move(n)`, parameterized queries
- **C header** — `petradb.h` with full API documentation
- **C test suite** — 67 tests
- **Rust FFI test** — 38 tests proving cross-language interop

### Virtual Tables

- **Extensible framework** — `CREATE VIRTUAL TABLE name USING module(args)`, read-only, appears in SHOW TABLES
- **Built-in CSV module** — `CREATE VIRTUAL TABLE t USING csv('file.csv')` with header/delimiter options
- **Custom modules** — `db.registerVirtualTableModule("name", module)` in Scala API

### csv_file() Table Function

Query CSV files directly without importing:

```sql
SELECT * FROM csv_file('data.csv');
SELECT e.name, d.dept FROM csv_file('employees.csv') e
  JOIN csv_file('departments.csv') d ON e.dept_id = d.id;
```

### Advanced Indexes

- **Partial indexes** — `CREATE INDEX ... WHERE condition` — only index rows matching the predicate
- **Expression indexes** — `CREATE INDEX ... ON table ((expr))` — index computed values like `lower(email)`
- **Combined** — partial + expression indexes work together

### Window Functions

- **FIRST_VALUE(expr)** — value at the first row of the window frame
- **LAST_VALUE(expr)** — value at the last row of the window frame
- **NTH_VALUE(expr, n)** — value at the nth row of the frame

### DELETE ... USING

Multi-table deletes matching PostgreSQL syntax:

```sql
DELETE FROM orders USING customers
WHERE orders.customer_id = customers.id AND customers.status = 'inactive';
```

### Quarry — Type-Safe AST Query Builder

New `@petradb/quarry` package: type-safe query builder that generates AST objects (not SQL strings):

- Schema definition with 21 column types
- Full CRUD: select, insert, update, delete with type-safe column references
- Joins: inner, left, right, full outer, cross with typed results
- Expressions: 50+ operators, aggregates, CASE/CAST/EXISTS, subqueries
- Upsert: `onConflictDoNothing()`, `onConflictDoUpdate()`
- Table aliases for self-joins
- Transactions, RETURNING, DISTINCT ON
- Compile-time type tests for all features
- Set operations: UNION, INTERSECT, EXCEPT
- Window functions, CTEs, named scalar helpers

### Bug Fixes

- **ByteaValue** — `ARRAY[...]` into BYTEA columns now correctly produces `ByteaValue` instead of `ArrayValue`
- **JS/Client result types** — added missing PL/pgSQL result type handlers (DoBlockResult, CreateFunctionResult, etc.) to prevent non-exhaustive match crashes
- **Codecs** — added serialization for all new result types for client/server communication
- **llms.txt** — fixed `type` field to `command` field, updated all result types and features

### Module Rename

- **shared → common** — renamed the shared types module from `petradb-shared` to `petradb-common`

### Documentation

- New **PL/pgSQL** reference page (triggers, functions, procedures, control flow)
- New **C API** reference page (full SQLite-style interface)
- New **Getting Started** guides for Java (JDBC) and C
- Updated DDL docs: partial/expression indexes, CHECK constraints, triggers, stored routines
- Updated DML docs: DELETE...USING, csv_file(), virtual tables
- Updated JS/Scala API docs: registerFunction, result types
- Landing page: four getting-started buttons (JS, Java, Scala, C)
- Rewritten llms.txt with all current features

### Version Bumps

| Component | Maven Central | npm |
|-----------|---------------|-----|
| common | 1.5.0 | — |
| engine | 1.5.0 | @petradb/engine 1.5.0 |
| client | 1.5.0 | @petradb/client 1.5.0 |
| server | 1.5.0 | @petradb/server 1.5.0 |
| cli | 1.5.0 | @petradb/cli 1.5.0 |
| jdbc | 1.5.0 | — |
| drizzle | — | @petradb/drizzle 1.5.0 |
| knex | — | @petradb/knex 1.5.0 |
| lucid | — | @petradb/lucid 1.5.0 |
| quarry | — | @petradb/quarry 1.5.0 |

## v1.4-20260314

### Bug fixes & improvements

- **Correlated IN subquery fix** — correlated `IN (SELECT ...)` with indexed tables now works correctly
- **Qualified column resolution** — fixes for ambiguous column references in complex joins
- **Empty IN/ANY handling** — `IN ()` and `= ANY('{}')` edge cases resolved
- **Foreign key CASCADE cleanup** — `DROP TABLE ... CASCADE` now properly removes FK constraints on child tables
- **DROP TABLE IF EXISTS ... CASCADE** — combining `IF EXISTS` with `CASCADE` no longer causes a parse error
- **Array literal casting** — support for `'{1,2,3}'::integer[]` PostgreSQL array literal syntax
- **Parameterized query fixes** — improved parameter binding for subqueries and correlated paths

### Version bumps

| Component | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.4.2 | — |
| engine | 1.4.9 | @petradb/engine 1.4.3 |
| client | 1.4.2 | @petradb/client 1.4.2 |
| server | — | @petradb/server 1.4.3 |
| cli | — | @petradb/cli 1.4.3 |
| jdbc | 1.4.3 | — |
| drizzle | — | @petradb/drizzle 1.4.3 |

## v1.4-20260312

### Common Table Expressions (CTEs)

Full CTE support with `WITH` and `WITH RECURSIVE`.

**Non-recursive CTEs** — named subqueries for readability and reuse:

```sql
WITH active_orders AS (
  SELECT * FROM orders WHERE status = 'active'
)
SELECT customer_id, SUM(amount)
FROM active_orders
GROUP BY customer_id;
```

Multiple CTEs can be defined in a single query, and later CTEs can reference earlier ones. Column aliases are supported: `WITH t(x, y) AS (...)`. CTEs shadow table names if they share the same name.

**Recursive CTEs** — iterative queries for hierarchical and graph data:

```sql
WITH RECURSIVE descendants(id, name, depth) AS (
  SELECT id, name, 0 FROM employees WHERE manager_id IS NULL
  UNION ALL
  SELECT e.id, e.name, d.depth + 1
  FROM employees e INNER JOIN descendants d ON e.manager_id = d.id
)
SELECT name, depth FROM descendants ORDER BY depth, name;
```

Both `UNION ALL` (keep duplicates) and `UNION` (deduplicated) are supported. Maximum 1000 iterations as a safety limit.

### Window functions

Full window function support with three categories:

**Ranking functions** — `ROW_NUMBER()`, `RANK()`, `DENSE_RANK()` with `PARTITION BY` and `ORDER BY`:

```sql
SELECT name, department, salary,
  RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dept_rank
FROM employees;
```

**Value functions** — `LAG()`, `LEAD()`, `NTILE()` with configurable offset and default values:

```sql
SELECT name, salary,
  LAG(salary, 1, 0) OVER (ORDER BY salary) AS prev_salary,
  NTILE(4) OVER (ORDER BY salary) AS quartile
FROM employees;
```

**Aggregate window functions** — any aggregate (`SUM`, `COUNT`, `AVG`, `MIN`, `MAX`, etc.) with `OVER()`, including frame specifications:

```sql
SELECT name, salary,
  SUM(salary) OVER (ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_total,
  AVG(salary) OVER (PARTITION BY department) AS dept_avg
FROM employees;
```

Frame bounds: `UNBOUNDED PRECEDING`, `UNBOUNDED FOLLOWING`, `CURRENT ROW`, `N PRECEDING`, `N FOLLOWING`. Without a frame clause, aggregate window functions compute over the entire partition.

### Aggregate FILTER clause

`FILTER (WHERE ...)` on aggregate functions, both in grouped queries and window functions:

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE status = 'active') AS active_count,
  SUM(amount) FILTER (WHERE amount > 100) OVER (ORDER BY created_at
    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_high_value
FROM orders;
```

### Hash join optimization

Equijoins without an index now use a hash join strategy instead of a cross product, reducing join complexity from O(n×m) to O(n+m). Applies to INNER, LEFT, RIGHT, and FULL joins. Index nested loop joins remain preferred when an index is available. Non-equijoin conditions still fall back to cross product. Visible in `EXPLAIN` output as `Hash Join`, `Hash Left Join`, `Hash Right Join`, `Hash Full Join`.

### Generated columns

`GENERATED ALWAYS AS (expr) STORED` computed columns:

```sql
CREATE TABLE products (
  price NUMERIC,
  tax_rate NUMERIC DEFAULT 0.08,
  total NUMERIC GENERATED ALWAYS AS (price * (1 + tax_rate)) STORED
);
```

Generated columns are recomputed on INSERT and UPDATE. They cannot be set directly.

### ORDER BY null ordering

`ORDER BY` now defaults to SQL-standard null ordering: `ASC` → `NULLS LAST`, `DESC` → `NULLS FIRST`. Explicit `NULLS FIRST` / `NULLS LAST` overrides are supported.

### Sequence support

Full PostgreSQL-compatible sequence support. `CREATE SEQUENCE` and `DROP SEQUENCE` with options (`INCREMENT BY`, `START WITH`, `MINVALUE`, `MAXVALUE`, `CYCLE`, `IF NOT EXISTS` / `IF EXISTS`). Sequence functions: `nextval()`, `currval()`, `setval()`, `lastval()`.

`SERIAL`, `SMALLSERIAL`, and `BIGSERIAL` columns now create backing sequences (named `<table>_<column>_seq`), matching PostgreSQL behavior. `DROP TABLE` cascades to drop owned sequences. `TRUNCATE` resets backing sequences. Sequence state is fully transactional — `ROLLBACK` restores sequence counters. Persistent databases serialize sequence state to the catalog.

New SQL commands: `SHOW SEQUENCES`, `SHOW INDEXES` (all indexes across all tables).

CLI: new `\ds` (list sequences) and `\di` (list indexes) meta-commands.

### CREATE INDEX USING clause

`CREATE INDEX ... USING btree` syntax is now accepted (btree is the only supported method). This improves compatibility with PostgreSQL-generated DDL and ORMs.

### Bug fixes

- `ORDER BY` with NULL values: comparators now return 0 when both values are NULL, fixing non-deterministic sort results with multiple sort keys
- `ORDER BY` alias resolution: SELECT aliases (e.g. `SELECT x AS y ... ORDER BY y`) now resolve correctly in non-grouped queries, both with and without window functions
- `Type.convert()` null handling: NULL values passed through type conversion (e.g. via prepared statement parameters in UPDATE SET) are now preserved as NULL instead of being converted to the type's text representation. Fixed in 13 types: TEXT, VARCHAR, CHAR, UUID, TIMESTAMP, DATE, TIME, TIMETZ, INTERVAL, TIMESTAMPTZ, BYTEA, JSON, ENUM
- Exhaustive match warning in ORDER BY parser for nulls clause
- Silent errors in playground terminal for synchronous throws

### Version bumps

All components bumped to 1.4.1:

| Component | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.4.1 | — |
| engine | 1.4.1 | @petradb/engine 1.4.1 |
| client | 1.4.1 | @petradb/client 1.4.1 |
| server | 1.4.1 | @petradb/server 1.4.1 |
| cli | 1.4.1 | @petradb/cli 1.4.1 |
| jdbc | 1.4.1 | — |
| knex | — | @petradb/knex 1.4.0 |
| lucid | — | @petradb/lucid 1.4.0 |
| drizzle | — | @petradb/drizzle 1.4.1 |

## v1.3-20260309

### Transactional DDL

DDL statements (CREATE TABLE, CREATE INDEX, DROP TABLE, etc.) are now fully supported inside transactions and roll back atomically with DML. DDL and DML can be freely interleaved within a single BEGIN/COMMIT block. Both MemoryDB and PersistentDB capture a full catalog snapshot at BEGIN time and restore it on ROLLBACK.

### Drizzle relational queries

Full support for Drizzle ORM relational queries (`db.query.*.findMany()`, `db.query.*.findFirst()`). Added `json_build_array` and `json_build_object` scalar functions, and fixed parameter substitution in LATERAL subqueries.

### Version bumps

| Component | Maven Central | npm |
|-----------|---------------|-----|
| engine | 1.3.1 | @petradb/engine 1.3.3 |
| server | 1.3.1 | @petradb/server 1.3.1 |
| cli | 1.3.1 | @petradb/cli 1.3.1 |
| jdbc | 1.3.1 | — |
| drizzle | — | @petradb/drizzle 1.3.1 |

## v1.3-20260308

### Schema support

PostgreSQL-style schema namespaces. Every database has a `public` schema by default; unqualified table names resolve to `public`. Schema-qualified names (`schema.table`) work in all DDL and DML statements — CREATE TABLE, INSERT, UPDATE, DELETE, SELECT, ALTER TABLE, DROP TABLE, TRUNCATE, CREATE INDEX, and COPY.

```sql
CREATE SCHEMA inventory;
CREATE TABLE inventory.products (id SERIAL PRIMARY KEY, name TEXT);
INSERT INTO inventory.products (name) VALUES ('Widget');
SELECT * FROM inventory.products;
```

### information_schema virtual tables

`information_schema.schemata`, `information_schema.tables`, and `information_schema.columns` are now queryable. Schema-qualified tables report their correct `table_schema`. These views are generated dynamically from database metadata.

### Drizzle ORM migrations

New `migrate()` function in `@petradb/drizzle` applies Drizzle Kit migration files. Reads the `meta/_journal.json` and executes SQL migration files in order, tracking applied migrations in `drizzle.__drizzle_migrations`.

```typescript
import { migrate } from "@petradb/drizzle";
await migrate(db, { migrationsFolder: "./drizzle" });
```

### JDBC metadata improvements

`DatabaseMetaData.getColumns()` now returns accurate `COLUMN_SIZE`, `DECIMAL_DIGITS`, and `CHAR_OCTET_LENGTH` values based on column type and precision/scale declarations.

### Version bumps

All components bumped to 1.3.0:

| Component | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.3.0 | — |
| engine | 1.3.0 | @petradb/engine 1.3.0 |
| client | 1.3.0 | @petradb/client 1.3.0 |
| server | 1.3.0 | @petradb/server 1.3.0 |
| cli | 1.3.0 | @petradb/cli 1.3.0 |
| jdbc | 1.3.0 | — |
| knex | — | @petradb/knex 1.3.0 |
| lucid | — | @petradb/lucid 1.3.0 |
| drizzle | — | @petradb/drizzle 1.3.0 |

## v1.2-20260308

### Drizzle ORM driver rewrite

`@petradb/drizzle` rewritten from a `drizzle-orm/pg-proxy` wrapper to a custom PostgreSQL dialect driver extending `PgSession`/`PgPreparedQuery`/`PgTransaction` directly. This gives full feature parity with `drizzle-orm/node-postgres`:

- `db.transaction()` with automatic commit/rollback
- `tx.rollback()` for explicit rollback
- `returning()` on insert/update/delete (including partial column selection)
- Relational query support (pending engine support for `json_build_array`/`json_agg`)

### Type coercion: text parameters to NUMERIC columns

`NumericType.convert` now accepts `TextValue` and parses it as a `BigDecimal`, matching the existing coercion behavior of `IntegerType`, `BigintType`, `SmallintType`, and `DoubleType`. This fixes parameterized INSERT/UPDATE via ORMs that send numeric values as text (standard PostgreSQL wire protocol behavior).

### Three-valued NULL logic

Full SQL three-valued logic for NULL handling:

- Comparison operators (`=`, `!=`, `<`, `>`, `<=`, `>=`) return NULL when either operand is NULL
- `AND`/`OR` implement proper three-valued truth tables (e.g., `FALSE AND NULL` → `FALSE`, `TRUE OR NULL` → `TRUE`)
- `IN`/`NOT IN` propagate NULL correctly (e.g., `3 NOT IN (1, 2, NULL)` → unknown)
- Arithmetic (`+`, `-`, `*`, `/`, `%`) and string concatenation (`||`) propagate NULL
- `LIKE` handles NULL operands

### Unified expression grammar

The SQL parser's separate `expression` and `booleanExpression` hierarchies have been merged into a single expression syntax. Boolean operators (`AND`, `OR`, `NOT`) are now regular operators in the precedence chain. This allows boolean expressions anywhere an expression is valid (e.g., `SELECT a > 5 AND b < 10`).

### Eager column reference validation

Column references in `WHERE`, `GROUP BY`, `HAVING`, and `ORDER BY` are now validated eagerly at query plan construction time, catching nonexistent columns even on empty tables or single-row sorts. Previously, bad references were only detected at eval time per-row, so queries against empty tables silently succeeded.

### Bug fixes

- NOT NULL constraint not enforced on UPDATE
- UNIQUE constraint rejected multiple NULLs (SQL standard: NULLs are distinct)
- Duplicate columns in INSERT column list not detected
- `SUM`/`AVG`/`MIN`/`MAX` on empty table returned 0 instead of NULL
- `LIKE '_'` matched empty string
- `LIMIT 0` threw an error

### Version bumps

| Component | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | — |
| engine | 1.2.9 | @petradb/engine 1.2.16 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | 1.2.6 | @petradb/server 1.2.9 |
| cli | 1.2.8 | @petradb/cli 1.2.9 |
| jdbc | 1.2.13 | — |
| knex | — | @petradb/knex 1.2.2 |
| lucid | — | @petradb/lucid 1.2.1 |
| drizzle | — | @petradb/drizzle 1.2.2 |

## v1.2-20260307

### SQL: `DEFAULT` keyword in INSERT VALUES

`INSERT INTO t (id, name) VALUES (DEFAULT, 'Alice')` now works. The SQL-standard `DEFAULT` keyword was previously rejected by the parser, breaking ORM-generated INSERT statements that explicitly pass `DEFAULT` for serial or defaulted columns.

### Drizzle ORM integration

New `@petradb/drizzle` package provides a [Drizzle ORM](https://orm.drizzle.team) driver with a custom PostgreSQL dialect implementation. Supports schema definitions with `pgTable`, insert/select/update/delete, returning clauses, `db.transaction()` with automatic commit/rollback, and type-safe queries. Full feature parity with `drizzle-orm/node-postgres`.

### Version bumps for dependent packages

Engine, server, cli, and jdbc bumped to pick up the DEFAULT keyword fix.

| Component | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | — |
| engine | 1.2.7 | @petradb/engine 1.2.14 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | — | @petradb/server 1.2.7 |
| cli | — | @petradb/cli 1.2.7 |
| jdbc | 1.2.11 | — |
| knex | — | @petradb/knex 1.2.2 |
| lucid | — | @petradb/lucid 1.2.1 |
| drizzle | — | @petradb/drizzle 1.2.0 |

## v1.2-20260306

### JS API: `close()` returns `Promise<void>`
`Session.close()` now returns `Promise<void>` instead of `void`, matching the client module's API for interchangeability.

### Timestamp parsing
`parseTimestamp` now handles `Z` suffix, `+/-HH:MM` offsets, milliseconds, and space-separated timestamps with timezone info. Strips timezone to `LocalDateTime` for `TIMESTAMP` columns.

### JS facade completeness
`toJS` and `typeString` now handle `DateValue`, `TimeValue`, `TimestampTZValue`, `TimeTZValue`, `IntervalValue`, and `ByteaValue`.

### SQL: qualified star (`table.*`)
`SELECT t.*` syntax now works in queries, including joins and mixed expressions.

### Type coercion in comparisons
- `NumberValue` and `TextValue` can now compare across types (text parameters vs numeric columns and vice versa)
- `TimestampValue` can now compare against `TextValue` by parsing the text as a timestamp

### Knex driver: Date binding
`_sanitizeBindings` converts JS `Date` objects to ISO strings before passing to the engine, preventing `DateTimeParseException` on `Date.toString()` format.

| Component | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | — |
| engine | 1.2.6 | @petradb/engine 1.2.13 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | — | @petradb/server 1.2.5 |
| cli | — | @petradb/cli 1.2.5 |
| jdbc | 1.2.10 | — |
| knex | — | @petradb/knex 1.2.2 |
| lucid | — | @petradb/lucid 1.2.1 |

## v1.2-20260305

### JDBC driver
- Fat jar publishing — `io.github.edadma:petradb-jdbc` is now a single self-contained jar on Maven Central
- Clean connection URLs — `jdbc:petradb:memory`, `jdbc:petradb:file:/path`, `jdbc:petradb://host:port`
- ServiceLoader auto-discovery — `DriverManager.getConnection()` works without `Class.forName`
- Fixed hardcoded metadata version strings

### JS/TS engine (`@petradb/engine`)
- Added `CreateViewResult`, `DropViewResult`, `ExplainResult`, `CopyResult` to JS facade
- Added `ExplainResult` and `CopyResult` to TypeScript type definitions

### Documentation
- New Knex.js guide with full examples
- JDBC docs: added Maven/Gradle/sbt install snippets, fixed port number

### Infrastructure
- `petradb-shared` now publishable to Maven Central
- Post-publish smoke test script covering npm, Scala, and JDBC artifacts

| Component | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.1 | — |
| engine | 1.2.2 | @petradb/engine 1.2.5 |
| client | 1.2.3 | @petradb/client 1.2.3 |
| server | — | @petradb/server 1.2.3 |
| cli | — | @petradb/cli 1.2.3 |
| jdbc | 1.2.6 | — |
| knex | — | @petradb/knex 1.2.0 |

## v1.2.2

### Engine subpackage restructure
- Engine moved to `io.github.edadma.petradb.engine` subpackage
- New shared `Session` trait extended by both engine and client

### CLI client support
- Connect to a remote PetraDB server: `petradb --host localhost --port 5480`
- `--user` and `--password` flags for authentication
- Meta-commands work over the network via SQL

### SQL
- `SHOW VIEWS` command returns view names and definitions

### Knex dialect
- `@petradb/knex` dialect adapter for using Knex.js query builder with PetraDB

### Fixes
- Fix client npm publish
- Fix CLI npm publish
- Fix hardcoded JDBC metadata version strings
- 1013+ tests passing across JVM, JS, and Native

## v1.2

### JDBC driver
- Published to Maven Central as `petradb-jdbc`
- `getGeneratedKeys()`, `addBatch()`/`executeBatch()`, FK/index metadata for DBeaver
- File mode (embedded) and server mode (network) connections

### SQL engine
- `COPY FROM/TO` for CSV import/export
- `CREATE TEMP TABLE`, `CREATE/DROP VIEW`
- `SHOW FOREIGN KEYS`/`SHOW INDEXES` introspection
- Index nested loop join optimization for equijoins
- Migrated parser to fastparse

### Server
- CORS support with TOML configuration
- Configurable `max_sessions`, default port 5480
- JS server platform with Node.js HTTP backend

### Client
- New `@petradb/client` npm package with JS facade
- `Session` class with `connect()`/`execute()`/`close()` returning Promises

### CLI
- `\timing`, `\copy` commands
- Persistent history on Native

### Build
- Scala 3.8.2, sbt 1.12.4
- 1000 tests passing across JVM, JS, and Native

## v1.1.0

### TextDB — human-editable text file persistence
A new storage backend that persists the database as a `.ptxt` text file. Loads into memory on open and rewrites the file after every change.

### Upsert — `ON CONFLICT DO UPDATE`
Insert-or-update semantics with the `EXCLUDED` pseudo-table.

### Improved exception hierarchy
Typed exception classes replace generic `problem()` calls.

### ALTER TABLE centralised
`DB.alterTable()` now centralises all ALTER TABLE dispatch.

## v1.0.1

- Rename `ConnectSQL` to `Session` in `@petradb/engine`
- Async `execute()` API returning `Promise<ExecuteResult[]>`
- New `@petradb/client` package for network usage
- Aligned response formats between engine and server

## v1.0.0

First stable release.

- Cross-platform SQL engine (JVM, JavaScript, Native)
- PostgreSQL-compatible syntax
- In-memory and persistent (crash-safe) storage
- DDL, DML, joins, subqueries, aggregations, transactions
- JSONB operators, array types, CHECK constraints
- 879 passing tests

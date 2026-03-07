---
title: Changelog
---

## v1.2-20260307

### SQL: `DEFAULT` keyword in INSERT VALUES

`INSERT INTO t (id, name) VALUES (DEFAULT, 'Alice')` now works. The SQL-standard `DEFAULT` keyword was previously rejected by the parser, breaking ORM-generated INSERT statements that explicitly pass `DEFAULT` for serial or defaulted columns.

### Drizzle ORM integration

New `@petradb/drizzle` package provides a [Drizzle ORM](https://orm.drizzle.team) driver via `drizzle-orm/pg-proxy`. Supports schema definitions with `pgTable`, insert/select/update/delete, returning clauses, and type-safe queries. Transactions use `db.$session` for manual `BEGIN`/`COMMIT`/`ROLLBACK`.

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

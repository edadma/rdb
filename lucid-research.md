# Lucid Driver Research

## The Problem

Lucid v22 has a **hardcoded allowlist** of client names. There is no plugin API or `registerDriver()` method.

### Two validation gates

1. **Lucid's check** (`connection/index.js` lines 67-74): validates `config.client` string against `clientsToDialectsMapping` keys: `mssql`, `mysql`, `mysql2`, `oracledb`, `postgres`, `redshift`, `sqlite3`, `libsql`, `better-sqlite3`

2. **Knex's check** (`knex/lib/knex-builder/internal/config-resolver.js`): has its own `SUPPORTED_CLIENTS` list, BUT has an escape hatch — if `config.client` is a **function** (constructor), it uses it directly and skips the string lookup

### How libsql got in

Lucid added `libsql` to its allowlist natively. In `getWriteConfig()` it swaps the string `'libsql'` for the actual `LibSQLClient` class before passing to Knex:
```js
if (this.config.client === 'libsql') {
    return { ...this.config, client: LibSQLClient };
}
```

## What a Driver Needs

### Lucid Dialect (`DialectContract`)

- `name`: string (one of the hardcoded names)
- `dateTimeFormat`: string
- Boolean flags: `supportsAdvisoryLocks`, `supportsViews`, `supportsTypes`, `supportsDomains`, `supportsReturningStatement`
- Methods: `getAllTables()`, `dropAllTables()`, `getAllViews()`, `dropAllViews()`, `getAllTypes()`, `dropAllTypes()`, `getAllDomains()`, `dropAllDomains()`, `truncate()`, `truncateAllTables()`, `getAdvisoryLock()`, `releaseAdvisoryLock()`

### Knex Client

Must extend `knex/lib/client`. Override: `_driver()`, `dialect`, `driverName`, `acquireRawConnection()`, `destroyRawConnection()`, `_query()`, `processResponse()`, query/schema/table/column compilers.

We already have this: `@petradb/knex` (PetraDBClient).

## Implementation Strategy: Monkey-patch

Since there's no extension API, the approach is to monkey-patch Lucid's internals at startup:

1. **Mutate `clientsToDialectsMapping`** — add `'petradb': PetraDBDialect` to the mapping object before any connections are created
2. **Create `PetraDBDialect`** — implement `DialectContract`, modeled after `BaseSqliteDialect` / PostgreSQL dialect (PetraDB supports views, types, returning statements)
3. **Reuse `@petradb/knex` client** — already implements the Knex client layer
4. **Intercept connection config** — swap `'petradb'` string for the PetraDBClient class constructor, mimicking the libsql pattern

### Package shape: `@petradb/lucid`

```
@petradb/lucid
├── src/
│   ├── index.ts          # configure() export that patches Lucid
│   ├── dialect.ts         # PetraDBDialect implementing DialectContract
│   └── define_config.ts   # typed config helper
├── package.json
└── README.md
```

Usage in AdonisJS app:
```ts
// config/database.ts
import { defineConfig } from '@petradb/lucid'

export default defineConfig({
  connection: 'petradb',
  connections: {
    petradb: {
      client: 'petradb',
      connection: { storage: 'memory' }
    }
  }
})
```

```ts
// providers/database_provider.ts or start/routes.ts
import '@petradb/lucid' // side-effect import patches Lucid
```

## Key Source Files (in node_modules/@adonisjs/lucid/)

| File | What it does |
|---|---|
| `build/src/dialects/index.js` | `clientsToDialectsMapping` — the allowlist |
| `build/src/connection/index.js:67-74` | Client name validation |
| `build/src/connection/index.js:143-148` | libsql string→class swap (template) |
| `build/src/query_client/index.js:48` | Dialect instantiation from mapping |
| `build/src/types/database.d.ts` | `DialectContract`, `ConnectionContract`, `QueryClientContract` |
| `build/src/dialects/libsql.js` | LibSQL dialect (closest template) |
| `build/src/dialects/base_sqlite.js` | Base SQLite dialect (shared logic) |
| `build/src/dialects/pg.js` | PostgreSQL dialect (PetraDB is PG-compatible) |
| `build/src/clients/libsql.cjs` | LibSQL knex client (template) |

## PetraDB Dialect Capabilities

Based on PetraDB's SQL support:

| Feature | Supported | Notes |
|---|---|---|
| `supportsViews` | yes | CREATE/DROP VIEW |
| `supportsTypes` | yes | CREATE TYPE AS ENUM |
| `supportsDomains` | no | |
| `supportsReturningStatement` | yes | INSERT/UPDATE/DELETE RETURNING |
| `supportsAdvisoryLocks` | no | |
| `getAllTables()` | yes | SHOW TABLES |
| `getAllViews()` | yes | SHOW VIEWS |
| `getAllTypes()` | yes | needs implementation |
| `truncate()` | yes | TRUNCATE TABLE |

## Risks

- **Monkey-patching is fragile** — Lucid upgrades could break the patch if they rename internal modules or change the mapping structure
- **TypeScript types are closed** — `ConnectionConfig` is a union type, so TypeScript won't accept `'petradb'` without type assertions or module augmentation
- **No upstream interest** — AdonisJS team is unlikely to add PetraDB to the allowlist natively without significant adoption

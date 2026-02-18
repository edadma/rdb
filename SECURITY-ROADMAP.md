# RDB Security Roadmap

This document outlines the plan for adding PostgreSQL-compatible database security to rdb. The goal is to build a solid authorization foundation so that, combined with indexes and full ACID compliance, rdb can be used in production-grade applications.

The design follows PostgreSQL's security model closely, adapted to rdb's architecture. Features are organized into phases, each building on the previous.

---

## Phase 1: Roles

**Objective:** Establish identity — every database session has a current role.

### SQL Syntax

```sql
CREATE ROLE name [WITH option [...]];
DROP ROLE name;
ALTER ROLE name [WITH option [...]];

-- Options:
--   SUPERUSER | NOSUPERUSER
--   CREATEDB | NOCREATEDB
--   CREATEROLE | NOCREATEROLE
--   LOGIN | NOLOGIN
--   PASSWORD 'secret'
--   INHERIT | NOINHERIT
--   IN ROLE role [, ...]
```

`CREATE USER` is an alias for `CREATE ROLE ... LOGIN`.

### Role Attributes

| Attribute | Default | Description |
|-----------|---------|-------------|
| `SUPERUSER` | no | Bypasses all permission checks |
| `CREATEDB` | no | Can create new databases |
| `CREATEROLE` | no | Can create/drop/alter other roles |
| `LOGIN` | no | Can authenticate (users vs. group roles) |
| `INHERIT` | yes | Automatically inherits privileges of member roles |
| `PASSWORD` | none | For authentication (Phase 6) |

### Role Membership

```sql
GRANT role TO role [, ...] [WITH ADMIN OPTION];
REVOKE role FROM role [, ...];
```

Role membership enables privilege inheritance: if role `analyst` is granted to role `alice`, then `alice` inherits all of `analyst`'s object privileges (when `INHERIT` is set).

### Built-in Roles

- **`superuser`** — Created automatically when a database is initialized. Has `SUPERUSER` and `LOGIN`. Owns all objects created before roles existed.

### Catalog Storage

A role catalog persisted alongside the existing table and type catalogs in `PersistentDB`:

- Role name, attributes (bitfield), password hash
- Role membership edges (role → member, with admin flag)

For `MemoryDB`, roles live in an in-memory map with the same structure.

### Implementation Notes

- `SQLParser`: add `CREATE/ALTER/DROP ROLE`, `CREATE USER`
- `Command.scala`: add `CreateRoleCommand`, `DropRoleCommand`, `AlterRoleCommand`
- `DB.scala`: add role catalog, role lookup/validation methods
- `executeSQL.scala`: dispatch new commands
- `Serialization.scala`: serialize/deserialize role catalog for `PersistentDB`

---

## Phase 2: Session Context

**Objective:** Give each database session an identity and configurable state that the rest of the security system can query.

### Session Identity

```sql
SET ROLE name;       -- switch effective role (must be a member of target role)
RESET ROLE;          -- revert to the session's original authenticated role
```

The `DB` trait gains two role fields:

- `sessionUser: String` — the role that originally authenticated (immutable for the session's lifetime)
- `currentUser: String` — the effective role, changed by `SET ROLE` / `RESET ROLE`

All privilege checks use `currentUser`. `SET ROLE` only succeeds if `sessionUser` is a member of the target role (or is a superuser).

### Built-in Identity Functions

| Function | Returns |
|----------|---------|
| `CURRENT_USER` | The effective role (`currentUser`) |
| `SESSION_USER` | The original authenticated role (`sessionUser`) |
| `CURRENT_DATABASE` | The name of the current database |
| `CURRENT_SCHEMA` | The first schema in `search_path` that exists |

These are usable anywhere an expression is valid — in `SELECT`, `WHERE`, `DEFAULT`, and later in RLS policies.

### Session Variables

```sql
SET variable = value;
SET variable TO value;
SHOW variable;
RESET variable;           -- revert to default
RESET ALL;                -- revert all to defaults
```

Built-in session variables:

| Variable | Default | Description |
|----------|---------|-------------|
| `search_path` | `"$user", public` | Schema resolution order (Phase 4) |
| `role` | session user | Effective role |
| `timezone` | `UTC` | Session time zone |

### Application-Defined Variables

Following PostgreSQL's convention, applications can define custom namespaced variables:

```sql
SET app.current_tenant = '42';
SET myapp.debug_mode = 'true';
```

Any variable name containing a dot is treated as an application-defined variable. These are stored in the session state and can be read back with `SHOW` or referenced via `current_setting('app.current_tenant')`. This is particularly useful for row-level security policies (Phase 5):

```sql
-- RLS policy using an application variable
CREATE POLICY tenant_isolation ON orders
  USING (tenant_id = current_setting('app.current_tenant')::integer);
```

### The `current_setting` Function

```sql
current_setting('variable_name')                    -- error if not set
current_setting('variable_name', true)              -- returns NULL if not set
```

Returns the current value of any session variable (built-in or application-defined) as text.

### Implementation Notes

- `DB.scala`: add `sessionUser`, `currentUser`, `sessionVariables: Map[String, String]`, `setRole()`, `resetRole()`, `setVariable()`, `getVariable()`
- `SQLParser`: add `SET`, `SHOW`, `RESET` statement parsing
- `Command.scala`: add `SetRoleCommand`, `ResetRoleCommand`, `SetVariableCommand`, `ShowVariableCommand`, `ResetVariableCommand`
- `executeSQL.scala`: dispatch new commands
- `ScalarFunction.scala`: add `current_user`, `session_user`, `current_database`, `current_schema`, `current_setting`
- `Variable.scala`: wire up `CURRENT_USER` and `SESSION_USER` as SQL variables

---

## Phase 3: Object Privileges

**Objective:** Control who can do what on which objects.

### SQL Syntax

```sql
GRANT privilege [, ...] ON TABLE table [, ...] TO role [, ...] [WITH GRANT OPTION];
REVOKE privilege [, ...] ON TABLE table [, ...] FROM role [, ...];

-- Shorthand
GRANT ALL PRIVILEGES ON TABLE table TO role;
REVOKE ALL PRIVILEGES ON TABLE table FROM role;
```

### Privilege Types

| Privilege | Applies To | Description |
|-----------|-----------|-------------|
| `SELECT` | table, column | Read rows |
| `INSERT` | table, column | Add rows |
| `UPDATE` | table, column | Modify rows |
| `DELETE` | table | Remove rows |
| `TRUNCATE` | table | Empty a table |
| `REFERENCES` | table, column | Create foreign keys referencing this table |

### Column-Level Privileges

```sql
GRANT SELECT (col1, col2) ON TABLE t TO role;
GRANT UPDATE (col1) ON TABLE t TO role;
```

When column-level privileges are granted, they are checked per-column. A table-level grant implies access to all columns.

### Ownership

- Every table has an owner (the role that created it).
- The owner implicitly has all privileges on the object and can `GRANT` them to others.
- Ownership can be transferred: `ALTER TABLE name OWNER TO role`.

### Privilege Resolution

When checking whether role R has privilege P on object O:

1. If R is a superuser → allow
2. If R owns O → allow
3. If R has been directly granted P on O → allow
4. For each role G that R is a member of (recursively, if `INHERIT`): if G has P on O → allow
5. Otherwise → deny with error: `permission denied for table <name>`

### Catalog Storage

A privilege catalog: `(grantor, grantee, object_type, object_name, privilege, column_name?, grant_option)` tuples. Persisted alongside roles.

### Permission Check Injection Points

Checks are added in `executeSQL.scala` before each operation:

| Operation | Required Privilege |
|-----------|--------------------|
| `SELECT` | `SELECT` on referenced tables/columns |
| `INSERT` | `INSERT` on target table/columns |
| `UPDATE` | `UPDATE` on target table/columns; `SELECT` if WHERE clause references columns |
| `DELETE` | `DELETE` on target table; `SELECT` if WHERE clause references columns |
| `CREATE TABLE` | (allowed for any role, ownership assigned to creator) |
| `DROP TABLE` | Must be owner or superuser |
| `ALTER TABLE` | Must be owner or superuser |
| `TRUNCATE` | `TRUNCATE` on target table |

### Implementation Notes

- `SQLParser`: add `GRANT`/`REVOKE` with privilege lists, object targets, role lists
- `Command.scala`: add `GrantCommand`, `RevokeCommand`
- `DB.scala`: add privilege catalog, `checkPrivilege(role, table, privilege)` method, table ownership tracking
- `executeSQL.scala`: inject privilege checks at each dispatch point
- `rewrite.scala`: collect referenced tables/columns during planning for privilege verification
- `Serialization.scala`: serialize/deserialize privilege catalog

---

## Phase 4: Schemas

**Objective:** Namespace isolation for tables and other objects.

### SQL Syntax

```sql
CREATE SCHEMA name [AUTHORIZATION role];
DROP SCHEMA name [CASCADE | RESTRICT];
ALTER SCHEMA name RENAME TO new_name;
ALTER SCHEMA name OWNER TO role;

SET search_path TO schema [, ...];
SHOW search_path;
```

### Behavior

- Tables are addressed as `schema.table`. Unqualified names resolve via `search_path`.
- Default schema: `public` (created automatically, all roles can create objects in it).
- Each role can have a personal schema matching their role name.
- `search_path` defaults to `"$user", public` — first looks in a schema matching the current role name, then `public`.

### Privileges on Schemas

| Privilege | Description |
|-----------|-------------|
| `CREATE` | Create objects in the schema |
| `USAGE` | Access objects in the schema (required in addition to object-level privileges) |

```sql
GRANT CREATE, USAGE ON SCHEMA name TO role;
```

### Implementation Notes

- Tables keyed by `(schema, name)` instead of just `name`
- `DB.scala`: schema catalog, `search_path` session variable, name resolution logic
- Privilege checks: `USAGE` on schema required before any object access, `CREATE` required to make new objects
- `SQLParser`: qualified identifiers (`schema.table`), `CREATE/DROP/ALTER SCHEMA`, `SET search_path`

---

## Phase 5: Row-Level Security

**Objective:** Restrict which rows a role can see or modify, per-table.

### SQL Syntax

```sql
ALTER TABLE name ENABLE ROW LEVEL SECURITY;
ALTER TABLE name DISABLE ROW LEVEL SECURITY;
ALTER TABLE name FORCE ROW LEVEL SECURITY;  -- applies to table owner too

CREATE POLICY name ON table
  [FOR {ALL | SELECT | INSERT | UPDATE | DELETE}]
  [TO role [, ...]]
  [USING (condition)]           -- filter for existing rows (SELECT, UPDATE, DELETE)
  [WITH CHECK (condition)];     -- filter for new/modified rows (INSERT, UPDATE)

DROP POLICY name ON table;
ALTER POLICY name ON table [TO role] [USING (condition)] [WITH CHECK (condition)];
```

### Behavior

- When RLS is enabled on a table, all queries by non-owner roles are filtered through applicable policies.
- Multiple policies for the same command are combined with `OR` (permissive) — any matching policy grants access.
- The `USING` expression can reference `CURRENT_USER`, `current_setting(...)`, and any column of the table.
- Superusers and table owners bypass RLS unless `FORCE ROW LEVEL SECURITY` is set.

### Example

```sql
-- Multi-tenant isolation using an application session variable
ALTER TABLE orders ENABLE ROW LEVEL SECURITY;

CREATE POLICY tenant_isolation ON orders
  USING (tenant_id = current_setting('app.current_tenant')::integer)
  WITH CHECK (tenant_id = current_setting('app.current_tenant')::integer);
```

### Implementation Notes

- Policies stored in the table catalog (per-table list of policy definitions)
- `rewrite.scala`: when RLS is enabled, inject an additional `SelectOperator` wrapping the table scan with the `USING` condition
- For `INSERT`/`UPDATE`, the `WITH CHECK` condition is evaluated after the mutation; if it fails, the operation is rolled back with an error
- Policies are expressions that go through the normal `rewrite` → `eval` pipeline

---

## Phase 6: Authentication

**Objective:** Verify the identity of connecting clients.

This phase only becomes relevant when rdb has a network server (TCP listener). Until then, the application embedding rdb sets `currentUser` directly.

### Password Storage

- Passwords stored as salted hashes (SCRAM-SHA-256, matching PostgreSQL's default).
- `ALTER ROLE name PASSWORD 'secret'` stores the hash, never plaintext.
- `ALTER ROLE name PASSWORD NULL` removes the password.

### Authentication Methods

Planned methods, in order of priority:

| Method | Description |
|--------|-------------|
| `trust` | No authentication (for local development) |
| `password` | Cleartext password (only over TLS) |
| `scram-sha-256` | Challenge-response, no password sent over wire |

### Host-Based Access Control

A configuration (analogous to `pg_hba.conf`) mapping connection source to authentication method:

```
# TYPE    DATABASE    USER    ADDRESS         METHOD
local     all         all                     trust
host      all         all     127.0.0.1/32    scram-sha-256
host      all         all     0.0.0.0/0       scram-sha-256
```

### Implementation Notes

- This phase is deferred until a network protocol/server layer exists
- For embedded use, the calling application is trusted to set the session role
- Password hashing uses SCRAM-SHA-256 (can be implemented with standard crypto primitives available on all three platforms)

---

## Phase Summary

| Phase | Feature | Depends On | Key Deliverables |
|-------|---------|------------|------------------|
| 1 | Roles | — | `CREATE/DROP/ALTER ROLE`, role catalog, membership |
| 2 | Session Context | Phase 1 | `SET ROLE`, `CURRENT_USER`, session variables, `current_setting()` |
| 3 | Object Privileges | Phase 2 | `GRANT`/`REVOKE`, ownership, privilege checks on all operations |
| 4 | Schemas | Phase 3 | Namespaces, `search_path`, schema privileges |
| 5 | Row-Level Security | Phase 3 | `CREATE POLICY`, per-row filtering injected into query plans |
| 6 | Authentication | Phase 1 | Password hashing, SCRAM-SHA-256, host-based access control |

Phases 1 through 3 together provide a complete, minimal authorization system suitable for multi-user applications. Phase 2 is also a prerequisite for Phase 5, since RLS policies rely on session identity and application variables.

---

## Design Principles

- **PostgreSQL compatibility:** Follow PostgreSQL's syntax and semantics wherever practical. Users familiar with PostgreSQL should feel at home.
- **Fail closed:** If a privilege check cannot determine access, deny. Missing grants mean no access.
- **Superuser escape hatch:** Superusers bypass all checks (except `FORCE ROW LEVEL SECURITY`). This keeps administration simple.
- **Minimal performance overhead:** Privilege checks happen once during query planning/dispatch, not per-row (except RLS). The common case (superuser or owner) short-circuits immediately.
- **Backend-agnostic:** Security metadata is part of the `DB` trait, so both `MemoryDB` and `PersistentDB` enforce the same rules.

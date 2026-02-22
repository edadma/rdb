---
title: API Reference
description: JavaScript/TypeScript and Scala API reference for PetraDB.
---

## JavaScript / TypeScript API

### `new Session(options?)`

Creates a new isolated in-memory database instance.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `rowMode` | `'object' \| 'array'` | `'object'` | Default row format for SELECT results |

### `db.execute(sql, options?)`

Executes one or more SQL statements separated by `;`. Returns a promise that resolves to an array of results.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `rowMode` | `'object' \| 'array'` | constructor default | Row format for this call |

```javascript
const [{ rows, fields }] = await db.execute('SELECT * FROM users');
```

### `db.prepare(sql)`

Creates a prepared statement with `$1`, `$2`, ... parameter placeholders. Returns a statement object with an `execute(params, options?)` method.

```javascript
const stmt = db.prepare('SELECT * FROM users WHERE id = $1');
const [{ rows }] = await stmt.execute([42]);

// With options
const [{ rows }] = await stmt.execute([42], { rowMode: 'array' });
```

### TypeScript Interfaces

```typescript
interface SessionOptions {
  rowMode?: 'object' | 'array';
}

interface ExecuteOptions {
  rowMode?: 'object' | 'array';
}

interface PreparedStatement {
  execute(params?: any[], options?: ExecuteOptions): Promise<ExecuteResult[]>
}

class Session {
  constructor(options?: SessionOptions)
  execute(sql: string, options?: ExecuteOptions): Promise<ExecuteResult[]>
  prepare(sql: string): PreparedStatement
}
```

### Result Types

Every result has a `command` field for discrimination:

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
{ command: 'insert', result: Record<string, any> }
{ command: 'select', rows: T[], fields: { name: string, dataType: string }[] }
{ command: 'update', rows: number }
{ command: 'delete', rows: number }

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

---

## Scala API

### In-Memory Database

```scala
import io.github.edadma.petradb.*

given Session = new MemoryDB().connect()
```

### Persistent Database

```scala
import io.github.edadma.petradb.*

// Create new
val db = PersistentDB.create("path/to/db", pageSize = 4096)
given Session = db.connect()

// Reopen existing
val db = PersistentDB.open("path/to/db")
given Session = db.connect()

// Close when done
db.close()
```

### `executeSQL(sql: String)(using Session): Seq[Result]`

Executes one or more semicolon-separated SQL statements and returns a sequence of results.

```scala
val results: Seq[Result] = executeSQL("SELECT * FROM users")
```

### Result Types

```scala
sealed trait Result
case class QueryResult(table: TableValue) extends Result
case class InsertResult(obj: Map[String, Value], table: TableValue) extends Result
case class CreateTableResult(table: String) extends Result
case class DropTableResult(table: String) extends Result
case class TruncateResult(table: String) extends Result
case class UpdateResult(rows: Int) extends Result
case class DeleteResult(rows: Int) extends Result
```

### Accessing Query Data

```scala
val QueryResult(table) = executeQuery("SELECT * FROM users")

// Access rows
val rows: IndexedSeq[Row] = table.data

for (row <- table.data) {
  val id: Int = row.getInt("id")
  val name: String = row.getString("name")
  val email: Option[String] = row.getStringOption("email")
}
```

### Value Extraction

```scala
val row: Row = table.data.head

// Type-safe extraction
val id: Int = row.getInt("id")
val name: String = row.getString("name")
val email: Option[String] = row.getStringOption("email")
val isActive: Boolean = row.getBoolean("is_active")

// Direct access
val value: Value = row("column_name")
```

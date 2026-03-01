---
title: Scala
description: Scala API reference for PetraDB.
---

## Installation

Add to your `build.sbt`:

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "<version>"
```

## In-Memory Database

```scala
import io.github.edadma.petradb.*

given Session = new MemoryDB().connect()
```

## Persistent Database

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

## Executing SQL

### `executeSQL(sql: String)(using Session): Seq[Result]`

Executes one or more semicolon-separated SQL statements and returns a sequence of results.

```scala
val results: Seq[Result] = executeSQL("SELECT * FROM users")
```

## Result Types

```scala
sealed trait Result
case class QueryResult(table: TableValue) extends Result
case class InsertResult(obj: Map[String, Value], table: TableValue) extends Result
case class CreateTableResult(table: String) extends Result
case class DropTableResult(table: String) extends Result
case class CreateViewResult(name: String) extends Result
case class DropViewResult(name: String) extends Result
case class TruncateResult(table: String) extends Result
case class UpdateResult(rows: Int) extends Result
case class DeleteResult(rows: Int) extends Result
case class CopyResult(count: Int) extends Result
```

## Accessing Query Data

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

## Value Extraction

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

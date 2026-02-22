---
title: Scala Usage
description: Using PetraDB from Scala on JVM, JS, and Native.
---

## Installation

Add to your `build.sbt`:

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.0.1"
```

The `%%%` operator selects the correct artifact for your platform (JVM, Scala.js, or Scala Native).

## In-Memory Database

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

Each `MemoryDB` instance is isolated and independent. All data lives in memory.

## Persistent Database

PetraDB supports crash-safe durable storage on JVM and Native via [stow](https://github.com/edadma/stow).

### Creating a New Database

```scala
import io.github.edadma.petradb.*

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
```

### Reopening an Existing Database

```scala
val db = PersistentDB.open("mydata.db")
given Session = db.connect()

val results = executeSQL("SELECT * FROM users")
results.foreach(println)

db.close()
```

All tables, data, enum types, and auto-increment state are restored when you reopen.

Persistent databases use copy-on-write pages and double-buffered headers for crash safety. All DDL and DML operations are durable.

## Executing SQL

`executeSQL(sql)` runs one or more semicolon-separated statements and returns a `Seq[Result]`.

```scala
val results: Seq[Result] = executeSQL("SELECT * FROM users; SELECT * FROM products;")
```

## Result Types

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

## Testing

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

## Platform Notes

- **JVM** — thread-safe, integrates with Spring Boot, Play Framework, Akka, etc.
- **Scala.js** — runs in Node.js and browsers; no persistent storage
- **Scala Native** — compiles to native executables; ideal for CLI tools and embedded systems

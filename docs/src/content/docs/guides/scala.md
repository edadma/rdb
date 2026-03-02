---
title: Scala Usage
description: Using PetraDB from Scala on JVM, JS, and Native.
---

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

## Text Database

`TextDB` stores the database as a human-editable `.ptxt` file. It loads into memory on open and rewrites the file after every change. Ideal for early development, configuration, and version control.

```scala
import io.github.edadma.petradb.*

val db = TextDB.open("mydata.ptxt")
given Session = db.connect()

executeSQL("""
  CREATE TABLE settings (key TEXT, value TEXT);
  INSERT INTO settings (key, value) VALUES ('theme', 'dark');
""")

db.close()
```

Reopen the same file to restore all data:

```scala
val db = TextDB.open("mydata.ptxt")
given Session = db.connect()

val results = executeSQL("SELECT * FROM settings")
results.foreach(println)
```

Works on JVM and Native. The `.ptxt` format is human-readable and diff-friendly. Use `PersistentDB` when you need crash-safe durability for production data.

## Executing SQL

`executeSQL(sql)` runs one or more semicolon-separated statements and returns a `Seq[Result]`.

```scala
val results: Seq[Result] = executeSQL("SELECT * FROM users; SELECT * FROM products;")
```

See the [Scala API reference](/reference/api-scala/) for result types, value extraction, and the full API.

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

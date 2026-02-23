---
title: Getting Started
description: Install PetraDB and run your first queries.
---

## Installation

### JavaScript / Node.js

```bash
npm install @petradb/engine
```

### Scala (SBT)

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.1.0"
```

## Quick Start

### JavaScript / TypeScript

```javascript
import { Session } from '@petradb/engine';

const db = new Session();

// Create a table
await db.execute(`
  CREATE TABLE users (
    id SERIAL,
    name TEXT NOT NULL,
    email TEXT,
    created_at TIMESTAMP
  )
`);

// Insert data
await db.execute(`
  INSERT INTO users (name, email, created_at)
  VALUES ('John Doe', 'john@example.com', CURRENT_TIMESTAMP)
`);

// Query data — rows are objects by default
const [{ rows, fields }] = await db.execute('SELECT * FROM users');
console.log(fields); // [{ name: 'id', dataType: 'serial' }, ...]
console.log(rows);   // [{ id: 1, name: 'John Doe', ... }]
```

### Scala (In-Memory)

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

### Scala (Persistent)

```scala
import io.github.edadma.petradb.*

// Create a new persistent database
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

// Reopen existing database — all tables, data, and auto-increment state are restored
val db2 = PersistentDB.open("mydata.db")
given Session = db2.connect()

val results = executeSQL("SELECT * FROM users")
results.foreach(println)

db2.close()
```

Persistent databases use crash-safe atomic writes via [stow](https://github.com/edadma/stow), with copy-on-write pages and double-buffered headers. All DDL and DML operations are durable — the catalog (table definitions, enum types, auto-increment state) and row data are persisted automatically.

### Scala (Text File)

`TextDB` stores the database as a human-editable `.ptxt` file — useful for development, configuration data, and version control.

```scala
import io.github.edadma.petradb.*

val db = TextDB.open("mydata.ptxt")
given Session = db.connect()

executeSQL("CREATE TABLE settings (key TEXT, value TEXT)")
executeSQL("INSERT INTO settings VALUES ('theme', 'dark')")
```

## Overview

PetraDB is designed to provide a lightweight, embeddable SQL database for applications that need relational data operations without the overhead of a full database server. It's useful for:

- **Testing and development** — quick setup without external database dependencies
- **Client-side applications** — running SQL queries in web browsers or Node.js
- **Data processing** — in-memory analytics and transformations
- **Embedded systems** — native compilation for resource-constrained environments
- **Persistent storage** — crash-safe durable storage backed by [stow](https://github.com/edadma/stow)

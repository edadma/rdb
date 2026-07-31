---
title: Getting Started with Scala
description: Add PetraDB to your Scala project and run your first SQL queries.
---

## Install

Add to your `build.sbt`:

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.6"
```

The `%%%` operator selects the correct artifact for your platform — JVM, Scala.js, or Scala Native.

## Run your first query

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

given Session = new MemoryDB().connect()

val results = executeSQL("""
  CREATE TABLE users (
    id SERIAL,
    name TEXT NOT NULL,
    email TEXT
  );

  INSERT INTO users (name, email) VALUES
    ('Alice', 'alice@example.com'),
    ('Bob', 'bob@example.com');

  SELECT * FROM users;
""")

results.foreach(println)
```

Each `MemoryDB` instance is a fully isolated in-memory database. All data lives in memory — nothing touches the filesystem.

## Persistent storage

When you need data to survive restarts, PetraDB has two options that require no external infrastructure:

**`PersistentDB`** — crash-safe durable storage in a single file, using copy-on-write pages and double-buffered headers via [stow](https://github.com/edadma/stow). Available on JVM and Native.

**`TextDB`** — stores the database as a human-readable `.ptxt` file. Ideal for development, config data, and version control.

Both are covered in detail in the [Scala guide](/guides/scala/).

## Try it in the browser

You can experiment with PetraDB's SQL support right now — no project setup needed. The [playground](/playground/) runs the full engine in your browser.

## Next steps

The [Scala guide](/guides/scala/) covers persistent and text databases, executing SQL, result handling, and the full API. For running PetraDB as a network service, see the [Server](/guides/server/) and [Client](/guides/client/) guides.

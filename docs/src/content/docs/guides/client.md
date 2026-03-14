---
title: Client
description: Connecting to a PetraDB server from JavaScript/TypeScript and Scala.
---

The client library connects to a running [PetraDB server](/guides/server/) over HTTP.

## Installation

### JavaScript / TypeScript

```bash
npm install @petradb/client
```

### Scala

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-client" % "1.4.2"
```

## Connection Options

| Option | Default | Description |
|--------|---------|-------------|
| `host` | `"localhost"` | Server hostname |
| `port` | `5480` | Server port |
| `username` | — | Username for Basic authentication |
| `password` | — | Password for Basic authentication |

## JavaScript / TypeScript

```javascript
import { Session } from '@petradb/client';

const db = new Session({
  host: 'localhost',
  port: 5480,
  username: 'admin',
  password: 'secret',
});
```

### Stateless Usage

Without calling `connect()`, each `execute()` call runs in a one-off transient session on the server. This is the simplest mode — no session to manage.

```javascript
const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows);
```

### Stateful Usage

Call `connect()` to create a server-side session. The session ID is sent automatically with every subsequent `execute()` call, so transactions and temporary state persist across requests.

```javascript
await db.connect();

await db.execute('BEGIN');
await db.execute("INSERT INTO users (name) VALUES ('Alice')");
await db.execute('COMMIT');

const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows);

await db.close();
```

### Full Example

```javascript
import { Session } from '@petradb/client';

const db = new Session({ host: 'localhost', port: 5480 });

await db.connect();

await db.execute(`
  CREATE TABLE products (
    id SERIAL,
    name TEXT NOT NULL,
    price NUMERIC(10,2),
    PRIMARY KEY (id)
  )
`);

await db.execute("INSERT INTO products (name, price) VALUES ('Laptop', 999.99)");
await db.execute("INSERT INTO products (name, price) VALUES ('Coffee', 4.50)");

const [{ rows }] = await db.execute('SELECT * FROM products ORDER BY price DESC');
console.log(rows);
// [{ id: 1, name: 'Laptop', price: 999.99 }, { id: 2, name: 'Coffee', price: 4.5 }]

await db.close();
```

## Scala

```scala
import io.github.edadma.petradb.client.*
import scala.concurrent.ExecutionContext.Implicits.global

val session = new Session(SessionOptions(
  host = "localhost",
  port = 5480,
  username = Some("admin"),
  password = Some("secret"),
))
```

### Stateless Usage

```scala
for results <- session.execute("SELECT * FROM users")
yield results.foreach(println)
```

### Stateful Usage

```scala
for
  _       <- session.connect()
  _       <- session.execute("BEGIN")
  _       <- session.execute("INSERT INTO users (name) VALUES ('Alice')")
  _       <- session.execute("COMMIT")
  results <- session.execute("SELECT * FROM users")
  _       <- session.close()
yield results.foreach(println)
```

All methods return `Future` values. `connect()` returns the session ID, `execute()` returns `Seq[Result]`, and `close()` returns `Unit`.

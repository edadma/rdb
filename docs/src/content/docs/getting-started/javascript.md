---
title: Getting Started with JavaScript / TypeScript
description: Install PetraDB and run your first SQL queries from JavaScript or TypeScript.
---

## Install

```bash
npm install @petradb/engine
```

No native modules, no post-install scripts — just JavaScript.

## Run your first query

```javascript
import { Session } from '@petradb/engine';

const db = new Session();

await db.execute(`
  CREATE TABLE users (
    id SERIAL,
    name TEXT NOT NULL,
    email TEXT
  );

  INSERT INTO users (name, email) VALUES
    ('Alice', 'alice@example.com'),
    ('Bob', 'bob@example.com');
`);

const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows);
// [
//   { id: 1, name: 'Alice', email: 'alice@example.com' },
//   { id: 2, name: 'Bob', email: 'bob@example.com' }
// ]
```

That's it — a full PostgreSQL-compatible SQL database, in-memory, with no setup. Each `Session` is an isolated database instance.

## Where it runs

The engine is pure JavaScript with zero native dependencies, so it works anywhere JavaScript runs: Node.js, Deno, Bun, and directly in the browser. You can [try it right now in the playground](/playground/).

## Persistent storage

When you need data to survive restarts, pass a `storage` option to the constructor:

```javascript
// Crash-safe durable storage in a single file (Node.js)
const db = new Session({ storage: 'persistent', path: './mydb' });

// Human-readable text file (Node.js)
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

For persistent databases, PetraDB auto-detects whether to create a new file or open an existing one. Call `await db.close()` when you're done to release the file handle.

For multi-process or networked access, run PetraDB as a [server](/guides/server/) and connect with the [client](/guides/client/) library.

## Next steps

The [JavaScript / TypeScript guide](/guides/javascript/) covers storage modes, row modes, prepared statements, and the full API. See also the [CLI](/guides/cli/), [Server](/guides/server/), and [Client](/guides/client/) guides.

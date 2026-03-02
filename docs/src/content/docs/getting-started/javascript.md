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

The JavaScript API is in-memory only. When you need data to survive restarts, run PetraDB as a [server](/guides/server/) and connect from your app with the [client](/guides/client/) library — the server supports crash-safe durable storage in a single file. See the [CLI guide](/guides/cli/) for a quick way to get a persistent database running.

## Next steps

The [JavaScript / TypeScript guide](/guides/javascript/) covers row modes, prepared statements, and the full API. If you need a database that persists to disk or a network server, see the [CLI](/guides/cli/), [Server](/guides/server/), and [Client](/guides/client/) guides.

Network client for [petradb-server](https://github.com/edadma/petradb) with the same `Session` API as `@petradb/engine`.

## Installation

```bash
npm install @petradb/client
```

## Quick Start

```javascript
import { Session } from '@petradb/client';

const db = new Session({ host: 'localhost', port: 5432 });

await db.execute(`
  CREATE TABLE users (
    id SERIAL,
    name TEXT NOT NULL,
    email TEXT,
    PRIMARY KEY (id)
  )
`);

await db.execute("INSERT INTO users (name, email) VALUES ('Alice', 'alice@example.com')");

const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows);
```

## Swapping Embedded and Network

Both `@petradb/engine` and `@petradb/client` export `Session` with the same `async execute()` API. Switch between them with a one-line import change:

```javascript
// Embedded (in-process, no server needed)
import { Session } from '@petradb/engine';
const db = new Session();

// Network (talks to petradb-server over HTTP)
import { Session } from '@petradb/client';
const db = new Session({ host: 'localhost', port: 5432 });

// Everything below works identically
await db.execute('CREATE TABLE ...');
const [{ rows }] = await db.execute('SELECT * FROM ...');
```

## API

### `new Session(options?)`

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `host` | `string` | `'localhost'` | Server hostname |
| `port` | `number` | `5432` | Server port |
| `rowMode` | `'object' \| 'array'` | `'object'` | Default row format for SELECT results |

### `db.execute(sql, options?)`

Sends SQL to the server. Returns a promise that resolves to an array of results.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `rowMode` | `'object' \| 'array'` | constructor default | Row format for this call |

### `db.connect()`

Creates a server-side session. The session ID is sent automatically with subsequent `execute()` calls via the `X-Session-Id` header.

### `db.close()`

Closes the server-side session.

## No Dependencies

Uses the native `fetch` API (Node.js 18+, all modern browsers).

## License

[ISC](https://opensource.org/licenses/ISC)

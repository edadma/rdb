# @petradb/engine

A lightweight, embeddable SQL database engine for JavaScript and TypeScript. No native dependencies, no external services — just import and query. Follows PostgreSQL conventions.

## Installation

```bash
npm install @petradb/engine
```

## Quick Start

```javascript
import { Session } from '@petradb/engine';

const db = new Session();

await db.execute(`
  CREATE TABLE users (id SERIAL, name TEXT NOT NULL, email TEXT);
  INSERT INTO users (name, email) VALUES ('Alice', 'alice@example.com');
`);

const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows); // [{ id: 1, name: 'Alice', email: 'alice@example.com' }]
```

## Documentation

Full documentation, SQL reference, and API details are available at **[petradb.dev](https://petradb.dev)**.

## License

[ISC](https://opensource.org/licenses/ISC)

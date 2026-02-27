# @petradb/client

Network client for connecting to a PetraDB server from JavaScript and TypeScript. Works in both Node.js and the browser.

## Installation

```bash
npm install @petradb/client
```

## Quick Start

```javascript
import { Session } from '@petradb/client';

const db = new Session({ host: 'localhost', port: 5480 });

await db.connect();

const results = await db.execute(`
  CREATE TABLE users (id SERIAL, name TEXT);
  INSERT INTO users (name) VALUES ('Alice');
  SELECT * FROM users;
`);

console.log(results);

await db.close();
```

## Documentation

Full documentation at **[petradb.dev](https://petradb.dev)**.

## License

[ISC](https://opensource.org/licenses/ISC)

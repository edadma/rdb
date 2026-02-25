# PetraDB

[![Maven Central](https://img.shields.io/maven-central/v/io.github.edadma/petradb-engine_sjs1_3)](https://central.sonatype.com/artifact/io.github.edadma/petradb-engine_sjs1_3)
[![npm](https://img.shields.io/npm/v/@petradb/engine)](https://www.npmjs.com/package/@petradb/engine)
[![License](https://img.shields.io/github/license/edadma/petradb)](LICENSE)

A lightweight, embeddable SQL database engine for JavaScript, TypeScript, and Scala. Compiles to JVM, Node.js, and Native. No external dependencies, no server required.

## Quick Start

### JavaScript / TypeScript

```bash
npm install @petradb/engine
```

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

### Scala

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.1.1"
```

```scala
import io.github.edadma.petradb.*

given Session = new MemoryDB().connect()

executeSQL("""
  CREATE TABLE users (id SERIAL, name TEXT NOT NULL, email TEXT);
  INSERT INTO users (name, email) VALUES ('Alice', 'alice@example.com');
  SELECT * FROM users;
""").foreach(println)
```

## Documentation

Full documentation, SQL reference, and guides are available at **[petradb.dev](https://petradb.dev)**.

## License

[ISC](LICENSE)

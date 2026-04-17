---
title: Client
description: Connexion à un serveur PetraDB depuis JavaScript/TypeScript et Scala.
---

La bibliothèque client se connecte à un [serveur PetraDB](/guides/server/) en cours d'exécution via HTTP.

## Installation

### JavaScript / TypeScript

```bash
npm install @petradb/client
```

### Scala

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-client" % "1.5.2"
```

## Options de connexion

| Option | Par défaut | Description |
|--------|-----------|-------------|
| `host` | `"localhost"` | Nom d'hôte du serveur |
| `port` | `5480` | Port du serveur |
| `username` | -- | Nom d'utilisateur pour l'authentification Basic |
| `password` | -- | Mot de passe pour l'authentification Basic |

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

### Utilisation sans état

Sans appeler `connect()`, chaque appel `execute()` s'exécute dans une session transitoire unique sur le serveur. C'est le mode le plus simple — pas de session à gérer.

```javascript
const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows);
```

### Utilisation avec état

Appelez `connect()` pour créer une session côté serveur. L'identifiant de session est envoyé automatiquement avec chaque appel `execute()` subséquent, de sorte que les transactions et l'état temporaire persistent entre les requêtes.

```javascript
await db.connect();

await db.execute('BEGIN');
await db.execute("INSERT INTO users (name) VALUES ('Alice')");
await db.execute('COMMIT');

const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows);

await db.close();
```

### Exemple complet

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

### Utilisation sans état

```scala
for results <- session.execute("SELECT * FROM users")
yield results.foreach(println)
```

### Utilisation avec état

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

Toutes les méthodes retournent des valeurs `Future`. `connect()` retourne l'identifiant de session, `execute()` retourne `Seq[Result]`, et `close()` retourne `Unit`.

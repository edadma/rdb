---
title: Cliente
description: Conectar a un servidor PetraDB desde JavaScript/TypeScript y Scala.
---

La biblioteca cliente conecta a un [servidor PetraDB](/guides/server/) en ejecucion a traves de HTTP.

## Instalacion

### JavaScript / TypeScript

```bash
npm install @petradb/client
```

### Scala

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-client" % "1.5.2"
```

## Opciones de conexion

| Opcion | Por defecto | Descripcion |
|--------|---------|-------------|
| `host` | `"localhost"` | Nombre de host del servidor |
| `port` | `5480` | Puerto del servidor |
| `username` | — | Nombre de usuario para autenticacion Basic |
| `password` | — | Contrasena para autenticacion Basic |

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

### Uso sin estado

Sin llamar a `connect()`, cada llamada a `execute()` se ejecuta en una sesion transitoria de un solo uso en el servidor. Este es el modo mas simple — no hay sesion que administrar.

```javascript
const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows);
```

### Uso con estado

Llama a `connect()` para crear una sesion del lado del servidor. El ID de sesion se envia automaticamente con cada llamada subsecuente a `execute()`, por lo que las transacciones y el estado temporal persisten entre solicitudes.

```javascript
await db.connect();

await db.execute('BEGIN');
await db.execute("INSERT INTO users (name) VALUES ('Alice')");
await db.execute('COMMIT');

const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows);

await db.close();
```

### Ejemplo completo

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

### Uso sin estado

```scala
for results <- session.execute("SELECT * FROM users")
yield results.foreach(println)
```

### Uso con estado

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

Todos los metodos retornan valores `Future`. `connect()` retorna el ID de sesion, `execute()` retorna `Seq[Result]`, y `close()` retorna `Unit`.

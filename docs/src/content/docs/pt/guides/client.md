---
title: Cliente
description: Conectando a um servidor PetraDB a partir de JavaScript/TypeScript e Scala.
---

A biblioteca cliente conecta a um [servidor PetraDB](/guides/server/) em execucao via HTTP.

## Instalacao

### JavaScript / TypeScript

```bash
npm install @petradb/client
```

### Scala

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-client" % "1.5.2"
```

## Opcoes de Conexao

| Opcao | Padrao | Descricao |
|--------|---------|-------------|
| `host` | `"localhost"` | Nome do host do servidor |
| `port` | `5480` | Porta do servidor |
| `username` | — | Nome de usuario para autenticacao Basic |
| `password` | — | Senha para autenticacao Basic |

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

### Uso Stateless

Sem chamar `connect()`, cada chamada `execute()` executa em uma sessao transiente unica no servidor. Este e o modo mais simples — sem sessao para gerenciar.

```javascript
const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows);
```

### Uso Stateful

Chame `connect()` para criar uma sessao no lado do servidor. O ID da sessao e enviado automaticamente com cada chamada `execute()` subsequente, para que transacoes e estado temporario persistam entre requisicoes.

```javascript
await db.connect();

await db.execute('BEGIN');
await db.execute("INSERT INTO users (name) VALUES ('Alice')");
await db.execute('COMMIT');

const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows);

await db.close();
```

### Exemplo Completo

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

### Uso Stateless

```scala
for results <- session.execute("SELECT * FROM users")
yield results.foreach(println)
```

### Uso Stateful

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

Todos os metodos retornam valores `Future`. `connect()` retorna o ID da sessao, `execute()` retorna `Seq[Result]` e `close()` retorna `Unit`.

---
title: クライアント
description: JavaScript/TypeScriptおよびScalaからPetraDBサーバーに接続する方法です。
---

クライアントライブラリは、実行中の[PetraDBサーバー](/guides/server/)にHTTP経由で接続します。

## インストール

### JavaScript / TypeScript

```bash
npm install @petradb/client
```

### Scala

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-client" % "1.5.2"
```

## 接続オプション

| オプション | デフォルト | 説明 |
|--------|---------|-------------|
| `host` | `"localhost"` | サーバーのホスト名 |
| `port` | `5480` | サーバーのポート |
| `username` | — | Basic認証のユーザー名 |
| `password` | — | Basic認証のパスワード |

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

### ステートレス使用

`connect()`を呼び出さない場合、各`execute()`呼び出しはサーバー上の一時的なワンオフセッションで実行されます。これは最もシンプルなモードで、セッション管理が不要です。

```javascript
const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows);
```

### ステートフル使用

`connect()`を呼び出してサーバー側セッションを作成します。セッションIDは後続のすべての`execute()`呼び出しで自動的に送信されるため、トランザクションと一時的な状態がリクエスト間で保持されます。

```javascript
await db.connect();

await db.execute('BEGIN');
await db.execute("INSERT INTO users (name) VALUES ('Alice')");
await db.execute('COMMIT');

const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows);

await db.close();
```

### 完全な例

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

### ステートレス使用

```scala
for results <- session.execute("SELECT * FROM users")
yield results.foreach(println)
```

### ステートフル使用

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

すべてのメソッドは`Future`値を返します。`connect()`はセッションIDを返し、`execute()`は`Seq[Result]`を返し、`close()`は`Unit`を返します。

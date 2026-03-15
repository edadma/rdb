---
title: 客户端
description: 从 JavaScript/TypeScript 和 Scala 连接到 PetraDB 服务器。
---

客户端库通过 HTTP 连接到正在运行的 [PetraDB 服务器](/guides/server/)。

## 安装

### JavaScript / TypeScript

```bash
npm install @petradb/client
```

### Scala

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-client" % "1.5.0"
```

## 连接选项

| 选项 | 默认值 | 描述 |
|--------|---------|-------------|
| `host` | `"localhost"` | 服务器主机名 |
| `port` | `5480` | 服务器端口 |
| `username` | — | Basic 认证用户名 |
| `password` | — | Basic 认证密码 |

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

### 无状态使用

不调用 `connect()` 时，每次 `execute()` 调用在服务器上的一次性临时会话中运行。这是最简单的模式 — 无需管理会话。

```javascript
const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows);
```

### 有状态使用

调用 `connect()` 创建服务器端会话。会话 ID 会自动随每次后续 `execute()` 调用发送，因此事务和临时状态在请求之间保持。

```javascript
await db.connect();

await db.execute('BEGIN');
await db.execute("INSERT INTO users (name) VALUES ('Alice')");
await db.execute('COMMIT');

const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows);

await db.close();
```

### 完整示例

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

### 无状态使用

```scala
for results <- session.execute("SELECT * FROM users")
yield results.foreach(println)
```

### 有状态使用

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

所有方法返回 `Future` 值。`connect()` 返回会话 ID，`execute()` 返回 `Seq[Result]`，`close()` 返回 `Unit`。

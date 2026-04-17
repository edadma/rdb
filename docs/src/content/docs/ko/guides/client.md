---
title: 클라이언트
description: JavaScript/TypeScript와 Scala에서 PetraDB 서버에 연결하기.
---

클라이언트 라이브러리는 실행 중인 [PetraDB 서버](/guides/server/)에 HTTP를 통해 연결합니다.

## 설치

### JavaScript / TypeScript

```bash
npm install @petradb/client
```

### Scala

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-client" % "1.5.2"
```

## 연결 옵션

| 옵션 | 기본값 | 설명 |
|--------|---------|-------------|
| `host` | `"localhost"` | 서버 호스트 이름 |
| `port` | `5480` | 서버 포트 |
| `username` | — | Basic 인증용 사용자 이름 |
| `password` | — | Basic 인증용 비밀번호 |

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

### 무상태 사용

`connect()`를 호출하지 않으면, 각 `execute()` 호출이 서버의 일회성 임시 세션에서 실행됩니다. 이것이 가장 간단한 모드입니다 — 관리할 세션이 없습니다.

```javascript
const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows);
```

### 상태 유지 사용

`connect()`를 호출하여 서버 측 세션을 생성합니다. 세션 ID가 이후 모든 `execute()` 호출과 함께 자동으로 전송되므로, 트랜잭션과 임시 상태가 요청 간에 유지됩니다.

```javascript
await db.connect();

await db.execute('BEGIN');
await db.execute("INSERT INTO users (name) VALUES ('Alice')");
await db.execute('COMMIT');

const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows);

await db.close();
```

### 전체 예제

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

### 무상태 사용

```scala
for results <- session.execute("SELECT * FROM users")
yield results.foreach(println)
```

### 상태 유지 사용

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

모든 메서드는 `Future` 값을 반환합니다. `connect()`는 세션 ID를, `execute()`는 `Seq[Result]`를, `close()`는 `Unit`을 반환합니다.

---
title: JavaScript / TypeScript 사용법
description: JavaScript와 TypeScript에서 PetraDB를 사용하는 방법.
---

## 데이터베이스 생성

각 `Session` 인스턴스는 완전히 격리된 데이터베이스입니다. 기본적으로 인메모리로 실행되지만, 영구 또는 텍스트 스토리지를 선택할 수 있습니다.

```javascript
import { Session } from '@petradb/engine';

// 인메모리 (기본값)
const db = new Session();
```

## 스토리지 모드

### 메모리 (기본값)

데이터가 메모리에 존재하며 프로세스 종료 시 사라집니다. Node.js, Deno, Bun, 브라우저 등 어디서든 작동합니다.

```javascript
const db = new Session();
// 또는 명시적으로:
const db = new Session({ storage: 'memory' });
```

### 영구 (Node.js)

copy-on-write 페이지와 이중 버퍼 헤더를 사용하는 단일 바이너리 파일의 충돌 안전 내구성 스토리지. 파일이 존재하면 열고, 없으면 새 데이터베이스를 생성합니다.

```javascript
const db = new Session({ storage: 'persistent', path: './mydb' });

// 선택사항: 페이지 크기 설정 (기본값 4096)
const db = new Session({ storage: 'persistent', path: './mydb', pageSize: 8192 });
```

### 텍스트 (Node.js)

사람이 읽을 수 있는 텍스트 파일(`.ptxt`)로 데이터를 저장합니다. 디버깅, 버전 관리, 수동 데이터 편집에 유용합니다.

```javascript
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

### 닫기

영구 또는 텍스트 스토리지를 사용할 때는 `await db.close()`를 호출하여 파일 핸들을 해제합니다. 메모리 데이터베이스에서는 `close()`가 아무 작업도 하지 않습니다.

```javascript
const db = new Session({ storage: 'persistent', path: './mydb' });
// ... 데이터베이스 사용 ...
await db.close();
```

## SQL 실행

`db.execute(sql)`을 사용하여 세미콜론으로 구분된 하나 이상의 SQL 문을 실행합니다. 결과 객체 배열로 해석되는 프라미스를 반환합니다.

```javascript
await db.execute(`
  CREATE TABLE users (
    id SERIAL,
    name TEXT NOT NULL,
    email TEXT,
    PRIMARY KEY (id)
  )
`);

await db.execute("INSERT INTO users (name, email) VALUES ('Alice', 'alice@example.com')");
await db.execute("INSERT INTO users (name, email) VALUES ('Bob', 'bob@example.com')");

const [{ rows, fields }] = await db.execute('SELECT * FROM users');
// rows:   [{ id: 1, name: 'Alice', email: 'alice@example.com' }, ...]
// fields: [{ name: 'id', dataType: 'serial' }, { name: 'name', dataType: 'text' }, ...]
```

## 행 모드

기본적으로 SELECT 행은 컬럼 이름을 키로 하는 객체로 반환됩니다. `rowMode: 'array'`를 사용하면 위치 배열로 반환됩니다.

```javascript
// 모든 쿼리의 기본값 설정
const db = new Session({ rowMode: 'array' });

// 또는 호출별 재정의
const [{ rows }] = await db.execute('SELECT id, name FROM users', { rowMode: 'array' });
// rows: [[1, 'Alice'], [2, 'Bob']]
```

## 준비된 구문

`db.prepare(sql)`을 `$1`, `$2`, ... 매개변수 플레이스홀더와 함께 사용합니다. `execute(params, options?)` 메서드가 있는 구문 객체를 반환합니다.

```javascript
const stmt = db.prepare('SELECT * FROM users WHERE id = $1');
const [{ rows }] = await stmt.execute([42]);

// 옵션과 함께
const [{ rows }] = await stmt.execute([42], { rowMode: 'array' });
```

SQL 수준의 `PREPARE` / `EXECUTE` / `DEALLOCATE`도 지원됩니다 — 자세한 내용은 [트랜잭션 레퍼런스](/reference/transactions/)를 참고하세요.

전체 TypeScript 인터페이스, 결과 타입, 값 매핑에 대해서는 [JavaScript API 레퍼런스](/reference/api-javascript/)를 참고하세요.

## TypeScript

완전한 타입 정의가 포함되어 있습니다. 구별된 유니온을 사용하여 결과 타입을 좁힐 수 있습니다:

```typescript
import { Session, ExecuteResult } from '@petradb/engine';

const db = new Session();
const results: ExecuteResult[] = await db.execute('SELECT * FROM users');

for (const result of results) {
  if (result.command === 'select') {
    // result.rows와 result.fields가 여기서 타입이 지정됩니다
  }
}
```

## 전체 예제

```javascript
import { Session } from '@petradb/engine';

const db = new Session();

await db.execute(`
  CREATE TYPE status AS ENUM ('active', 'inactive');
  CREATE TABLE products (
    id SERIAL,
    name TEXT NOT NULL,
    price NUMERIC(10,2),
    status status DEFAULT 'active',
    tags JSON,
    created_at TIMESTAMP,
    PRIMARY KEY (id)
  )
`);

await db.execute(`
  INSERT INTO products (name, price, tags, created_at) VALUES
    ('Laptop', 999.99, '["electronics", "computers"]', '2025-01-15 10:30:00');
  INSERT INTO products (name, price, tags, created_at) VALUES
    ('Coffee', 4.50, '["food", "organic"]', '2025-01-16 08:00:00')
`);

const [{ rows }] = await db.execute(`
  SELECT name, price FROM products
  WHERE price > 10
  ORDER BY price DESC
`);

console.log(rows); // [{ name: 'Laptop', price: 999.99 }]
```

## 오류 처리

```javascript
async function safeExecute(db, sql) {
  try {
    return await db.execute(sql);
  } catch (error) {
    console.error('SQL Error:', error.message);
    return null;
  }
}
```

## 플랫폼 참고사항

- 인메모리 모드는 어디서든 작동합니다: Node.js, Deno, Bun, 브라우저(번들러 사용 시)
- 영구 및 텍스트 스토리지는 Node.js가 필요합니다(파일시스템 사용)
- 외부 의존성이나 네이티브 모듈이 필요 없습니다
- TypeScript 정의 포함
- 브라우저에서 대용량 데이터셋의 경우 Web Worker를 고려하세요

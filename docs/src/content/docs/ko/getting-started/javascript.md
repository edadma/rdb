---
title: JavaScript / TypeScript 시작하기
description: PetraDB를 설치하고 JavaScript 또는 TypeScript에서 첫 번째 SQL 쿼리를 실행합니다.
---

## 설치

```bash
npm install @petradb/engine
```

네이티브 모듈도 없고, 설치 후 스크립트도 없습니다 — 순수 JavaScript입니다.

## 첫 번째 쿼리 실행

```javascript
import { Session } from '@petradb/engine';

const db = new Session();

await db.execute(`
  CREATE TABLE users (
    id SERIAL,
    name TEXT NOT NULL,
    email TEXT
  );

  INSERT INTO users (name, email) VALUES
    ('Alice', 'alice@example.com'),
    ('Bob', 'bob@example.com');
`);

const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows);
// [
//   { id: 1, name: 'Alice', email: 'alice@example.com' },
//   { id: 2, name: 'Bob', email: 'bob@example.com' }
// ]
```

이것이 전부입니다 — 설정 없이 인메모리에서 작동하는 완전한 PostgreSQL 호환 SQL 데이터베이스입니다. 각 `Session`은 독립된 데이터베이스 인스턴스입니다.

## 실행 환경

엔진은 네이티브 의존성이 없는 순수 JavaScript이므로, JavaScript가 실행되는 어디서든 작동합니다: Node.js, Deno, Bun, 그리고 브라우저에서 직접 실행됩니다. [플레이그라운드](/playground/)에서 지금 바로 사용해 볼 수 있습니다.

## 영구 스토리지

재시작 후에도 데이터를 유지해야 할 때는 생성자에 `storage` 옵션을 전달합니다:

```javascript
// 단일 파일의 충돌 안전 내구성 스토리지 (Node.js)
const db = new Session({ storage: 'persistent', path: './mydb' });

// 사람이 읽을 수 있는 텍스트 파일 (Node.js)
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

영구 데이터베이스의 경우, PetraDB는 새 파일을 생성할지 기존 파일을 열지 자동 감지합니다. 파일 핸들을 해제하려면 작업이 끝나면 `await db.close()`를 호출하세요.

멀티 프로세스 또는 네트워크 접근이 필요한 경우, PetraDB를 [서버](/guides/server/)로 실행하고 [클라이언트](/guides/client/) 라이브러리로 연결합니다.

## 다음 단계

[JavaScript / TypeScript 가이드](/guides/javascript/)에서 스토리지 모드, 행 모드, 준비된 구문, 전체 API를 다룹니다. [CLI](/guides/cli/), [서버](/guides/server/), [클라이언트](/guides/client/) 가이드도 참고하세요.

---
title: JavaScript / TypeScript
description: PetraDB의 JavaScript 및 TypeScript API 레퍼런스.
---

## 설치

```bash
npm install @petradb/engine
```

## Session

### `new Session(options?)`

새 데이터베이스 인스턴스를 생성합니다.

| 옵션 | 타입 | 기본값 | 설명 |
|--------|------|---------|-------------|
| `rowMode` | `'object' \| 'array'` | `'object'` | SELECT 결과의 기본 행 형식 |
| `storage` | `'memory' \| 'persistent' \| 'text'` | `'memory'` | 스토리지 백엔드 |
| `path` | `string` | — | 파일 경로 (persistent와 text에 필수) |
| `pageSize` | `number` | `4096` | 페이지 크기 (바이트, persistent만 해당) |

```javascript
// 인메모리 (기본값)
const db = new Session();

// 충돌 안전 영구 스토리지 (Node.js)
const db = new Session({ storage: 'persistent', path: './mydb' });

// 사람이 읽을 수 있는 텍스트 파일 (Node.js)
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

영구 스토리지의 경우, PetraDB는 새 파일을 생성할지 기존 파일을 열지 자동 감지합니다.

### `db.close()`

파일 핸들을 해제합니다. `Promise<void>`를 반환합니다. 영구 및 텍스트 스토리지에 필수입니다. 메모리 데이터베이스에서는 아무 작업도 하지 않습니다(하지만 여전히 비동기).

```javascript
await db.close();
```

### `db.execute(sql, options?)`

`;`로 구분된 하나 이상의 SQL 문을 실행합니다. 결과 배열로 해석되는 프라미스를 반환합니다.

| 옵션 | 타입 | 기본값 | 설명 |
|--------|------|---------|-------------|
| `rowMode` | `'object' \| 'array'` | 생성자 기본값 | 이 호출의 행 형식 |

```javascript
const [{ rows, fields }] = await db.execute('SELECT * FROM users');
```

### `db.prepare(sql)`

`$1`, `$2`, ... 매개변수 플레이스홀더로 준비된 구문을 생성합니다. `execute(params, options?)` 메서드가 있는 구문 객체를 반환합니다.

```javascript
const stmt = db.prepare('SELECT * FROM users WHERE id = $1');
const [{ rows }] = await stmt.execute([42]);

// 옵션과 함께
const [{ rows }] = await stmt.execute([42], { rowMode: 'array' });
```

### `db.registerFunction(name, callback)`

SQL, 트리거, 저장 프로시저에서 호출 가능한 네이티브 JavaScript 함수를 등록합니다:

```javascript
db.registerFunction('my_double', (args) => args[0] * 2);

// SQL에서 사용 가능:
const [{ rows }] = await db.execute('SELECT my_double(21) AS val');
// rows[0].val === 42
```

콜백은 JavaScript 값 배열(숫자, 문자열, 부울, 또는 `null`)을 받고 JavaScript 값을 반환해야 합니다.

## TypeScript 인터페이스

```typescript
interface SessionOptions {
  rowMode?: 'object' | 'array';
  storage?: 'memory' | 'persistent' | 'text';
  path?: string;
  pageSize?: number;
}

interface ExecuteOptions {
  rowMode?: 'object' | 'array';
}

interface PreparedStatement {
  execute(params?: any[], options?: ExecuteOptions): Promise<ExecuteResult[]>
}

class Session {
  constructor(options?: SessionOptions)
  execute(sql: string, options?: ExecuteOptions): Promise<ExecuteResult[]>
  prepare(sql: string): PreparedStatement
  close(): Promise<void>
}
```

## 결과 타입

모든 결과에는 구별을 위한 `command` 필드가 있습니다:

```typescript
// DDL
{ command: 'create table', table: string }
{ command: 'drop table', table: string }
{ command: 'create type', type: string }
{ command: 'drop type', type: string }
{ command: 'create index', index: string }
{ command: 'drop index', index: string }
{ command: 'truncate table', table: string }
{ command: 'alter table' }
{ command: 'create view', view: string }
{ command: 'drop view', view: string }
{ command: 'create sequence', sequence: string }
{ command: 'drop sequence', sequence: string }
{ command: 'create schema', schema: string }
{ command: 'create trigger', trigger: string }
{ command: 'drop trigger', trigger: string }

// PL/pgSQL
{ command: 'do' }
{ command: 'create function', function: string }
{ command: 'drop function', function: string }
{ command: 'create procedure', procedure: string }
{ command: 'drop procedure', procedure: string }
{ command: 'call' }

// 쿼리 계획
{ command: 'explain', plan: string }

// DML
{ command: 'insert', result: Record<string, any>, rows: T[], fields: FieldInfo[] }
{ command: 'select', rows: T[], fields: { name: string, dataType: string }[] }
{ command: 'update', rowCount: number }
{ command: 'delete', rowCount: number }
{ command: 'copy', rowCount: number }

// 트랜잭션
{ command: 'begin' }
{ command: 'commit' }
{ command: 'rollback' }

// 준비된 구문
{ command: 'prepare', name: string }
{ command: 'deallocate', name: string }
```

## 값 매핑

| SQL 타입 | JavaScript 타입 |
|----------|----------------|
| INT, BIGINT, DOUBLE, NUMERIC | `number` |
| TEXT, CHAR, VARCHAR | `string` |
| BOOLEAN | `boolean` |
| UUID | `string` |
| TIMESTAMP | `Date` |
| ENUM | `string` (라벨) |
| JSON 배열 | `Array` |
| JSON 객체 | `Object` |
| NULL | `null` |

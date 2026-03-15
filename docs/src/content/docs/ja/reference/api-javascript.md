---
title: JavaScript / TypeScript
description: PetraDBのJavaScriptおよびTypeScript APIリファレンスです。
---

## インストール

```bash
npm install @petradb/engine
```

## Session

### `new Session(options?)`

新しいデータベースインスタンスを作成します。

| オプション | 型 | デフォルト | 説明 |
|--------|------|---------|-------------|
| `rowMode` | `'object' \| 'array'` | `'object'` | SELECT結果のデフォルト行フォーマット |
| `storage` | `'memory' \| 'persistent' \| 'text'` | `'memory'` | ストレージバックエンド |
| `path` | `string` | — | ファイルパス（persistentおよびtextで必須） |
| `pageSize` | `number` | `4096` | ページサイズ（バイト）（persistentのみ） |

```javascript
// インメモリ（デフォルト）
const db = new Session();

// クラッシュセーフな永続ストレージ（Node.js）
const db = new Session({ storage: 'persistent', path: './mydb' });

// 人間が読めるテキストファイル（Node.js）
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

永続ストレージの場合、PetraDBは新しいファイルを作成するか既存のファイルを開くかを自動検出します。

### `db.close()`

ファイルハンドルを解放します。`Promise<void>`を返します。永続およびテキストストレージで必須です。メモリデータベースではノーオペレーションです（ただし非同期）。

```javascript
await db.close();
```

### `db.execute(sql, options?)`

`;`で区切られた1つ以上のSQL文を実行します。結果の配列に解決されるPromiseを返します。

| オプション | 型 | デフォルト | 説明 |
|--------|------|---------|-------------|
| `rowMode` | `'object' \| 'array'` | コンストラクタのデフォルト | この呼び出しの行フォーマット |

```javascript
const [{ rows, fields }] = await db.execute('SELECT * FROM users');
```

### `db.prepare(sql)`

`$1`、`$2`、...のパラメータプレースホルダーを持つプリペアドステートメントを作成します。`execute(params, options?)`メソッドを持つステートメントオブジェクトを返します。

```javascript
const stmt = db.prepare('SELECT * FROM users WHERE id = $1');
const [{ rows }] = await stmt.execute([42]);

// オプション付き
const [{ rows }] = await stmt.execute([42], { rowMode: 'array' });
```

### `db.registerFunction(name, callback)`

SQLから呼び出し可能なネイティブJavaScript関数を登録します。トリガーやストアドプロシージャからも使用できます。

```javascript
db.registerFunction('my_double', (args) => args[0] * 2);

// SQLで使用可能に:
const [{ rows }] = await db.execute('SELECT my_double(21) AS val');
// rows[0].val === 42
```

コールバックはJavaScript値（数値、文字列、ブーリアン、または`null`）の配列を受け取り、JavaScript値を返す必要があります。

## TypeScriptインターフェース

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

## 結果型

すべての結果には判別用の`command`フィールドがあります。

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

// クエリプラン
{ command: 'explain', plan: string }

// DML
{ command: 'insert', result: Record<string, any>, rows: T[], fields: FieldInfo[] }
{ command: 'select', rows: T[], fields: { name: string, dataType: string }[] }
{ command: 'update', rowCount: number }
{ command: 'delete', rowCount: number }
{ command: 'copy', rowCount: number }

// トランザクション
{ command: 'begin' }
{ command: 'commit' }
{ command: 'rollback' }

// プリペアドステートメント
{ command: 'prepare', name: string }
{ command: 'deallocate', name: string }
```

## 値マッピング

| SQL型 | JavaScript型 |
|----------|----------------|
| INT, BIGINT, DOUBLE, NUMERIC | `number` |
| TEXT, CHAR, VARCHAR | `string` |
| BOOLEAN | `boolean` |
| UUID | `string` |
| TIMESTAMP | `Date` |
| ENUM | `string`（ラベル） |
| JSON配列 | `Array` |
| JSONオブジェクト | `Object` |
| NULL | `null` |

---
title: JavaScript / TypeScript の使い方
description: JavaScriptおよびTypeScriptからPetraDBを使用する方法です。
---

## データベースの作成

各`Session`インスタンスは完全に独立したデータベースです。デフォルトではインメモリで動作しますが、永続ストレージまたはテキストストレージを選択できます。

```javascript
import { Session } from '@petradb/engine';

// インメモリ（デフォルト）
const db = new Session();
```

## ストレージモード

### メモリ（デフォルト）

データはメモリ上に存在し、プロセス終了時に失われます。Node.js、Deno、Bun、ブラウザのどこでも動作します。

```javascript
const db = new Session();
// または明示的に:
const db = new Session({ storage: 'memory' });
```

### 永続（Node.js）

コピーオンライトページとダブルバッファードヘッダーを使用した、単一バイナリファイルによるクラッシュセーフな永続ストレージです。ファイルが存在する場合は開かれ、そうでなければ新しいデータベースが作成されます。

```javascript
const db = new Session({ storage: 'persistent', path: './mydb' });

// オプション: ページサイズの設定（デフォルト4096）
const db = new Session({ storage: 'persistent', path: './mydb', pageSize: 8192 });
```

### テキスト（Node.js）

人間が読めるテキストファイル（`.ptxt`）にデータを保存します。デバッグ、バージョン管理、手動でのデータ編集に便利です。

```javascript
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

### クローズ

永続またはテキストストレージを使用する場合は、`await db.close()`を呼び出してファイルハンドルを解放してください。メモリデータベースの場合、`close()`はノーオペレーションです。

```javascript
const db = new Session({ storage: 'persistent', path: './mydb' });
// ... データベースを使用 ...
await db.close();
```

## SQLの実行

`db.execute(sql)`を使用して、セミコロンで区切られた1つ以上のSQL文を実行します。結果オブジェクトの配列に解決されるPromiseを返します。

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

## 行モード

デフォルトでは、SELECTの行はカラム名をキーとしたオブジェクトとして返されます。位置配列を使用するには`rowMode: 'array'`を指定します。

```javascript
// すべてのクエリのデフォルトを設定
const db = new Session({ rowMode: 'array' });

// または呼び出しごとにオーバーライド
const [{ rows }] = await db.execute('SELECT id, name FROM users', { rowMode: 'array' });
// rows: [[1, 'Alice'], [2, 'Bob']]
```

## プリペアドステートメント

`db.prepare(sql)`で`$1`、`$2`、...のパラメータプレースホルダーを使用します。`execute(params, options?)`メソッドを持つステートメントオブジェクトを返します。

```javascript
const stmt = db.prepare('SELECT * FROM users WHERE id = $1');
const [{ rows }] = await stmt.execute([42]);

// オプション付き
const [{ rows }] = await stmt.execute([42], { rowMode: 'array' });
```

SQLレベルの`PREPARE` / `EXECUTE` / `DEALLOCATE`もサポートされています — 詳細は[トランザクションリファレンス](/reference/transactions/)をご覧ください。

[JavaScript APIリファレンス](/reference/api-javascript/)で結果型、値マッピング、完全なTypeScriptインターフェースをご覧ください。

## TypeScript

完全な型定義が含まれています。判別共用体を使用して結果型を絞り込みます。

```typescript
import { Session, ExecuteResult } from '@petradb/engine';

const db = new Session();
const results: ExecuteResult[] = await db.execute('SELECT * FROM users');

for (const result of results) {
  if (result.command === 'select') {
    // ここでresult.rowsとresult.fieldsは型付けされています
  }
}
```

## 完全な例

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

## エラー処理

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

## プラットフォームに関する注意事項

- インメモリモードはどこでも動作します：Node.js、Deno、Bun、ブラウザ（バンドラー使用時）
- 永続ストレージとテキストストレージにはNode.jsが必要です（ファイルシステムを使用するため）
- 外部依存関係やネイティブモジュールは不要です
- TypeScript定義が含まれています
- ブラウザでの大規模データセットにはWeb Workerの使用を検討してください

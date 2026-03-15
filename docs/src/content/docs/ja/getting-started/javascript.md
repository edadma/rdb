---
title: JavaScript / TypeScript ではじめる
description: PetraDBをインストールして、JavaScriptまたはTypeScriptから最初のSQLクエリを実行します。
---

## インストール

```bash
npm install @petradb/engine
```

ネイティブモジュールもポストインストールスクリプトも不要です — 純粋なJavaScriptです。

## 最初のクエリを実行する

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

以上です — セットアップ不要で、インメモリの完全なPostgreSQL互換SQLデータベースです。各`Session`は独立したデータベースインスタンスです。

## 動作環境

エンジンはネイティブ依存関係のない純粋なJavaScriptなので、JavaScriptが動作するあらゆる場所で使えます：Node.js、Deno、Bun、そしてブラウザで直接動作します。[プレイグラウンドで今すぐ試す](/playground/)こともできます。

## 永続ストレージ

再起動後もデータを保持する必要がある場合は、コンストラクタに`storage`オプションを渡します。

```javascript
// 単一ファイルのクラッシュセーフな永続ストレージ（Node.js）
const db = new Session({ storage: 'persistent', path: './mydb' });

// 人間が読めるテキストファイル（Node.js）
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

永続データベースの場合、PetraDBは新しいファイルを作成するか既存のファイルを開くかを自動検出します。ファイルハンドルを解放するには、完了時に`await db.close()`を呼び出してください。

マルチプロセスまたはネットワークアクセスの場合は、PetraDBを[サーバー](/guides/server/)として実行し、[クライアント](/guides/client/)ライブラリで接続してください。

## 次のステップ

[JavaScript / TypeScriptガイド](/guides/javascript/)では、ストレージモード、行モード、プリペアドステートメント、完全なAPIについて説明しています。[CLI](/guides/cli/)、[サーバー](/guides/server/)、[クライアント](/guides/client/)ガイドもご覧ください。

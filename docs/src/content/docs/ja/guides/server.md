---
title: サーバー
description: PetraDBをHTTPサーバーとして実行する方法です。
---

## インストール

```bash
npm install -g @petradb/server
```

## 使い方

```bash
petradb-server [OPTIONS] [path]
```

パスが指定された場合、サーバーはその場所に永続データベースを開く（または作成する）します。パスなしの場合、インメモリデータベースが使用されます。

### オプション

| オプション | 説明 |
|--------|-------------|
| `-m`, `--memory` | インメモリデータベースを使用 |
| `-p`, `--port` | ポート番号（デフォルト：`5480`） |
| `-h`, `--host` | ホストアドレス（デフォルト：`127.0.0.1`） |
| `-c`, `--config` | TOML設定ファイルのパス |

### 例

```bash
# デフォルトポートでインメモリデータベース
petradb-server

# カスタムポートで永続データベース
petradb-server -p 8080 mydata.db

# 設定ファイル付き
petradb-server -c petradb.toml mydata.db
```

## 設定

TOMLファイルで認証、CORS、セッション制限を制御します。

```toml
auth = "basic"

[[users]]
username = "admin"
password = "$HASHED_PASSWORD"

[[users]]
username = "reader"
password = "$HASHED_PASSWORD"

[cors]
origin = "*"          # "*"（デフォルト）、"none"、または特定のオリジン

[sessions]
max_sessions = 100    # 0 = 無制限（デフォルト）
```

### 認証モード

| モード | 説明 |
|------|-------------|
| `"none"` | 認証なし（設定ファイルがない場合のデフォルト） |
| `"basic"` | `[[users]]`リストに対するHTTP Basic認証 |

設定ファイル内のパスワードはPBKDF2ハッシュとして保存されます。

`auth = "basic"`の場合、すべてのリクエスト（`GET /health`を除く）に`Authorization: Basic <credentials>`ヘッダーが必要です。

### CORS

| `origin`の値 | 動作 |
|----------------|-----------|
| `"*"`（デフォルト） | すべてのオリジンを許可 |
| `"none"` | CORSヘッダーなし |
| URL（例：`"https://app.example.com"`） | そのオリジンのみ許可 |

## HTTP API

すべてのSQLリクエストとレスポンスはPetraDBバイナリコーデック（`application/octet-stream`）を使用します。これらのエンドポイントを直接呼び出す代わりに、[`@petradb/client`](/guides/client/)ライブラリを使用してください。

### SQLの実行

```
POST /sql
Content-Type: application/octet-stream
X-Session-Id: <session-id>   (オプション)

<sql text>
```

バイナリエンコードされた`Seq[Result]`を返します。`X-Session-Id`ヘッダーなしの場合、各リクエストは一時的なワンオフセッションで実行されます。

### セッションの作成

```
POST /session
```

`{ "sessionId": "<id>" }`を返します。返されたIDを後続の`X-Session-Id`ヘッダーで使用して、リクエスト間でトランザクション状態を共有します。

### セッションの終了

```
DELETE /session/<id>
```

成功時に`{ "ok": "true" }`を返し、セッションが存在しない場合は`404`を返します。

### ヘルスチェック

```
GET /health
```

`{ "status": "ok" }`を返します。認証の対象外です。

## エラーレスポンス

| ステータス | 意味 |
|--------|---------|
| 400 | SQLパースエラー、型エラー、または未定義の参照 |
| 401 | 認証情報が欠落または無効 |
| 409 | スキーマまたは制約違反 |
| 503 | セッション制限に達した |

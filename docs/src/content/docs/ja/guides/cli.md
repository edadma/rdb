---
title: CLI
description: PetraDBの対話型SQLシェルとコマンドラインツールです。
---

## インストール

```bash
npm install -g @petradb/cli
```

## 使い方

```bash
petradb [OPTIONS] [path]
```

パスが指定された場合、CLIはその場所に永続データベースを開く（または作成する）します。パスなしの場合、インメモリデータベースが使用されます。

### オプション

| オプション | 説明 |
|--------|-------------|
| `-m`, `--memory` | インメモリデータベースを使用（パスが指定されていても） |
| `-e`, `--execute` | SQLを実行して終了（繰り返し可能） |
| `-f`, `--file` | SQLファイルを実行して終了（繰り返し可能） |
| `--stdin` | 標準入力からSQLを読み取って終了 |
| `--host` | リモートPetraDBサーバーに接続 |
| `--port` | サーバーポート（デフォルト：5480） |
| `--user` | 認証用ユーザー名 |
| `--password` | 認証用パスワード |

### 例

```bash
# 対話型インメモリセッションを開始
petradb

# 永続データベースを開くまたは作成
petradb mydata.db

# 非対話的にSQLを実行
petradb mydata.db -e "SELECT * FROM users"

# ファイルを実行してからクエリを実行
petradb mydata.db -f schema.sql -e "SELECT count(*) FROM users"

# 標準入力からSQLをパイプ
echo "SELECT 1 + 1" | petradb --stdin

# リモートサーバーに接続
petradb --host myserver.example.com --port 5480

# 認証付きで接続
petradb --host localhost --user admin --password secret
```

## データベースモード

ファイル拡張子によってストレージモードが決まります。

| 拡張子 | モード | 説明 |
|-----------|------|-------------|
| `.petra`（またはその他） | 永続 | コピーオンライトページによるクラッシュセーフな永続ストレージ |
| `.ptxt` | テキスト | 人間が読めるファイル、変更のたびに書き換え |
| *（パスなし）* | インメモリ | セッション期間中のみデータが存在 |

## ダンプ

`dump`サブコマンドは、永続データベースの完全なスキーマとデータをSQL文として出力します。

```bash
petradb dump mydata.db
```

出力には`CREATE TYPE`、`CREATE TABLE`、`INSERT INTO`、`CREATE VIEW`文が含まれ、データベースをゼロから再作成できます。

## 対話型REPL

`-e`、`-f`、`--stdin`なしで起動すると、CLIはreadline履歴付きの対話型SQLシェルに入ります。

`;`で終わるSQLを入力すると実行されます。複数行入力がサポートされており、`;`で終わるまで継続プロンプト（`  -> `）が表示されます。

### メタコマンド

| コマンド | 説明 |
|---------|-------------|
| `\dt` | すべてのテーブルを一覧表示 |
| `\dv` | すべてのビューを一覧表示 |
| `\ds` | すべてのシーケンスを一覧表示 |
| `\di` | すべてのインデックスを一覧表示 |
| `\d <name>` | テーブルまたはビューの説明（カラム、型、NULL可否） |
| `\i <file>` | ファイルからSQLを実行 |
| `\dump` | 完全なスキーマとデータをSQLとして出力 |
| `\copy <args>` | `COPY`文を実行 |
| `\timing` | クエリ実行時間の表示を切り替え |
| `\q` | 終了 |

### セッション例

```
$ petradb
PetraDB — interactive SQL shell
Type \q to quit, \dt to list tables, \d <table> to describe a table.

petra> CREATE TABLE cities (name TEXT, population INT);
CREATE TABLE
petra> INSERT INTO cities (name, population) VALUES ('Oslo', 709037);
INSERT 0 1
petra> SELECT * FROM cities;
 name | population
------+------------
 Oslo |     709037
(1 row)
petra> \dt
 Table
--------
 cities
(1 row)
petra> \d cities
Table "cities"
 Column     | Type | Nullable
------------+------+----------
 name       | TEXT | null
 population | INT  | null
petra> \timing
Timing is on.
petra> SELECT count(*) FROM cities;
 count
-------
     1
(1 row)
Time: 0.002 s
petra> \q
```

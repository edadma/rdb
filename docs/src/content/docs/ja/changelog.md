---
title: 変更履歴
---

## v1.5-20260315

### PL/pgSQL — ストアドプロシージャ、関数、トリガー

DOブロック、ストアド関数、ストアドプロシージャにおける完全な手続き型言語サポートです。

- **DOブロック** — DECLARE、BEGIN...ENDを持つ匿名PL/pgSQLブロック
- **ストアド関数** — `CREATE FUNCTION name(params) RETURNS type AS $$ ... $$ LANGUAGE plpgsql`、任意のSQL式で呼び出し可能
- **ストアドプロシージャ** — `CREATE PROCEDURE name(params) AS $$ ... $$ LANGUAGE plpgsql`、`CALL`で呼び出し
- **トリガー** — `CREATE TRIGGER name BEFORE|AFTER INSERT|UPDATE|DELETE ON table FOR EACH ROW EXECUTE FUNCTION func()`
  - BEFOREトリガーはNULLを返すことで操作をキャンセルできます
  - AFTERトリガーは操作の後に実行されます
  - トリガーはINSERT、UPDATE、DELETE、COPY FROMで発火します
  - TG_OP、TG_TABLE_NAME、OLD、NEW変数が利用可能です
- **制御フロー** — IF/ELSIF/ELSE、WHILE LOOP、FORレンジ/クエリLOOP、RETURN、RAISE NOTICE/EXCEPTION、PERFORM、EXCEPTION WHEN
- **永続化** — 関数、プロシージャ、トリガーはPersistentDBおよびTextDBでclose/reopenしても保持されます
- **OR REPLACE** — 既存の関数やプロシージャを上書きできます

### ユーザー定義ネイティブ関数

ホスト言語のコールバックをSQL関数として登録し、クエリ、トリガー、プロシージャから呼び出せます。

- **Scala** — `db.registerScalarFunction("name", { args => result })`
- **JavaScript** — `session.registerFunction("name", (args) => result)`
- **C** — `petradb_create_function(db, "name", nargs, user_data, callback)` SQLiteスタイルの型付きvalue/context API

### Cライブラリとカーソル API

- **ネイティブ共有ライブラリ** — Scala Nativeの`@exported`でC呼び出し可能な関数を持つ`libpetradb-engine.so`
- **SQLiteスタイルのC API** — `petradb_open`、`petradb_exec`、`petradb_prepare/step/finalize`、型付きカラムアクセサ
- **ユーザー定義関数** — `petradb_value_int/double/text`、`petradb_result_int/double/text/null/error`、`petradb_user_data`
- **カーソルAPI** — `session.openCursor(sql)`による遅延行単位イテレーション、`step()`、型付きカラムアクセサ、`fetch(n)`、`move(n)`、パラメータ化クエリ
- **Cヘッダー** — 完全なAPIドキュメント付きの`petradb.h`
- **Cテストスイート** — 67テスト
- **Rust FFIテスト** — 38テストで言語間相互運用を実証

### 仮想テーブル

- **拡張可能なフレームワーク** — `CREATE VIRTUAL TABLE name USING module(args)`、読み取り専用、SHOW TABLESに表示
- **組み込みCSVモジュール** — `CREATE VIRTUAL TABLE t USING csv('file.csv')` ヘッダー/デリミタオプション付き
- **カスタムモジュール** — Scala APIの`db.registerVirtualTableModule("name", module)`

### csv_file()テーブル関数

インポートなしでCSVファイルを直接クエリできます。

```sql
SELECT * FROM csv_file('data.csv');
SELECT e.name, d.dept FROM csv_file('employees.csv') e
  JOIN csv_file('departments.csv') d ON e.dept_id = d.id;
```

### 高度なインデックス

- **部分インデックス** — `CREATE INDEX ... WHERE condition` — 述語に一致する行のみをインデックス化
- **式インデックス** — `CREATE INDEX ... ON table ((expr))` — `lower(email)`のような計算値をインデックス化
- **組み合わせ** — 部分インデックスと式インデックスを併用可能

### ウィンドウ関数

- **FIRST_VALUE(expr)** — ウィンドウフレームの最初の行の値
- **LAST_VALUE(expr)** — ウィンドウフレームの最後の行の値
- **NTH_VALUE(expr, n)** — フレームのn番目の行の値

### DELETE ... USING

PostgreSQL構文に準拠した複数テーブルの削除です。

```sql
DELETE FROM orders USING customers
WHERE orders.customer_id = customers.id AND customers.status = 'inactive';
```

### Quarry — 型安全なASTクエリビルダー

新しい`@petradb/quarry`パッケージ：SQL文字列ではなくASTオブジェクトを生成する型安全なクエリビルダーです。

- 21のカラム型によるスキーマ定義
- 完全なCRUD：型安全なカラム参照によるselect、insert、update、delete
- ジョイン：inner、left、right、full outer、cross（型付き結果）
- 式：50以上の演算子、集計、CASE/CAST/EXISTS、サブクエリ
- Upsert：`onConflictDoNothing()`、`onConflictDoUpdate()`
- セルフジョイン用のテーブルエイリアス
- トランザクション、RETURNING、DISTINCT ON
- すべての機能に対するコンパイル時型テスト
- 集合演算：UNION、INTERSECT、EXCEPT
- ウィンドウ関数、CTE、名前付きスカラーヘルパー

### バグ修正

- **ByteaValue** — BYTEAカラムへの`ARRAY[...]`が`ArrayValue`ではなく正しく`ByteaValue`を生成するようになりました
- **JS/Clientの結果型** — 非網羅的マッチによるクラッシュを防ぐため、欠落していたPL/pgSQL結果型ハンドラー（DoBlockResult、CreateFunctionResultなど）を追加しました
- **コーデック** — クライアント/サーバー通信用のすべての新しい結果型のシリアライゼーションを追加しました
- **llms.txt** — `type`フィールドを`command`フィールドに修正し、すべての結果型と機能を更新しました

### モジュール名変更

- **shared → common** — 共有型モジュールを`petradb-shared`から`petradb-common`に名称変更しました

### ドキュメント

- 新しい**PL/pgSQL**リファレンスページ（トリガー、関数、プロシージャ、制御フロー）
- 新しい**C API**リファレンスページ（完全なSQLiteスタイルインターフェース）
- JavaおよびCの新しい**はじめに**ガイド
- DDLドキュメント更新：部分/式インデックス、CHECK制約、トリガー、ストアドルーチン
- DMLドキュメント更新：DELETE...USING、csv_file()、仮想テーブル
- JS/Scala APIドキュメント更新：registerFunction、結果型
- ランディングページ：4つのはじめにボタン（JS、Java、Scala、C）
- すべての現在の機能を含むllms.txtの書き換え

### バージョンアップ

| コンポーネント | Maven Central | npm |
|-----------|---------------|-----|
| common | 1.5.0 | — |
| engine | 1.5.0 | @petradb/engine 1.5.0 |
| client | 1.5.0 | @petradb/client 1.5.0 |
| server | 1.5.0 | @petradb/server 1.5.0 |
| cli | 1.5.0 | @petradb/cli 1.5.0 |
| jdbc | 1.5.0 | — |
| drizzle | — | @petradb/drizzle 1.5.0 |
| knex | — | @petradb/knex 1.5.0 |
| lucid | — | @petradb/lucid 1.5.0 |
| quarry | — | @petradb/quarry 1.5.0 |

## v1.4-20260314

### バグ修正と改善

- **相関INサブクエリの修正** — インデックス付きテーブルでの相関`IN (SELECT ...)`が正しく動作するようになりました
- **修飾カラム解決** — 複雑なジョインでの曖昧なカラム参照の修正
- **空のIN/ANY処理** — `IN ()`および`= ANY('{}')`のエッジケースを解決
- **外部キーCASCADEクリーンアップ** — `DROP TABLE ... CASCADE`が子テーブルのFK制約を正しく削除するようになりました
- **DROP TABLE IF EXISTS ... CASCADE** — `IF EXISTS`と`CASCADE`の組み合わせがパースエラーを起こさなくなりました
- **配列リテラルのキャスト** — `'{1,2,3}'::integer[]` PostgreSQL配列リテラル構文のサポート
- **パラメータ化クエリの修正** — サブクエリと相関パスのパラメータバインディングを改善

### バージョンアップ

| コンポーネント | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.4.2 | — |
| engine | 1.4.9 | @petradb/engine 1.4.3 |
| client | 1.4.2 | @petradb/client 1.4.2 |
| server | — | @petradb/server 1.4.3 |
| cli | — | @petradb/cli 1.4.3 |
| jdbc | 1.4.3 | — |
| drizzle | — | @petradb/drizzle 1.4.3 |

## v1.4-20260312

### 共通テーブル式（CTE）

`WITH`および`WITH RECURSIVE`による完全なCTEサポートです。

**非再帰CTE** — 可読性と再利用のための名前付きサブクエリです。

```sql
WITH active_orders AS (
  SELECT * FROM orders WHERE status = 'active'
)
SELECT customer_id, SUM(amount)
FROM active_orders
GROUP BY customer_id;
```

単一クエリ内で複数のCTEを定義でき、後のCTEは先のCTEを参照できます。カラムエイリアスがサポートされています：`WITH t(x, y) AS (...)`。CTEは同名のテーブルをシャドウします。

**再帰CTE** — 階層データやグラフデータのための反復クエリです。

```sql
WITH RECURSIVE descendants(id, name, depth) AS (
  SELECT id, name, 0 FROM employees WHERE manager_id IS NULL
  UNION ALL
  SELECT e.id, e.name, d.depth + 1
  FROM employees e INNER JOIN descendants d ON e.manager_id = d.id
)
SELECT name, depth FROM descendants ORDER BY depth, name;
```

`UNION ALL`（重複保持）と`UNION`（重複排除）の両方がサポートされています。安全制限として最大1000回の反復です。

### ウィンドウ関数

3つのカテゴリによる完全なウィンドウ関数サポートです。

**ランキング関数** — `PARTITION BY`と`ORDER BY`を使った`ROW_NUMBER()`、`RANK()`、`DENSE_RANK()`です。

```sql
SELECT name, department, salary,
  RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dept_rank
FROM employees;
```

**値関数** — 設定可能なオフセットとデフォルト値を持つ`LAG()`、`LEAD()`、`NTILE()`です。

```sql
SELECT name, salary,
  LAG(salary, 1, 0) OVER (ORDER BY salary) AS prev_salary,
  NTILE(4) OVER (ORDER BY salary) AS quartile
FROM employees;
```

**集約ウィンドウ関数** — フレーム指定を含む、`OVER()`付きの任意の集約関数（`SUM`、`COUNT`、`AVG`、`MIN`、`MAX`など）です。

```sql
SELECT name, salary,
  SUM(salary) OVER (ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_total,
  AVG(salary) OVER (PARTITION BY department) AS dept_avg
FROM employees;
```

フレーム境界：`UNBOUNDED PRECEDING`、`UNBOUNDED FOLLOWING`、`CURRENT ROW`、`N PRECEDING`、`N FOLLOWING`。フレーム句がない場合、集約ウィンドウ関数はパーティション全体で計算されます。

### 集約FILTER句

グループ化クエリとウィンドウ関数の両方で、集約関数に`FILTER (WHERE ...)`を適用できます。

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE status = 'active') AS active_count,
  SUM(amount) FILTER (WHERE amount > 100) OVER (ORDER BY created_at
    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_high_value
FROM orders;
```

### ハッシュジョイン最適化

インデックスのない等価結合がクロス積の代わりにハッシュジョイン戦略を使用するようになり、ジョインの計算量がO(n×m)からO(n+m)に削減されました。INNER、LEFT、RIGHT、FULLジョインに適用されます。インデックスが利用可能な場合はインデックスネストループジョインが優先されます。非等価結合条件は引き続きクロス積にフォールバックします。`EXPLAIN`出力では`Hash Join`、`Hash Left Join`、`Hash Right Join`、`Hash Full Join`として表示されます。

### 生成カラム

`GENERATED ALWAYS AS (expr) STORED`による計算カラムです。

```sql
CREATE TABLE products (
  price NUMERIC,
  tax_rate NUMERIC DEFAULT 0.08,
  total NUMERIC GENERATED ALWAYS AS (price * (1 + tax_rate)) STORED
);
```

生成カラムはINSERTおよびUPDATE時に再計算されます。直接設定することはできません。

### ORDER BY NULLの順序

`ORDER BY`がSQL標準のNULL順序をデフォルトとするようになりました：`ASC` → `NULLS LAST`、`DESC` → `NULLS FIRST`。明示的な`NULLS FIRST` / `NULLS LAST`によるオーバーライドがサポートされています。

### シーケンスサポート

PostgreSQL互換の完全なシーケンスサポートです。オプション付きの`CREATE SEQUENCE`および`DROP SEQUENCE`（`INCREMENT BY`、`START WITH`、`MINVALUE`、`MAXVALUE`、`CYCLE`、`IF NOT EXISTS` / `IF EXISTS`）。シーケンス関数：`nextval()`、`currval()`、`setval()`、`lastval()`。

`SERIAL`、`SMALLSERIAL`、`BIGSERIAL`カラムがバッキングシーケンス（`<table>_<column>_seq`という名前）を作成するようになり、PostgreSQLの動作と一致します。`DROP TABLE`は所有シーケンスのドロップにカスケードします。`TRUNCATE`はバッキングシーケンスをリセットします。シーケンスの状態は完全にトランザクショナルです — `ROLLBACK`でシーケンスカウンターが復元されます。永続データベースはシーケンスの状態をカタログにシリアライズします。

新しいSQLコマンド：`SHOW SEQUENCES`、`SHOW INDEXES`（すべてのテーブルのすべてのインデックス）。

CLI：新しい`\ds`（シーケンス一覧）および`\di`（インデックス一覧）メタコマンド。

### CREATE INDEX USING句

`CREATE INDEX ... USING btree`構文が受け入れられるようになりました（btreeが唯一のサポートされるメソッドです）。これによりPostgrSQL生成DDLおよびORMとの互換性が向上します。

### バグ修正

- `ORDER BY`とNULL値：比較子が両方の値がNULLの場合に0を返すようになり、複数のソートキーでの非決定的なソート結果を修正しました
- `ORDER BY`のエイリアス解決：SELECTエイリアス（例：`SELECT x AS y ... ORDER BY y`）がウィンドウ関数の有無にかかわらず、非グループ化クエリで正しく解決されるようになりました
- `Type.convert()`のNULL処理：型変換を通過するNULL値（例：UPDATE SETのプリペアドステートメントパラメータ経由）が型のテキスト表現に変換される代わりにNULLとして保持されるようになりました。13型で修正：TEXT、VARCHAR、CHAR、UUID、TIMESTAMP、DATE、TIME、TIMETZ、INTERVAL、TIMESTAMPTZ、BYTEA、JSON、ENUM
- ORDER BYパーサーのnulls句における網羅的マッチ警告
- プレイグラウンドターミナルでの同期throwのサイレントエラー

### バージョンアップ

すべてのコンポーネントが1.4.1にアップされました。

| コンポーネント | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.4.1 | — |
| engine | 1.4.1 | @petradb/engine 1.4.1 |
| client | 1.4.1 | @petradb/client 1.4.1 |
| server | 1.4.1 | @petradb/server 1.4.1 |
| cli | 1.4.1 | @petradb/cli 1.4.1 |
| jdbc | 1.4.1 | — |
| knex | — | @petradb/knex 1.4.0 |
| lucid | — | @petradb/lucid 1.4.0 |
| drizzle | — | @petradb/drizzle 1.4.1 |

## v1.3-20260309

### トランザクショナルDDL

DDL文（CREATE TABLE、CREATE INDEX、DROP TABLEなど）がトランザクション内で完全にサポートされるようになり、DMLとともにアトミックにロールバックされます。DDLとDMLは単一のBEGIN/COMMITブロック内で自由にインターリーブできます。MemoryDBとPersistentDBの両方がBEGIN時に完全なカタログスナップショットをキャプチャし、ROLLBACK時に復元します。

### Drizzleリレーショナルクエリ

Drizzle ORMリレーショナルクエリの完全サポート（`db.query.*.findMany()`、`db.query.*.findFirst()`）。`json_build_array`および`json_build_object`スカラー関数を追加し、LATERALサブクエリでのパラメータ置換を修正しました。

### バージョンアップ

| コンポーネント | Maven Central | npm |
|-----------|---------------|-----|
| engine | 1.3.1 | @petradb/engine 1.3.3 |
| server | 1.3.1 | @petradb/server 1.3.1 |
| cli | 1.3.1 | @petradb/cli 1.3.1 |
| jdbc | 1.3.1 | — |
| drizzle | — | @petradb/drizzle 1.3.1 |

## v1.3-20260308

### スキーマサポート

PostgreSQLスタイルのスキーマ名前空間です。すべてのデータベースにデフォルトで`public`スキーマがあり、非修飾テーブル名は`public`に解決されます。スキーマ修飾名（`schema.table`）はすべてのDDLおよびDML文で機能します — CREATE TABLE、INSERT、UPDATE、DELETE、SELECT、ALTER TABLE、DROP TABLE、TRUNCATE、CREATE INDEX、COPY。

```sql
CREATE SCHEMA inventory;
CREATE TABLE inventory.products (id SERIAL PRIMARY KEY, name TEXT);
INSERT INTO inventory.products (name) VALUES ('Widget');
SELECT * FROM inventory.products;
```

### information_schema仮想テーブル

`information_schema.schemata`、`information_schema.tables`、`information_schema.columns`がクエリ可能になりました。スキーマ修飾テーブルは正しい`table_schema`を報告します。これらのビューはデータベースメタデータから動的に生成されます。

### Drizzle ORMマイグレーション

`@petradb/drizzle`の新しい`migrate()`関数がDrizzle Kitマイグレーションファイルを適用します。`meta/_journal.json`を読み取り、SQLマイグレーションファイルを順番に実行し、適用されたマイグレーションを`drizzle.__drizzle_migrations`で追跡します。

```typescript
import { migrate } from "@petradb/drizzle";
await migrate(db, { migrationsFolder: "./drizzle" });
```

### JDBCメタデータの改善

`DatabaseMetaData.getColumns()`がカラム型と精度/スケール宣言に基づいて正確な`COLUMN_SIZE`、`DECIMAL_DIGITS`、`CHAR_OCTET_LENGTH`値を返すようになりました。

### バージョンアップ

すべてのコンポーネントが1.3.0にアップされました。

| コンポーネント | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.3.0 | — |
| engine | 1.3.0 | @petradb/engine 1.3.0 |
| client | 1.3.0 | @petradb/client 1.3.0 |
| server | 1.3.0 | @petradb/server 1.3.0 |
| cli | 1.3.0 | @petradb/cli 1.3.0 |
| jdbc | 1.3.0 | — |
| knex | — | @petradb/knex 1.3.0 |
| lucid | — | @petradb/lucid 1.3.0 |
| drizzle | — | @petradb/drizzle 1.3.0 |

## v1.2-20260308

### Drizzle ORMドライバーの書き換え

`@petradb/drizzle`が`drizzle-orm/pg-proxy`ラッパーから、`PgSession`/`PgPreparedQuery`/`PgTransaction`を直接拡張するカスタムPostgreSQLダイアレクトドライバーに書き換えられました。これにより`drizzle-orm/node-postgres`と完全な機能パリティが実現されました。

- `db.transaction()` 自動コミット/ロールバック付き
- `tx.rollback()` 明示的ロールバック用
- `returning()` insert/update/delete（部分カラム選択を含む）
- リレーショナルクエリサポート（エンジンの`json_build_array`/`json_agg`サポート待ち）

### 型変換：テキストパラメータからNUMERICカラムへ

`NumericType.convert`が`TextValue`を受け入れ、`BigDecimal`としてパースするようになり、`IntegerType`、`BigintType`、`SmallintType`、`DoubleType`の既存の変換動作と一致しました。これにより、数値をテキストとして送信するORM経由のパラメータ化INSERT/UPDATEが修正されました（標準的なPostgreSQLワイヤプロトコルの動作）。

### 三値NULLロジック

NULL処理のための完全なSQL三値ロジックです。

- 比較演算子（`=`、`!=`、`<`、`>`、`<=`、`>=`）はいずれかのオペランドがNULLの場合にNULLを返します
- `AND`/`OR`は適切な三値真理値表を実装します（例：`FALSE AND NULL` → `FALSE`、`TRUE OR NULL` → `TRUE`）
- `IN`/`NOT IN`はNULLを正しく伝播します（例：`3 NOT IN (1, 2, NULL)` → unknown）
- 算術演算（`+`、`-`、`*`、`/`、`%`）および文字列連結（`||`）はNULLを伝播します
- `LIKE`はNULLオペランドを処理します

### 統一式文法

SQLパーサーの別個の`expression`と`booleanExpression`階層が単一の式構文に統合されました。ブール演算子（`AND`、`OR`、`NOT`）は優先順位チェーン内の通常の演算子になりました。これにより、式が有効な場所であればどこでもブール式を使用できます（例：`SELECT a > 5 AND b < 10`）。

### 即時カラム参照バリデーション

`WHERE`、`GROUP BY`、`HAVING`、`ORDER BY`のカラム参照がクエリプラン構築時に即座にバリデーションされるようになり、空のテーブルや単一行のソートでも存在しないカラムを検出できます。以前は、不正な参照は行ごとの評価時にのみ検出されていたため、空のテーブルに対するクエリが暗黙的に成功していました。

### バグ修正

- UPDATEでNOT NULL制約が適用されていなかった問題
- UNIQUE制約が複数のNULLを拒否していた問題（SQL標準：NULLは区別される）
- INSERTカラムリストの重複カラムが検出されていなかった問題
- 空のテーブルで`SUM`/`AVG`/`MIN`/`MAX`がNULLの代わりに0を返していた問題
- `LIKE '_'`が空文字列にマッチしていた問題
- `LIMIT 0`がエラーを発生させていた問題

### バージョンアップ

| コンポーネント | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | — |
| engine | 1.2.9 | @petradb/engine 1.2.16 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | 1.2.6 | @petradb/server 1.2.9 |
| cli | 1.2.8 | @petradb/cli 1.2.9 |
| jdbc | 1.2.13 | — |
| knex | — | @petradb/knex 1.2.2 |
| lucid | — | @petradb/lucid 1.2.1 |
| drizzle | — | @petradb/drizzle 1.2.2 |

## v1.2-20260307

### SQL：INSERT VALUESでの`DEFAULT`キーワード

`INSERT INTO t (id, name) VALUES (DEFAULT, 'Alice')`が動作するようになりました。SQL標準の`DEFAULT`キーワードは以前パーサーに拒否されており、シリアルやデフォルト付きカラムに対して明示的に`DEFAULT`を渡すORM生成のINSERT文が動作しませんでした。

### Drizzle ORM統合

新しい`@petradb/drizzle`パッケージがカスタムPostgreSQLダイアレクト実装による[Drizzle ORM](https://orm.drizzle.team)ドライバーを提供します。`pgTable`によるスキーマ定義、insert/select/update/delete、returning句、自動コミット/ロールバック付き`db.transaction()`、型安全なクエリをサポートします。`drizzle-orm/node-postgres`と完全な機能パリティです。

### 依存パッケージのバージョンアップ

DEFAULTキーワード修正を反映するため、engine、server、cli、jdbcがバージョンアップされました。

| コンポーネント | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | — |
| engine | 1.2.7 | @petradb/engine 1.2.14 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | — | @petradb/server 1.2.7 |
| cli | — | @petradb/cli 1.2.7 |
| jdbc | 1.2.11 | — |
| knex | — | @petradb/knex 1.2.2 |
| lucid | — | @petradb/lucid 1.2.1 |
| drizzle | — | @petradb/drizzle 1.2.0 |

## v1.2-20260306

### JS API：`close()`が`Promise<void>`を返すように変更
`Session.close()`が`void`の代わりに`Promise<void>`を返すようになり、相互運用性のためにclientモジュールのAPIと一致しました。

### タイムスタンプパース
`parseTimestamp`が`Z`サフィックス、`+/-HH:MM`オフセット、ミリ秒、およびタイムゾーン情報付きのスペース区切りタイムスタンプを処理するようになりました。`TIMESTAMP`カラムではタイムゾーンを除去して`LocalDateTime`にします。

### JSファサードの完全性
`toJS`と`typeString`が`DateValue`、`TimeValue`、`TimestampTZValue`、`TimeTZValue`、`IntervalValue`、`ByteaValue`を処理するようになりました。

### SQL：修飾スター（`table.*`）
`SELECT t.*`構文がジョインや混合式を含むクエリで動作するようになりました。

### 比較における型変換
- `NumberValue`と`TextValue`が型をまたいで比較できるようになりました（テキストパラメータと数値カラム、およびその逆）
- `TimestampValue`がテキストをタイムスタンプとしてパースすることで`TextValue`と比較できるようになりました

### Knexドライバー：Dateバインディング
`_sanitizeBindings`がJSの`Date`オブジェクトをISO文字列に変換してからエンジンに渡すようになり、`Date.toString()`フォーマットでの`DateTimeParseException`を防止します。

| コンポーネント | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | — |
| engine | 1.2.6 | @petradb/engine 1.2.13 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | — | @petradb/server 1.2.5 |
| cli | — | @petradb/cli 1.2.5 |
| jdbc | 1.2.10 | — |
| knex | — | @petradb/knex 1.2.2 |
| lucid | — | @petradb/lucid 1.2.1 |

## v1.2-20260305

### JDBCドライバー
- ファットjar公開 — `io.github.edadma:petradb-jdbc`がMaven Central上の単一の自己完結型jarになりました
- クリーンな接続URL — `jdbc:petradb:memory`、`jdbc:petradb:file:/path`、`jdbc:petradb://host:port`
- ServiceLoader自動検出 — `Class.forName`なしで`DriverManager.getConnection()`が動作します
- ハードコードされたメタデータバージョン文字列を修正

### JS/TSエンジン（`@petradb/engine`）
- JSファサードに`CreateViewResult`、`DropViewResult`、`ExplainResult`、`CopyResult`を追加
- TypeScript型定義に`ExplainResult`と`CopyResult`を追加

### ドキュメント
- 完全な例を含む新しいKnex.jsガイド
- JDBCドキュメント：Maven/Gradle/sbtインストールスニペットを追加、ポート番号を修正

### インフラストラクチャ
- `petradb-shared`がMaven Centralに公開可能に
- npm、Scala、JDBCアーティファクトを網羅する公開後スモークテストスクリプト

| コンポーネント | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.1 | — |
| engine | 1.2.2 | @petradb/engine 1.2.5 |
| client | 1.2.3 | @petradb/client 1.2.3 |
| server | — | @petradb/server 1.2.3 |
| cli | — | @petradb/cli 1.2.3 |
| jdbc | 1.2.6 | — |
| knex | — | @petradb/knex 1.2.0 |

## v1.2.2

### エンジンサブパッケージの再構成
- エンジンが`io.github.edadma.petradb.engine`サブパッケージに移動
- エンジンとクライアントの両方が拡張する新しい共有`Session`トレイト

### CLIクライアントサポート
- リモートPetraDBサーバーに接続：`petradb --host localhost --port 5480`
- 認証用の`--user`と`--password`フラグ
- メタコマンドがSQL経由でネットワーク越しに動作

### SQL
- `SHOW VIEWS`コマンドがビュー名と定義を返すように

### Knexダイアレクト
- PetraDBでKnex.jsクエリビルダーを使用するための`@petradb/knex`ダイアレクトアダプター

### 修正
- clientのnpm公開を修正
- CLIのnpm公開を修正
- ハードコードされたJDBCメタデータバージョン文字列を修正
- JVM、JS、Nativeで1013以上のテストが合格

## v1.2

### JDBCドライバー
- Maven Centralに`petradb-jdbc`として公開
- `getGeneratedKeys()`、`addBatch()`/`executeBatch()`、DBeaver用のFK/インデックスメタデータ
- ファイルモード（組み込み）とサーバーモード（ネットワーク）接続

### SQLエンジン
- CSV入出力のための`COPY FROM/TO`
- `CREATE TEMP TABLE`、`CREATE/DROP VIEW`
- `SHOW FOREIGN KEYS`/`SHOW INDEXES`イントロスペクション
- 等価結合のためのインデックスネストループジョイン最適化
- パーサーをfastparseに移行

### サーバー
- TOML設定によるCORSサポート
- 設定可能な`max_sessions`、デフォルトポート5480
- Node.js HTTPバックエンドによるJSサーバープラットフォーム

### クライアント
- JSファサード付きの新しい`@petradb/client` npmパッケージ
- Promiseを返す`connect()`/`execute()`/`close()`を持つ`Session`クラス

### CLI
- `\timing`、`\copy`コマンド
- Nativeでの永続履歴

### ビルド
- Scala 3.8.2、sbt 1.12.4
- JVM、JS、Nativeで1000テスト合格

## v1.1.0

### TextDB — 人間が編集可能なテキストファイル永続化
データベースを`.ptxt`テキストファイルとして永続化する新しいストレージバックエンドです。開くとメモリにロードされ、変更のたびにファイルを書き換えます。

### Upsert — `ON CONFLICT DO UPDATE`
`EXCLUDED`擬似テーブルによるinsert-or-updateセマンティクスです。

### 改善された例外階層
汎用的な`problem()`呼び出しに代わる型付き例外クラスです。

### ALTER TABLEの集約
`DB.alterTable()`がすべてのALTER TABLEディスパッチを集約するようになりました。

## v1.0.1

- `@petradb/engine`で`ConnectSQL`を`Session`にリネーム
- `Promise<ExecuteResult[]>`を返す非同期`execute()` API
- ネットワーク使用のための新しい`@petradb/client`パッケージ
- エンジンとサーバー間のレスポンスフォーマットの統一

## v1.0.0

最初の安定リリースです。

- クロスプラットフォームSQLエンジン（JVM、JavaScript、Native）
- PostgreSQL互換構文
- インメモリおよび永続（クラッシュセーフ）ストレージ
- DDL、DML、ジョイン、サブクエリ、集計、トランザクション
- JSONB演算子、配列型、CHECK制約
- 879のテストが合格

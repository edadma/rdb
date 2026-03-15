---
title: DDL
description: データ定義言語 — CREATE、ALTER、DROP、TRUNCATE文です。
---

## スキーマ

PetraDBはPostgreSQLスタイルのスキーマ名前空間をサポートしています。すべてのデータベースにデフォルトで`public`スキーマがあります。非修飾テーブル名は`public`に解決されます。

### CREATE SCHEMA

```sql
CREATE SCHEMA inventory;
CREATE SCHEMA IF NOT EXISTS inventory;
```

### スキーマ修飾テーブル

任意のDDLまたはDML文で`schema.table`構文を使用します。

```sql
CREATE TABLE inventory.products (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  price NUMERIC(10,2)
);

INSERT INTO inventory.products (name, price) VALUES ('Widget', 9.99);
SELECT * FROM inventory.products;
```

同じ名前のテーブルが異なるスキーマに存在できます。

```sql
CREATE SCHEMA staging;
CREATE TABLE staging.products (id SERIAL, name TEXT);
CREATE TABLE public.products (id SERIAL, name TEXT);
-- これらは別々のテーブルです
```

### information_schema

PetraDBはデータベース構造をイントロスペクションするための`information_schema`仮想テーブルを提供します。

```sql
SELECT * FROM information_schema.schemata;
SELECT * FROM information_schema.tables;
SELECT * FROM information_schema.columns WHERE table_name = 'products';
```

利用可能なビュー：`schemata`、`tables`、`columns`。

## テーブル

### CREATE TABLE

```sql
CREATE TABLE orders (
  id UUID DEFAULT gen_random_uuid(),
  customer_name TEXT NOT NULL,
  amount NUMERIC(10,2),
  status order_status,
  tags INT[],
  metadata JSON,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS orders (...);
```

### 外部キー

```sql
CREATE TABLE line_items (
  id SERIAL,
  order_id UUID REFERENCES orders (id) ON DELETE CASCADE ON UPDATE CASCADE,
  product TEXT NOT NULL
);
```

### 生成カラム

他のカラムから自動的に導出される計算カラムです。

```sql
CREATE TABLE products (
  price NUMERIC,
  tax_rate NUMERIC DEFAULT 0.08,
  total NUMERIC GENERATED ALWAYS AS (price * (1 + tax_rate)) STORED
);

INSERT INTO products (price) VALUES (100);
SELECT * FROM products;
-- price: 100, tax_rate: 0.08, total: 108
```

生成カラムはINSERTおよびUPDATE時に再計算されます。直接書き込むことはできません。

### ALTER TABLE

カラムの追加、削除、名前変更をします。

```sql
ALTER TABLE orders ADD COLUMN notes TEXT;
ALTER TABLE orders DROP COLUMN notes;
ALTER TABLE orders RENAME COLUMN amount TO total;
ALTER TABLE orders RENAME TO purchases;
```

カラムプロパティの変更をします。

```sql
ALTER TABLE orders ALTER COLUMN notes SET NOT NULL;
ALTER TABLE orders ALTER COLUMN notes DROP NOT NULL;
ALTER TABLE orders ALTER COLUMN notes SET DEFAULT 'none';
ALTER TABLE orders ALTER COLUMN notes DROP DEFAULT;
ALTER TABLE orders ALTER COLUMN amount SET DATA TYPE NUMERIC(10,2);
```

制約の追加と削除をします。

```sql
ALTER TABLE orders ADD CONSTRAINT chk_amount CHECK (amount > 0);
ALTER TABLE orders ADD CONSTRAINT uq_email UNIQUE (email);
ALTER TABLE orders ADD CONSTRAINT fk_customer
  FOREIGN KEY (customer_id) REFERENCES customers (id) ON DELETE CASCADE;
ALTER TABLE orders DROP CONSTRAINT chk_amount;
```

### CHECK制約

カラム値に条件を強制します。

```sql
CREATE TABLE products (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  price NUMERIC CHECK (price >= 0),
  quantity INT,
  CONSTRAINT positive_qty CHECK (quantity >= 0)
);
```

CHECK制約はINSERTおよびUPDATE時に適用されます。

## トリガー

完全なドキュメントについては[PL/pgSQL — トリガー](/reference/plpgsql/#トリガー)をご覧ください。

```sql
CREATE TRIGGER trg_audit AFTER INSERT ON orders
  FOR EACH ROW EXECUTE FUNCTION audit_log();

DROP TRIGGER trg_audit ON orders;
DROP TRIGGER IF EXISTS trg_audit ON orders;
```

### TRUNCATE TABLE

すべての行を削除し、シリアルシーケンスをリセットします。

```sql
TRUNCATE TABLE orders;
```

### DROP TABLE

```sql
DROP TABLE orders;
DROP TABLE IF EXISTS orders;
```

## ビュー

### CREATE VIEW

クエリに基づく名前付きビューを作成します。既存のビューを上書きするには`OR REPLACE`を使用します。

```sql
CREATE VIEW active_orders AS
  SELECT * FROM orders WHERE status = 'pending';

CREATE OR REPLACE VIEW active_orders AS
  SELECT * FROM orders WHERE status != 'delivered';
```

### DROP VIEW

```sql
DROP VIEW active_orders;
DROP VIEW IF EXISTS active_orders;
```

## カスタム型

### CREATE TYPE

列挙型を定義します。

```sql
CREATE TYPE order_status AS ENUM ('pending', 'shipped', 'delivered');
```

### DROP TYPE

```sql
DROP TYPE order_status CASCADE;
```

## シーケンス

シーケンスは連続した数値を生成する名前付きカウンターです。主キーの生成に一般的に使用されます。

### CREATE SEQUENCE

```sql
CREATE SEQUENCE order_seq;
CREATE SEQUENCE order_seq START WITH 100 INCREMENT BY 10;
CREATE SEQUENCE IF NOT EXISTS order_seq;
```

オプション：

| オプション | デフォルト | 説明 |
|--------|---------|-------------|
| `INCREMENT BY n` | 1 | ステップサイズ |
| `START WITH n` | 1 | 初期値 |
| `MINVALUE n` / `NO MINVALUE` | 1 | 最小値 |
| `MAXVALUE n` / `NO MAXVALUE` | 4611686018427387903 | 最大値 |
| `CYCLE` / `NO CYCLE` | `NO CYCLE` | 制限でラップアラウンドするかどうか |

### DROP SEQUENCE

```sql
DROP SEQUENCE order_seq;
DROP SEQUENCE IF EXISTS order_seq;
```

### SERIALとシーケンス

`SERIAL`、`SMALLSERIAL`、`BIGSERIAL`カラムは自動的に`<table>_<column>_seq`という名前のバッキングシーケンスを作成します。これはPostgreSQLの動作と一致します。

```sql
CREATE TABLE orders (id SERIAL PRIMARY KEY, name TEXT);
-- 暗黙的にシーケンス"orders_id_seq"を作成

SELECT nextval('orders_id_seq');   -- 動作します
SELECT currval('orders_id_seq');   -- INSERTまたはnextvalの後に動作します
```

テーブルの削除は所有シーケンスの削除にカスケードします。`TRUNCATE`はバッキングシーケンスを開始値にリセットします。

### シーケンス関数

| 関数 | 説明 |
|----------|-------------|
| `nextval('seq_name')` | シーケンスを進めて次の値を返す |
| `currval('seq_name')` | 現在の値を返す（セッションで先にnextvalの呼び出しが必要） |
| `setval('seq_name', value)` | 現在の値を設定。次のnextvalはvalue + incrementを返す |
| `setval('seq_name', value, false)` | 現在の値を設定。次のnextvalはvalueを返す |
| `lastval()` | このセッションで任意のシーケンスから返された最後の値 |

## ストアド関数とプロシージャ

完全なドキュメントについては[PL/pgSQL](/reference/plpgsql/)をご覧ください。

```sql
CREATE FUNCTION double(x INT) RETURNS INT AS $$
BEGIN RETURN x * 2; END $$ LANGUAGE plpgsql;

CREATE PROCEDURE reset_counts() AS $$
BEGIN UPDATE counters SET val = 0; END $$ LANGUAGE plpgsql;

DROP FUNCTION double;
DROP PROCEDURE IF EXISTS reset_counts;
```

## インデックス

```sql
CREATE INDEX idx_orders_status ON orders (status);
CREATE UNIQUE INDEX idx_orders_email ON orders (email);
DROP INDEX idx_orders_status;
```

### 部分インデックス

条件に一致する行のみをインデックス化し、インデックスをより小さく高速にします。

```sql
CREATE INDEX idx_active_orders ON orders (customer_id) WHERE status = 'active';
CREATE UNIQUE INDEX idx_unique_active_email ON users (email) WHERE active = true;
```

クエリプランナーは、クエリの`WHERE`句にインデックス条件が含まれる場合にのみ部分インデックスを使用します。

### 式インデックス

生のカラムではなく計算値にインデックスを作成します。

```sql
CREATE INDEX idx_lower_email ON users ((lower(email)));
CREATE UNIQUE INDEX idx_lower_name ON users ((lower(name)));
```

式は括弧で囲む必要があります。プランナーは`WHERE lower(email) = 'alice@test.com'`を自動的にインデックスにマッチします。

部分インデックスと式インデックスは組み合わせることができます。

```sql
CREATE INDEX idx_active_lower ON users ((lower(name))) WHERE status = 'active';
```

## SHOWコマンド

データベースメタデータを検査します。

```sql
SHOW TABLES;
SHOW VIEWS;
SHOW SEQUENCES;
SHOW COLUMNS orders;
SHOW PRIMARY KEY orders;
SHOW FOREIGN KEYS orders;
SHOW INDEXES orders;
SHOW INDEXES;              -- すべてのテーブルのすべてのインデックス
```

### SHOW VIEWSの出力

| カラム | 型 | 説明 |
|--------|------|-------------|
| `view_name` | TEXT | ビュー名 |
| `definition` | TEXT | ビューを定義するSQLクエリ |

### SHOW COLUMNSの出力

| カラム | 型 | 説明 |
|--------|------|-------------|
| `name` | TEXT | カラム名 |
| `type` | TEXT | データ型 |
| `required` | BOOLEAN | NOT NULL制約 |
| `indexed` | BOOLEAN | インデックスがあるか |
| `unique` | BOOLEAN | ユニーク制約があるか |
| `fk_table` | TEXT | 外部キーのターゲットテーブル |
| `fk_column` | TEXT | 外部キーのターゲットカラム |
| `fk_on_delete` | TEXT | ON DELETEアクション |
| `fk_on_update` | TEXT | ON UPDATEアクション |
| `default_value` | TEXT | デフォルト式 |

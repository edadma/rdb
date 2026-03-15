---
title: DML
description: データ操作言語 — INSERT、UPDATE、DELETE、COPY文です。
---

## INSERT

```sql
INSERT INTO orders (customer_name, amount)
VALUES ('Alice Smith', 149.99);
```

### RETURNING

挿入された行から値を返します。

```sql
INSERT INTO orders (customer_name, amount)
VALUES ('Bob Johnson', 75.50)
RETURNING id;

INSERT INTO orders (customer_name, amount)
VALUES ('Carol', 200.00)
RETURNING *;
```

### クエリからのInsert

```sql
INSERT INTO archive (customer_name, amount)
SELECT customer_name, amount FROM orders WHERE status = 'delivered';
```

### INSERT ... ON CONFLICT（Upsert）

コンフリクト行をスキップします。

```sql
INSERT INTO users (email, name)
VALUES ('alice@example.com', 'Alice')
ON CONFLICT DO NOTHING;
```

特定のカラムでコンフリクト時に更新します。

```sql
INSERT INTO users (email, name)
VALUES ('alice@example.com', 'Alice Updated')
ON CONFLICT (email) DO UPDATE SET name = 'Alice Updated';
```

RETURNINGと組み合わせできます。

```sql
INSERT INTO users (email, name)
VALUES ('alice@example.com', 'Alice')
ON CONFLICT (email) DO UPDATE SET name = 'Alice'
RETURNING *;
```

## UPDATE

```sql
UPDATE orders SET status = 'shipped' WHERE amount > 100;
```

### VALUESからの一括更新

PostgreSQLスタイルの`UPDATE ... FROM`です。

```sql
UPDATE orders
  SET status = d.status
  FROM (VALUES ('ord-1', 'shipped'), ('ord-2', 'delivered'))
       AS d (id, status)
  WHERE orders.id = d.id;
```

### RETURNING付きUPDATE

```sql
UPDATE orders SET status = 'shipped'
WHERE id = 42
RETURNING id, status;
```

## DELETE

```sql
DELETE FROM orders WHERE status = 'delivered';
```

### DELETE ... USING

他のテーブルをジョインして削除する行を決定します。

```sql
DELETE FROM orders
USING customers
WHERE orders.customer_id = customers.id
  AND customers.status = 'inactive';
```

複数のUSINGテーブル：

```sql
DELETE FROM order_items
USING products, categories
WHERE order_items.product_id = products.id
  AND products.category_id = categories.id
  AND categories.name = 'discontinued';
```

### RETURNING付きDELETE

```sql
DELETE FROM orders WHERE status = 'delivered'
RETURNING *;
```

## COPY

CSV形式でのデータの一括インポートとエクスポートです。

### COPY FROM

CSVファイルから行をインポートします。

```sql
COPY orders FROM 'data/orders.csv';
COPY orders FROM 'data/orders.csv' WITH (HEADER);
COPY orders FROM 'data/orders.csv' WITH (HEADER, DELIMITER '|');
COPY orders (customer_name, amount) FROM 'data/partial.csv' WITH (HEADER);
```

### COPY TO

テーブルまたはクエリをCSVファイルにエクスポートします。

```sql
COPY orders TO 'export/orders.csv';
COPY orders TO 'export/orders.csv' WITH (HEADER);
COPY (SELECT * FROM orders WHERE status = 'pending') TO 'export/pending.csv' WITH (HEADER);
```

### オプション

| オプション | 説明 |
|--------|-------------|
| `HEADER` | 最初の行はヘッダー（インポート時にスキップ、エクスポート時に書き込み） |
| `DELIMITER 'c'` | フィールド区切り文字（デフォルト：`,`） |

## csv_file() — CSVファイルを直接クエリ

インポートなしでCSVファイルを仮想テーブルとしてクエリします。

```sql
SELECT * FROM csv_file('data/sales.csv');
SELECT name, age::int FROM csv_file('data/people.csv') WHERE age::int > 25;
```

すべての値は`TEXT`として返されます — キャストするには`::type`を使用してください。`WHERE`、`ORDER BY`、`LIMIT`、`JOIN`、集計をサポートします。

オプション：

```sql
csv_file('path')                        -- ヘッダー付き（デフォルト）
csv_file('path', false)                 -- ヘッダーなし（カラム名はcolumn1, column2, ...）
csv_file('path', true, '|')            -- カスタムデリミタ
```

CSVファイル同士またはデータベーステーブルとのジョイン：

```sql
SELECT e.name, d.department
FROM csv_file('employees.csv') e
JOIN csv_file('departments.csv') d ON e.dept_id = d.id;
```

## 仮想テーブル

`CREATE VIRTUAL TABLE`を使用して外部データソースをクエリ可能なテーブルとして登録します。

```sql
CREATE VIRTUAL TABLE sales USING csv('data/sales.csv');
CREATE VIRTUAL TABLE sales USING csv('data/sales.csv', 'no_header', '|');

SELECT * FROM sales WHERE amount::int > 100;
DROP TABLE sales;
```

仮想テーブルは`SHOW TABLES`に表示され、`SELECT`、`WHERE`、`JOIN`、`ORDER BY`、集計をサポートします。読み取り専用で、`INSERT`、`UPDATE`、`DELETE`はサポートされていません。

組み込みの`csv`モジュールはデフォルトで登録されています。カスタムモジュールはScala APIで登録できます。

```scala
db.registerVirtualTableModule("mymodule", new VirtualTableModule { ... })
```

---
title: トランザクション
description: トランザクションとプリペアドステートメントです。
---

## トランザクション

アトミック性を保証するために複数の文をトランザクションにラップします。

```sql
BEGIN;
INSERT INTO accounts (name, balance) VALUES ('Alice', 1000);
UPDATE accounts SET balance = balance - 100 WHERE name = 'Alice';
COMMIT;
```

エラー時にロールバックします。

```sql
BEGIN;
UPDATE accounts SET balance = balance - 9999 WHERE name = 'Alice';
ROLLBACK;
```

### トランザクション内のDDL

DDL文（CREATE TABLE、CREATE INDEX、DROP TABLEなど）はトランザクション内で完全にサポートされ、ROLLBACK時にDMLとともにアトミックにロールバックされます。DDLとDMLは単一のトランザクション内で自由にインターリーブできます。

```sql
BEGIN;
CREATE TABLE orders (id SERIAL, product TEXT, qty INTEGER);
INSERT INTO orders (product, qty) VALUES ('Widget', 10);
CREATE INDEX idx_orders_product ON orders (product);
COMMIT;  -- テーブル、データ、インデックスがすべてアトミックにコミット
```

トランザクションがロールバックされると、テーブル、データ、インデックスすべてが元に戻ります。

## プリペアドステートメント

`$1`、`$2`、...プレースホルダーでパラメータ化された文を作成します。

```sql
PREPARE get_user AS SELECT * FROM users WHERE id = $1;
EXECUTE get_user(42);
DEALLOCATE get_user;
```

パラメータ化されたInsert：

```sql
PREPARE add_user AS INSERT INTO users (name, email) VALUES ($1, $2);
EXECUTE add_user('Alice', 'alice@example.com');
```

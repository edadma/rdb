---
title: PL/pgSQL
description: 手続き型言語 — DOブロック、ストアド関数、ストアドプロシージャです。
---

PetraDBは、制御フローロジック、ループ、条件付きSQL実行を記述するためのPostgreSQLの手続き型言語であるPL/pgSQLをサポートしています。

## DOブロック

保存されずに即座に実行される匿名ブロックです。

```sql
DO $$
DECLARE
  i INT;
  total INT := 0;
BEGIN
  FOR i IN 1..10 LOOP
    total := total + i;
  END LOOP;
  INSERT INTO results (sum) VALUES (total);
END $$;
```

## ストアド関数

関数は値を返し、任意のSQL式で呼び出すことができます。

```sql
CREATE FUNCTION factorial(n INT) RETURNS INT AS $$
DECLARE
  result INT := 1;
  i INT := 1;
BEGIN
  WHILE i <= n LOOP
    result := result * i;
    i := i + 1;
  END LOOP;
  RETURN result;
END $$ LANGUAGE plpgsql;

SELECT factorial(5);  -- 120
```

既存の関数を上書きするには`CREATE OR REPLACE FUNCTION`を使用します。

### クエリでの関数の使用

関数は式が許される場所であればどこでも動作します。

```sql
SELECT name, classify(score) AS grade FROM students;
SELECT * FROM orders WHERE is_valid(status);
INSERT INTO logs VALUES (format_msg(code, detail));
```

## ストアドプロシージャ

プロシージャはアクションを実行し、`CALL`で呼び出します。

```sql
CREATE PROCEDURE seed_users(n INT) AS $$
DECLARE
  i INT;
BEGIN
  FOR i IN 1..n LOOP
    INSERT INTO users (name) VALUES ('User ' || i::TEXT);
  END LOOP;
END $$ LANGUAGE plpgsql;

CALL seed_users(100);
```

既存のプロシージャを上書きするには`CREATE OR REPLACE PROCEDURE`を使用します。

## DROP

```sql
DROP FUNCTION factorial;
DROP FUNCTION IF EXISTS factorial;
DROP PROCEDURE seed_users;
DROP PROCEDURE IF EXISTS seed_users;
```

## ブロック構造

すべてのPL/pgSQLブロック（DOブロック、関数本体、プロシージャ本体）は同じ構造を共有します。

```
[DECLARE
  variable_name type [:= default_value];
  ...]
BEGIN
  statements...
[EXCEPTION
  WHEN condition THEN
    statements...]
END
```

## 変数

型とオプションのデフォルト値で変数を宣言します。

```sql
DECLARE
  x INT := 0;
  name TEXT;
  total NUMERIC := 100.50;
```

デフォルトのない変数は`NULL`で初期化されます。代入には`:=`を使用します。

```sql
x := x + 1;
name := 'Alice';
total := (SELECT SUM(amount) FROM orders);
```

変数はブロック内の任意のSQL文で使用できます — `VALUES`、`WHERE`、`SET`など。

## 制御フロー

### IF / ELSIF / ELSE

```sql
IF amount > 1000 THEN
  INSERT INTO vip_orders VALUES (order_id);
ELSIF amount > 100 THEN
  INSERT INTO normal_orders VALUES (order_id);
ELSE
  INSERT INTO small_orders VALUES (order_id);
END IF;
```

### WHILEループ

```sql
WHILE balance > 0 LOOP
  balance := balance - payment;
  month := month + 1;
END LOOP;
```

安全制限として最大10,000回の反復です。

### FORレンジループ

```sql
FOR i IN 1..10 LOOP
  INSERT INTO numbers VALUES (i);
END LOOP;
```

ループ変数は`DECLARE`で宣言する必要があります。両端は包含です。

### FORクエリループ

クエリ結果をイテレートします。

```sql
FOR name IN SELECT name FROM users ORDER BY id LOOP
  INSERT INTO greetings VALUES ('Hello, ' || name);
END LOOP;
```

単一カラムクエリの場合、変数はスカラー値を受け取ります。複数カラムクエリの場合、変数はレコードを受け取ります。

## RETURN

ブロックを終了するか、関数から値を返します。

```sql
-- 関数内:
RETURN x * 2;

-- DOブロックまたはプロシージャ内（早期終了）:
RETURN;
```

## RAISE

メッセージを出力するかエラーをスローします。

```sql
RAISE NOTICE 'Processing row %', row_id;
RAISE EXCEPTION 'Invalid input: %', value;
```

`%`プレースホルダーは引数の値で順番に置換されます。`RAISE EXCEPTION`は実行を中断します。

## PERFORM

クエリを実行して結果を破棄します。

```sql
PERFORM SELECT notify_user(user_id);
```

## 例外処理

ブロック内でエラーをキャッチします。

```sql
DO $$
BEGIN
  CREATE TABLE t (id INT);
EXCEPTION
  WHEN duplicate_object THEN
    NULL;  -- テーブルは既に存在するため無視
END $$;
```

例外条件：
- `duplicate_object` — テーブル/型/制約が既に存在する
- `unique_violation` — ユニーク制約に違反
- `others` — すべての例外をキャッチ

## NULL文

ノーオペレーション文で、例外ハンドラーでよく使用されます。

```sql
EXCEPTION
  WHEN others THEN NULL;
```

## トリガー

トリガーは行がinsert、update、deleteされた時に自動的に関数を実行します。

```sql
CREATE FUNCTION audit_changes() RETURNS INT AS $$
BEGIN
  INSERT INTO audit_log VALUES (tg_op || ' on ' || tg_table_name);
  RETURN 0;
END $$ LANGUAGE plpgsql;

CREATE TRIGGER trg_audit AFTER INSERT ON orders
  FOR EACH ROW EXECUTE FUNCTION audit_changes();

CREATE TRIGGER trg_audit_del AFTER DELETE ON orders
  FOR EACH ROW EXECUTE FUNCTION audit_changes();
```

### 構文

```sql
CREATE TRIGGER name BEFORE|AFTER INSERT|UPDATE|DELETE
  ON table FOR EACH ROW EXECUTE FUNCTION function_name();

DROP TRIGGER name ON table;
DROP TRIGGER IF EXISTS name ON table;
```

### タイミング

- **BEFORE**トリガーは操作の前に実行されます。`NULL`を返すと行操作がキャンセルされます。非NULL値を返すと処理が続行されます。
- **AFTER**トリガーは操作の後に実行されます。戻り値は無視されます。

### 特殊変数

トリガー関数は以下にアクセスできます。

| 変数 | 説明 |
|----------|-------------|
| `tg_op` | 操作名：`'INSERT'`、`'UPDATE'`、または`'DELETE'` |
| `tg_table_name` | トリガーを発火させたテーブル名 |
| `OLD` | 操作前の行（UPDATE、DELETE） |
| `NEW` | 操作後の行（INSERT、UPDATE） |

### イベント

トリガーは以下で発火します。
- `INSERT` — `COPY FROM`経由で挿入された行を含む
- `UPDATE` — 更新された行ごとに発火
- `DELETE` — 削除された行ごとに発火

### ガードトリガーの例

```sql
CREATE FUNCTION prevent_delete() RETURNS INT AS $$
BEGIN
  RETURN NULL;  -- DELETEをキャンセル
END $$ LANGUAGE plpgsql;

CREATE TRIGGER trg_protect BEFORE DELETE ON important_data
  FOR EACH ROW EXECUTE FUNCTION prevent_delete();
```

### ネイティブ関数コールバック

登録されたネイティブ関数（Scala、JavaScript、またはC）はトリガー関数から呼び出すことができ、外部システムとの統合を可能にします。

```sql
-- notify_webhook()がネイティブ関数として登録されていると仮定
CREATE FUNCTION on_order() RETURNS INT AS $$
DECLARE dummy INT;
BEGIN
  dummy := notify_webhook(tg_op);
  RETURN 0;
END $$ LANGUAGE plpgsql;

CREATE TRIGGER trg AFTER INSERT ON orders
  FOR EACH ROW EXECUTE FUNCTION on_order();
```

ネイティブ関数の登録については[Scala API](/reference/api-scala/)、[JavaScript API](/reference/api-javascript/)、[C API](/reference/api-c/)をご覧ください。

## 永続化

ストアド関数、プロシージャ、トリガーはインメモリ（セッションの寿命）と永続ストレージ（close/reopenしても保持）の両方でデータベースの再起動をまたいで保持されます。ソースSQLが保存され、データベースのopen時に再実行されます。

## 合成

関数は他の関数を呼び出すことができます。プロシージャは関数を呼び出すことができます。

```sql
CREATE FUNCTION double(x INT) RETURNS INT AS $$
BEGIN RETURN x * 2; END $$ LANGUAGE plpgsql;

CREATE FUNCTION quadruple(x INT) RETURNS INT AS $$
BEGIN RETURN double(double(x)); END $$ LANGUAGE plpgsql;

SELECT quadruple(5);  -- 20
```

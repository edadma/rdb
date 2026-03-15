---
title: データ型
description: PetraDBがサポートするSQLデータ型です。
---

PetraDBはSQL構文、識別子の処理、型キャストについてPostgreSQLの規約に従います。

## SQL互換性

- **大文字小文字を区別しないキーワード** — `SELECT`、`select`、`Select`は同等です
- **非引用符識別子の小文字化** — 非引用符の識別子は小文字に変換されます（`CREATE TABLE Users` → テーブル名`users`）
- **ダブルクォート識別子** — 大文字小文字を保持します（`"MixedCase"`はそのまま）
- **文字列エスケープ** — シングルクォートの二重化（`'it''s'`）およびEストリング（`E'it\'s'`）
- **演算子** — 不等号には`!=`と`<>`の両方が使えます

## サポートされる型

| 型 | 説明 |
|------|-------------|
| `SMALLINT` | 16ビット整数（-32768から32767） |
| `INT` / `INTEGER` | 32ビット整数 |
| `BIGINT` | 64ビット整数 |
| `SMALLSERIAL` | 自動インクリメント16ビット整数（バッキングシーケンスを作成） |
| `SERIAL` | 自動インクリメント32ビット整数（バッキングシーケンスを作成） |
| `BIGSERIAL` | 自動インクリメント64ビット整数（バッキングシーケンスを作成） |
| `DOUBLE` / `FLOAT` / `REAL` | 倍精度浮動小数点 |
| `NUMERIC(p,s)` / `DECIMAL(p,s)` | 固定精度小数 |
| `TEXT` | 可変長文字列 |
| `CHAR(n)` | 固定長文字列（右側をスペースで埋める） |
| `VARCHAR(n)` | 可変長文字列（最大n文字、パディングなし） |
| `BOOLEAN` | 真/偽 |
| `DATE` | 日付（`yyyy-MM-dd`） |
| `TIME` | 時刻（`HH:mm:ss`） |
| `TIMESTAMP` | 日付と時刻 |
| `TIMESTAMP WITH TIME ZONE` | タイムゾーンオフセット付き日付と時刻 |
| `INTERVAL` | 期間（ISO 8601または`N days N hours N minutes N seconds`） |
| `UUID` | ユニバーサル一意識別子 |
| `JSON` / `JSONB` | 構造化JSONオブジェクトと配列 |
| `BYTEA` | バイナリデータ |
| `ENUM` | カスタム列挙型（`CREATE TYPE ... AS ENUM`で定義） |
| `INT[]`、`TEXT[]`など | 型付き配列（任意の基本型に`[]`サフィックス） |

## 型キャスト

`::`演算子または`CAST(expr AS type)`で型を変換します。

```sql
SELECT '2024-06-15'::DATE;
SELECT CAST('14:30:00' AS TIME);
SELECT '2 hours 30 minutes'::INTERVAL;
SELECT CAST(val AS TEXT);
SELECT '42'::INT;
SELECT 1::BOOLEAN;
SELECT EXTRACT(year FROM created_at);
```

## 日付/時刻演算

```sql
SELECT '2024-01-01'::DATE + 10;                        -- 日数を加算
SELECT '2024-01-15'::DATE - '2024-01-10'::DATE;        -- 日数の差
SELECT now() + '2 hours'::INTERVAL;                     -- タイムスタンプ + インターバル
SELECT now() - '30 minutes'::INTERVAL;                  -- タイムスタンプ - インターバル
SELECT '1 hour'::INTERVAL * 3;                          -- インターバルのスケーリング
SELECT EXTRACT(year FROM now());                        -- フィールドの抽出
SELECT date_trunc('month', now());                      -- 切り捨て
```

## 制約

```sql
PRIMARY KEY (id)
UNIQUE (email)
NOT NULL
DEFAULT value
FOREIGN KEY (col) REFERENCES other_table (col) ON DELETE CASCADE ON UPDATE CASCADE
```

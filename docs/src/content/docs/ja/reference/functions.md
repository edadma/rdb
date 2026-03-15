---
title: 関数
description: スカラー関数と集約関数のリファレンスです。
---

## テキスト関数

| 関数 | 説明 |
|----------|-------------|
| `lower(text)` | 小文字に変換 |
| `upper(text)` | 大文字に変換 |
| `initcap(text)` | 各単語の先頭を大文字に |
| `length(text)` / `char_length(text)` | 文字列長 |
| `trim(text)` / `ltrim(text)` / `rtrim(text)` | 空白を除去 |
| `btrim(text [, chars])` | 両端から文字を除去 |
| `substring(text, start [, len])` | 部分文字列を抽出 |
| `left(text, n)` / `right(text, n)` | 先頭/末尾のn文字 |
| `lpad(text, len [, pad])` / `rpad(text, len [, pad])` | 文字列をパディング |
| `replace(text, from, to)` | 出現箇所を置換 |
| `translate(text, from, to)` | 文字ごとの置換 |
| `concat(a, b)` / `concat_ws(sep, ...)` | 連結（区切り文字付き） |
| `repeat(text, n)` | 文字列を繰り返す |
| `reverse(text)` | 文字列を反転 |
| `position(substr, text)` | 部分文字列の位置を検索（1ベース） |
| `split_part(text, delim, n)` | 分割してn番目を取得 |
| `ascii(text)` / `chr(int)` | 文字/コードポイント変換 |
| `regexp_replace(text, pat, repl [, flags])` | 正規表現置換（`'g'`でグローバル） |
| `regexp_match(text, pattern)` | 最初の正規表現マッチを配列として返す |
| `regexp_split_to_array(text, pattern)` | 正規表現で分割して配列に |
| `starts_with(text, prefix)` | テキストがprefixで始まる場合にtrue |
| `ends_with(text, suffix)` | テキストがsuffixで終わる場合にtrue |
| `format(formatstr, ...)` | フォーマット文字列（下記参照） |
| `quote_ident(value)` | SQL識別子としてクォート |
| `quote_literal(value)` | SQLリテラルとしてクォート |

### format()

以下のフォーマット指定子をサポートします。

| 指定子 | 説明 |
|-----------|-------------|
| `%s` | 文字列置換 |
| `%I` | SQL識別子（クォートしてエスケープ） |
| `%L` | SQLリテラル（クォートしてエスケープ） |
| `%%` | リテラルのパーセント記号 |

```sql
SELECT format('Hello, %s!', 'world');
-- Hello, world!

SELECT format('SELECT %I FROM %I WHERE id = %L', 'name', 'users', '42');
-- SELECT "name" FROM "users" WHERE id = '42'
```

## 数値関数

| 関数 | 説明 |
|----------|-------------|
| `abs(x)` | 絶対値 |
| `ceil(x)` / `floor(x)` | 切り上げ/切り捨て |
| `round(x [, digits])` / `trunc(x [, digits])` | 丸め/切り捨て |
| `sign(x)` | 符号（-1、0、1） |
| `mod(x, y)` | 剰余 |
| `power(x, y)` / `sqrt(x)` | べき乗/平方根 |
| `cbrt(x)` | 立方根 |
| `exp(x)` / `ln(x)` / `log10(x)` / `log(base, x)` | 指数/対数 |
| `div(x, y)` | 整数除算 |
| `factorial(n)` | 階乗 |
| `gcd(a, b)` / `lcm(a, b)` | 最大公約数 / 最小公倍数 |
| `pi()` | 円周率定数 |
| `degrees(rad)` / `radians(deg)` | 角度変換 |
| `sin` / `cos` / `tan` / `asin` / `acos` / `atan` / `atan2` | 三角関数 |
| `sinh` / `cosh` / `tanh` / `asinh` / `acosh` / `atanh` | 双曲線三角関数 |
| `random()` | ランダム数 [0, 1) |
| `setseed(seed)` | 乱数生成器のシードを設定 |
| `width_bucket(value, low, high, count)` | 値をバケットに割り当て（下記参照） |
| `greatest(a, b, ...)` / `least(a, b, ...)` | 値の最大/最小 |

### ビット演算子

| 演算子 | 説明 |
|----------|-------------|
| `x % y` | 剰余 |
| `x & y` | ビットAND |
| `x \| y` | ビットOR |
| `x # y` | ビットXOR |
| `~x` | ビットNOT |
| `x << n` | 左ビットシフト |
| `x >> n` | 右ビットシフト |

### width_bucket()

値を`[low, high)`の範囲で`count`個の等幅バケットの1つに割り当てます。

```sql
SELECT width_bucket(35, 0, 100, 10);
-- 4  （30-39の値のバケット）
```

`low`未満の値には0を返し、`high`以上の値には`count + 1`を返します。

## 日付/時刻関数

| 関数 | 説明 |
|----------|-------------|
| `now()` | 現在のタイムスタンプ（UTC） |
| `clock_timestamp()` | 現在のタイムスタンプ（UTC） |
| `current_date()` | 現在の日付（UTC） |
| `current_time()` | 現在の時刻（UTC） |
| `date_part(field, source)` | 日付/時刻からフィールドを抽出 |
| `EXTRACT(field FROM source)` | SQL標準の抽出 |
| `date_trunc(field, source)` | 精度で切り捨て（year/quarter/month/week/day/hour/minute/second） |
| `make_date(y, m, d)` / `make_time(h, m, s)` | 日付/時刻の構築 |
| `make_timestamp(y, mo, d, h, mi, s)` | タイムスタンプの構築 |
| `make_interval(days [, hours [, mins [, secs]]])` | インターバルの構築 |
| `age(ts1, ts2)` / `age(ts)` | タイムスタンプ間のインターバル |
| `to_char(value, format)` | テキストとしてフォーマット |
| `to_date(text, format)` / `to_timestamp(text, format)` | フォーマットでパース |
| `to_number(text, format)` | 数値文字列をパース |
| `isfinite(date\|timestamp)` | 常にtrue（Java timeに無限大なし） |

## 配列関数

| 関数 | 説明 |
|----------|-------------|
| `array_length(arr)` | 要素数 |
| `array_append(arr, val)` / `array_prepend(val, arr)` | 要素を追加 |
| `array_concat(arr1, arr2)` / `array_cat(arr1, arr2)` | 配列を連結 |
| `array_slice(arr, start [, end])` | 配列をスライス |
| `array_remove(arr, val)` | すべての出現を削除 |
| `array_position(arr, val)` | 要素の位置を検索（1ベース） |
| `array_distinct(arr)` | 重複を除去 |
| `array_replace(arr, old, new)` | 一致する要素を置換 |
| `array_lower(arr, dim)` / `array_upper(arr, dim)` | 配列の境界（1ベース） |
| `array_ndims(arr)` | 次元数（常に1） |
| `cardinality(arr)` | 要素数 |
| `string_to_array(text, delim)` | 文字列を配列に分割 |
| `array_to_string(arr, sep)` | 配列を文字列に結合 |

## バイナリ関数

| 関数 | 説明 |
|----------|-------------|
| `octet_length(bytea)` | バイト数 |
| `get_byte(bytea, offset)` | 0ベースオフセットのバイトを取得（0-255を返す） |
| `set_byte(bytea, offset, value)` | オフセットのバイトを設定、新しいbyteaを返す |
| `encode(bytea, format)` / `decode(text, format)` | バイナリエンコーディング（hex、base64） |

## シーケンス関数

| 関数 | 説明 |
|----------|-------------|
| `nextval('name')` | シーケンスを進めて次の値を返す |
| `currval('name')` | 現在の値（セッションで先に`nextval`が必要） |
| `setval('name', value [, is_called])` | シーケンスの値を設定（`is_called`のデフォルトは`true`） |
| `lastval()` | このセッションで任意のシーケンスから返された最後の値 |

```sql
CREATE SEQUENCE order_seq START WITH 100;
SELECT nextval('order_seq');   -- 100
SELECT nextval('order_seq');   -- 101
SELECT currval('order_seq');   -- 101
SELECT setval('order_seq', 200);
SELECT nextval('order_seq');   -- 201
SELECT lastval();              -- 201
```

## その他のスカラー関数

| 関数 | 説明 |
|----------|-------------|
| `coalesce(a, b, ...)` | 最初の非NULL値 |
| `nullif(a, b)` | a = bの場合にNULL |
| `typeof(value)` | 型名をテキストとして返す |
| `gen_random_uuid()` | UUID v4を生成 |

## 集約関数

| 関数 | 説明 |
|----------|-------------|
| `COUNT(*)` / `COUNT(expr)` | 行数をカウント |
| `SUM(expr)` | 値の合計 |
| `AVG(expr)` | 平均 |
| `MIN(expr)` / `MAX(expr)` | 最小/最大 |
| `string_agg(text, separator)` | 区切り文字で連結 |
| `array_agg(expr)` | 値を配列に収集 |
| `bool_and(expr)` / `bool_or(expr)` / `every(expr)` | 行全体の論理AND/OR |
| `bit_and(expr)` | 行全体のビットAND |
| `bit_or(expr)` | 行全体のビットOR |
| `bit_xor(expr)` | 行全体のビットXOR |
| `variance(expr)` / `var_samp(expr)` | 標本分散 |
| `var_pop(expr)` | 母分散 |
| `stddev(expr)` / `stddev_samp(expr)` | 標本標準偏差 |
| `stddev_pop(expr)` | 母標準偏差 |

すべての集約関数は、どの行を含めるかを制限する`FILTER (WHERE ...)`句をサポートしています。

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE active) AS active_count
FROM users;
```

関連項目：[JSON集約関数](/reference/json/#集約関数)

## ウィンドウ関数

ウィンドウ関数は、関連する行のグループに基づいて各行の値を計算します（行を集約しません）。

### ランキング関数

| 関数 | 説明 |
|----------|-------------|
| `ROW_NUMBER()` | パーティション内の連番 |
| `RANK()` | 同順位でギャップありのランク |
| `DENSE_RANK()` | 同順位でギャップなしのランク |

### オフセット関数

| 関数 | 説明 |
|----------|-------------|
| `LAG(expr [, offset [, default]])` | 前の行の値（デフォルトオフセット：1） |
| `LEAD(expr [, offset [, default]])` | 次の行の値（デフォルトオフセット：1） |
| `NTILE(n)` | 行をn個のほぼ等しいグループに分割 |

### 値関数

| 関数 | 説明 |
|----------|-------------|
| `FIRST_VALUE(expr)` | ウィンドウフレームの最初の行の`expr`の値 |
| `LAST_VALUE(expr)` | ウィンドウフレームの最後の行の`expr`の値 |
| `NTH_VALUE(expr, n)` | フレームのn番目の行（1ベース）の`expr`の値、該当行がなければNULL |

### 集約ウィンドウ関数

任意の集約関数を`OVER()`でウィンドウ関数として使用できます。構文とフレーム指定については[クエリ — ウィンドウ関数](/reference/queries/#ウィンドウ関数)をご覧ください。

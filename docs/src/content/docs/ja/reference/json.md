---
title: JSON
description: JSONおよびJSONBの関数と演算子です。
---

PetraDBは構造化データの保存に`JSON`と`JSONB`型をサポートしています。両型は同一の動作をします — 値は内部的にネイティブオブジェクトとして保存されます。

## 演算子

### アクセス演算子

| 演算子 | 説明 | 例 |
|----------|-------------|---------|
| `->` | キーまたはインデックスでJSON値を取得 | `'{"a":1}'::jsonb -> 'a'` → `1` |
| `->>` | JSON値をテキストとして取得 | `'{"a":1}'::jsonb ->> 'a'` → `'1'` |
| `#>` | パスでJSON値を取得 | `'{"a":{"b":1}}'::jsonb #> '{a,b}'` → `1` |
| `#>>` | パスでJSON値をテキストとして取得 | `'{"a":{"b":1}}'::jsonb #>> '{a,b}'` → `'1'` |

配列アクセスは0ベースの整数インデックスを使用し、負のインデックスは末尾からカウントします。

```sql
SELECT '[10, 20, 30]'::jsonb -> 0;    -- 10
SELECT '[10, 20, 30]'::jsonb -> -1;   -- 30
```

### 包含演算子

| 演算子 | 説明 | 例 |
|----------|-------------|---------|
| `@>` | 左が右を含む | `'{"a":1,"b":2}'::jsonb @> '{"a":1}'` → `true` |
| `<@` | 左が右に含まれる | `'{"a":1}'::jsonb <@ '{"a":1,"b":2}'` → `true` |

オブジェクトの場合、包含は右オペランドのすべてのキーバリューペアが左に存在することを意味します。配列の場合、右のすべての要素が左に存在する必要があります。

### 存在演算子

| 演算子 | 説明 | 例 |
|----------|-------------|---------|
| `?` | キー/要素が存在する | `'{"a":1}'::jsonb ? 'a'` → `true` |
| `?\|` | いずれかのキーが存在する | `'{"a":1}'::jsonb ?\| array['a','b']` → `true` |
| `?&` | すべてのキーが存在する | `'{"a":1,"b":2}'::jsonb ?& array['a','b']` → `true` |

### 配列オーバーラップ

| 演算子 | 説明 | 例 |
|----------|-------------|---------|
| `&&` | 配列が共通要素を持つ | `ARRAY[1,2] && ARRAY[2,3]` → `true` |

## スカラー関数

### jsonb_typeof(value)

JSON値の型を文字列として返します：`"object"`、`"array"`、`"string"`、`"number"`、`"boolean"`、または`"null"`。

```sql
SELECT jsonb_typeof('{"a":1}'::jsonb);     -- object
SELECT jsonb_typeof('[1,2]'::jsonb);        -- array
SELECT jsonb_typeof('"hello"'::jsonb);      -- string
```

`json_typeof`は同一動作のエイリアスです。

### jsonb_array_length(value)

JSON配列の要素数を返します。

```sql
SELECT jsonb_array_length('[1, 2, 3]'::jsonb);   -- 3
```

### jsonb_keys(value) / jsonb_object_keys(value)

JSONオブジェクトのキーを配列として返します。

```sql
SELECT jsonb_keys('{"a":1, "b":2}'::jsonb);   -- {a,b}
```

### jsonb_extract_path(json, VARIADIC keys)

ネストされたパスの値を抽出します。

```sql
SELECT jsonb_extract_path('{"a":{"b":{"c":42}}}'::jsonb, 'a', 'b', 'c');
-- 42
```

### jsonb_extract_path_text(json, VARIADIC keys)

`jsonb_extract_path`と同じですが、結果をテキストとして返します。

```sql
SELECT jsonb_extract_path_text('{"a":{"b":1}}'::jsonb, 'a', 'b');
-- '1'
```

### jsonb_set(target, path, new_value [, create_missing])

JSON構造内のパスに値を設定します。`create_missing`のデフォルトは`true`です。

```sql
SELECT jsonb_set('{"a":1}'::jsonb, '{b}', '2'::jsonb);
-- {"a":1,"b":2}

SELECT jsonb_set('{"a":1}'::jsonb, '{b}', '2'::jsonb, false);
-- {"a":1}  (create_missingがfalseのためbは作成されない)
```

### jsonb_insert(target, path, new_value [, insert_after])

パスに値を挿入します。配列の場合、デフォルトでは位置の前に挿入します。`insert_after`を`true`に設定すると後に挿入します。

```sql
SELECT jsonb_insert('[1, 3]'::jsonb, '{1}', '2'::jsonb);
-- [1, 2, 3]

SELECT jsonb_insert('[1, 3]'::jsonb, '{1}', '2'::jsonb, true);
-- [1, 3, 2]
```

オブジェクトの場合、キーが既に存在しない場合にのみ追加します。

### jsonb_strip_nulls(value)

再帰的にnull値を持つすべてのオブジェクトキーを削除します。

```sql
SELECT jsonb_strip_nulls('{"a":1, "b":null, "c":{"d":null}}'::jsonb);
-- {"a":1,"c":{}}
```

### jsonb_pretty(value)

4スペースインデントで整形されたJSON文字列を返します。

```sql
SELECT jsonb_pretty('{"a":1,"b":[2,3]}'::jsonb);
```

### jsonb_build_object(key1, value1, ...)

交互のキーバリュー引数からJSONオブジェクトを構築します。

```sql
SELECT jsonb_build_object('name', 'Alice', 'age', 30);
-- {"name":"Alice","age":30}
```

### jsonb_build_array(value1, value2, ...)

引数からJSON配列を構築します。

```sql
SELECT jsonb_build_array(1, 'two', true);
-- [1,"two",true]
```

### to_jsonb(value) / to_json(value)

値をJSONに変換します。JSON互換型はそのまま通過し、その他の型はJSON文字列に変換されます。

```sql
SELECT to_jsonb(42);        -- 42
SELECT to_jsonb('hello');   -- "hello"
```

## 集約関数

### json_agg(expr) / jsonb_agg(expr)

値をJSON配列に収集します。

```sql
SELECT json_agg(name) FROM users;
-- ["Alice","Bob","Carol"]
```

### json_object_agg(key, value) / jsonb_object_agg(key, value)

行をまたいでキーバリューペアからJSONオブジェクトを構築します。

```sql
SELECT json_object_agg(name, age) FROM users;
-- {"Alice":30,"Bob":25,"Carol":35}
```

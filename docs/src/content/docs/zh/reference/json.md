---
title: JSON
description: JSON 和 JSONB 函数及运算符。
---

PetraDB 支持 `JSON` 和 `JSONB` 类型用于存储结构化数据。两种类型行为一致 — 值在内部以原生对象存储。

## 运算符

### 访问运算符

| 运算符 | 描述 | 示例 |
|----------|-------------|---------|
| `->` | 按键或索引获取 JSON 值 | `'{"a":1}'::jsonb -> 'a'` → `1` |
| `->>` | 获取 JSON 值为文本 | `'{"a":1}'::jsonb ->> 'a'` → `'1'` |
| `#>` | 按路径获取 JSON 值 | `'{"a":{"b":1}}'::jsonb #> '{a,b}'` → `1` |
| `#>>` | 按路径获取 JSON 值为文本 | `'{"a":{"b":1}}'::jsonb #>> '{a,b}'` → `'1'` |

数组访问使用从 0 开始的整数索引，负索引从末尾计数：

```sql
SELECT '[10, 20, 30]'::jsonb -> 0;    -- 10
SELECT '[10, 20, 30]'::jsonb -> -1;   -- 30
```

### 包含运算符

| 运算符 | 描述 | 示例 |
|----------|-------------|---------|
| `@>` | 左侧包含右侧 | `'{"a":1,"b":2}'::jsonb @> '{"a":1}'` → `true` |
| `<@` | 左侧被右侧包含 | `'{"a":1}'::jsonb <@ '{"a":1,"b":2}'` → `true` |

对于对象，包含意味着右操作数中的每个键值对都存在于左操作数中。对于数组，右侧的每个元素都必须出现在左侧。

### 存在运算符

| 运算符 | 描述 | 示例 |
|----------|-------------|---------|
| `?` | 键/元素是否存在 | `'{"a":1}'::jsonb ? 'a'` → `true` |
| `?\|` | 任一键是否存在 | `'{"a":1}'::jsonb ?\| array['a','b']` → `true` |
| `?&` | 所有键是否存在 | `'{"a":1,"b":2}'::jsonb ?& array['a','b']` → `true` |

### 数组重叠

| 运算符 | 描述 | 示例 |
|----------|-------------|---------|
| `&&` | 数组是否有共同元素 | `ARRAY[1,2] && ARRAY[2,3]` → `true` |

## 标量函数

### jsonb_typeof(value)

以字符串返回 JSON 值的类型：`"object"`、`"array"`、`"string"`、`"number"`、`"boolean"` 或 `"null"`。

```sql
SELECT jsonb_typeof('{"a":1}'::jsonb);     -- object
SELECT jsonb_typeof('[1,2]'::jsonb);        -- array
SELECT jsonb_typeof('"hello"'::jsonb);      -- string
```

`json_typeof` 是行为完全相同的别名。

### jsonb_array_length(value)

返回 JSON 数组中的元素数量：

```sql
SELECT jsonb_array_length('[1, 2, 3]'::jsonb);   -- 3
```

### jsonb_keys(value) / jsonb_object_keys(value)

以数组形式返回 JSON 对象的键：

```sql
SELECT jsonb_keys('{"a":1, "b":2}'::jsonb);   -- {a,b}
```

### jsonb_extract_path(json, VARIADIC keys)

在嵌套路径上提取值：

```sql
SELECT jsonb_extract_path('{"a":{"b":{"c":42}}}'::jsonb, 'a', 'b', 'c');
-- 42
```

### jsonb_extract_path_text(json, VARIADIC keys)

与 `jsonb_extract_path` 相同但返回文本结果：

```sql
SELECT jsonb_extract_path_text('{"a":{"b":1}}'::jsonb, 'a', 'b');
-- '1'
```

### jsonb_set(target, path, new_value [, create_missing])

在 JSON 结构中的路径上设置值。`create_missing` 默认为 `true`：

```sql
SELECT jsonb_set('{"a":1}'::jsonb, '{b}', '2'::jsonb);
-- {"a":1,"b":2}

SELECT jsonb_set('{"a":1}'::jsonb, '{b}', '2'::jsonb, false);
-- {"a":1}（因为 create_missing 为 false，b 未创建）
```

### jsonb_insert(target, path, new_value [, insert_after])

在路径处插入值。对于数组，默认在位置之前插入。设置 `insert_after` 为 `true` 在之后插入：

```sql
SELECT jsonb_insert('[1, 3]'::jsonb, '{1}', '2'::jsonb);
-- [1, 2, 3]

SELECT jsonb_insert('[1, 3]'::jsonb, '{1}', '2'::jsonb, true);
-- [1, 3, 2]
```

对于对象，仅在键不存在时添加。

### jsonb_strip_nulls(value)

递归移除所有值为 null 的对象键：

```sql
SELECT jsonb_strip_nulls('{"a":1, "b":null, "c":{"d":null}}'::jsonb);
-- {"a":1,"c":{}}
```

### jsonb_pretty(value)

返回带有 4 空格缩进的格式化 JSON 字符串：

```sql
SELECT jsonb_pretty('{"a":1,"b":[2,3]}'::jsonb);
```

### jsonb_build_object(key1, value1, ...)

从交替的键值参数构造 JSON 对象：

```sql
SELECT jsonb_build_object('name', 'Alice', 'age', 30);
-- {"name":"Alice","age":30}
```

### jsonb_build_array(value1, value2, ...)

从参数构造 JSON 数组：

```sql
SELECT jsonb_build_array(1, 'two', true);
-- [1,"two",true]
```

### to_jsonb(value) / to_json(value)

将值转换为 JSON。JSON 兼容类型直接传递；其他类型转换为 JSON 字符串：

```sql
SELECT to_jsonb(42);        -- 42
SELECT to_jsonb('hello');   -- "hello"
```

## 聚合函数

### json_agg(expr) / jsonb_agg(expr)

将值收集为 JSON 数组：

```sql
SELECT json_agg(name) FROM users;
-- ["Alice","Bob","Carol"]
```

### json_object_agg(key, value) / jsonb_object_agg(key, value)

从跨行的键值对构建 JSON 对象：

```sql
SELECT json_object_agg(name, age) FROM users;
-- {"Alice":30,"Bob":25,"Carol":35}
```

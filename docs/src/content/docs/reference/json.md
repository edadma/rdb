---
title: JSON
description: JSON and JSONB functions and operators.
---

PetraDB supports `JSON` and `JSONB` types for storing structured data. Both types behave identically — values are stored as native objects internally.

## Operators

### Access Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `->` | Get JSON value by key or index | `'{"a":1}'::jsonb -> 'a'` → `1` |
| `->>` | Get JSON value as text | `'{"a":1}'::jsonb ->> 'a'` → `'1'` |
| `#>` | Get JSON value at path | `'{"a":{"b":1}}'::jsonb #> '{a,b}'` → `1` |
| `#>>` | Get JSON value at path as text | `'{"a":{"b":1}}'::jsonb #>> '{a,b}'` → `'1'` |

Array access uses 0-based integer indexes, with negative indexes counting from the end:

```sql
SELECT '[10, 20, 30]'::jsonb -> 0;    -- 10
SELECT '[10, 20, 30]'::jsonb -> -1;   -- 30
```

### Containment Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `@>` | Left contains right | `'{"a":1,"b":2}'::jsonb @> '{"a":1}'` → `true` |
| `<@` | Left is contained by right | `'{"a":1}'::jsonb <@ '{"a":1,"b":2}'` → `true` |

For objects, containment means every key-value pair in the right operand exists in the left. For arrays, every element in the right must appear in the left.

### Existence Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `?` | Key/element exists | `'{"a":1}'::jsonb ? 'a'` → `true` |
| `?\|` | Any key exists | `'{"a":1}'::jsonb ?\| array['a','b']` → `true` |
| `?&` | All keys exist | `'{"a":1,"b":2}'::jsonb ?& array['a','b']` → `true` |

### Array Overlap

| Operator | Description | Example |
|----------|-------------|---------|
| `&&` | Arrays share common elements | `ARRAY[1,2] && ARRAY[2,3]` → `true` |

## Scalar Functions

### jsonb_typeof(value)

Returns the type of a JSON value as a string: `"object"`, `"array"`, `"string"`, `"number"`, `"boolean"`, or `"null"`.

```sql
SELECT jsonb_typeof('{"a":1}'::jsonb);     -- object
SELECT jsonb_typeof('[1,2]'::jsonb);        -- array
SELECT jsonb_typeof('"hello"'::jsonb);      -- string
```

`json_typeof` is an alias with identical behavior.

### jsonb_array_length(value)

Returns the number of elements in a JSON array:

```sql
SELECT jsonb_array_length('[1, 2, 3]'::jsonb);   -- 3
```

### jsonb_keys(value) / jsonb_object_keys(value)

Returns the keys of a JSON object as an array:

```sql
SELECT jsonb_keys('{"a":1, "b":2}'::jsonb);   -- {a,b}
```

### jsonb_extract_path(json, VARIADIC keys)

Extracts a value at a nested path:

```sql
SELECT jsonb_extract_path('{"a":{"b":{"c":42}}}'::jsonb, 'a', 'b', 'c');
-- 42
```

### jsonb_extract_path_text(json, VARIADIC keys)

Same as `jsonb_extract_path` but returns the result as text:

```sql
SELECT jsonb_extract_path_text('{"a":{"b":1}}'::jsonb, 'a', 'b');
-- '1'
```

### jsonb_set(target, path, new_value [, create_missing])

Sets a value at a path within a JSON structure. `create_missing` defaults to `true`:

```sql
SELECT jsonb_set('{"a":1}'::jsonb, '{b}', '2'::jsonb);
-- {"a":1,"b":2}

SELECT jsonb_set('{"a":1}'::jsonb, '{b}', '2'::jsonb, false);
-- {"a":1}  (b not created because create_missing is false)
```

### jsonb_insert(target, path, new_value [, insert_after])

Inserts a value at a path. For arrays, inserts before the position by default. Set `insert_after` to `true` to insert after:

```sql
SELECT jsonb_insert('[1, 3]'::jsonb, '{1}', '2'::jsonb);
-- [1, 2, 3]

SELECT jsonb_insert('[1, 3]'::jsonb, '{1}', '2'::jsonb, true);
-- [1, 3, 2]
```

For objects, adds the key only if it doesn't already exist.

### jsonb_strip_nulls(value)

Recursively removes all object keys with null values:

```sql
SELECT jsonb_strip_nulls('{"a":1, "b":null, "c":{"d":null}}'::jsonb);
-- {"a":1,"c":{}}
```

### jsonb_pretty(value)

Returns a pretty-printed JSON string with 4-space indentation:

```sql
SELECT jsonb_pretty('{"a":1,"b":[2,3]}'::jsonb);
```

### jsonb_build_object(key1, value1, ...)

Constructs a JSON object from alternating key-value arguments:

```sql
SELECT jsonb_build_object('name', 'Alice', 'age', 30);
-- {"name":"Alice","age":30}
```

### jsonb_build_array(value1, value2, ...)

Constructs a JSON array from arguments:

```sql
SELECT jsonb_build_array(1, 'two', true);
-- [1,"two",true]
```

### to_jsonb(value) / to_json(value)

Converts a value to JSON. JSON-compatible types pass through unchanged; other types are converted to JSON strings:

```sql
SELECT to_jsonb(42);        -- 42
SELECT to_jsonb('hello');   -- "hello"
```

## Aggregate Functions

### json_agg(expr) / jsonb_agg(expr)

Collects values into a JSON array:

```sql
SELECT json_agg(name) FROM users;
-- ["Alice","Bob","Carol"]
```

### json_object_agg(key, value) / jsonb_object_agg(key, value)

Builds a JSON object from key-value pairs across rows:

```sql
SELECT json_object_agg(name, age) FROM users;
-- {"Alice":30,"Bob":25,"Carol":35}
```

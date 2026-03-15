---
title: JSON
description: JSON 및 JSONB 함수와 연산자.
---

PetraDB는 구조화된 데이터를 저장하기 위한 `JSON`과 `JSONB` 타입을 지원합니다. 두 타입의 동작은 동일합니다 — 값은 내부적으로 네이티브 객체로 저장됩니다.

## 연산자

### 접근 연산자

| 연산자 | 설명 | 예제 |
|----------|-------------|---------|
| `->` | 키 또는 인덱스로 JSON 값 가져오기 | `'{"a":1}'::jsonb -> 'a'` → `1` |
| `->>` | JSON 값을 텍스트로 가져오기 | `'{"a":1}'::jsonb ->> 'a'` → `'1'` |
| `#>` | 경로로 JSON 값 가져오기 | `'{"a":{"b":1}}'::jsonb #> '{a,b}'` → `1` |
| `#>>` | 경로로 JSON 값을 텍스트로 가져오기 | `'{"a":{"b":1}}'::jsonb #>> '{a,b}'` → `'1'` |

배열 접근은 0부터 시작하는 정수 인덱스를 사용하며, 음수 인덱스는 끝에서부터 셉니다:

```sql
SELECT '[10, 20, 30]'::jsonb -> 0;    -- 10
SELECT '[10, 20, 30]'::jsonb -> -1;   -- 30
```

### 포함 연산자

| 연산자 | 설명 | 예제 |
|----------|-------------|---------|
| `@>` | 왼쪽이 오른쪽을 포함 | `'{"a":1,"b":2}'::jsonb @> '{"a":1}'` → `true` |
| `<@` | 왼쪽이 오른쪽에 포함됨 | `'{"a":1}'::jsonb <@ '{"a":1,"b":2}'` → `true` |

객체의 경우, 포함은 오른쪽 피연산자의 모든 키-값 쌍이 왼쪽에 존재함을 의미합니다. 배열의 경우, 오른쪽의 모든 요소가 왼쪽에 나타나야 합니다.

### 존재 연산자

| 연산자 | 설명 | 예제 |
|----------|-------------|---------|
| `?` | 키/요소 존재 | `'{"a":1}'::jsonb ? 'a'` → `true` |
| `?\|` | 어떤 키든 존재 | `'{"a":1}'::jsonb ?\| array['a','b']` → `true` |
| `?&` | 모든 키 존재 | `'{"a":1,"b":2}'::jsonb ?& array['a','b']` → `true` |

### 배열 겹침

| 연산자 | 설명 | 예제 |
|----------|-------------|---------|
| `&&` | 배열이 공통 요소를 공유 | `ARRAY[1,2] && ARRAY[2,3]` → `true` |

## 스칼라 함수

### jsonb_typeof(value)

JSON 값의 타입을 문자열로 반환합니다: `"object"`, `"array"`, `"string"`, `"number"`, `"boolean"`, 또는 `"null"`.

```sql
SELECT jsonb_typeof('{"a":1}'::jsonb);     -- object
SELECT jsonb_typeof('[1,2]'::jsonb);        -- array
SELECT jsonb_typeof('"hello"'::jsonb);      -- string
```

`json_typeof`는 동일한 동작의 별칭입니다.

### jsonb_array_length(value)

JSON 배열의 요소 수를 반환합니다:

```sql
SELECT jsonb_array_length('[1, 2, 3]'::jsonb);   -- 3
```

### jsonb_keys(value) / jsonb_object_keys(value)

JSON 객체의 키를 배열로 반환합니다:

```sql
SELECT jsonb_keys('{"a":1, "b":2}'::jsonb);   -- {a,b}
```

### jsonb_extract_path(json, VARIADIC keys)

중첩 경로에서 값을 추출합니다:

```sql
SELECT jsonb_extract_path('{"a":{"b":{"c":42}}}'::jsonb, 'a', 'b', 'c');
-- 42
```

### jsonb_extract_path_text(json, VARIADIC keys)

`jsonb_extract_path`와 동일하지만 결과를 텍스트로 반환합니다:

```sql
SELECT jsonb_extract_path_text('{"a":{"b":1}}'::jsonb, 'a', 'b');
-- '1'
```

### jsonb_set(target, path, new_value [, create_missing])

JSON 구조 내 경로에 값을 설정합니다. `create_missing`의 기본값은 `true`입니다:

```sql
SELECT jsonb_set('{"a":1}'::jsonb, '{b}', '2'::jsonb);
-- {"a":1,"b":2}

SELECT jsonb_set('{"a":1}'::jsonb, '{b}', '2'::jsonb, false);
-- {"a":1}  (create_missing이 false이므로 b가 생성되지 않음)
```

### jsonb_insert(target, path, new_value [, insert_after])

경로에 값을 삽입합니다. 배열의 경우, 기본적으로 위치 앞에 삽입합니다. `insert_after`를 `true`로 설정하면 뒤에 삽입합니다:

```sql
SELECT jsonb_insert('[1, 3]'::jsonb, '{1}', '2'::jsonb);
-- [1, 2, 3]

SELECT jsonb_insert('[1, 3]'::jsonb, '{1}', '2'::jsonb, true);
-- [1, 3, 2]
```

객체의 경우, 키가 아직 존재하지 않는 경우에만 추가합니다.

### jsonb_strip_nulls(value)

null 값을 가진 모든 객체 키를 재귀적으로 제거합니다:

```sql
SELECT jsonb_strip_nulls('{"a":1, "b":null, "c":{"d":null}}'::jsonb);
-- {"a":1,"c":{}}
```

### jsonb_pretty(value)

4칸 들여쓰기로 정렬된 JSON 문자열을 반환합니다:

```sql
SELECT jsonb_pretty('{"a":1,"b":[2,3]}'::jsonb);
```

### jsonb_build_object(key1, value1, ...)

교대하는 키-값 인수에서 JSON 객체를 구성합니다:

```sql
SELECT jsonb_build_object('name', 'Alice', 'age', 30);
-- {"name":"Alice","age":30}
```

### jsonb_build_array(value1, value2, ...)

인수에서 JSON 배열을 구성합니다:

```sql
SELECT jsonb_build_array(1, 'two', true);
-- [1,"two",true]
```

### to_jsonb(value) / to_json(value)

값을 JSON으로 변환합니다. JSON 호환 타입은 변경 없이 통과합니다; 다른 타입은 JSON 문자열로 변환됩니다:

```sql
SELECT to_jsonb(42);        -- 42
SELECT to_jsonb('hello');   -- "hello"
```

## 집계 함수

### json_agg(expr) / jsonb_agg(expr)

값을 JSON 배열로 수집합니다:

```sql
SELECT json_agg(name) FROM users;
-- ["Alice","Bob","Carol"]
```

### json_object_agg(key, value) / jsonb_object_agg(key, value)

행의 키-값 쌍에서 JSON 객체를 구축합니다:

```sql
SELECT json_object_agg(name, age) FROM users;
-- {"Alice":30,"Bob":25,"Carol":35}
```

---
title: 함수
description: 스칼라 및 집계 함수 레퍼런스.
---

## 텍스트 함수

| 함수 | 설명 |
|----------|-------------|
| `lower(text)` | 소문자로 변환 |
| `upper(text)` | 대문자로 변환 |
| `initcap(text)` | 각 단어의 첫 글자 대문자 |
| `length(text)` / `char_length(text)` | 문자열 길이 |
| `trim(text)` / `ltrim(text)` / `rtrim(text)` | 공백 제거 |
| `btrim(text [, chars])` | 양쪽에서 문자 제거 |
| `substring(text, start [, len])` | 부분 문자열 추출 |
| `left(text, n)` / `right(text, n)` | 처음/마지막 n자 |
| `lpad(text, len [, pad])` / `rpad(text, len [, pad])` | 문자열 패딩 |
| `replace(text, from, to)` | 발생 치환 |
| `translate(text, from, to)` | 문자별 치환 |
| `concat(a, b)` / `concat_ws(sep, ...)` | 연결 (구분자 포함) |
| `repeat(text, n)` | 문자열 반복 |
| `reverse(text)` | 문자열 뒤집기 |
| `position(substr, text)` | 부분 문자열 위치 찾기 (1부터 시작) |
| `split_part(text, delim, n)` | 분리 후 n번째 부분 가져오기 |
| `ascii(text)` / `chr(int)` | 문자/코드 포인트 변환 |
| `regexp_replace(text, pat, repl [, flags])` | 정규식 치환 (전역은 `'g'`) |
| `regexp_match(text, pattern)` | 첫 번째 정규식 매치를 배열로 |
| `regexp_split_to_array(text, pattern)` | 정규식으로 분리하여 배열로 |
| `starts_with(text, prefix)` | 텍스트가 접두사로 시작하면 true |
| `ends_with(text, suffix)` | 텍스트가 접미사로 끝나면 true |
| `format(formatstr, ...)` | 문자열 포맷 (아래 참고) |
| `quote_ident(value)` | SQL 식별자로 인용 |
| `quote_literal(value)` | SQL 리터럴로 인용 |

### format()

다음 포맷 지정자를 지원합니다:

| 지정자 | 설명 |
|-----------|-------------|
| `%s` | 문자열 치환 |
| `%I` | SQL 식별자 (인용 및 이스케이프) |
| `%L` | SQL 리터럴 (인용 및 이스케이프) |
| `%%` | 리터럴 퍼센트 기호 |

```sql
SELECT format('Hello, %s!', 'world');
-- Hello, world!

SELECT format('SELECT %I FROM %I WHERE id = %L', 'name', 'users', '42');
-- SELECT "name" FROM "users" WHERE id = '42'
```

## 숫자 함수

| 함수 | 설명 |
|----------|-------------|
| `abs(x)` | 절대값 |
| `ceil(x)` / `floor(x)` | 올림/내림 |
| `round(x [, digits])` / `trunc(x [, digits])` | 반올림/절삭 |
| `sign(x)` | 부호 (-1, 0, 1) |
| `mod(x, y)` | 나머지 |
| `power(x, y)` / `sqrt(x)` | 거듭제곱/제곱근 |
| `cbrt(x)` | 세제곱근 |
| `exp(x)` / `ln(x)` / `log10(x)` / `log(base, x)` | 지수/로그 |
| `div(x, y)` | 정수 나눗셈 |
| `factorial(n)` | 팩토리얼 |
| `gcd(a, b)` / `lcm(a, b)` | 최대공약수 / 최소공배수 |
| `pi()` | 원주율 상수 |
| `degrees(rad)` / `radians(deg)` | 각도 변환 |
| `sin` / `cos` / `tan` / `asin` / `acos` / `atan` / `atan2` | 삼각함수 |
| `sinh` / `cosh` / `tanh` / `asinh` / `acosh` / `atanh` | 쌍곡 삼각함수 |
| `random()` | 임의 숫자 [0, 1) |
| `setseed(seed)` | 난수 생성기 시드 설정 |
| `width_bucket(value, low, high, count)` | 값을 버킷에 할당 (아래 참고) |
| `greatest(a, b, ...)` / `least(a, b, ...)` | 값들의 최대/최소 |

### 비트 연산자

| 연산자 | 설명 |
|----------|-------------|
| `x % y` | 나머지 |
| `x & y` | 비트 AND |
| `x \| y` | 비트 OR |
| `x # y` | 비트 XOR |
| `~x` | 비트 NOT |
| `x << n` | 왼쪽 비트 시프트 |
| `x >> n` | 오른쪽 비트 시프트 |

### width_bucket()

`[low, high)` 범위의 `count`개 등간격 버킷 중 하나에 값을 할당합니다:

```sql
SELECT width_bucket(35, 0, 100, 10);
-- 4  (30-39 값의 버킷)
```

`low` 미만의 값에는 0을, `high` 이상의 값에는 `count + 1`을 반환합니다.

## 날짜/시간 함수

| 함수 | 설명 |
|----------|-------------|
| `now()` | 현재 타임스탬프 (UTC) |
| `clock_timestamp()` | 현재 타임스탬프 (UTC) |
| `current_date()` | 현재 날짜 (UTC) |
| `current_time()` | 현재 시간 (UTC) |
| `date_part(field, source)` | 날짜/시간에서 필드 추출 |
| `EXTRACT(field FROM source)` | SQL 표준 추출 |
| `date_trunc(field, source)` | 정밀도로 절삭 (year/quarter/month/week/day/hour/minute/second) |
| `make_date(y, m, d)` / `make_time(h, m, s)` | 날짜/시간 생성 |
| `make_timestamp(y, mo, d, h, mi, s)` | 타임스탬프 생성 |
| `make_interval(days [, hours [, mins [, secs]]])` | 인터벌 생성 |
| `age(ts1, ts2)` / `age(ts)` | 타임스탬프 간 인터벌 |
| `to_char(value, format)` | 텍스트로 포맷 |
| `to_date(text, format)` / `to_timestamp(text, format)` | 포맷으로 파싱 |
| `to_number(text, format)` | 숫자 문자열 파싱 |
| `isfinite(date\|timestamp)` | 항상 true (Java time에 무한대 없음) |

## 배열 함수

| 함수 | 설명 |
|----------|-------------|
| `array_length(arr)` | 요소 수 |
| `array_append(arr, val)` / `array_prepend(val, arr)` | 요소 추가 |
| `array_concat(arr1, arr2)` / `array_cat(arr1, arr2)` | 배열 연결 |
| `array_slice(arr, start [, end])` | 배열 슬라이스 |
| `array_remove(arr, val)` | 모든 발생 제거 |
| `array_position(arr, val)` | 요소 위치 찾기 (1부터 시작) |
| `array_distinct(arr)` | 중복 제거 |
| `array_replace(arr, old, new)` | 일치하는 요소 치환 |
| `array_lower(arr, dim)` / `array_upper(arr, dim)` | 배열 경계 (1부터 시작) |
| `array_ndims(arr)` | 차원 수 (항상 1) |
| `cardinality(arr)` | 요소 수 |
| `string_to_array(text, delim)` | 문자열을 배열로 분리 |
| `array_to_string(arr, sep)` | 배열을 문자열로 결합 |

## 바이너리 함수

| 함수 | 설명 |
|----------|-------------|
| `octet_length(bytea)` | 바이트 수 |
| `get_byte(bytea, offset)` | 0부터 시작하는 오프셋의 바이트 가져오기 (0-255 반환) |
| `set_byte(bytea, offset, value)` | 오프셋의 바이트 설정, 새 bytea 반환 |
| `encode(bytea, format)` / `decode(text, format)` | 바이너리 인코딩 (hex, base64) |

## 시퀀스 함수

| 함수 | 설명 |
|----------|-------------|
| `nextval('name')` | 시퀀스를 진행하고 다음 값 반환 |
| `currval('name')` | 현재 값 (세션에서 이전에 `nextval` 필요) |
| `setval('name', value [, is_called])` | 시퀀스 값 설정 (`is_called` 기본값 `true`) |
| `lastval()` | 이 세션에서 어떤 시퀀스든 마지막 값 반환 |

```sql
CREATE SEQUENCE order_seq START WITH 100;
SELECT nextval('order_seq');   -- 100
SELECT nextval('order_seq');   -- 101
SELECT currval('order_seq');   -- 101
SELECT setval('order_seq', 200);
SELECT nextval('order_seq');   -- 201
SELECT lastval();              -- 201
```

## 기타 스칼라 함수

| 함수 | 설명 |
|----------|-------------|
| `coalesce(a, b, ...)` | 첫 번째 non-null 값 |
| `nullif(a, b)` | a = b이면 NULL |
| `typeof(value)` | 타입 이름을 텍스트로 |
| `gen_random_uuid()` | UUID v4 생성 |

## 집계 함수

| 함수 | 설명 |
|----------|-------------|
| `COUNT(*)` / `COUNT(expr)` | 행 수 |
| `SUM(expr)` | 값의 합 |
| `AVG(expr)` | 평균 |
| `MIN(expr)` / `MAX(expr)` | 최소/최대 |
| `string_agg(text, separator)` | 구분자로 연결 |
| `array_agg(expr)` | 값을 배열로 수집 |
| `bool_and(expr)` / `bool_or(expr)` / `every(expr)` | 행 간 논리 AND/OR |
| `bit_and(expr)` | 행 간 비트 AND |
| `bit_or(expr)` | 행 간 비트 OR |
| `bit_xor(expr)` | 행 간 비트 XOR |
| `variance(expr)` / `var_samp(expr)` | 표본 분산 |
| `var_pop(expr)` | 모집단 분산 |
| `stddev(expr)` / `stddev_samp(expr)` | 표본 표준편차 |
| `stddev_pop(expr)` | 모집단 표준편차 |

모든 집계 함수는 포함되는 행을 제한하기 위해 `FILTER (WHERE ...)` 절을 지원합니다:

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE active) AS active_count
FROM users;
```

참고: [JSON 집계 함수](/reference/json/#집계-함수)

## 윈도우 함수

윈도우 함수는 행을 축소하지 않고 관련 행 그룹을 기반으로 각 행의 값을 계산합니다.

### 순위 함수

| 함수 | 설명 |
|----------|-------------|
| `ROW_NUMBER()` | 파티션 내 순차 행 번호 |
| `RANK()` | 동순위에 갭이 있는 순위 |
| `DENSE_RANK()` | 동순위에 갭이 없는 순위 |

### 오프셋 함수

| 함수 | 설명 |
|----------|-------------|
| `LAG(expr [, offset [, default]])` | 앞 행의 값 (기본 오프셋: 1) |
| `LEAD(expr [, offset [, default]])` | 뒤 행의 값 (기본 오프셋: 1) |
| `NTILE(n)` | 행을 대략 균등한 n개 그룹으로 나눔 |

### 값 함수

| 함수 | 설명 |
|----------|-------------|
| `FIRST_VALUE(expr)` | 윈도우 프레임의 첫 번째 행에서의 `expr` 값 |
| `LAST_VALUE(expr)` | 윈도우 프레임의 마지막 행에서의 `expr` 값 |
| `NTH_VALUE(expr, n)` | 프레임의 n번째 행에서의 `expr` 값 (1부터 시작), 해당 행이 없으면 NULL |

### 집계 윈도우 함수

모든 집계 함수를 `OVER()`와 함께 윈도우 함수로 사용할 수 있습니다. 구문과 프레임 사양은 [쿼리 — 윈도우 함수](/reference/queries/#윈도우-함수)를 참고하세요.

---
title: Functions
description: Scalar and aggregate functions reference.
---

## Text Functions

| Function | Description |
|----------|-------------|
| `lower(text)` | Convert to lowercase |
| `upper(text)` | Convert to uppercase |
| `initcap(text)` | Capitalize each word |
| `length(text)` / `char_length(text)` | String length |
| `trim(text)` / `ltrim(text)` / `rtrim(text)` | Trim whitespace |
| `btrim(text [, chars])` | Trim characters from both ends |
| `substring(text, start [, len])` | Extract substring |
| `left(text, n)` / `right(text, n)` | First/last n characters |
| `lpad(text, len [, pad])` / `rpad(text, len [, pad])` | Pad string |
| `replace(text, from, to)` | Replace occurrences |
| `translate(text, from, to)` | Character-by-character substitution |
| `concat(a, b)` / `concat_ws(sep, ...)` | Concatenate (with separator) |
| `repeat(text, n)` | Repeat string |
| `reverse(text)` | Reverse string |
| `position(substr, text)` | Find substring position (1-based) |
| `split_part(text, delim, n)` | Split and get nth part |
| `ascii(text)` / `chr(int)` | Character/code point conversion |
| `regexp_replace(text, pat, repl [, flags])` | Regex replace (`'g'` for global) |
| `regexp_match(text, pattern)` | First regex match as array |
| `regexp_split_to_array(text, pattern)` | Split by regex into array |
| `starts_with(text, prefix)` | True if text starts with prefix |
| `ends_with(text, suffix)` | True if text ends with suffix |
| `format(formatstr, ...)` | Format string (see below) |
| `quote_ident(value)` | Quote as SQL identifier |
| `quote_literal(value)` | Quote as SQL literal |

### format()

Supports the following format specifiers:

| Specifier | Description |
|-----------|-------------|
| `%s` | String substitution |
| `%I` | SQL identifier (quoted and escaped) |
| `%L` | SQL literal (quoted and escaped) |
| `%%` | Literal percent sign |

```sql
SELECT format('Hello, %s!', 'world');
-- Hello, world!

SELECT format('SELECT %I FROM %I WHERE id = %L', 'name', 'users', '42');
-- SELECT "name" FROM "users" WHERE id = '42'
```

## Numeric Functions

| Function | Description |
|----------|-------------|
| `abs(x)` | Absolute value |
| `ceil(x)` / `floor(x)` | Round up/down |
| `round(x [, digits])` / `trunc(x [, digits])` | Round/truncate |
| `sign(x)` | Sign (-1, 0, 1) |
| `mod(x, y)` | Modulo |
| `power(x, y)` / `sqrt(x)` | Power/square root |
| `cbrt(x)` | Cube root |
| `exp(x)` / `ln(x)` / `log10(x)` / `log(base, x)` | Exponential/logarithm |
| `div(x, y)` | Integer division |
| `factorial(n)` | Factorial |
| `gcd(a, b)` / `lcm(a, b)` | Greatest common divisor / least common multiple |
| `pi()` | Pi constant |
| `degrees(rad)` / `radians(deg)` | Angle conversion |
| `sin` / `cos` / `tan` / `asin` / `acos` / `atan` / `atan2` | Trigonometry |
| `sinh` / `cosh` / `tanh` / `asinh` / `acosh` / `atanh` | Hyperbolic trigonometry |
| `random()` | Random number [0, 1) |
| `setseed(seed)` | Seed the random number generator |
| `width_bucket(value, low, high, count)` | Assign value to a bucket (see below) |
| `greatest(a, b, ...)` / `least(a, b, ...)` | Max/min of values |

### Bitwise Operators

| Operator | Description |
|----------|-------------|
| `x % y` | Modulo |
| `x & y` | Bitwise AND |
| `x \| y` | Bitwise OR |
| `x # y` | Bitwise XOR |
| `~x` | Bitwise NOT |
| `x << n` | Bit shift left |
| `x >> n` | Bit shift right |

### width_bucket()

Assigns a value to one of `count` equal-width buckets in the range `[low, high)`:

```sql
SELECT width_bucket(35, 0, 100, 10);
-- 4  (bucket for values 30-39)
```

Returns 0 for values below `low`, and `count + 1` for values at or above `high`.

## Date/Time Functions

| Function | Description |
|----------|-------------|
| `now()` | Current timestamp (UTC) |
| `clock_timestamp()` | Current timestamp (UTC) |
| `current_date()` | Current date (UTC) |
| `current_time()` | Current time (UTC) |
| `date_part(field, source)` | Extract field from date/time |
| `EXTRACT(field FROM source)` | SQL standard extract |
| `date_trunc(field, source)` | Truncate to precision (year/quarter/month/week/day/hour/minute/second) |
| `make_date(y, m, d)` / `make_time(h, m, s)` | Construct date/time |
| `make_timestamp(y, mo, d, h, mi, s)` | Construct timestamp |
| `make_interval(days [, hours [, mins [, secs]]])` | Construct interval |
| `age(ts1, ts2)` / `age(ts)` | Interval between timestamps |
| `to_char(value, format)` | Format as text |
| `to_date(text, format)` / `to_timestamp(text, format)` | Parse with format |
| `to_number(text, format)` | Parse numeric string |
| `isfinite(date\|timestamp)` | Always true (no infinities in Java time) |

## Array Functions

| Function | Description |
|----------|-------------|
| `array_length(arr)` | Number of elements |
| `array_append(arr, val)` / `array_prepend(val, arr)` | Add element |
| `array_concat(arr1, arr2)` / `array_cat(arr1, arr2)` | Concatenate arrays |
| `array_slice(arr, start [, end])` | Slice array |
| `array_remove(arr, val)` | Remove all occurrences |
| `array_position(arr, val)` | Find element position (1-based) |
| `array_distinct(arr)` | Remove duplicates |
| `array_replace(arr, old, new)` | Replace matching elements |
| `array_lower(arr, dim)` / `array_upper(arr, dim)` | Array bounds (1-based) |
| `array_ndims(arr)` | Number of dimensions (always 1) |
| `cardinality(arr)` | Number of elements |
| `string_to_array(text, delim)` | Split string to array |
| `array_to_string(arr, sep)` | Join array to string |

## Binary Functions

| Function | Description |
|----------|-------------|
| `octet_length(bytea)` | Byte count |
| `get_byte(bytea, offset)` | Get byte at 0-based offset (returns 0–255) |
| `set_byte(bytea, offset, value)` | Set byte at offset, returns new bytea |
| `encode(bytea, format)` / `decode(text, format)` | Binary encoding (hex, base64) |

## Sequence Functions

| Function | Description |
|----------|-------------|
| `nextval('name')` | Advance sequence and return next value |
| `currval('name')` | Current value (requires prior `nextval` in session) |
| `setval('name', value [, is_called])` | Set sequence value (`is_called` defaults to `true`) |
| `lastval()` | Last value returned by any sequence in this session |

```sql
CREATE SEQUENCE order_seq START WITH 100;
SELECT nextval('order_seq');   -- 100
SELECT nextval('order_seq');   -- 101
SELECT currval('order_seq');   -- 101
SELECT setval('order_seq', 200);
SELECT nextval('order_seq');   -- 201
SELECT lastval();              -- 201
```

## Other Scalar Functions

| Function | Description |
|----------|-------------|
| `coalesce(a, b, ...)` | First non-null value |
| `nullif(a, b)` | NULL if a = b |
| `typeof(value)` | Type name as text |
| `gen_random_uuid()` | Generate UUID v4 |

## Aggregate Functions

| Function | Description |
|----------|-------------|
| `COUNT(*)` / `COUNT(expr)` | Count rows |
| `SUM(expr)` | Sum of values |
| `AVG(expr)` | Average |
| `MIN(expr)` / `MAX(expr)` | Minimum/maximum |
| `string_agg(text, separator)` | Concatenate with separator |
| `array_agg(expr)` | Collect values into array |
| `bool_and(expr)` / `bool_or(expr)` / `every(expr)` | Logical AND/OR across rows |
| `bit_and(expr)` | Bitwise AND across rows |
| `bit_or(expr)` | Bitwise OR across rows |
| `bit_xor(expr)` | Bitwise XOR across rows |
| `variance(expr)` / `var_samp(expr)` | Sample variance |
| `var_pop(expr)` | Population variance |
| `stddev(expr)` / `stddev_samp(expr)` | Sample standard deviation |
| `stddev_pop(expr)` | Population standard deviation |

All aggregate functions support the `FILTER (WHERE ...)` clause to restrict which rows are included:

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE active) AS active_count
FROM users;
```

See also: [JSON aggregate functions](/reference/json/#aggregate-functions)

## Window Functions

Window functions compute a value for each row based on a group of related rows, without collapsing them.

### Ranking functions

| Function | Description |
|----------|-------------|
| `ROW_NUMBER()` | Sequential row number within partition |
| `RANK()` | Rank with gaps for ties |
| `DENSE_RANK()` | Rank without gaps for ties |

### Offset functions

| Function | Description |
|----------|-------------|
| `LAG(expr [, offset [, default]])` | Value from a preceding row (default offset: 1) |
| `LEAD(expr [, offset [, default]])` | Value from a following row (default offset: 1) |
| `NTILE(n)` | Divide rows into n roughly equal groups |

### Value functions

| Function | Description |
|----------|-------------|
| `FIRST_VALUE(expr)` | Value of `expr` at the first row of the window frame |
| `LAST_VALUE(expr)` | Value of `expr` at the last row of the window frame |
| `NTH_VALUE(expr, n)` | Value of `expr` at the nth row of the frame (1-based), or NULL if no such row |

### Aggregate window functions

Any aggregate function can be used as a window function with `OVER()`. See [Queries — Window Functions](/reference/queries/#window-functions) for syntax and frame specifications.

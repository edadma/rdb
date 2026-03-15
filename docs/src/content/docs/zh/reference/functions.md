---
title: 函数
description: 标量函数和聚合函数参考。
---

## 文本函数

| 函数 | 描述 |
|----------|-------------|
| `lower(text)` | 转为小写 |
| `upper(text)` | 转为大写 |
| `initcap(text)` | 每个单词首字母大写 |
| `length(text)` / `char_length(text)` | 字符串长度 |
| `trim(text)` / `ltrim(text)` / `rtrim(text)` | 去除空白 |
| `btrim(text [, chars])` | 去除两端的字符 |
| `substring(text, start [, len])` | 提取子字符串 |
| `left(text, n)` / `right(text, n)` | 前/后 n 个字符 |
| `lpad(text, len [, pad])` / `rpad(text, len [, pad])` | 填充字符串 |
| `replace(text, from, to)` | 替换出现的内容 |
| `translate(text, from, to)` | 逐字符替换 |
| `concat(a, b)` / `concat_ws(sep, ...)` | 连接（带分隔符） |
| `repeat(text, n)` | 重复字符串 |
| `reverse(text)` | 反转字符串 |
| `position(substr, text)` | 查找子字符串位置（从 1 开始） |
| `split_part(text, delim, n)` | 分割并获取第 n 部分 |
| `ascii(text)` / `chr(int)` | 字符/码点转换 |
| `regexp_replace(text, pat, repl [, flags])` | 正则替换（`'g'` 全局替换） |
| `regexp_match(text, pattern)` | 第一个正则匹配作为数组 |
| `regexp_split_to_array(text, pattern)` | 按正则分割为数组 |
| `starts_with(text, prefix)` | 文本是否以前缀开头 |
| `ends_with(text, suffix)` | 文本是否以后缀结尾 |
| `format(formatstr, ...)` | 格式化字符串（见下文） |
| `quote_ident(value)` | 引用为 SQL 标识符 |
| `quote_literal(value)` | 引用为 SQL 字面量 |

### format()

支持以下格式说明符：

| 说明符 | 描述 |
|-----------|-------------|
| `%s` | 字符串替换 |
| `%I` | SQL 标识符（带引号和转义） |
| `%L` | SQL 字面量（带引号和转义） |
| `%%` | 字面百分号 |

```sql
SELECT format('Hello, %s!', 'world');
-- Hello, world!

SELECT format('SELECT %I FROM %I WHERE id = %L', 'name', 'users', '42');
-- SELECT "name" FROM "users" WHERE id = '42'
```

## 数值函数

| 函数 | 描述 |
|----------|-------------|
| `abs(x)` | 绝对值 |
| `ceil(x)` / `floor(x)` | 向上/向下取整 |
| `round(x [, digits])` / `trunc(x [, digits])` | 四舍五入/截断 |
| `sign(x)` | 符号（-1、0、1） |
| `mod(x, y)` | 取模 |
| `power(x, y)` / `sqrt(x)` | 幂/平方根 |
| `cbrt(x)` | 立方根 |
| `exp(x)` / `ln(x)` / `log10(x)` / `log(base, x)` | 指数/对数 |
| `div(x, y)` | 整除 |
| `factorial(n)` | 阶乘 |
| `gcd(a, b)` / `lcm(a, b)` | 最大公约数 / 最小公倍数 |
| `pi()` | Pi 常量 |
| `degrees(rad)` / `radians(deg)` | 角度转换 |
| `sin` / `cos` / `tan` / `asin` / `acos` / `atan` / `atan2` | 三角函数 |
| `sinh` / `cosh` / `tanh` / `asinh` / `acosh` / `atanh` | 双曲三角函数 |
| `random()` | 随机数 [0, 1) |
| `setseed(seed)` | 设置随机数生成器种子 |
| `width_bucket(value, low, high, count)` | 将值分配到桶中（见下文） |
| `greatest(a, b, ...)` / `least(a, b, ...)` | 最大值/最小值 |

### 位运算符

| 运算符 | 描述 |
|----------|-------------|
| `x % y` | 取模 |
| `x & y` | 位与 |
| `x \| y` | 位或 |
| `x # y` | 位异或 |
| `~x` | 位取反 |
| `x << n` | 左移 |
| `x >> n` | 右移 |

### width_bucket()

将值分配到范围 `[low, high)` 中 `count` 个等宽桶之一：

```sql
SELECT width_bucket(35, 0, 100, 10);
-- 4（值 30-39 的桶）
```

低于 `low` 的值返回 0，等于或高于 `high` 的值返回 `count + 1`。

## 日期/时间函数

| 函数 | 描述 |
|----------|-------------|
| `now()` | 当前时间戳（UTC） |
| `clock_timestamp()` | 当前时间戳（UTC） |
| `current_date()` | 当前日期（UTC） |
| `current_time()` | 当前时间（UTC） |
| `date_part(field, source)` | 从日期/时间中提取字段 |
| `EXTRACT(field FROM source)` | SQL 标准提取 |
| `date_trunc(field, source)` | 截断到精度（year/quarter/month/week/day/hour/minute/second） |
| `make_date(y, m, d)` / `make_time(h, m, s)` | 构造日期/时间 |
| `make_timestamp(y, mo, d, h, mi, s)` | 构造时间戳 |
| `make_interval(days [, hours [, mins [, secs]]])` | 构造间隔 |
| `age(ts1, ts2)` / `age(ts)` | 时间戳之间的间隔 |
| `to_char(value, format)` | 格式化为文本 |
| `to_date(text, format)` / `to_timestamp(text, format)` | 按格式解析 |
| `to_number(text, format)` | 解析数值字符串 |
| `isfinite(date\|timestamp)` | 始终为 true（Java 时间无无穷大） |

## 数组函数

| 函数 | 描述 |
|----------|-------------|
| `array_length(arr)` | 元素数量 |
| `array_append(arr, val)` / `array_prepend(val, arr)` | 添加元素 |
| `array_concat(arr1, arr2)` / `array_cat(arr1, arr2)` | 连接数组 |
| `array_slice(arr, start [, end])` | 数组切片 |
| `array_remove(arr, val)` | 移除所有匹配项 |
| `array_position(arr, val)` | 查找元素位置（从 1 开始） |
| `array_distinct(arr)` | 去重 |
| `array_replace(arr, old, new)` | 替换匹配元素 |
| `array_lower(arr, dim)` / `array_upper(arr, dim)` | 数组边界（从 1 开始） |
| `array_ndims(arr)` | 维度数（始终为 1） |
| `cardinality(arr)` | 元素数量 |
| `string_to_array(text, delim)` | 将字符串分割为数组 |
| `array_to_string(arr, sep)` | 将数组连接为字符串 |

## 二进制函数

| 函数 | 描述 |
|----------|-------------|
| `octet_length(bytea)` | 字节数 |
| `get_byte(bytea, offset)` | 获取从 0 开始偏移处的字节（返回 0-255） |
| `set_byte(bytea, offset, value)` | 设置偏移处的字节，返回新 bytea |
| `encode(bytea, format)` / `decode(text, format)` | 二进制编码（hex、base64） |

## 序列函数

| 函数 | 描述 |
|----------|-------------|
| `nextval('name')` | 推进序列并返回下一个值 |
| `currval('name')` | 当前值（会话中需要先调用 `nextval`） |
| `setval('name', value [, is_called])` | 设置序列值（`is_called` 默认为 `true`） |
| `lastval()` | 本会话中任何序列返回的最后一个值 |

```sql
CREATE SEQUENCE order_seq START WITH 100;
SELECT nextval('order_seq');   -- 100
SELECT nextval('order_seq');   -- 101
SELECT currval('order_seq');   -- 101
SELECT setval('order_seq', 200);
SELECT nextval('order_seq');   -- 201
SELECT lastval();              -- 201
```

## 其他标量函数

| 函数 | 描述 |
|----------|-------------|
| `coalesce(a, b, ...)` | 第一个非 null 值 |
| `nullif(a, b)` | 如果 a = b 则返回 NULL |
| `typeof(value)` | 类型名称（文本） |
| `gen_random_uuid()` | 生成 UUID v4 |

## 聚合函数

| 函数 | 描述 |
|----------|-------------|
| `COUNT(*)` / `COUNT(expr)` | 计数行 |
| `SUM(expr)` | 求和 |
| `AVG(expr)` | 平均值 |
| `MIN(expr)` / `MAX(expr)` | 最小值/最大值 |
| `string_agg(text, separator)` | 带分隔符连接 |
| `array_agg(expr)` | 将值收集为数组 |
| `bool_and(expr)` / `bool_or(expr)` / `every(expr)` | 跨行逻辑与/或 |
| `bit_and(expr)` | 跨行位与 |
| `bit_or(expr)` | 跨行位或 |
| `bit_xor(expr)` | 跨行位异或 |
| `variance(expr)` / `var_samp(expr)` | 样本方差 |
| `var_pop(expr)` | 总体方差 |
| `stddev(expr)` / `stddev_samp(expr)` | 样本标准差 |
| `stddev_pop(expr)` | 总体标准差 |

所有聚合函数支持 `FILTER (WHERE ...)` 子句来限制包含的行：

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE active) AS active_count
FROM users;
```

另请参阅：[JSON 聚合函数](/reference/json/#聚合函数)

## 窗口函数

窗口函数基于与当前行相关的一组行计算值，而不折叠它们。

### 排名函数

| 函数 | 描述 |
|----------|-------------|
| `ROW_NUMBER()` | 分区内的连续行号 |
| `RANK()` | 并列时有间隔的排名 |
| `DENSE_RANK()` | 并列时无间隔的排名 |

### 偏移函数

| 函数 | 描述 |
|----------|-------------|
| `LAG(expr [, offset [, default]])` | 前一行的值（默认偏移：1） |
| `LEAD(expr [, offset [, default]])` | 后一行的值（默认偏移：1） |
| `NTILE(n)` | 将行分为 n 个大致相等的组 |

### 值函数

| 函数 | 描述 |
|----------|-------------|
| `FIRST_VALUE(expr)` | 窗口帧第一行的 `expr` 值 |
| `LAST_VALUE(expr)` | 窗口帧最后一行的 `expr` 值 |
| `NTH_VALUE(expr, n)` | 窗口帧第 n 行的 `expr` 值（从 1 开始），如果不存在则为 NULL |

### 聚合窗口函数

任何聚合函数都可以配合 `OVER()` 作为窗口函数使用。语法和帧规范请参阅[查询 — 窗口函数](/reference/queries/#窗口函数)。

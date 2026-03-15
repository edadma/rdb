---
title: C API
description: 用于在 C、C++、Rust、Go、Python 和其他语言中嵌入 PetraDB 的 SQLite 风格 C API。
---

PetraDB 提供原生共享库（`libpetradb-engine.so` / `.dylib`），带有仿照 SQLite 的 C API。该库是自包含的 — 无需 JVM 或运行时。

从 [GitHub Releases](https://github.com/edadma/petradb/releases) 下载或使用 `sbt engineNative/nativeLink` 从源码构建。设置说明请参阅 [C 语言入门](/getting-started/c/)。

包含 `petradb.h` 并使用 `-lpetradb-engine` 链接。

## 快速开始

```c
#include "petradb.h"

int db = petradb_open();
int conn = petradb_connect(db);

petradb_exec(conn, "CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT)");
petradb_exec(conn, "INSERT INTO users (name) VALUES ('Alice')");

int cur = petradb_prepare(conn, "SELECT id, name FROM users");
while (petradb_step(cur) == PETRADB_ROW) {
    int id = petradb_column_int(cur, 0);
    const char *name = petradb_column_text(cur, 1);
    printf("%d: %s\n", id, name);
}
petradb_finalize(cur);
petradb_close(db);
```

## 数据库生命周期

```c
int petradb_open(void);                       // 内存数据库
int petradb_open_persistent(const char *path); // 持久化数据库
int petradb_close(int db);                     // 关闭并释放
int petradb_connect(int db);                   // 创建会话
```

## 执行语句

```c
int petradb_exec(int conn, const char *sql);   // 返回受影响行数，错误时返回 -1
```

## 游标（查询结果）

```c
int petradb_prepare(int conn, const char *sql); // 返回游标句柄
int petradb_step(int cursor);                   // PETRADB_ROW (1)、PETRADB_DONE (0)、PETRADB_ERROR (-1)
int petradb_finalize(int cursor);               // 关闭游标
```

### 列元数据

```c
int petradb_column_count(int cursor);
const char *petradb_column_name(int cursor, int index);
```

### 列值

```c
int         petradb_column_type(int cursor, int index);     // PETRADB_INTEGER/FLOAT/TEXT/BLOB/NULL
int         petradb_column_int(int cursor, int index);
long long   petradb_column_int64(int cursor, int index);
double      petradb_column_double(int cursor, int index);
const char *petradb_column_text(int cursor, int index);     // 不要释放
const void *petradb_column_blob(int cursor, int index);     // 不要释放
int         petradb_column_bytes(int cursor, int index);    // text/blob 的字节长度
int         petradb_column_is_null(int cursor, int index);
```

## 用户自定义函数

注册可从 SQL、触发器和存储过程中调用的原生 C 函数：

```c
void my_double(int ctx, int argc, const int* argv) {
    if (petradb_value_is_null(argv[0])) {
        petradb_result_null(ctx);
        return;
    }
    int x = petradb_value_int(argv[0]);
    petradb_result_int(ctx, x * 2);
}

petradb_create_function(db, "my_double", 1, NULL, my_double);
// 现在可以使用：SELECT my_double(age) FROM users;
```

### 注册

```c
typedef void (*petradb_func_callback)(int ctx, int argc, const int* argv);
int petradb_create_function(int db, const char* name, int nargs, void* user_data, petradb_func_callback fn);
```

### 读取参数

```c
int         petradb_value_int(int value);
long long   petradb_value_int64(int value);
double      petradb_value_double(int value);
const char *petradb_value_text(int value);
int         petradb_value_type(int value);
int         petradb_value_is_null(int value);
```

### 设置结果

```c
void petradb_result_int(int ctx, int value);
void petradb_result_int64(int ctx, long long value);
void petradb_result_double(int ctx, double value);
void petradb_result_text(int ctx, const char* value);
void petradb_result_null(int ctx);
void petradb_result_error(int ctx, const char* msg);  // 中止 SQL 语句
```

### 用户数据

通过 `user_data` 传递应用特定的上下文：

```c
static int call_count = 0;

void my_counter(int ctx, int argc, const int* argv) {
    int *p = (int*)petradb_user_data(ctx);
    (*p)++;
    petradb_result_int(ctx, *p);
}

petradb_create_function(db, "call_count", 0, &call_count, my_counter);
```

## 错误处理

```c
const char *petradb_errmsg(void);   // 最后的错误消息，无错误时为空字符串
```

所有返回句柄的函数在错误时返回 `0`。返回状态码的函数在错误时返回 `-1`。任何错误之后，调用 `petradb_errmsg()` 获取详细信息。成功的操作会清除错误。

## 类型常量

| 常量 | 值 | 描述 |
|----------|-------|-------------|
| `PETRADB_INTEGER` | 1 | 整数值 |
| `PETRADB_FLOAT` | 2 | 浮点数值 |
| `PETRADB_TEXT` | 3 | 文本字符串 |
| `PETRADB_BLOB` | 4 | 二进制数据 |
| `PETRADB_NULL` | 5 | SQL NULL |

## 内存管理

- 来自 `petradb_column_text`、`petradb_value_text` 和 `petradb_errmsg` 的字符串指针由 PetraDB 拥有。不要释放它们。它们在下一次返回字符串的调用之前有效。
- 共享库包含 Scala Native 的垃圾收集器。数据库对象不需要手动内存管理 — 只需在完成后调用 `petradb_finalize` 和 `petradb_close`。

## 语言绑定

C API 适用于任何支持 C FFI 的语言：

- **Rust**：`unsafe extern "C"` 声明 + `-lpetradb-engine`
- **Python**：`ctypes.cdll.LoadLibrary("libpetradb-engine.so")`
- **Go**：`// #cgo LDFLAGS: -lpetradb-engine` + `import "C"`
- **Ruby**：使用 `ffi` gem 的 `FFI::Library`

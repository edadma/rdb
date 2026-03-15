---
title: C API
description: SQLite-style C API for embedding PetraDB in C, C++, Rust, Go, Python, and other languages.
---

PetraDB compiles to a native shared library (`libpetradb-engine.so` / `.dylib`) via Scala Native, providing a C API modeled after SQLite.

Build with:
```bash
sbt engineNative/nativeLink
```

Include `petradb.h` and link with `-lpetradb-engine`.

## Quick Start

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

## Database Lifecycle

```c
int petradb_open(void);                       // in-memory database
int petradb_open_persistent(const char *path); // persistent database
int petradb_close(int db);                     // close and free
int petradb_connect(int db);                   // create a session
```

## Execute Statements

```c
int petradb_exec(int conn, const char *sql);   // returns affected row count, -1 on error
```

## Cursors (Query Results)

```c
int petradb_prepare(int conn, const char *sql); // returns cursor handle
int petradb_step(int cursor);                   // PETRADB_ROW (1), PETRADB_DONE (0), PETRADB_ERROR (-1)
int petradb_finalize(int cursor);               // close cursor
```

### Column Metadata

```c
int petradb_column_count(int cursor);
const char *petradb_column_name(int cursor, int index);
```

### Column Values

```c
int         petradb_column_type(int cursor, int index);     // PETRADB_INTEGER/FLOAT/TEXT/BLOB/NULL
int         petradb_column_int(int cursor, int index);
long long   petradb_column_int64(int cursor, int index);
double      petradb_column_double(int cursor, int index);
const char *petradb_column_text(int cursor, int index);     // do NOT free
const void *petradb_column_blob(int cursor, int index);     // do NOT free
int         petradb_column_bytes(int cursor, int index);    // byte length of text/blob
int         petradb_column_is_null(int cursor, int index);
```

## User-Defined Functions

Register native C functions callable from SQL, triggers, and stored procedures:

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
// Now usable: SELECT my_double(age) FROM users;
```

### Registration

```c
typedef void (*petradb_func_callback)(int ctx, int argc, const int* argv);
int petradb_create_function(int db, const char* name, int nargs, void* user_data, petradb_func_callback fn);
```

### Reading Arguments

```c
int         petradb_value_int(int value);
long long   petradb_value_int64(int value);
double      petradb_value_double(int value);
const char *petradb_value_text(int value);
int         petradb_value_type(int value);
int         petradb_value_is_null(int value);
```

### Setting Results

```c
void petradb_result_int(int ctx, int value);
void petradb_result_int64(int ctx, long long value);
void petradb_result_double(int ctx, double value);
void petradb_result_text(int ctx, const char* value);
void petradb_result_null(int ctx);
void petradb_result_error(int ctx, const char* msg);  // aborts the SQL statement
```

### User Data

Pass application-specific context through `user_data`:

```c
static int call_count = 0;

void my_counter(int ctx, int argc, const int* argv) {
    int *p = (int*)petradb_user_data(ctx);
    (*p)++;
    petradb_result_int(ctx, *p);
}

petradb_create_function(db, "call_count", 0, &call_count, my_counter);
```

## Error Handling

```c
const char *petradb_errmsg(void);   // last error message, empty string if none
```

All functions that return handles return `0` on error. Functions that return status codes return `-1` on error. After any error, call `petradb_errmsg()` for details. Successful operations clear the error.

## Type Constants

| Constant | Value | Description |
|----------|-------|-------------|
| `PETRADB_INTEGER` | 1 | Integer value |
| `PETRADB_FLOAT` | 2 | Floating-point value |
| `PETRADB_TEXT` | 3 | Text string |
| `PETRADB_BLOB` | 4 | Binary data |
| `PETRADB_NULL` | 5 | SQL NULL |

## Memory Management

- String pointers from `petradb_column_text`, `petradb_value_text`, and `petradb_errmsg` are owned by PetraDB. Do NOT free them. They remain valid until the next call that returns a string.
- The shared library includes Scala Native's garbage collector. No manual memory management is needed for database objects — just call `petradb_finalize` and `petradb_close` when done.

## Language Bindings

The C API works with any language that supports C FFI:

- **Rust**: `unsafe extern "C"` declarations + `-lpetradb-engine`
- **Python**: `ctypes.cdll.LoadLibrary("libpetradb-engine.so")`
- **Go**: `// #cgo LDFLAGS: -lpetradb-engine` + `import "C"`
- **Ruby**: `FFI::Library` from the `ffi` gem

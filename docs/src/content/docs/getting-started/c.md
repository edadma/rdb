---
title: Getting Started with C
description: Build the PetraDB shared library and run your first SQL queries from C.
---

## Build the Library

PetraDB compiles to a native shared library via Scala Native. You need [sbt](https://www.scala-sbt.org/) installed.

```bash
git clone https://github.com/edadma/petradb.git
cd petradb
sbt engineNative/nativeLink
```

This produces `engine/native/target/scala-3.8.2/libpetradb-engine.so` (Linux) or `.dylib` (macOS).

The C header is at `engine/native/petradb.h`.

## Your First Program

```c
#include <stdio.h>
#include "petradb.h"

int main(void) {
    int db = petradb_open();
    int conn = petradb_connect(db);

    petradb_exec(conn, "CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, email TEXT)");
    petradb_exec(conn, "INSERT INTO users (name, email) VALUES ('Alice', 'alice@example.com')");
    petradb_exec(conn, "INSERT INTO users (name, email) VALUES ('Bob', 'bob@example.com')");

    int cur = petradb_prepare(conn, "SELECT id, name, email FROM users ORDER BY id");
    while (petradb_step(cur) == PETRADB_ROW) {
        int id = petradb_column_int(cur, 0);
        const char *name = petradb_column_text(cur, 1);
        const char *email = petradb_column_text(cur, 2);
        printf("%d: %s <%s>\n", id, name, email);
    }
    petradb_finalize(cur);
    petradb_close(db);
    return 0;
}
```

## Compile and Run

```bash
gcc -o myapp myapp.c \
    -I/path/to/petradb/engine/native \
    -L/path/to/petradb/engine/native/target/scala-3.8.2 \
    -lpetradb-engine \
    -Wl,-rpath,/path/to/petradb/engine/native/target/scala-3.8.2

./myapp
```

Output:
```
1: Alice <alice@example.com>
2: Bob <bob@example.com>
```

## Persistent Storage

For data that survives restarts, use `petradb_open_persistent`:

```c
int db = petradb_open_persistent("mydata.db");
```

The database file is created on first use and reopened on subsequent runs. All tables, data, indexes, triggers, and stored procedures persist automatically.

## User-Defined Functions

Register native C functions callable from SQL:

```c
void my_upper(int ctx, int argc, const int* argv) {
    const char *s = petradb_value_text(argv[0]);
    if (s == NULL) { petradb_result_null(ctx); return; }
    // (uppercase logic here)
    petradb_result_text(ctx, result);
}

petradb_create_function(db, "my_upper", 1, NULL, my_upper);
```

## Other Languages

The same shared library works with any language that supports C FFI:

- **Rust**: `unsafe extern "C"` declarations
- **Python**: `ctypes.cdll.LoadLibrary()`
- **Go**: `cgo` with `// #cgo LDFLAGS: -lpetradb-engine`

## Next Steps

See the [C API reference](/reference/api-c/) for the complete function listing, including cursors, column accessors, user-defined functions, and error handling.

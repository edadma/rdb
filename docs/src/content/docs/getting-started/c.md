---
title: Getting Started with C
description: Use PetraDB from C, C++, Rust, Go, Python, or any language with C FFI support.
---

PetraDB provides a native shared library (`libpetradb-engine.so` on Linux, `.dylib` on macOS) with a SQLite-style C API. The library is self-contained — no JVM, Scala, or other runtime needed.

## Get the Library

You need two files: the shared library and the header.

### Option 1: Download from GitHub Releases

Download `libpetradb-engine.so` and `petradb.h` from the [latest release](https://github.com/edadma/petradb/releases). Place them in a directory of your choice (e.g. `/usr/local/lib` and `/usr/local/include`, or a project-local directory).

### Option 2: Build from Source

Requires [sbt](https://www.scala-sbt.org/) and a C toolchain (gcc/clang).

```bash
git clone https://github.com/edadma/petradb.git
cd petradb
sbt engineNative/nativeLink
```

This produces:
- **Library**: `engine/native/target/scala-3.8.2/libpetradb-engine.so`
- **Header**: `engine/native/petradb.h`

## Your First Program

Create `myapp.c`:

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

Assuming the library and header are in `/usr/local/lib` and `/usr/local/include`:

```bash
gcc -o myapp myapp.c -lpetradb-engine
./myapp
```

If the files are in a project-local directory (e.g. `./lib` and `./include`):

```bash
gcc -o myapp myapp.c \
    -I./include \
    -L./lib \
    -lpetradb-engine \
    -Wl,-rpath,./lib

./myapp
```

The flags:
- `-I` tells the compiler where to find `petradb.h`
- `-L` tells the linker where to find `libpetradb-engine.so`
- `-l` specifies the library name (the linker adds the `lib` prefix and `.so` suffix)
- `-Wl,-rpath` embeds the library path in the executable so it can find the `.so` at runtime

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

Register native C functions callable from SQL, triggers, and stored procedures:

```c
void my_double(int ctx, int argc, const int* argv) {
    if (petradb_value_is_null(argv[0])) {
        petradb_result_null(ctx);
        return;
    }
    petradb_result_int(ctx, petradb_value_int(argv[0]) * 2);
}

petradb_create_function(db, "my_double", 1, NULL, my_double);
// Now usable: SELECT my_double(age) FROM users;
```

## Other Languages

The same shared library works with any language that supports C FFI:

- **Rust**: `unsafe extern "C"` declarations + link with `-lpetradb-engine`
- **Python**: `ctypes.cdll.LoadLibrary("libpetradb-engine.so")`
- **Go**: `cgo` with `// #cgo LDFLAGS: -lpetradb-engine`
- **Ruby**: `FFI::Library` from the `ffi` gem

## Next Steps

See the [C API reference](/reference/api-c/) for the complete function listing, including cursors, column accessors, user-defined functions, and error handling.

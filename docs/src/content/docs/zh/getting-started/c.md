---
title: C 语言入门
description: 从 C、C++、Rust、Go、Python 或任何支持 C FFI 的语言使用 PetraDB。
---

PetraDB 提供原生共享库（Linux 上为 `libpetradb-engine.so`，macOS 上为 `.dylib`），带有 SQLite 风格的 C API。该库是自包含的 — 无需 JVM、Scala 或其他运行时。

## 获取库文件

你需要两个文件：共享库和头文件。

### 方式一：从 GitHub Releases 下载

从[最新版本](https://github.com/edadma/petradb/releases)下载 `libpetradb-engine.so` 和 `petradb.h`。将它们放在你选择的目录中（例如 `/usr/local/lib` 和 `/usr/local/include`，或项目本地目录）。

### 方式二：从源码构建

需要 [sbt](https://www.scala-sbt.org/) 和 C 工具链（gcc/clang）。

```bash
git clone https://github.com/edadma/petradb.git
cd petradb
sbt engineNative/nativeLink
```

这会生成：
- **库文件**：`engine/native/target/scala-3.8.2/libpetradb-engine.so`
- **头文件**：`engine/native/petradb.h`

## 你的第一个程序

创建 `myapp.c`：

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

## 编译和运行

假设库文件和头文件在 `/usr/local/lib` 和 `/usr/local/include`：

```bash
gcc -o myapp myapp.c -lpetradb-engine
./myapp
```

如果文件在项目本地目录（例如 `./lib` 和 `./include`）：

```bash
gcc -o myapp myapp.c \
    -I./include \
    -L./lib \
    -lpetradb-engine \
    -Wl,-rpath,./lib

./myapp
```

这些标志的含义：
- `-I` 告诉编译器在哪里找到 `petradb.h`
- `-L` 告诉链接器在哪里找到 `libpetradb-engine.so`
- `-l` 指定库名称（链接器会添加 `lib` 前缀和 `.so` 后缀）
- `-Wl,-rpath` 在可执行文件中嵌入库路径，以便在运行时找到 `.so`

输出：
```
1: Alice <alice@example.com>
2: Bob <bob@example.com>
```

## 持久化存储

对于需要在重启后保留的数据，使用 `petradb_open_persistent`：

```c
int db = petradb_open_persistent("mydata.db");
```

数据库文件在首次使用时创建，后续运行时重新打开。所有表、数据、索引、触发器和存储过程都会自动持久化。

## 用户自定义函数

注册可从 SQL、触发器和存储过程中调用的原生 C 函数：

```c
void my_double(int ctx, int argc, const int* argv) {
    if (petradb_value_is_null(argv[0])) {
        petradb_result_null(ctx);
        return;
    }
    petradb_result_int(ctx, petradb_value_int(argv[0]) * 2);
}

petradb_create_function(db, "my_double", 1, NULL, my_double);
// 现在可以使用：SELECT my_double(age) FROM users;
```

## 其他语言

同样的共享库可用于任何支持 C FFI 的语言：

- **Rust**：`unsafe extern "C"` 声明 + 链接 `-lpetradb-engine`
- **Python**：`ctypes.cdll.LoadLibrary("libpetradb-engine.so")`
- **Go**：`cgo` 配合 `// #cgo LDFLAGS: -lpetradb-engine`
- **Ruby**：使用 `ffi` gem 的 `FFI::Library`

## 下一步

查看 [C API 参考](/reference/api-c/)了解完整函数列表，包括游标、列访问器、用户自定义函数和错误处理。

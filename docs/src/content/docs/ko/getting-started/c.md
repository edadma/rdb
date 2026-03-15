---
title: C 시작하기
description: C, C++, Rust, Go, Python 또는 C FFI를 지원하는 모든 언어에서 PetraDB를 사용합니다.
---

PetraDB는 SQLite 스타일의 C API를 가진 네이티브 공유 라이브러리(Linux에서는 `libpetradb-engine.so`, macOS에서는 `.dylib`)를 제공합니다. 라이브러리는 자체 포함되어 있어 JVM, Scala 또는 다른 런타임이 필요 없습니다.

## 라이브러리 얻기

공유 라이브러리와 헤더 두 파일이 필요합니다.

### 옵션 1: GitHub Releases에서 다운로드

[최신 릴리스](https://github.com/edadma/petradb/releases)에서 `libpetradb-engine.so`와 `petradb.h`를 다운로드합니다. 원하는 디렉토리에 배치합니다(예: `/usr/local/lib`과 `/usr/local/include`, 또는 프로젝트 로컬 디렉토리).

### 옵션 2: 소스에서 빌드

[sbt](https://www.scala-sbt.org/)와 C 툴체인(gcc/clang)이 필요합니다.

```bash
git clone https://github.com/edadma/petradb.git
cd petradb
sbt engineNative/nativeLink
```

생성되는 파일:
- **라이브러리**: `engine/native/target/scala-3.8.2/libpetradb-engine.so`
- **헤더**: `engine/native/petradb.h`

## 첫 번째 프로그램

`myapp.c`를 작성합니다:

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

## 컴파일 및 실행

라이브러리와 헤더가 `/usr/local/lib`과 `/usr/local/include`에 있다고 가정합니다:

```bash
gcc -o myapp myapp.c -lpetradb-engine
./myapp
```

프로젝트 로컬 디렉토리(예: `./lib`과 `./include`)에 있는 경우:

```bash
gcc -o myapp myapp.c \
    -I./include \
    -L./lib \
    -lpetradb-engine \
    -Wl,-rpath,./lib

./myapp
```

플래그 설명:
- `-I`는 컴파일러에게 `petradb.h`를 찾을 위치를 알려줍니다
- `-L`은 링커에게 `libpetradb-engine.so`를 찾을 위치를 알려줍니다
- `-l`은 라이브러리 이름을 지정합니다(링커가 `lib` 접두사와 `.so` 접미사를 추가합니다)
- `-Wl,-rpath`는 실행 파일에 라이브러리 경로를 삽입하여 런타임에 `.so`를 찾을 수 있게 합니다

출력:
```
1: Alice <alice@example.com>
2: Bob <bob@example.com>
```

## 영구 스토리지

재시작 후에도 데이터를 유지하려면 `petradb_open_persistent`를 사용합니다:

```c
int db = petradb_open_persistent("mydata.db");
```

데이터베이스 파일은 처음 사용 시 생성되고 이후 실행 시 다시 열립니다. 모든 테이블, 데이터, 인덱스, 트리거, 저장 프로시저가 자동으로 영속됩니다.

## 사용자 정의 함수

SQL, 트리거, 저장 프로시저에서 호출 가능한 네이티브 C 함수를 등록합니다:

```c
void my_double(int ctx, int argc, const int* argv) {
    if (petradb_value_is_null(argv[0])) {
        petradb_result_null(ctx);
        return;
    }
    petradb_result_int(ctx, petradb_value_int(argv[0]) * 2);
}

petradb_create_function(db, "my_double", 1, NULL, my_double);
// 이제 사용 가능: SELECT my_double(age) FROM users;
```

## 다른 언어

동일한 공유 라이브러리가 C FFI를 지원하는 모든 언어에서 작동합니다:

- **Rust**: `unsafe extern "C"` 선언 + `-lpetradb-engine` 링크
- **Python**: `ctypes.cdll.LoadLibrary("libpetradb-engine.so")`
- **Go**: `cgo`와 `// #cgo LDFLAGS: -lpetradb-engine`
- **Ruby**: `ffi` gem의 `FFI::Library`

## 다음 단계

커서, 컬럼 접근자, 사용자 정의 함수, 오류 처리를 포함한 전체 함수 목록은 [C API 레퍼런스](/reference/api-c/)를 참고하세요.

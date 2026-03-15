---
title: C API
description: C, C++, Rust, Go, Python 및 기타 언어에 PetraDB를 임베딩하기 위한 SQLite 스타일 C API.
---

PetraDB는 SQLite를 모델로 한 C API를 가진 네이티브 공유 라이브러리(`libpetradb-engine.so` / `.dylib`)를 제공합니다. 라이브러리는 자체 포함되어 있어 JVM이나 런타임이 필요 없습니다.

[GitHub Releases](https://github.com/edadma/petradb/releases)에서 다운로드하거나 `sbt engineNative/nativeLink`로 소스에서 빌드합니다. 설정 방법은 [C 시작하기](/getting-started/c/)를 참고하세요.

`petradb.h`를 포함하고 `-lpetradb-engine`으로 링크합니다.

## 빠른 시작

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

## 데이터베이스 생명주기

```c
int petradb_open(void);                       // 인메모리 데이터베이스
int petradb_open_persistent(const char *path); // 영구 데이터베이스
int petradb_close(int db);                     // 닫기 및 해제
int petradb_connect(int db);                   // 세션 생성
```

## 구문 실행

```c
int petradb_exec(int conn, const char *sql);   // 영향받은 행 수 반환, 오류 시 -1
```

## 커서 (쿼리 결과)

```c
int petradb_prepare(int conn, const char *sql); // 커서 핸들 반환
int petradb_step(int cursor);                   // PETRADB_ROW (1), PETRADB_DONE (0), PETRADB_ERROR (-1)
int petradb_finalize(int cursor);               // 커서 닫기
```

### 컬럼 메타데이터

```c
int petradb_column_count(int cursor);
const char *petradb_column_name(int cursor, int index);
```

### 컬럼 값

```c
int         petradb_column_type(int cursor, int index);     // PETRADB_INTEGER/FLOAT/TEXT/BLOB/NULL
int         petradb_column_int(int cursor, int index);
long long   petradb_column_int64(int cursor, int index);
double      petradb_column_double(int cursor, int index);
const char *petradb_column_text(int cursor, int index);     // 해제하지 마세요
const void *petradb_column_blob(int cursor, int index);     // 해제하지 마세요
int         petradb_column_bytes(int cursor, int index);    // 텍스트/blob의 바이트 길이
int         petradb_column_is_null(int cursor, int index);
```

## 사용자 정의 함수

SQL, 트리거, 저장 프로시저에서 호출 가능한 네이티브 C 함수를 등록합니다:

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
// 이제 사용 가능: SELECT my_double(age) FROM users;
```

### 등록

```c
typedef void (*petradb_func_callback)(int ctx, int argc, const int* argv);
int petradb_create_function(int db, const char* name, int nargs, void* user_data, petradb_func_callback fn);
```

### 인수 읽기

```c
int         petradb_value_int(int value);
long long   petradb_value_int64(int value);
double      petradb_value_double(int value);
const char *petradb_value_text(int value);
int         petradb_value_type(int value);
int         petradb_value_is_null(int value);
```

### 결과 설정

```c
void petradb_result_int(int ctx, int value);
void petradb_result_int64(int ctx, long long value);
void petradb_result_double(int ctx, double value);
void petradb_result_text(int ctx, const char* value);
void petradb_result_null(int ctx);
void petradb_result_error(int ctx, const char* msg);  // SQL 문을 중단
```

### 사용자 데이터

`user_data`를 통해 애플리케이션별 컨텍스트를 전달합니다:

```c
static int call_count = 0;

void my_counter(int ctx, int argc, const int* argv) {
    int *p = (int*)petradb_user_data(ctx);
    (*p)++;
    petradb_result_int(ctx, *p);
}

petradb_create_function(db, "call_count", 0, &call_count, my_counter);
```

## 오류 처리

```c
const char *petradb_errmsg(void);   // 마지막 오류 메시지, 없으면 빈 문자열
```

핸들을 반환하는 모든 함수는 오류 시 `0`을 반환합니다. 상태 코드를 반환하는 함수는 오류 시 `-1`을 반환합니다. 오류 후에는 `petradb_errmsg()`를 호출하여 상세 정보를 확인합니다. 성공한 작업은 오류를 지웁니다.

## 타입 상수

| 상수 | 값 | 설명 |
|----------|-------|-------------|
| `PETRADB_INTEGER` | 1 | 정수 값 |
| `PETRADB_FLOAT` | 2 | 부동 소수점 값 |
| `PETRADB_TEXT` | 3 | 텍스트 문자열 |
| `PETRADB_BLOB` | 4 | 바이너리 데이터 |
| `PETRADB_NULL` | 5 | SQL NULL |

## 메모리 관리

- `petradb_column_text`, `petradb_value_text`, `petradb_errmsg`의 문자열 포인터는 PetraDB가 소유합니다. 해제하지 마세요. 문자열을 반환하는 다음 호출까지 유효합니다.
- 공유 라이브러리에는 Scala Native의 가비지 컬렉터가 포함되어 있습니다. 데이터베이스 객체에 대한 수동 메모리 관리는 필요 없습니다 — 작업이 끝나면 `petradb_finalize`와 `petradb_close`를 호출하기만 하면 됩니다.

## 언어 바인딩

C API는 C FFI를 지원하는 모든 언어에서 작동합니다:

- **Rust**: `unsafe extern "C"` 선언 + `-lpetradb-engine`
- **Python**: `ctypes.cdll.LoadLibrary("libpetradb-engine.so")`
- **Go**: `// #cgo LDFLAGS: -lpetradb-engine` + `import "C"`
- **Ruby**: `ffi` gem의 `FFI::Library`

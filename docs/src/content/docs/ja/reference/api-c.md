---
title: C API
description: C、C++、Rust、Go、Python、その他の言語にPetraDBを埋め込むためのSQLiteスタイルC APIです。
---

PetraDBはSQLiteをモデルとしたC APIを持つネイティブ共有ライブラリ（`libpetradb-engine.so` / `.dylib`）を提供します。このライブラリは自己完結型で、JVMやランタイムは不要です。

[GitHubリリース](https://github.com/edadma/petradb/releases)からダウンロードするか、`sbt engineNative/nativeLink`でソースからビルドしてください。セットアップ手順については[C ではじめる](/getting-started/c/)をご覧ください。

`petradb.h`をインクルードし、`-lpetradb-engine`でリンクしてください。

## クイックスタート

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

## データベースライフサイクル

```c
int petradb_open(void);                       // インメモリデータベース
int petradb_open_persistent(const char *path); // 永続データベース
int petradb_close(int db);                     // クローズして解放
int petradb_connect(int db);                   // セッションを作成
```

## ステートメントの実行

```c
int petradb_exec(int conn, const char *sql);   // 影響行数を返す、エラー時は-1
```

## カーソル（クエリ結果）

```c
int petradb_prepare(int conn, const char *sql); // カーソルハンドルを返す
int petradb_step(int cursor);                   // PETRADB_ROW (1)、PETRADB_DONE (0)、PETRADB_ERROR (-1)
int petradb_finalize(int cursor);               // カーソルをクローズ
```

### カラムメタデータ

```c
int petradb_column_count(int cursor);
const char *petradb_column_name(int cursor, int index);
```

### カラム値

```c
int         petradb_column_type(int cursor, int index);     // PETRADB_INTEGER/FLOAT/TEXT/BLOB/NULL
int         petradb_column_int(int cursor, int index);
long long   petradb_column_int64(int cursor, int index);
double      petradb_column_double(int cursor, int index);
const char *petradb_column_text(int cursor, int index);     // freeしないでください
const void *petradb_column_blob(int cursor, int index);     // freeしないでください
int         petradb_column_bytes(int cursor, int index);    // text/blobのバイト長
int         petradb_column_is_null(int cursor, int index);
```

## ユーザー定義関数

SQLから呼び出し可能なネイティブC関数を登録します。トリガーやストアドプロシージャからも使用できます。

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
// 使用例: SELECT my_double(age) FROM users;
```

### 登録

```c
typedef void (*petradb_func_callback)(int ctx, int argc, const int* argv);
int petradb_create_function(int db, const char* name, int nargs, void* user_data, petradb_func_callback fn);
```

### 引数の読み取り

```c
int         petradb_value_int(int value);
long long   petradb_value_int64(int value);
double      petradb_value_double(int value);
const char *petradb_value_text(int value);
int         petradb_value_type(int value);
int         petradb_value_is_null(int value);
```

### 結果の設定

```c
void petradb_result_int(int ctx, int value);
void petradb_result_int64(int ctx, long long value);
void petradb_result_double(int ctx, double value);
void petradb_result_text(int ctx, const char* value);
void petradb_result_null(int ctx);
void petradb_result_error(int ctx, const char* msg);  // SQL文を中断
```

### ユーザーデータ

`user_data`を通じてアプリケーション固有のコンテキストを渡します。

```c
static int call_count = 0;

void my_counter(int ctx, int argc, const int* argv) {
    int *p = (int*)petradb_user_data(ctx);
    (*p)++;
    petradb_result_int(ctx, *p);
}

petradb_create_function(db, "call_count", 0, &call_count, my_counter);
```

## エラー処理

```c
const char *petradb_errmsg(void);   // 最後のエラーメッセージ、なければ空文字列
```

ハンドルを返す関数はすべてエラー時に`0`を返します。ステータスコードを返す関数はエラー時に`-1`を返します。エラー後は`petradb_errmsg()`で詳細を取得してください。操作が成功するとエラーはクリアされます。

## 型定数

| 定数 | 値 | 説明 |
|----------|-------|-------------|
| `PETRADB_INTEGER` | 1 | 整数値 |
| `PETRADB_FLOAT` | 2 | 浮動小数点値 |
| `PETRADB_TEXT` | 3 | テキスト文字列 |
| `PETRADB_BLOB` | 4 | バイナリデータ |
| `PETRADB_NULL` | 5 | SQL NULL |

## メモリ管理

- `petradb_column_text`、`petradb_value_text`、`petradb_errmsg`からの文字列ポインタはPetraDBが所有しています。freeしないでください。次の文字列を返す呼び出しまで有効です。
- 共有ライブラリにはScala Nativeのガベージコレクタが含まれています。データベースオブジェクトの手動メモリ管理は不要です — 完了したら`petradb_finalize`と`petradb_close`を呼び出すだけです。

## 言語バインディング

C APIはC FFIをサポートする任意の言語で動作します。

- **Rust**：`unsafe extern "C"`宣言 + `-lpetradb-engine`
- **Python**：`ctypes.cdll.LoadLibrary("libpetradb-engine.so")`
- **Go**：`// #cgo LDFLAGS: -lpetradb-engine` + `import "C"`
- **Ruby**：`ffi` gemの`FFI::Library`

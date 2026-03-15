---
title: C ではじめる
description: C、C++、Rust、Go、Python、またはC FFIをサポートする任意の言語からPetraDBを使用します。
---

PetraDBは、SQLiteスタイルのC APIを持つネイティブ共有ライブラリ（Linuxでは`libpetradb-engine.so`、macOSでは`.dylib`）を提供します。このライブラリは自己完結型で、JVM、Scala、その他のランタイムは不要です。

## ライブラリの取得

共有ライブラリとヘッダーの2つのファイルが必要です。

### オプション1：GitHubリリースからダウンロード

[最新リリース](https://github.com/edadma/petradb/releases)から`libpetradb-engine.so`と`petradb.h`をダウンロードします。任意のディレクトリ（例：`/usr/local/lib`と`/usr/local/include`、またはプロジェクトローカルのディレクトリ）に配置してください。

### オプション2：ソースからビルド

[sbt](https://www.scala-sbt.org/)とCツールチェーン（gcc/clang）が必要です。

```bash
git clone https://github.com/edadma/petradb.git
cd petradb
sbt engineNative/nativeLink
```

これにより以下が生成されます。
- **ライブラリ**：`engine/native/target/scala-3.8.2/libpetradb-engine.so`
- **ヘッダー**：`engine/native/petradb.h`

## 最初のプログラム

`myapp.c`を作成します。

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

## コンパイルと実行

ライブラリとヘッダーが`/usr/local/lib`と`/usr/local/include`にある場合：

```bash
gcc -o myapp myapp.c -lpetradb-engine
./myapp
```

ファイルがプロジェクトローカルのディレクトリ（例：`./lib`と`./include`）にある場合：

```bash
gcc -o myapp myapp.c \
    -I./include \
    -L./lib \
    -lpetradb-engine \
    -Wl,-rpath,./lib

./myapp
```

フラグの説明：
- `-I`はコンパイラに`petradb.h`の場所を指定します
- `-L`はリンカーに`libpetradb-engine.so`の場所を指定します
- `-l`はライブラリ名を指定します（リンカーが`lib`プレフィックスと`.so`サフィックスを追加します）
- `-Wl,-rpath`は実行ファイルにライブラリパスを埋め込み、実行時に`.so`を見つけられるようにします

出力：
```
1: Alice <alice@example.com>
2: Bob <bob@example.com>
```

## 永続ストレージ

再起動後もデータを保持するには、`petradb_open_persistent`を使用します。

```c
int db = petradb_open_persistent("mydata.db");
```

データベースファイルは初回使用時に作成され、以降の実行で再度開かれます。すべてのテーブル、データ、インデックス、トリガー、ストアドプロシージャが自動的に保持されます。

## ユーザー定義関数

SQLから呼び出し可能なネイティブC関数を登録します。トリガーやストアドプロシージャからも使用できます。

```c
void my_double(int ctx, int argc, const int* argv) {
    if (petradb_value_is_null(argv[0])) {
        petradb_result_null(ctx);
        return;
    }
    petradb_result_int(ctx, petradb_value_int(argv[0]) * 2);
}

petradb_create_function(db, "my_double", 1, NULL, my_double);
// 使用例: SELECT my_double(age) FROM users;
```

## 他の言語

同じ共有ライブラリはC FFIをサポートする任意の言語で動作します。

- **Rust**：`unsafe extern "C"`宣言 + `-lpetradb-engine`でリンク
- **Python**：`ctypes.cdll.LoadLibrary("libpetradb-engine.so")`
- **Go**：`cgo`で`// #cgo LDFLAGS: -lpetradb-engine`
- **Ruby**：`ffi` gemの`FFI::Library`

## 次のステップ

カーソル、カラムアクセサ、ユーザー定義関数、エラー処理を含む完全な関数一覧については、[C APIリファレンス](/reference/api-c/)をご覧ください。

---
title: Primeiros Passos com C
description: Use o PetraDB a partir de C, C++, Rust, Go, Python ou qualquer linguagem com suporte a FFI C.
---

O PetraDB fornece uma biblioteca nativa compartilhada (`libpetradb-engine.so` no Linux, `.dylib` no macOS) com uma API C estilo SQLite. A biblioteca e autocontida — sem necessidade de JVM, Scala ou outro runtime.

## Obtenha a Biblioteca

Voce precisa de dois arquivos: a biblioteca compartilhada e o header.

### Opcao 1: Baixar dos Releases do GitHub

Baixe `libpetradb-engine.so` e `petradb.h` do [ultimo release](https://github.com/edadma/petradb/releases). Coloque-os em um diretorio de sua escolha (ex: `/usr/local/lib` e `/usr/local/include`, ou um diretorio local do projeto).

### Opcao 2: Compilar a partir do Codigo Fonte

Requer [sbt](https://www.scala-sbt.org/) e uma toolchain C (gcc/clang).

```bash
git clone https://github.com/edadma/petradb.git
cd petradb
sbt engineNative/nativeLink
```

Isso produz:
- **Biblioteca**: `engine/native/target/scala-3.8.2/libpetradb-engine.so`
- **Header**: `engine/native/petradb.h`

## Seu Primeiro Programa

Crie `myapp.c`:

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

## Compilar e Executar

Assumindo que a biblioteca e o header estao em `/usr/local/lib` e `/usr/local/include`:

```bash
gcc -o myapp myapp.c -lpetradb-engine
./myapp
```

Se os arquivos estao em um diretorio local do projeto (ex: `./lib` e `./include`):

```bash
gcc -o myapp myapp.c \
    -I./include \
    -L./lib \
    -lpetradb-engine \
    -Wl,-rpath,./lib

./myapp
```

As flags:
- `-I` diz ao compilador onde encontrar `petradb.h`
- `-L` diz ao linker onde encontrar `libpetradb-engine.so`
- `-l` especifica o nome da biblioteca (o linker adiciona o prefixo `lib` e o sufixo `.so`)
- `-Wl,-rpath` embute o caminho da biblioteca no executavel para que ele encontre o `.so` em tempo de execucao

Saida:
```
1: Alice <alice@example.com>
2: Bob <bob@example.com>
```

## Armazenamento Persistente

Para dados que sobrevivam a reinicializacoes, use `petradb_open_persistent`:

```c
int db = petradb_open_persistent("mydata.db");
```

O arquivo do banco de dados e criado no primeiro uso e reaberto nas execucoes subsequentes. Todas as tabelas, dados, indices, triggers e stored procedures persistem automaticamente.

## Funcoes Definidas pelo Usuario

Registre funcoes C nativas chamaveis a partir de SQL, triggers e stored procedures:

```c
void my_double(int ctx, int argc, const int* argv) {
    if (petradb_value_is_null(argv[0])) {
        petradb_result_null(ctx);
        return;
    }
    petradb_result_int(ctx, petradb_value_int(argv[0]) * 2);
}

petradb_create_function(db, "my_double", 1, NULL, my_double);
// Agora utilizavel: SELECT my_double(age) FROM users;
```

## Outras Linguagens

A mesma biblioteca compartilhada funciona com qualquer linguagem que suporte FFI C:

- **Rust**: declaracoes `unsafe extern "C"` + link com `-lpetradb-engine`
- **Python**: `ctypes.cdll.LoadLibrary("libpetradb-engine.so")`
- **Go**: `cgo` com `// #cgo LDFLAGS: -lpetradb-engine`
- **Ruby**: `FFI::Library` da gem `ffi`

## Proximos Passos

Veja a [referencia da API C](/reference/api-c/) para a listagem completa de funcoes, incluindo cursores, acessores de coluna, funcoes definidas pelo usuario e tratamento de erros.

---
title: API C
description: API C estilo SQLite para embutir o PetraDB em C, C++, Rust, Go, Python e outras linguagens.
---

O PetraDB fornece uma biblioteca nativa compartilhada (`libpetradb-engine.so` / `.dylib`) com uma API C modelada segundo o SQLite. A biblioteca e autocontida — sem necessidade de JVM ou runtime.

Baixe dos [Releases do GitHub](https://github.com/edadma/petradb/releases) ou compile a partir do codigo fonte com `sbt engineNative/nativeLink`. Veja [Primeiros Passos com C](/getting-started/c/) para instrucoes de configuracao.

Inclua `petradb.h` e faca link com `-lpetradb-engine`.

## Inicio Rapido

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

## Ciclo de Vida do Banco de Dados

```c
int petradb_open(void);                       // banco de dados em memoria
int petradb_open_persistent(const char *path); // banco de dados persistente
int petradb_close(int db);                     // fechar e liberar
int petradb_connect(int db);                   // criar uma sessao
```

## Executar Comandos

```c
int petradb_exec(int conn, const char *sql);   // retorna contagem de linhas afetadas, -1 em caso de erro
```

## Cursores (Resultados de Consulta)

```c
int petradb_prepare(int conn, const char *sql); // retorna handle do cursor
int petradb_step(int cursor);                   // PETRADB_ROW (1), PETRADB_DONE (0), PETRADB_ERROR (-1)
int petradb_finalize(int cursor);               // fechar cursor
```

### Metadados de Coluna

```c
int petradb_column_count(int cursor);
const char *petradb_column_name(int cursor, int index);
```

### Valores de Coluna

```c
int         petradb_column_type(int cursor, int index);     // PETRADB_INTEGER/FLOAT/TEXT/BLOB/NULL
int         petradb_column_int(int cursor, int index);
long long   petradb_column_int64(int cursor, int index);
double      petradb_column_double(int cursor, int index);
const char *petradb_column_text(int cursor, int index);     // NAO libere
const void *petradb_column_blob(int cursor, int index);     // NAO libere
int         petradb_column_bytes(int cursor, int index);    // tamanho em bytes de text/blob
int         petradb_column_is_null(int cursor, int index);
```

## Funcoes Definidas pelo Usuario

Registre funcoes C nativas chamaveis a partir de SQL, triggers e stored procedures:

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
// Agora utilizavel: SELECT my_double(age) FROM users;
```

### Registro

```c
typedef void (*petradb_func_callback)(int ctx, int argc, const int* argv);
int petradb_create_function(int db, const char* name, int nargs, void* user_data, petradb_func_callback fn);
```

### Leitura de Argumentos

```c
int         petradb_value_int(int value);
long long   petradb_value_int64(int value);
double      petradb_value_double(int value);
const char *petradb_value_text(int value);
int         petradb_value_type(int value);
int         petradb_value_is_null(int value);
```

### Definicao de Resultados

```c
void petradb_result_int(int ctx, int value);
void petradb_result_int64(int ctx, long long value);
void petradb_result_double(int ctx, double value);
void petradb_result_text(int ctx, const char* value);
void petradb_result_null(int ctx);
void petradb_result_error(int ctx, const char* msg);  // aborta o comando SQL
```

### Dados do Usuario

Passe contexto especifico da aplicacao atraves de `user_data`:

```c
static int call_count = 0;

void my_counter(int ctx, int argc, const int* argv) {
    int *p = (int*)petradb_user_data(ctx);
    (*p)++;
    petradb_result_int(ctx, *p);
}

petradb_create_function(db, "call_count", 0, &call_count, my_counter);
```

## Tratamento de Erros

```c
const char *petradb_errmsg(void);   // ultima mensagem de erro, string vazia se nenhuma
```

Todas as funcoes que retornam handles retornam `0` em caso de erro. Funcoes que retornam codigos de status retornam `-1` em caso de erro. Apos qualquer erro, chame `petradb_errmsg()` para detalhes. Operacoes bem-sucedidas limpam o erro.

## Constantes de Tipo

| Constante | Valor | Descricao |
|----------|-------|-------------|
| `PETRADB_INTEGER` | 1 | Valor inteiro |
| `PETRADB_FLOAT` | 2 | Valor de ponto flutuante |
| `PETRADB_TEXT` | 3 | String de texto |
| `PETRADB_BLOB` | 4 | Dados binarios |
| `PETRADB_NULL` | 5 | SQL NULL |

## Gerenciamento de Memoria

- Ponteiros de string de `petradb_column_text`, `petradb_value_text` e `petradb_errmsg` pertencem ao PetraDB. NAO os libere. Eles permanecem validos ate a proxima chamada que retorne uma string.
- A biblioteca compartilhada inclui o coletor de lixo do Scala Native. Nenhum gerenciamento manual de memoria e necessario para objetos do banco de dados — apenas chame `petradb_finalize` e `petradb_close` quando terminar.

## Bindings de Linguagem

A API C funciona com qualquer linguagem que suporte FFI C:

- **Rust**: declaracoes `unsafe extern "C"` + `-lpetradb-engine`
- **Python**: `ctypes.cdll.LoadLibrary("libpetradb-engine.so")`
- **Go**: `// #cgo LDFLAGS: -lpetradb-engine` + `import "C"`
- **Ruby**: `FFI::Library` da gem `ffi`

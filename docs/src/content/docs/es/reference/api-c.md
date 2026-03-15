---
title: API de C
description: API C al estilo SQLite para embeber PetraDB en C, C++, Rust, Go, Python y otros lenguajes.
---

PetraDB proporciona una biblioteca compartida nativa (`libpetradb-engine.so` / `.dylib`) con una API C modelada segun SQLite. La biblioteca es autocontenida — no se necesita JVM ni runtime.

Descarga desde [GitHub Releases](https://github.com/edadma/petradb/releases) o compila desde el codigo fuente con `sbt engineNative/nativeLink`. Consulta [Primeros pasos con C](/getting-started/c/) para instrucciones de configuracion.

Incluye `petradb.h` y enlaza con `-lpetradb-engine`.

## Inicio rapido

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

## Ciclo de vida de la base de datos

```c
int petradb_open(void);                       // base de datos en memoria
int petradb_open_persistent(const char *path); // base de datos persistente
int petradb_close(int db);                     // cerrar y liberar
int petradb_connect(int db);                   // crear una sesion
```

## Ejecutar sentencias

```c
int petradb_exec(int conn, const char *sql);   // retorna cantidad de filas afectadas, -1 en error
```

## Cursores (resultados de consulta)

```c
int petradb_prepare(int conn, const char *sql); // retorna handle de cursor
int petradb_step(int cursor);                   // PETRADB_ROW (1), PETRADB_DONE (0), PETRADB_ERROR (-1)
int petradb_finalize(int cursor);               // cerrar cursor
```

### Metadatos de columna

```c
int petradb_column_count(int cursor);
const char *petradb_column_name(int cursor, int index);
```

### Valores de columna

```c
int         petradb_column_type(int cursor, int index);     // PETRADB_INTEGER/FLOAT/TEXT/BLOB/NULL
int         petradb_column_int(int cursor, int index);
long long   petradb_column_int64(int cursor, int index);
double      petradb_column_double(int cursor, int index);
const char *petradb_column_text(int cursor, int index);     // NO liberar
const void *petradb_column_blob(int cursor, int index);     // NO liberar
int         petradb_column_bytes(int cursor, int index);    // longitud en bytes de text/blob
int         petradb_column_is_null(int cursor, int index);
```

## Funciones definidas por el usuario

Registra funciones C nativas invocables desde SQL, triggers y procedimientos almacenados:

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
// Ahora utilizable: SELECT my_double(age) FROM users;
```

### Registro

```c
typedef void (*petradb_func_callback)(int ctx, int argc, const int* argv);
int petradb_create_function(int db, const char* name, int nargs, void* user_data, petradb_func_callback fn);
```

### Lectura de argumentos

```c
int         petradb_value_int(int value);
long long   petradb_value_int64(int value);
double      petradb_value_double(int value);
const char *petradb_value_text(int value);
int         petradb_value_type(int value);
int         petradb_value_is_null(int value);
```

### Establecer resultados

```c
void petradb_result_int(int ctx, int value);
void petradb_result_int64(int ctx, long long value);
void petradb_result_double(int ctx, double value);
void petradb_result_text(int ctx, const char* value);
void petradb_result_null(int ctx);
void petradb_result_error(int ctx, const char* msg);  // aborta la sentencia SQL
```

### Datos de usuario

Pasa contexto especifico de la aplicacion a traves de `user_data`:

```c
static int call_count = 0;

void my_counter(int ctx, int argc, const int* argv) {
    int *p = (int*)petradb_user_data(ctx);
    (*p)++;
    petradb_result_int(ctx, *p);
}

petradb_create_function(db, "call_count", 0, &call_count, my_counter);
```

## Manejo de errores

```c
const char *petradb_errmsg(void);   // ultimo mensaje de error, cadena vacia si no hay ninguno
```

Todas las funciones que retornan handles retornan `0` en caso de error. Las funciones que retornan codigos de estado retornan `-1` en caso de error. Despues de cualquier error, llama a `petradb_errmsg()` para obtener detalles. Las operaciones exitosas limpian el error.

## Constantes de tipo

| Constante | Valor | Descripcion |
|----------|-------|-------------|
| `PETRADB_INTEGER` | 1 | Valor entero |
| `PETRADB_FLOAT` | 2 | Valor de punto flotante |
| `PETRADB_TEXT` | 3 | Cadena de texto |
| `PETRADB_BLOB` | 4 | Datos binarios |
| `PETRADB_NULL` | 5 | SQL NULL |

## Gestion de memoria

- Los punteros a cadenas de `petradb_column_text`, `petradb_value_text` y `petradb_errmsg` son propiedad de PetraDB. NO los liberes. Permanecen validos hasta la siguiente llamada que retorne una cadena.
- La biblioteca compartida incluye el recolector de basura de Scala Native. No se necesita gestion de memoria manual para objetos de base de datos — solo llama a `petradb_finalize` y `petradb_close` cuando termines.

## Bindings para otros lenguajes

La API C funciona con cualquier lenguaje que soporte FFI de C:

- **Rust**: declaraciones `unsafe extern "C"` + `-lpetradb-engine`
- **Python**: `ctypes.cdll.LoadLibrary("libpetradb-engine.so")`
- **Go**: `// #cgo LDFLAGS: -lpetradb-engine` + `import "C"`
- **Ruby**: `FFI::Library` del gem `ffi`

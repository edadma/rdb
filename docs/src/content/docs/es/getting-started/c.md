---
title: Primeros pasos con C
description: Usa PetraDB desde C, C++, Rust, Go, Python o cualquier lenguaje con soporte FFI de C.
---

PetraDB proporciona una biblioteca compartida nativa (`libpetradb-engine.so` en Linux, `.dylib` en macOS) con una API C al estilo SQLite. La biblioteca es autocontenida — no se necesita JVM, Scala ni ningun otro runtime.

## Obtener la biblioteca

Necesitas dos archivos: la biblioteca compartida y el encabezado.

### Opcion 1: Descargar desde GitHub Releases

Descarga `libpetradb-engine.so` y `petradb.h` desde la [ultima version](https://github.com/edadma/petradb/releases). Colocalos en un directorio de tu eleccion (ej. `/usr/local/lib` y `/usr/local/include`, o un directorio local del proyecto).

### Opcion 2: Compilar desde el codigo fuente

Requiere [sbt](https://www.scala-sbt.org/) y una cadena de herramientas C (gcc/clang).

```bash
git clone https://github.com/edadma/petradb.git
cd petradb
sbt engineNative/nativeLink
```

Esto produce:
- **Biblioteca**: `engine/native/target/scala-3.8.2/libpetradb-engine.so`
- **Encabezado**: `engine/native/petradb.h`

## Tu primer programa

Crea `myapp.c`:

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

## Compilar y ejecutar

Asumiendo que la biblioteca y el encabezado estan en `/usr/local/lib` y `/usr/local/include`:

```bash
gcc -o myapp myapp.c -lpetradb-engine
./myapp
```

Si los archivos estan en un directorio local del proyecto (ej. `./lib` y `./include`):

```bash
gcc -o myapp myapp.c \
    -I./include \
    -L./lib \
    -lpetradb-engine \
    -Wl,-rpath,./lib

./myapp
```

Los flags:
- `-I` indica al compilador donde encontrar `petradb.h`
- `-L` indica al enlazador donde encontrar `libpetradb-engine.so`
- `-l` especifica el nombre de la biblioteca (el enlazador agrega el prefijo `lib` y el sufijo `.so`)
- `-Wl,-rpath` incorpora la ruta de la biblioteca en el ejecutable para que pueda encontrar el `.so` en tiempo de ejecucion

Salida:
```
1: Alice <alice@example.com>
2: Bob <bob@example.com>
```

## Almacenamiento persistente

Para datos que sobrevivan a los reinicios, usa `petradb_open_persistent`:

```c
int db = petradb_open_persistent("mydata.db");
```

El archivo de base de datos se crea en el primer uso y se reabre en ejecuciones posteriores. Todas las tablas, datos, indices, triggers y procedimientos almacenados se persisten automaticamente.

## Funciones definidas por el usuario

Registra funciones C nativas invocables desde SQL, triggers y procedimientos almacenados:

```c
void my_double(int ctx, int argc, const int* argv) {
    if (petradb_value_is_null(argv[0])) {
        petradb_result_null(ctx);
        return;
    }
    petradb_result_int(ctx, petradb_value_int(argv[0]) * 2);
}

petradb_create_function(db, "my_double", 1, NULL, my_double);
// Ahora utilizable: SELECT my_double(age) FROM users;
```

## Otros lenguajes

La misma biblioteca compartida funciona con cualquier lenguaje que soporte FFI de C:

- **Rust**: declaraciones `unsafe extern "C"` + enlazar con `-lpetradb-engine`
- **Python**: `ctypes.cdll.LoadLibrary("libpetradb-engine.so")`
- **Go**: `cgo` con `// #cgo LDFLAGS: -lpetradb-engine`
- **Ruby**: `FFI::Library` del gem `ffi`

## Siguientes pasos

Consulta la [referencia de la API C](/reference/api-c/) para el listado completo de funciones, incluyendo cursores, accesores de columna, funciones definidas por el usuario y manejo de errores.

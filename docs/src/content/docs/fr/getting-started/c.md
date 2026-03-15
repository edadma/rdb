---
title: Premiers pas avec C
description: Utilisez PetraDB depuis C, C++, Rust, Go, Python ou tout langage supportant le FFI C.
---

PetraDB fournit une bibliotheque partagee native (`libpetradb-engine.so` sous Linux, `.dylib` sous macOS) avec une API C style SQLite. La bibliotheque est autonome -- pas besoin de JVM, Scala ou autre runtime.

## Obtenir la bibliotheque

Vous avez besoin de deux fichiers : la bibliotheque partagee et l'en-tete.

### Option 1 : Telecharger depuis les releases GitHub

Telechargez `libpetradb-engine.so` et `petradb.h` depuis la [derniere release](https://github.com/edadma/petradb/releases). Placez-les dans un repertoire de votre choix (ex. `/usr/local/lib` et `/usr/local/include`, ou un repertoire local au projet).

### Option 2 : Compiler depuis les sources

Necessite [sbt](https://www.scala-sbt.org/) et une chaine de compilation C (gcc/clang).

```bash
git clone https://github.com/edadma/petradb.git
cd petradb
sbt engineNative/nativeLink
```

Cela produit :
- **Bibliotheque** : `engine/native/target/scala-3.8.2/libpetradb-engine.so`
- **En-tete** : `engine/native/petradb.h`

## Votre premier programme

Creez `myapp.c` :

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

## Compiler et executer

En supposant que la bibliotheque et l'en-tete sont dans `/usr/local/lib` et `/usr/local/include` :

```bash
gcc -o myapp myapp.c -lpetradb-engine
./myapp
```

Si les fichiers sont dans un repertoire local au projet (ex. `./lib` et `./include`) :

```bash
gcc -o myapp myapp.c \
    -I./include \
    -L./lib \
    -lpetradb-engine \
    -Wl,-rpath,./lib

./myapp
```

Les options :
- `-I` indique au compilateur ou trouver `petradb.h`
- `-L` indique a l'editeur de liens ou trouver `libpetradb-engine.so`
- `-l` specifie le nom de la bibliotheque (l'editeur de liens ajoute le prefixe `lib` et le suffixe `.so`)
- `-Wl,-rpath` integre le chemin de la bibliotheque dans l'executable pour qu'il puisse trouver le `.so` a l'execution

Sortie :
```
1: Alice <alice@example.com>
2: Bob <bob@example.com>
```

## Stockage persistant

Pour des donnees qui survivent aux redemarrages, utilisez `petradb_open_persistent` :

```c
int db = petradb_open_persistent("mydata.db");
```

Le fichier de base de donnees est cree lors de la premiere utilisation et rouvert lors des executions suivantes. Toutes les tables, donnees, index, declencheurs et procedures stockees persistent automatiquement.

## Fonctions definies par l'utilisateur

Enregistrez des fonctions C natives appelables depuis SQL, les declencheurs et les procedures stockees :

```c
void my_double(int ctx, int argc, const int* argv) {
    if (petradb_value_is_null(argv[0])) {
        petradb_result_null(ctx);
        return;
    }
    petradb_result_int(ctx, petradb_value_int(argv[0]) * 2);
}

petradb_create_function(db, "my_double", 1, NULL, my_double);
// Maintenant utilisable : SELECT my_double(age) FROM users;
```

## Autres langages

La meme bibliotheque partagee fonctionne avec tout langage supportant le FFI C :

- **Rust** : declarations `unsafe extern "C"` + liaison avec `-lpetradb-engine`
- **Python** : `ctypes.cdll.LoadLibrary("libpetradb-engine.so")`
- **Go** : `cgo` avec `// #cgo LDFLAGS: -lpetradb-engine`
- **Ruby** : `FFI::Library` du gem `ffi`

## Etapes suivantes

Consultez la [reference API C](/reference/api-c/) pour la liste complete des fonctions, y compris les curseurs, les accesseurs de colonnes, les fonctions definies par l'utilisateur et la gestion des erreurs.

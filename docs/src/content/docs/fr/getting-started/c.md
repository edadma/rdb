---
title: Premiers pas avec C
description: Utilisez PetraDB depuis C, C++, Rust, Go, Python ou tout langage supportant le FFI C.
---

PetraDB fournit une bibliothèque partagée native (`libpetradb-engine.so` sous Linux, `.dylib` sous macOS) avec une API C style SQLite. La bibliothèque est autonome — pas besoin de JVM, Scala ou autre runtime.

## Obtenir la bibliothèque

Vous avez besoin de deux fichiers : la bibliothèque partagée et l'en-tête.

### Option 1 : Télécharger depuis les releases GitHub

Téléchargez `libpetradb-engine.so` et `petradb.h` depuis la [dernière release](https://github.com/edadma/petradb/releases). Placez-les dans un répertoire de votre choix (ex. `/usr/local/lib` et `/usr/local/include`, ou un répertoire local au projet).

### Option 2 : Compiler depuis les sources

Nécessite [sbt](https://www.scala-sbt.org/) et une chaîne de compilation C (gcc/clang).

```bash
git clone https://github.com/edadma/petradb.git
cd petradb
sbt engineNative/nativeLink
```

Cela produit :
- **Bibliothèque** : `engine/native/target/scala-3.8.2/libpetradb-engine.so`
- **En-tête** : `engine/native/petradb.h`

## Votre premier programme

Créez `myapp.c` :

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

## Compiler et exécuter

En supposant que la bibliothèque et l'en-tête sont dans `/usr/local/lib` et `/usr/local/include` :

```bash
gcc -o myapp myapp.c -lpetradb-engine
./myapp
```

Si les fichiers sont dans un répertoire local au projet (ex. `./lib` et `./include`) :

```bash
gcc -o myapp myapp.c \
    -I./include \
    -L./lib \
    -lpetradb-engine \
    -Wl,-rpath,./lib

./myapp
```

Les options :
- `-I` indique au compilateur où trouver `petradb.h`
- `-L` indique à l'éditeur de liens où trouver `libpetradb-engine.so`
- `-l` spécifie le nom de la bibliothèque (l'éditeur de liens ajoute le préfixe `lib` et le suffixe `.so`)
- `-Wl,-rpath` intègre le chemin de la bibliothèque dans l'exécutable pour qu'il puisse trouver le `.so` à l'exécution

Sortie :
```
1: Alice <alice@example.com>
2: Bob <bob@example.com>
```

## Stockage persistant

Pour des données qui survivent aux redémarrages, utilisez `petradb_open_persistent` :

```c
int db = petradb_open_persistent("mydata.db");
```

Le fichier de base de données est créé lors de la première utilisation et rouvert lors des exécutions suivantes. Toutes les tables, données, index, déclencheurs et procédures stockées persistent automatiquement.

## Fonctions définies par l'utilisateur

Enregistrez des fonctions C natives appelables depuis SQL, les déclencheurs et les procédures stockées :

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

La même bibliothèque partagée fonctionne avec tout langage supportant le FFI C :

- **Rust** : déclarations `unsafe extern "C"` + liaison avec `-lpetradb-engine`
- **Python** : `ctypes.cdll.LoadLibrary("libpetradb-engine.so")`
- **Go** : `cgo` avec `// #cgo LDFLAGS: -lpetradb-engine`
- **Ruby** : `FFI::Library` du gem `ffi`

## Étapes suivantes

Consultez la [référence API C](/reference/api-c/) pour la liste complète des fonctions, y compris les curseurs, les accesseurs de colonnes, les fonctions définies par l'utilisateur et la gestion des erreurs.

---
title: API C
description: API C style SQLite pour embarquer PetraDB en C, C++, Rust, Go, Python et d'autres langages.
---

PetraDB fournit une bibliotheque partagee native (`libpetradb-engine.so` / `.dylib`) avec une API C modelee d'apres SQLite. La bibliotheque est autonome -- pas de JVM ou de runtime necessaire.

Telechargez depuis les [releases GitHub](https://github.com/edadma/petradb/releases) ou compilez depuis les sources avec `sbt engineNative/nativeLink`. Consultez [Premiers pas avec C](/getting-started/c/) pour les instructions de configuration.

Incluez `petradb.h` et liez avec `-lpetradb-engine`.

## Demarrage rapide

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

## Cycle de vie de la base de donnees

```c
int petradb_open(void);                       // base de donnees en memoire
int petradb_open_persistent(const char *path); // base de donnees persistante
int petradb_close(int db);                     // fermer et liberer
int petradb_connect(int db);                   // creer une session
```

## Executer des instructions

```c
int petradb_exec(int conn, const char *sql);   // retourne le nombre de lignes affectees, -1 en cas d'erreur
```

## Curseurs (resultats de requete)

```c
int petradb_prepare(int conn, const char *sql); // retourne un handle de curseur
int petradb_step(int cursor);                   // PETRADB_ROW (1), PETRADB_DONE (0), PETRADB_ERROR (-1)
int petradb_finalize(int cursor);               // fermer le curseur
```

### Metadonnees de colonnes

```c
int petradb_column_count(int cursor);
const char *petradb_column_name(int cursor, int index);
```

### Valeurs de colonnes

```c
int         petradb_column_type(int cursor, int index);     // PETRADB_INTEGER/FLOAT/TEXT/BLOB/NULL
int         petradb_column_int(int cursor, int index);
long long   petradb_column_int64(int cursor, int index);
double      petradb_column_double(int cursor, int index);
const char *petradb_column_text(int cursor, int index);     // ne PAS liberer
const void *petradb_column_blob(int cursor, int index);     // ne PAS liberer
int         petradb_column_bytes(int cursor, int index);    // longueur en octets du texte/blob
int         petradb_column_is_null(int cursor, int index);
```

## Fonctions definies par l'utilisateur

Enregistrez des fonctions C natives appelables depuis SQL, les declencheurs et les procedures stockees :

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
// Maintenant utilisable : SELECT my_double(age) FROM users;
```

### Enregistrement

```c
typedef void (*petradb_func_callback)(int ctx, int argc, const int* argv);
int petradb_create_function(int db, const char* name, int nargs, void* user_data, petradb_func_callback fn);
```

### Lecture des arguments

```c
int         petradb_value_int(int value);
long long   petradb_value_int64(int value);
double      petradb_value_double(int value);
const char *petradb_value_text(int value);
int         petradb_value_type(int value);
int         petradb_value_is_null(int value);
```

### Definition des resultats

```c
void petradb_result_int(int ctx, int value);
void petradb_result_int64(int ctx, long long value);
void petradb_result_double(int ctx, double value);
void petradb_result_text(int ctx, const char* value);
void petradb_result_null(int ctx);
void petradb_result_error(int ctx, const char* msg);  // interrompt l'instruction SQL
```

### Donnees utilisateur

Passez un contexte specifique a l'application via `user_data` :

```c
static int call_count = 0;

void my_counter(int ctx, int argc, const int* argv) {
    int *p = (int*)petradb_user_data(ctx);
    (*p)++;
    petradb_result_int(ctx, *p);
}

petradb_create_function(db, "call_count", 0, &call_count, my_counter);
```

## Gestion des erreurs

```c
const char *petradb_errmsg(void);   // dernier message d'erreur, chaine vide si aucun
```

Toutes les fonctions retournant des handles retournent `0` en cas d'erreur. Les fonctions retournant des codes de statut retournent `-1` en cas d'erreur. Apres toute erreur, appelez `petradb_errmsg()` pour les details. Les operations reussies effacent l'erreur.

## Constantes de types

| Constante | Valeur | Description |
|-----------|--------|-------------|
| `PETRADB_INTEGER` | 1 | Valeur entiere |
| `PETRADB_FLOAT` | 2 | Valeur a virgule flottante |
| `PETRADB_TEXT` | 3 | Chaine de texte |
| `PETRADB_BLOB` | 4 | Donnees binaires |
| `PETRADB_NULL` | 5 | SQL NULL |

## Gestion de la memoire

- Les pointeurs de chaines provenant de `petradb_column_text`, `petradb_value_text` et `petradb_errmsg` appartiennent a PetraDB. Ne les liberez PAS. Ils restent valides jusqu'au prochain appel retournant une chaine.
- La bibliotheque partagee inclut le ramasse-miettes de Scala Native. Aucune gestion manuelle de la memoire n'est necessaire pour les objets de base de donnees -- appelez simplement `petradb_finalize` et `petradb_close` lorsque vous avez termine.

## Liaisons de langages

L'API C fonctionne avec tout langage supportant le FFI C :

- **Rust** : declarations `unsafe extern "C"` + `-lpetradb-engine`
- **Python** : `ctypes.cdll.LoadLibrary("libpetradb-engine.so")`
- **Go** : `// #cgo LDFLAGS: -lpetradb-engine` + `import "C"`
- **Ruby** : `FFI::Library` du gem `ffi`

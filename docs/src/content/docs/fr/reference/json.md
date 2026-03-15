---
title: JSON
description: Fonctions et opérateurs JSON et JSONB.
---

PetraDB supporte les types `JSON` et `JSONB` pour stocker des données structurées. Les deux types se comportent de manière identique — les valeurs sont stockées en tant qu'objets natifs en interne.

## Opérateurs

### Opérateurs d'accès

| Opérateur | Description | Exemple |
|-----------|-------------|---------|
| `->` | Obtenir la valeur JSON par clé ou index | `'{"a":1}'::jsonb -> 'a'` -> `1` |
| `->>` | Obtenir la valeur JSON sous forme de texte | `'{"a":1}'::jsonb ->> 'a'` -> `'1'` |
| `#>` | Obtenir la valeur JSON au chemin | `'{"a":{"b":1}}'::jsonb #> '{a,b}'` -> `1` |
| `#>>` | Obtenir la valeur JSON au chemin sous forme de texte | `'{"a":{"b":1}}'::jsonb #>> '{a,b}'` -> `'1'` |

L'accès aux tableaux utilise des index entiers base 0, avec des index négatifs comptant depuis la fin :

```sql
SELECT '[10, 20, 30]'::jsonb -> 0;    -- 10
SELECT '[10, 20, 30]'::jsonb -> -1;   -- 30
```

### Opérateurs de contenance

| Opérateur | Description | Exemple |
|-----------|-------------|---------|
| `@>` | La gauche contient la droite | `'{"a":1,"b":2}'::jsonb @> '{"a":1}'` -> `true` |
| `<@` | La gauche est contenue par la droite | `'{"a":1}'::jsonb <@ '{"a":1,"b":2}'` -> `true` |

Pour les objets, la contenance signifie que chaque paire clé-valeur de l'opérande droit existe dans le gauche. Pour les tableaux, chaque élément du droit doit apparaître dans le gauche.

### Opérateurs d'existence

| Opérateur | Description | Exemple |
|-----------|-------------|---------|
| `?` | La clé/l'élément existe | `'{"a":1}'::jsonb ? 'a'` -> `true` |
| `?\|` | Toute clé existe | `'{"a":1}'::jsonb ?\| array['a','b']` -> `true` |
| `?&` | Toutes les clés existent | `'{"a":1,"b":2}'::jsonb ?& array['a','b']` -> `true` |

### Chevauchement de tableaux

| Opérateur | Description | Exemple |
|-----------|-------------|---------|
| `&&` | Les tableaux partagent des éléments communs | `ARRAY[1,2] && ARRAY[2,3]` -> `true` |

## Fonctions scalaires

### jsonb_typeof(value)

Retourne le type d'une valeur JSON sous forme de chaîne : `"object"`, `"array"`, `"string"`, `"number"`, `"boolean"` ou `"null"`.

```sql
SELECT jsonb_typeof('{"a":1}'::jsonb);     -- object
SELECT jsonb_typeof('[1,2]'::jsonb);        -- array
SELECT jsonb_typeof('"hello"'::jsonb);      -- string
```

`json_typeof` est un alias avec un comportement identique.

### jsonb_array_length(value)

Retourne le nombre d'éléments dans un tableau JSON :

```sql
SELECT jsonb_array_length('[1, 2, 3]'::jsonb);   -- 3
```

### jsonb_keys(value) / jsonb_object_keys(value)

Retourne les clés d'un objet JSON sous forme de tableau :

```sql
SELECT jsonb_keys('{"a":1, "b":2}'::jsonb);   -- {a,b}
```

### jsonb_extract_path(json, VARIADIC keys)

Extrait une valeur à un chemin imbriqué :

```sql
SELECT jsonb_extract_path('{"a":{"b":{"c":42}}}'::jsonb, 'a', 'b', 'c');
-- 42
```

### jsonb_extract_path_text(json, VARIADIC keys)

Identique à `jsonb_extract_path` mais retourne le résultat sous forme de texte :

```sql
SELECT jsonb_extract_path_text('{"a":{"b":1}}'::jsonb, 'a', 'b');
-- '1'
```

### jsonb_set(target, path, new_value [, create_missing])

Définit une valeur à un chemin dans une structure JSON. `create_missing` est `true` par défaut :

```sql
SELECT jsonb_set('{"a":1}'::jsonb, '{b}', '2'::jsonb);
-- {"a":1,"b":2}

SELECT jsonb_set('{"a":1}'::jsonb, '{b}', '2'::jsonb, false);
-- {"a":1}  (b non créé car create_missing est false)
```

### jsonb_insert(target, path, new_value [, insert_after])

Insère une valeur à un chemin. Pour les tableaux, insère avant la position par défaut. Définissez `insert_after` à `true` pour insérer après :

```sql
SELECT jsonb_insert('[1, 3]'::jsonb, '{1}', '2'::jsonb);
-- [1, 2, 3]

SELECT jsonb_insert('[1, 3]'::jsonb, '{1}', '2'::jsonb, true);
-- [1, 3, 2]
```

Pour les objets, ajoute la clé uniquement si elle n'existe pas déjà.

### jsonb_strip_nulls(value)

Supprime récursivement toutes les clés d'objets avec des valeurs null :

```sql
SELECT jsonb_strip_nulls('{"a":1, "b":null, "c":{"d":null}}'::jsonb);
-- {"a":1,"c":{}}
```

### jsonb_pretty(value)

Retourne une chaîne JSON formatée avec une indentation de 4 espaces :

```sql
SELECT jsonb_pretty('{"a":1,"b":[2,3]}'::jsonb);
```

### jsonb_build_object(key1, value1, ...)

Construit un objet JSON à partir d'arguments clé-valeur alternés :

```sql
SELECT jsonb_build_object('name', 'Alice', 'age', 30);
-- {"name":"Alice","age":30}
```

### jsonb_build_array(value1, value2, ...)

Construit un tableau JSON à partir d'arguments :

```sql
SELECT jsonb_build_array(1, 'two', true);
-- [1,"two",true]
```

### to_jsonb(value) / to_json(value)

Convertit une valeur en JSON. Les types compatibles JSON passent tels quels ; les autres types sont convertis en chaînes JSON :

```sql
SELECT to_jsonb(42);        -- 42
SELECT to_jsonb('hello');   -- "hello"
```

## Fonctions d'agrégation

### json_agg(expr) / jsonb_agg(expr)

Collecte les valeurs dans un tableau JSON :

```sql
SELECT json_agg(name) FROM users;
-- ["Alice","Bob","Carol"]
```

### json_object_agg(key, value) / jsonb_object_agg(key, value)

Construit un objet JSON à partir de paires clé-valeur sur les lignes :

```sql
SELECT json_object_agg(name, age) FROM users;
-- {"Alice":30,"Bob":25,"Carol":35}
```

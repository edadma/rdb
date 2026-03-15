---
title: JSON
description: Funciones y operadores JSON y JSONB.
---

PetraDB soporta los tipos `JSON` y `JSONB` para almacenar datos estructurados. Ambos tipos se comportan de forma identica — los valores se almacenan como objetos nativos internamente.

## Operadores

### Operadores de acceso

| Operador | Descripcion | Ejemplo |
|----------|-------------|---------|
| `->` | Obtener valor JSON por clave o indice | `'{"a":1}'::jsonb -> 'a'` → `1` |
| `->>` | Obtener valor JSON como texto | `'{"a":1}'::jsonb ->> 'a'` → `'1'` |
| `#>` | Obtener valor JSON en ruta | `'{"a":{"b":1}}'::jsonb #> '{a,b}'` → `1` |
| `#>>` | Obtener valor JSON en ruta como texto | `'{"a":{"b":1}}'::jsonb #>> '{a,b}'` → `'1'` |

El acceso a arrays usa indices enteros base 0, con indices negativos contando desde el final:

```sql
SELECT '[10, 20, 30]'::jsonb -> 0;    -- 10
SELECT '[10, 20, 30]'::jsonb -> -1;   -- 30
```

### Operadores de contencion

| Operador | Descripcion | Ejemplo |
|----------|-------------|---------|
| `@>` | El izquierdo contiene al derecho | `'{"a":1,"b":2}'::jsonb @> '{"a":1}'` → `true` |
| `<@` | El izquierdo esta contenido por el derecho | `'{"a":1}'::jsonb <@ '{"a":1,"b":2}'` → `true` |

Para objetos, contencion significa que cada par clave-valor del operando derecho existe en el izquierdo. Para arrays, cada elemento del derecho debe aparecer en el izquierdo.

### Operadores de existencia

| Operador | Descripcion | Ejemplo |
|----------|-------------|---------|
| `?` | Clave/elemento existe | `'{"a":1}'::jsonb ? 'a'` → `true` |
| `?\|` | Alguna clave existe | `'{"a":1}'::jsonb ?\| array['a','b']` → `true` |
| `?&` | Todas las claves existen | `'{"a":1,"b":2}'::jsonb ?& array['a','b']` → `true` |

### Superposicion de arrays

| Operador | Descripcion | Ejemplo |
|----------|-------------|---------|
| `&&` | Los arrays comparten elementos comunes | `ARRAY[1,2] && ARRAY[2,3]` → `true` |

## Funciones escalares

### jsonb_typeof(value)

Retorna el tipo de un valor JSON como cadena: `"object"`, `"array"`, `"string"`, `"number"`, `"boolean"` o `"null"`.

```sql
SELECT jsonb_typeof('{"a":1}'::jsonb);     -- object
SELECT jsonb_typeof('[1,2]'::jsonb);        -- array
SELECT jsonb_typeof('"hello"'::jsonb);      -- string
```

`json_typeof` es un alias con comportamiento identico.

### jsonb_array_length(value)

Retorna el numero de elementos en un array JSON:

```sql
SELECT jsonb_array_length('[1, 2, 3]'::jsonb);   -- 3
```

### jsonb_keys(value) / jsonb_object_keys(value)

Retorna las claves de un objeto JSON como array:

```sql
SELECT jsonb_keys('{"a":1, "b":2}'::jsonb);   -- {a,b}
```

### jsonb_extract_path(json, VARIADIC keys)

Extrae un valor en una ruta anidada:

```sql
SELECT jsonb_extract_path('{"a":{"b":{"c":42}}}'::jsonb, 'a', 'b', 'c');
-- 42
```

### jsonb_extract_path_text(json, VARIADIC keys)

Igual que `jsonb_extract_path` pero retorna el resultado como texto:

```sql
SELECT jsonb_extract_path_text('{"a":{"b":1}}'::jsonb, 'a', 'b');
-- '1'
```

### jsonb_set(target, path, new_value [, create_missing])

Establece un valor en una ruta dentro de una estructura JSON. `create_missing` por defecto es `true`:

```sql
SELECT jsonb_set('{"a":1}'::jsonb, '{b}', '2'::jsonb);
-- {"a":1,"b":2}

SELECT jsonb_set('{"a":1}'::jsonb, '{b}', '2'::jsonb, false);
-- {"a":1}  (b no se crea porque create_missing es false)
```

### jsonb_insert(target, path, new_value [, insert_after])

Inserta un valor en una ruta. Para arrays, inserta antes de la posicion por defecto. Establece `insert_after` a `true` para insertar despues:

```sql
SELECT jsonb_insert('[1, 3]'::jsonb, '{1}', '2'::jsonb);
-- [1, 2, 3]

SELECT jsonb_insert('[1, 3]'::jsonb, '{1}', '2'::jsonb, true);
-- [1, 3, 2]
```

Para objetos, agrega la clave solo si no existe ya.

### jsonb_strip_nulls(value)

Elimina recursivamente todas las claves de objeto con valores nulos:

```sql
SELECT jsonb_strip_nulls('{"a":1, "b":null, "c":{"d":null}}'::jsonb);
-- {"a":1,"c":{}}
```

### jsonb_pretty(value)

Retorna una cadena JSON con formato legible con indentacion de 4 espacios:

```sql
SELECT jsonb_pretty('{"a":1,"b":[2,3]}'::jsonb);
```

### jsonb_build_object(key1, value1, ...)

Construye un objeto JSON a partir de argumentos alternados de clave-valor:

```sql
SELECT jsonb_build_object('name', 'Alice', 'age', 30);
-- {"name":"Alice","age":30}
```

### jsonb_build_array(value1, value2, ...)

Construye un array JSON a partir de argumentos:

```sql
SELECT jsonb_build_array(1, 'two', true);
-- [1,"two",true]
```

### to_jsonb(value) / to_json(value)

Convierte un valor a JSON. Los tipos compatibles con JSON pasan sin cambios; otros tipos se convierten a cadenas JSON:

```sql
SELECT to_jsonb(42);        -- 42
SELECT to_jsonb('hello');   -- "hello"
```

## Funciones de agregado

### json_agg(expr) / jsonb_agg(expr)

Recopila valores en un array JSON:

```sql
SELECT json_agg(name) FROM users;
-- ["Alice","Bob","Carol"]
```

### json_object_agg(key, value) / jsonb_object_agg(key, value)

Construye un objeto JSON a partir de pares clave-valor entre filas:

```sql
SELECT json_object_agg(name, age) FROM users;
-- {"Alice":30,"Bob":25,"Carol":35}
```

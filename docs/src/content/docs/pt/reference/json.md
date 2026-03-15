---
title: JSON
description: Funcoes e operadores JSON e JSONB.
---

O PetraDB suporta os tipos `JSON` e `JSONB` para armazenar dados estruturados. Ambos os tipos se comportam de forma identica — os valores sao armazenados como objetos nativos internamente.

## Operadores

### Operadores de Acesso

| Operador | Descricao | Exemplo |
|----------|-------------|---------|
| `->` | Obter valor JSON por chave ou indice | `'{"a":1}'::jsonb -> 'a'` -> `1` |
| `->>` | Obter valor JSON como texto | `'{"a":1}'::jsonb ->> 'a'` -> `'1'` |
| `#>` | Obter valor JSON no caminho | `'{"a":{"b":1}}'::jsonb #> '{a,b}'` -> `1` |
| `#>>` | Obter valor JSON no caminho como texto | `'{"a":{"b":1}}'::jsonb #>> '{a,b}'` -> `'1'` |

Acesso a array usa indices inteiros base 0, com indices negativos contando a partir do final:

```sql
SELECT '[10, 20, 30]'::jsonb -> 0;    -- 10
SELECT '[10, 20, 30]'::jsonb -> -1;   -- 30
```

### Operadores de Contencao

| Operador | Descricao | Exemplo |
|----------|-------------|---------|
| `@>` | Esquerdo contem direito | `'{"a":1,"b":2}'::jsonb @> '{"a":1}'` -> `true` |
| `<@` | Esquerdo esta contido pelo direito | `'{"a":1}'::jsonb <@ '{"a":1,"b":2}'` -> `true` |

Para objetos, contencao significa que todo par chave-valor no operando direito existe no esquerdo. Para arrays, todo elemento no direito deve aparecer no esquerdo.

### Operadores de Existencia

| Operador | Descricao | Exemplo |
|----------|-------------|---------|
| `?` | Chave/elemento existe | `'{"a":1}'::jsonb ? 'a'` -> `true` |
| `?\|` | Qualquer chave existe | `'{"a":1}'::jsonb ?\| array['a','b']` -> `true` |
| `?&` | Todas as chaves existem | `'{"a":1,"b":2}'::jsonb ?& array['a','b']` -> `true` |

### Sobreposicao de Arrays

| Operador | Descricao | Exemplo |
|----------|-------------|---------|
| `&&` | Arrays compartilham elementos comuns | `ARRAY[1,2] && ARRAY[2,3]` -> `true` |

## Funcoes Escalares

### jsonb_typeof(value)

Retorna o tipo de um valor JSON como string: `"object"`, `"array"`, `"string"`, `"number"`, `"boolean"` ou `"null"`.

```sql
SELECT jsonb_typeof('{"a":1}'::jsonb);     -- object
SELECT jsonb_typeof('[1,2]'::jsonb);        -- array
SELECT jsonb_typeof('"hello"'::jsonb);      -- string
```

`json_typeof` e um alias com comportamento identico.

### jsonb_array_length(value)

Retorna o numero de elementos em um array JSON:

```sql
SELECT jsonb_array_length('[1, 2, 3]'::jsonb);   -- 3
```

### jsonb_keys(value) / jsonb_object_keys(value)

Retorna as chaves de um objeto JSON como um array:

```sql
SELECT jsonb_keys('{"a":1, "b":2}'::jsonb);   -- {a,b}
```

### jsonb_extract_path(json, VARIADIC keys)

Extrai um valor em um caminho aninhado:

```sql
SELECT jsonb_extract_path('{"a":{"b":{"c":42}}}'::jsonb, 'a', 'b', 'c');
-- 42
```

### jsonb_extract_path_text(json, VARIADIC keys)

Igual a `jsonb_extract_path`, mas retorna o resultado como texto:

```sql
SELECT jsonb_extract_path_text('{"a":{"b":1}}'::jsonb, 'a', 'b');
-- '1'
```

### jsonb_set(target, path, new_value [, create_missing])

Define um valor em um caminho dentro de uma estrutura JSON. `create_missing` padrao `true`:

```sql
SELECT jsonb_set('{"a":1}'::jsonb, '{b}', '2'::jsonb);
-- {"a":1,"b":2}

SELECT jsonb_set('{"a":1}'::jsonb, '{b}', '2'::jsonb, false);
-- {"a":1}  (b nao criado porque create_missing e false)
```

### jsonb_insert(target, path, new_value [, insert_after])

Insere um valor em um caminho. Para arrays, insere antes da posicao por padrao. Defina `insert_after` como `true` para inserir depois:

```sql
SELECT jsonb_insert('[1, 3]'::jsonb, '{1}', '2'::jsonb);
-- [1, 2, 3]

SELECT jsonb_insert('[1, 3]'::jsonb, '{1}', '2'::jsonb, true);
-- [1, 3, 2]
```

Para objetos, adiciona a chave apenas se ela ainda nao existir.

### jsonb_strip_nulls(value)

Remove recursivamente todas as chaves de objeto com valores nulos:

```sql
SELECT jsonb_strip_nulls('{"a":1, "b":null, "c":{"d":null}}'::jsonb);
-- {"a":1,"c":{}}
```

### jsonb_pretty(value)

Retorna uma string JSON formatada com indentacao de 4 espacos:

```sql
SELECT jsonb_pretty('{"a":1,"b":[2,3]}'::jsonb);
```

### jsonb_build_object(key1, value1, ...)

Constroi um objeto JSON a partir de argumentos alternados chave-valor:

```sql
SELECT jsonb_build_object('name', 'Alice', 'age', 30);
-- {"name":"Alice","age":30}
```

### jsonb_build_array(value1, value2, ...)

Constroi um array JSON a partir de argumentos:

```sql
SELECT jsonb_build_array(1, 'two', true);
-- [1,"two",true]
```

### to_jsonb(value) / to_json(value)

Converte um valor para JSON. Tipos compativeis com JSON passam sem alteracao; outros tipos sao convertidos para strings JSON:

```sql
SELECT to_jsonb(42);        -- 42
SELECT to_jsonb('hello');   -- "hello"
```

## Funcoes de Agregacao

### json_agg(expr) / jsonb_agg(expr)

Coleta valores em um array JSON:

```sql
SELECT json_agg(name) FROM users;
-- ["Alice","Bob","Carol"]
```

### json_object_agg(key, value) / jsonb_object_agg(key, value)

Constroi um objeto JSON a partir de pares chave-valor entre linhas:

```sql
SELECT json_object_agg(name, age) FROM users;
-- {"Alice":30,"Bob":25,"Carol":35}
```

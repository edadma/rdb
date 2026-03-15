---
title: Funciones
description: Referencia de funciones escalares y de agregado.
---

## Funciones de texto

| Funcion | Descripcion |
|----------|-------------|
| `lower(text)` | Convertir a minusculas |
| `upper(text)` | Convertir a mayusculas |
| `initcap(text)` | Capitalizar cada palabra |
| `length(text)` / `char_length(text)` | Longitud de cadena |
| `trim(text)` / `ltrim(text)` / `rtrim(text)` | Recortar espacios en blanco |
| `btrim(text [, chars])` | Recortar caracteres de ambos extremos |
| `substring(text, start [, len])` | Extraer subcadena |
| `left(text, n)` / `right(text, n)` | Primeros/ultimos n caracteres |
| `lpad(text, len [, pad])` / `rpad(text, len [, pad])` | Rellenar cadena |
| `replace(text, from, to)` | Reemplazar ocurrencias |
| `translate(text, from, to)` | Sustitucion caracter por caracter |
| `concat(a, b)` / `concat_ws(sep, ...)` | Concatenar (con separador) |
| `repeat(text, n)` | Repetir cadena |
| `reverse(text)` | Invertir cadena |
| `position(substr, text)` | Encontrar posicion de subcadena (base 1) |
| `split_part(text, delim, n)` | Dividir y obtener la parte n |
| `ascii(text)` / `chr(int)` | Conversion caracter/punto de codigo |
| `regexp_replace(text, pat, repl [, flags])` | Reemplazo con regex (`'g'` para global) |
| `regexp_match(text, pattern)` | Primera coincidencia regex como array |
| `regexp_split_to_array(text, pattern)` | Dividir por regex en array |
| `starts_with(text, prefix)` | Verdadero si el texto comienza con el prefijo |
| `ends_with(text, suffix)` | Verdadero si el texto termina con el sufijo |
| `format(formatstr, ...)` | Formatear cadena (ver abajo) |
| `quote_ident(value)` | Citar como identificador SQL |
| `quote_literal(value)` | Citar como literal SQL |

### format()

Soporta los siguientes especificadores de formato:

| Especificador | Descripcion |
|-----------|-------------|
| `%s` | Sustitucion de cadena |
| `%I` | Identificador SQL (citado y escapado) |
| `%L` | Literal SQL (citado y escapado) |
| `%%` | Signo de porcentaje literal |

```sql
SELECT format('Hello, %s!', 'world');
-- Hello, world!

SELECT format('SELECT %I FROM %I WHERE id = %L', 'name', 'users', '42');
-- SELECT "name" FROM "users" WHERE id = '42'
```

## Funciones numericas

| Funcion | Descripcion |
|----------|-------------|
| `abs(x)` | Valor absoluto |
| `ceil(x)` / `floor(x)` | Redondear hacia arriba/abajo |
| `round(x [, digits])` / `trunc(x [, digits])` | Redondear/truncar |
| `sign(x)` | Signo (-1, 0, 1) |
| `mod(x, y)` | Modulo |
| `power(x, y)` / `sqrt(x)` | Potencia/raiz cuadrada |
| `cbrt(x)` | Raiz cubica |
| `exp(x)` / `ln(x)` / `log10(x)` / `log(base, x)` | Exponencial/logaritmo |
| `div(x, y)` | Division entera |
| `factorial(n)` | Factorial |
| `gcd(a, b)` / `lcm(a, b)` | Maximo comun divisor / minimo comun multiplo |
| `pi()` | Constante Pi |
| `degrees(rad)` / `radians(deg)` | Conversion de angulos |
| `sin` / `cos` / `tan` / `asin` / `acos` / `atan` / `atan2` | Trigonometria |
| `sinh` / `cosh` / `tanh` / `asinh` / `acosh` / `atanh` | Trigonometria hiperbolica |
| `random()` | Numero aleatorio [0, 1) |
| `setseed(seed)` | Inicializar el generador de numeros aleatorios |
| `width_bucket(value, low, high, count)` | Asignar valor a un intervalo (ver abajo) |
| `greatest(a, b, ...)` / `least(a, b, ...)` | Maximo/minimo de valores |

### Operadores bit a bit

| Operador | Descripcion |
|----------|-------------|
| `x % y` | Modulo |
| `x & y` | AND bit a bit |
| `x \| y` | OR bit a bit |
| `x # y` | XOR bit a bit |
| `~x` | NOT bit a bit |
| `x << n` | Desplazamiento de bits a la izquierda |
| `x >> n` | Desplazamiento de bits a la derecha |

### width_bucket()

Asigna un valor a uno de `count` intervalos de igual ancho en el rango `[low, high)`:

```sql
SELECT width_bucket(35, 0, 100, 10);
-- 4  (intervalo para valores 30-39)
```

Retorna 0 para valores por debajo de `low`, y `count + 1` para valores iguales o superiores a `high`.

## Funciones de fecha/hora

| Funcion | Descripcion |
|----------|-------------|
| `now()` | Timestamp actual (UTC) |
| `clock_timestamp()` | Timestamp actual (UTC) |
| `current_date()` | Fecha actual (UTC) |
| `current_time()` | Hora actual (UTC) |
| `date_part(field, source)` | Extraer campo de fecha/hora |
| `EXTRACT(field FROM source)` | Extraccion estandar SQL |
| `date_trunc(field, source)` | Truncar a precision (year/quarter/month/week/day/hour/minute/second) |
| `make_date(y, m, d)` / `make_time(h, m, s)` | Construir fecha/hora |
| `make_timestamp(y, mo, d, h, mi, s)` | Construir timestamp |
| `make_interval(days [, hours [, mins [, secs]]])` | Construir intervalo |
| `age(ts1, ts2)` / `age(ts)` | Intervalo entre timestamps |
| `to_char(value, format)` | Formatear como texto |
| `to_date(text, format)` / `to_timestamp(text, format)` | Analizar con formato |
| `to_number(text, format)` | Analizar cadena numerica |
| `isfinite(date\|timestamp)` | Siempre verdadero (sin infinitos en Java time) |

## Funciones de array

| Funcion | Descripcion |
|----------|-------------|
| `array_length(arr)` | Numero de elementos |
| `array_append(arr, val)` / `array_prepend(val, arr)` | Agregar elemento |
| `array_concat(arr1, arr2)` / `array_cat(arr1, arr2)` | Concatenar arrays |
| `array_slice(arr, start [, end])` | Segmentar array |
| `array_remove(arr, val)` | Eliminar todas las ocurrencias |
| `array_position(arr, val)` | Encontrar posicion del elemento (base 1) |
| `array_distinct(arr)` | Eliminar duplicados |
| `array_replace(arr, old, new)` | Reemplazar elementos coincidentes |
| `array_lower(arr, dim)` / `array_upper(arr, dim)` | Limites del array (base 1) |
| `array_ndims(arr)` | Numero de dimensiones (siempre 1) |
| `cardinality(arr)` | Numero de elementos |
| `string_to_array(text, delim)` | Dividir cadena en array |
| `array_to_string(arr, sep)` | Unir array en cadena |

## Funciones binarias

| Funcion | Descripcion |
|----------|-------------|
| `octet_length(bytea)` | Cantidad de bytes |
| `get_byte(bytea, offset)` | Obtener byte en desplazamiento base 0 (retorna 0-255) |
| `set_byte(bytea, offset, value)` | Establecer byte en desplazamiento, retorna nuevo bytea |
| `encode(bytea, format)` / `decode(text, format)` | Codificacion binaria (hex, base64) |

## Funciones de secuencia

| Funcion | Descripcion |
|----------|-------------|
| `nextval('name')` | Avanzar secuencia y retornar siguiente valor |
| `currval('name')` | Valor actual (requiere `nextval` previo en la sesion) |
| `setval('name', value [, is_called])` | Establecer valor de secuencia (`is_called` por defecto `true`) |
| `lastval()` | Ultimo valor retornado por cualquier secuencia en esta sesion |

```sql
CREATE SEQUENCE order_seq START WITH 100;
SELECT nextval('order_seq');   -- 100
SELECT nextval('order_seq');   -- 101
SELECT currval('order_seq');   -- 101
SELECT setval('order_seq', 200);
SELECT nextval('order_seq');   -- 201
SELECT lastval();              -- 201
```

## Otras funciones escalares

| Funcion | Descripcion |
|----------|-------------|
| `coalesce(a, b, ...)` | Primer valor no nulo |
| `nullif(a, b)` | NULL si a = b |
| `typeof(value)` | Nombre del tipo como texto |
| `gen_random_uuid()` | Generar UUID v4 |

## Funciones de agregado

| Funcion | Descripcion |
|----------|-------------|
| `COUNT(*)` / `COUNT(expr)` | Contar filas |
| `SUM(expr)` | Suma de valores |
| `AVG(expr)` | Promedio |
| `MIN(expr)` / `MAX(expr)` | Minimo/maximo |
| `string_agg(text, separator)` | Concatenar con separador |
| `array_agg(expr)` | Recopilar valores en array |
| `bool_and(expr)` / `bool_or(expr)` / `every(expr)` | AND/OR logico entre filas |
| `bit_and(expr)` | AND bit a bit entre filas |
| `bit_or(expr)` | OR bit a bit entre filas |
| `bit_xor(expr)` | XOR bit a bit entre filas |
| `variance(expr)` / `var_samp(expr)` | Varianza muestral |
| `var_pop(expr)` | Varianza poblacional |
| `stddev(expr)` / `stddev_samp(expr)` | Desviacion estandar muestral |
| `stddev_pop(expr)` | Desviacion estandar poblacional |

Todas las funciones de agregado soportan la clausula `FILTER (WHERE ...)` para restringir que filas se incluyen:

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE active) AS active_count
FROM users;
```

Ver tambien: [Funciones de agregado JSON](/reference/json/#aggregate-functions)

## Funciones de ventana

Las funciones de ventana calculan un valor para cada fila basado en un grupo de filas relacionadas, sin colapsarlas.

### Funciones de clasificacion

| Funcion | Descripcion |
|----------|-------------|
| `ROW_NUMBER()` | Numero de fila secuencial dentro de la particion |
| `RANK()` | Rango con huecos para empates |
| `DENSE_RANK()` | Rango sin huecos para empates |

### Funciones de desplazamiento

| Funcion | Descripcion |
|----------|-------------|
| `LAG(expr [, offset [, default]])` | Valor de una fila anterior (desplazamiento por defecto: 1) |
| `LEAD(expr [, offset [, default]])` | Valor de una fila posterior (desplazamiento por defecto: 1) |
| `NTILE(n)` | Dividir filas en n grupos aproximadamente iguales |

### Funciones de valor

| Funcion | Descripcion |
|----------|-------------|
| `FIRST_VALUE(expr)` | Valor de `expr` en la primera fila del marco de ventana |
| `LAST_VALUE(expr)` | Valor de `expr` en la ultima fila del marco de ventana |
| `NTH_VALUE(expr, n)` | Valor de `expr` en la fila n del marco (base 1), o NULL si no existe tal fila |

### Funciones de ventana con agregados

Cualquier funcion de agregado puede usarse como funcion de ventana con `OVER()`. Consulta [Consultas — Funciones de ventana](/reference/queries/#window-functions) para sintaxis y especificaciones de marco.

---
title: Funcoes
description: Referencia de funcoes escalares e de agregacao.
---

## Funcoes de Texto

| Funcao | Descricao |
|----------|-------------|
| `lower(text)` | Converter para minusculas |
| `upper(text)` | Converter para maiusculas |
| `initcap(text)` | Capitalizar cada palavra |
| `length(text)` / `char_length(text)` | Comprimento da string |
| `trim(text)` / `ltrim(text)` / `rtrim(text)` | Remover espacos em branco |
| `btrim(text [, chars])` | Remover caracteres de ambas as extremidades |
| `substring(text, start [, len])` | Extrair substring |
| `left(text, n)` / `right(text, n)` | Primeiros/ultimos n caracteres |
| `lpad(text, len [, pad])` / `rpad(text, len [, pad])` | Preencher string |
| `replace(text, from, to)` | Substituir ocorrencias |
| `translate(text, from, to)` | Substituicao caractere a caractere |
| `concat(a, b)` / `concat_ws(sep, ...)` | Concatenar (com separador) |
| `repeat(text, n)` | Repetir string |
| `reverse(text)` | Inverter string |
| `position(substr, text)` | Encontrar posicao da substring (base 1) |
| `split_part(text, delim, n)` | Dividir e obter enesima parte |
| `ascii(text)` / `chr(int)` | Conversao caractere/code point |
| `regexp_replace(text, pat, repl [, flags])` | Substituicao por regex (`'g'` para global) |
| `regexp_match(text, pattern)` | Primeira correspondencia regex como array |
| `regexp_split_to_array(text, pattern)` | Dividir por regex em array |
| `starts_with(text, prefix)` | Verdadeiro se texto comeca com prefixo |
| `ends_with(text, suffix)` | Verdadeiro se texto termina com sufixo |
| `format(formatstr, ...)` | Formatar string (veja abaixo) |
| `quote_ident(value)` | Citar como identificador SQL |
| `quote_literal(value)` | Citar como literal SQL |

### format()

Suporta os seguintes especificadores de formato:

| Especificador | Descricao |
|-----------|-------------|
| `%s` | Substituicao de string |
| `%I` | Identificador SQL (citado e escapado) |
| `%L` | Literal SQL (citado e escapado) |
| `%%` | Sinal de porcentagem literal |

```sql
SELECT format('Hello, %s!', 'world');
-- Hello, world!

SELECT format('SELECT %I FROM %I WHERE id = %L', 'name', 'users', '42');
-- SELECT "name" FROM "users" WHERE id = '42'
```

## Funcoes Numericas

| Funcao | Descricao |
|----------|-------------|
| `abs(x)` | Valor absoluto |
| `ceil(x)` / `floor(x)` | Arredondar para cima/baixo |
| `round(x [, digits])` / `trunc(x [, digits])` | Arredondar/truncar |
| `sign(x)` | Sinal (-1, 0, 1) |
| `mod(x, y)` | Modulo |
| `power(x, y)` / `sqrt(x)` | Potencia/raiz quadrada |
| `cbrt(x)` | Raiz cubica |
| `exp(x)` / `ln(x)` / `log10(x)` / `log(base, x)` | Exponencial/logaritmo |
| `div(x, y)` | Divisao inteira |
| `factorial(n)` | Fatorial |
| `gcd(a, b)` / `lcm(a, b)` | Maximo divisor comum / minimo multiplo comum |
| `pi()` | Constante Pi |
| `degrees(rad)` / `radians(deg)` | Conversao de angulo |
| `sin` / `cos` / `tan` / `asin` / `acos` / `atan` / `atan2` | Trigonometria |
| `sinh` / `cosh` / `tanh` / `asinh` / `acosh` / `atanh` | Trigonometria hiperbolica |
| `random()` | Numero aleatorio [0, 1) |
| `setseed(seed)` | Definir semente do gerador de numeros aleatorios |
| `width_bucket(value, low, high, count)` | Atribuir valor a um bucket (veja abaixo) |
| `greatest(a, b, ...)` / `least(a, b, ...)` | Maximo/minimo de valores |

### Operadores Bitwise

| Operador | Descricao |
|----------|-------------|
| `x % y` | Modulo |
| `x & y` | AND bitwise |
| `x \| y` | OR bitwise |
| `x # y` | XOR bitwise |
| `~x` | NOT bitwise |
| `x << n` | Deslocamento de bit a esquerda |
| `x >> n` | Deslocamento de bit a direita |

### width_bucket()

Atribui um valor a um dos `count` buckets de largura igual no intervalo `[low, high)`:

```sql
SELECT width_bucket(35, 0, 100, 10);
-- 4  (bucket para valores 30-39)
```

Retorna 0 para valores abaixo de `low` e `count + 1` para valores em ou acima de `high`.

## Funcoes de Data/Hora

| Funcao | Descricao |
|----------|-------------|
| `now()` | Timestamp atual (UTC) |
| `clock_timestamp()` | Timestamp atual (UTC) |
| `current_date()` | Data atual (UTC) |
| `current_time()` | Hora atual (UTC) |
| `date_part(field, source)` | Extrair campo de data/hora |
| `EXTRACT(field FROM source)` | Extracao padrao SQL |
| `date_trunc(field, source)` | Truncar para precisao (year/quarter/month/week/day/hour/minute/second) |
| `make_date(y, m, d)` / `make_time(h, m, s)` | Construir data/hora |
| `make_timestamp(y, mo, d, h, mi, s)` | Construir timestamp |
| `make_interval(days [, hours [, mins [, secs]]])` | Construir intervalo |
| `age(ts1, ts2)` / `age(ts)` | Intervalo entre timestamps |
| `to_char(value, format)` | Formatar como texto |
| `to_date(text, format)` / `to_timestamp(text, format)` | Analisar com formato |
| `to_number(text, format)` | Analisar string numerica |
| `isfinite(date\|timestamp)` | Sempre verdadeiro (sem infinitos no Java time) |

## Funcoes de Array

| Funcao | Descricao |
|----------|-------------|
| `array_length(arr)` | Numero de elementos |
| `array_append(arr, val)` / `array_prepend(val, arr)` | Adicionar elemento |
| `array_concat(arr1, arr2)` / `array_cat(arr1, arr2)` | Concatenar arrays |
| `array_slice(arr, start [, end])` | Fatiar array |
| `array_remove(arr, val)` | Remover todas as ocorrencias |
| `array_position(arr, val)` | Encontrar posicao do elemento (base 1) |
| `array_distinct(arr)` | Remover duplicatas |
| `array_replace(arr, old, new)` | Substituir elementos correspondentes |
| `array_lower(arr, dim)` / `array_upper(arr, dim)` | Limites do array (base 1) |
| `array_ndims(arr)` | Numero de dimensoes (sempre 1) |
| `cardinality(arr)` | Numero de elementos |
| `string_to_array(text, delim)` | Dividir string em array |
| `array_to_string(arr, sep)` | Juntar array em string |

## Funcoes Binarias

| Funcao | Descricao |
|----------|-------------|
| `octet_length(bytea)` | Contagem de bytes |
| `get_byte(bytea, offset)` | Obter byte no offset base 0 (retorna 0-255) |
| `set_byte(bytea, offset, value)` | Definir byte no offset, retorna novo bytea |
| `encode(bytea, format)` / `decode(text, format)` | Codificacao binaria (hex, base64) |

## Funcoes de Sequencia

| Funcao | Descricao |
|----------|-------------|
| `nextval('name')` | Avancar sequencia e retornar proximo valor |
| `currval('name')` | Valor atual (requer `nextval` anterior na sessao) |
| `setval('name', value [, is_called])` | Definir valor da sequencia (`is_called` padrao `true`) |
| `lastval()` | Ultimo valor retornado por qualquer sequencia nesta sessao |

```sql
CREATE SEQUENCE order_seq START WITH 100;
SELECT nextval('order_seq');   -- 100
SELECT nextval('order_seq');   -- 101
SELECT currval('order_seq');   -- 101
SELECT setval('order_seq', 200);
SELECT nextval('order_seq');   -- 201
SELECT lastval();              -- 201
```

## Outras Funcoes Escalares

| Funcao | Descricao |
|----------|-------------|
| `coalesce(a, b, ...)` | Primeiro valor nao nulo |
| `nullif(a, b)` | NULL se a = b |
| `typeof(value)` | Nome do tipo como texto |
| `gen_random_uuid()` | Gerar UUID v4 |

## Funcoes de Agregacao

| Funcao | Descricao |
|----------|-------------|
| `COUNT(*)` / `COUNT(expr)` | Contar linhas |
| `SUM(expr)` | Soma dos valores |
| `AVG(expr)` | Media |
| `MIN(expr)` / `MAX(expr)` | Minimo/maximo |
| `string_agg(text, separator)` | Concatenar com separador |
| `array_agg(expr)` | Coletar valores em array |
| `bool_and(expr)` / `bool_or(expr)` / `every(expr)` | AND/OR logico entre linhas |
| `bit_and(expr)` | AND bitwise entre linhas |
| `bit_or(expr)` | OR bitwise entre linhas |
| `bit_xor(expr)` | XOR bitwise entre linhas |
| `variance(expr)` / `var_samp(expr)` | Variancia amostral |
| `var_pop(expr)` | Variancia populacional |
| `stddev(expr)` / `stddev_samp(expr)` | Desvio padrao amostral |
| `stddev_pop(expr)` | Desvio padrao populacional |

Todas as funcoes de agregacao suportam a clausula `FILTER (WHERE ...)` para restringir quais linhas sao incluidas:

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE active) AS active_count
FROM users;
```

Veja tambem: [Funcoes de agregacao JSON](/reference/json/#funcoes-de-agregacao)

## Funcoes de Janela

Funcoes de janela calculam um valor para cada linha com base em um grupo de linhas relacionadas, sem colapsalas.

### Funcoes de classificacao

| Funcao | Descricao |
|----------|-------------|
| `ROW_NUMBER()` | Numero sequencial da linha dentro da particao |
| `RANK()` | Classificacao com lacunas para empates |
| `DENSE_RANK()` | Classificacao sem lacunas para empates |

### Funcoes de offset

| Funcao | Descricao |
|----------|-------------|
| `LAG(expr [, offset [, default]])` | Valor de uma linha anterior (offset padrao: 1) |
| `LEAD(expr [, offset [, default]])` | Valor de uma linha seguinte (offset padrao: 1) |
| `NTILE(n)` | Dividir linhas em n grupos aproximadamente iguais |

### Funcoes de valor

| Funcao | Descricao |
|----------|-------------|
| `FIRST_VALUE(expr)` | Valor de `expr` na primeira linha do quadro da janela |
| `LAST_VALUE(expr)` | Valor de `expr` na ultima linha do quadro da janela |
| `NTH_VALUE(expr, n)` | Valor de `expr` na enesima linha do quadro (base 1), ou NULL se nao existir |

### Funcoes de janela agregadas

Qualquer funcao de agregacao pode ser usada como funcao de janela com `OVER()`. Veja [Consultas — Funcoes de Janela](/reference/queries/#funcoes-de-janela) para sintaxe e especificacoes de quadro.

---
title: Tipos de Dados
description: Tipos de dados SQL suportados pelo PetraDB.
---

O PetraDB segue as convencoes do PostgreSQL para sintaxe SQL, tratamento de identificadores e conversao de tipos.

## Compatibilidade SQL

- **Palavras-chave insensíveis a maiusculas** — `SELECT`, `select` e `Select` sao equivalentes
- **Conversao de identificadores nao cotados** — identificadores nao cotados sao convertidos para minusculas (`CREATE TABLE Users` -> nome da tabela `users`)
- **Identificadores com aspas duplas** — preservam maiusculas/minusculas (`"MixedCase"` permanece como esta)
- **Escape de strings** — aspas simples duplicadas (`'it''s'`) e E-strings (`E'it\'s'`)
- **Operadores** — tanto `!=` quanto `<>` para diferente

## Tipos Suportados

| Tipo | Descricao |
|------|-------------|
| `SMALLINT` | Inteiro de 16 bits (-32768 a 32767) |
| `INT` / `INTEGER` | Inteiro de 32 bits |
| `BIGINT` | Inteiro de 64 bits |
| `SMALLSERIAL` | Inteiro de 16 bits auto-incremento (cria sequencia auxiliar) |
| `SERIAL` | Inteiro de 32 bits auto-incremento (cria sequencia auxiliar) |
| `BIGSERIAL` | Inteiro de 64 bits auto-incremento (cria sequencia auxiliar) |
| `DOUBLE` / `FLOAT` / `REAL` | Ponto flutuante de dupla precisao |
| `NUMERIC(p,s)` / `DECIMAL(p,s)` | Decimal de precisao fixa |
| `TEXT` | String de comprimento variavel |
| `CHAR(n)` | String de comprimento fixo (preenchida com espacos a direita) |
| `VARCHAR(n)` | String de comprimento variavel (maximo n caracteres, sem preenchimento) |
| `BOOLEAN` | Verdadeiro/falso |
| `DATE` | Data do calendario (`yyyy-MM-dd`) |
| `TIME` | Hora do dia (`HH:mm:ss`) |
| `TIMESTAMP` | Data e hora |
| `TIMESTAMP WITH TIME ZONE` | Data e hora com deslocamento de fuso horario |
| `INTERVAL` | Duracao (ISO 8601 ou `N days N hours N minutes N seconds`) |
| `UUID` | Identificador unico universal |
| `JSON` / `JSONB` | Objetos e arrays JSON estruturados |
| `BYTEA` | Dados binarios |
| `ENUM` | Tipos enumerados personalizados (via `CREATE TYPE ... AS ENUM`) |
| `INT[]`, `TEXT[]`, etc. | Arrays tipados (qualquer tipo base com sufixo `[]`) |

## Conversao de Tipos

Use o operador `::` ou `CAST(expr AS type)` para converter entre tipos:

```sql
SELECT '2024-06-15'::DATE;
SELECT CAST('14:30:00' AS TIME);
SELECT '2 hours 30 minutes'::INTERVAL;
SELECT CAST(val AS TEXT);
SELECT '42'::INT;
SELECT 1::BOOLEAN;
SELECT EXTRACT(year FROM created_at);
```

## Aritmetica de Data/Hora

```sql
SELECT '2024-01-01'::DATE + 10;                        -- adicionar dias
SELECT '2024-01-15'::DATE - '2024-01-10'::DATE;        -- dias entre
SELECT now() + '2 hours'::INTERVAL;                     -- timestamp + intervalo
SELECT now() - '30 minutes'::INTERVAL;                  -- timestamp - intervalo
SELECT '1 hour'::INTERVAL * 3;                          -- escalar intervalo
SELECT EXTRACT(year FROM now());                        -- extrair campo
SELECT date_trunc('month', now());                      -- truncar
```

## Restricoes

```sql
PRIMARY KEY (id)
UNIQUE (email)
NOT NULL
DEFAULT value
FOREIGN KEY (col) REFERENCES other_table (col) ON DELETE CASCADE ON UPDATE CASCADE
```

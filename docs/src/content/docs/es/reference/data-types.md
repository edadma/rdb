---
title: Tipos de datos
description: Tipos de datos SQL soportados por PetraDB.
---

PetraDB sigue las convenciones de PostgreSQL para sintaxis SQL, manejo de identificadores y conversion de tipos.

## Compatibilidad SQL

- **Palabras clave insensibles a mayusculas** — `SELECT`, `select` y `Select` son equivalentes
- **Plegado de identificadores sin comillas** — los identificadores sin comillas se pliegan a minusculas (`CREATE TABLE Users` → nombre de tabla `users`)
- **Identificadores con comillas dobles** — preservan las mayusculas (`"MixedCase"` se mantiene tal cual)
- **Escape de cadenas** — comillas simples duplicadas (`'it''s'`) y E-strings (`E'it\'s'`)
- **Operadores** — tanto `!=` como `<>` para no-igual

## Tipos soportados

| Tipo | Descripcion |
|------|-------------|
| `SMALLINT` | Entero de 16 bits (-32768 a 32767) |
| `INT` / `INTEGER` | Entero de 32 bits |
| `BIGINT` | Entero de 64 bits |
| `SMALLSERIAL` | Entero de 16 bits auto-incrementable (crea secuencia de respaldo) |
| `SERIAL` | Entero de 32 bits auto-incrementable (crea secuencia de respaldo) |
| `BIGSERIAL` | Entero de 64 bits auto-incrementable (crea secuencia de respaldo) |
| `DOUBLE` / `FLOAT` / `REAL` | Punto flotante de doble precision |
| `NUMERIC(p,s)` / `DECIMAL(p,s)` | Decimal de precision fija |
| `TEXT` | Cadena de longitud variable |
| `CHAR(n)` | Cadena de longitud fija (rellenada con espacios a la derecha) |
| `VARCHAR(n)` | Cadena de longitud variable (maximo n caracteres, sin relleno) |
| `BOOLEAN` | Verdadero/falso |
| `DATE` | Fecha de calendario (`yyyy-MM-dd`) |
| `TIME` | Hora del dia (`HH:mm:ss`) |
| `TIMESTAMP` | Fecha y hora |
| `TIMESTAMP WITH TIME ZONE` | Fecha y hora con desplazamiento de zona horaria |
| `INTERVAL` | Duracion (ISO 8601 o `N days N hours N minutes N seconds`) |
| `UUID` | Identificador unico universal |
| `JSON` / `JSONB` | Objetos y arrays JSON estructurados |
| `BYTEA` | Datos binarios |
| `ENUM` | Tipos enumerados personalizados (via `CREATE TYPE ... AS ENUM`) |
| `INT[]`, `TEXT[]`, etc. | Arrays tipados (cualquier tipo base con sufijo `[]`) |

## Conversion de tipos

Usa el operador `::` o `CAST(expr AS type)` para convertir entre tipos:

```sql
SELECT '2024-06-15'::DATE;
SELECT CAST('14:30:00' AS TIME);
SELECT '2 hours 30 minutes'::INTERVAL;
SELECT CAST(val AS TEXT);
SELECT '42'::INT;
SELECT 1::BOOLEAN;
SELECT EXTRACT(year FROM created_at);
```

## Aritmetica de fecha/hora

```sql
SELECT '2024-01-01'::DATE + 10;                        -- sumar dias
SELECT '2024-01-15'::DATE - '2024-01-10'::DATE;        -- dias entre
SELECT now() + '2 hours'::INTERVAL;                     -- timestamp + intervalo
SELECT now() - '30 minutes'::INTERVAL;                  -- timestamp - intervalo
SELECT '1 hour'::INTERVAL * 3;                          -- escalar intervalo
SELECT EXTRACT(year FROM now());                        -- extraer campo
SELECT date_trunc('month', now());                      -- truncar
```

## Restricciones

```sql
PRIMARY KEY (id)
UNIQUE (email)
NOT NULL
DEFAULT value
FOREIGN KEY (col) REFERENCES other_table (col) ON DELETE CASCADE ON UPDATE CASCADE
```

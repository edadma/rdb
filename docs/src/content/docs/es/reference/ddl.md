---
title: DDL
description: Lenguaje de definicion de datos — sentencias CREATE, ALTER, DROP y TRUNCATE.
---

## Esquemas

PetraDB soporta espacios de nombres de esquema al estilo PostgreSQL. Cada base de datos tiene un esquema `public` por defecto. Los nombres de tabla no calificados se resuelven a `public`.

### CREATE SCHEMA

```sql
CREATE SCHEMA inventory;
CREATE SCHEMA IF NOT EXISTS inventory;
```

### Tablas calificadas con esquema

Usa la sintaxis `schema.tabla` en cualquier sentencia DDL o DML:

```sql
CREATE TABLE inventory.products (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  price NUMERIC(10,2)
);

INSERT INTO inventory.products (name, price) VALUES ('Widget', 9.99);
SELECT * FROM inventory.products;
```

Las tablas con el mismo nombre pueden existir en diferentes esquemas:

```sql
CREATE SCHEMA staging;
CREATE TABLE staging.products (id SERIAL, name TEXT);
CREATE TABLE public.products (id SERIAL, name TEXT);
-- Son tablas separadas
```

### information_schema

PetraDB proporciona tablas virtuales `information_schema` para introspeccion de la estructura de la base de datos:

```sql
SELECT * FROM information_schema.schemata;
SELECT * FROM information_schema.tables;
SELECT * FROM information_schema.columns WHERE table_name = 'products';
```

Vistas disponibles: `schemata`, `tables`, `columns`.

## Tablas

### CREATE TABLE

```sql
CREATE TABLE orders (
  id UUID DEFAULT gen_random_uuid(),
  customer_name TEXT NOT NULL,
  amount NUMERIC(10,2),
  status order_status,
  tags INT[],
  metadata JSON,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS orders (...);
```

### Claves foraneas

```sql
CREATE TABLE line_items (
  id SERIAL,
  order_id UUID REFERENCES orders (id) ON DELETE CASCADE ON UPDATE CASCADE,
  product TEXT NOT NULL
);
```

### Columnas generadas

Columnas calculadas que se derivan automaticamente de otras columnas:

```sql
CREATE TABLE products (
  price NUMERIC,
  tax_rate NUMERIC DEFAULT 0.08,
  total NUMERIC GENERATED ALWAYS AS (price * (1 + tax_rate)) STORED
);

INSERT INTO products (price) VALUES (100);
SELECT * FROM products;
-- price: 100, tax_rate: 0.08, total: 108
```

Las columnas generadas se recalculan en INSERT y UPDATE. No se pueden escribir directamente.

### ALTER TABLE

Agregar, eliminar o renombrar columnas:

```sql
ALTER TABLE orders ADD COLUMN notes TEXT;
ALTER TABLE orders DROP COLUMN notes;
ALTER TABLE orders RENAME COLUMN amount TO total;
ALTER TABLE orders RENAME TO purchases;
```

Modificar propiedades de columna:

```sql
ALTER TABLE orders ALTER COLUMN notes SET NOT NULL;
ALTER TABLE orders ALTER COLUMN notes DROP NOT NULL;
ALTER TABLE orders ALTER COLUMN notes SET DEFAULT 'none';
ALTER TABLE orders ALTER COLUMN notes DROP DEFAULT;
ALTER TABLE orders ALTER COLUMN amount SET DATA TYPE NUMERIC(10,2);
```

Agregar y eliminar restricciones:

```sql
ALTER TABLE orders ADD CONSTRAINT chk_amount CHECK (amount > 0);
ALTER TABLE orders ADD CONSTRAINT uq_email UNIQUE (email);
ALTER TABLE orders ADD CONSTRAINT fk_customer
  FOREIGN KEY (customer_id) REFERENCES customers (id) ON DELETE CASCADE;
ALTER TABLE orders DROP CONSTRAINT chk_amount;
```

### Restricciones CHECK

Aplicar condiciones sobre valores de columna:

```sql
CREATE TABLE products (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  price NUMERIC CHECK (price >= 0),
  quantity INT,
  CONSTRAINT positive_qty CHECK (quantity >= 0)
);
```

Las restricciones CHECK se aplican en INSERT y UPDATE.

## Triggers

Consulta [PL/pgSQL — Triggers](/reference/plpgsql/#triggers) para documentacion completa.

```sql
CREATE TRIGGER trg_audit AFTER INSERT ON orders
  FOR EACH ROW EXECUTE FUNCTION audit_log();

DROP TRIGGER trg_audit ON orders;
DROP TRIGGER IF EXISTS trg_audit ON orders;
```

### TRUNCATE TABLE

Elimina todas las filas y reinicia las secuencias serial:

```sql
TRUNCATE TABLE orders;
```

### DROP TABLE

```sql
DROP TABLE orders;
DROP TABLE IF EXISTS orders;
```

## Vistas

### CREATE VIEW

Crea una vista nombrada respaldada por una consulta. Usa `OR REPLACE` para sobreescribir una vista existente:

```sql
CREATE VIEW active_orders AS
  SELECT * FROM orders WHERE status = 'pending';

CREATE OR REPLACE VIEW active_orders AS
  SELECT * FROM orders WHERE status != 'delivered';
```

### DROP VIEW

```sql
DROP VIEW active_orders;
DROP VIEW IF EXISTS active_orders;
```

## Tipos personalizados

### CREATE TYPE

Define un tipo enumerado:

```sql
CREATE TYPE order_status AS ENUM ('pending', 'shipped', 'delivered');
```

### DROP TYPE

```sql
DROP TYPE order_status CASCADE;
```

## Secuencias

Las secuencias son contadores con nombre que generan valores numericos secuenciales. Se usan comunmente para la generacion de claves primarias.

### CREATE SEQUENCE

```sql
CREATE SEQUENCE order_seq;
CREATE SEQUENCE order_seq START WITH 100 INCREMENT BY 10;
CREATE SEQUENCE IF NOT EXISTS order_seq;
```

Opciones:

| Opcion | Por defecto | Descripcion |
|--------|---------|-------------|
| `INCREMENT BY n` | 1 | Tamano del paso |
| `START WITH n` | 1 | Valor inicial |
| `MINVALUE n` / `NO MINVALUE` | 1 | Valor minimo |
| `MAXVALUE n` / `NO MAXVALUE` | 4611686018427387903 | Valor maximo |
| `CYCLE` / `NO CYCLE` | `NO CYCLE` | Si debe reiniciar al alcanzar los limites |

### DROP SEQUENCE

```sql
DROP SEQUENCE order_seq;
DROP SEQUENCE IF EXISTS order_seq;
```

### SERIAL y secuencias

Las columnas `SERIAL`, `SMALLSERIAL` y `BIGSERIAL` crean automaticamente una secuencia de respaldo nombrada `<tabla>_<columna>_seq`. Esto coincide con el comportamiento de PostgreSQL:

```sql
CREATE TABLE orders (id SERIAL PRIMARY KEY, name TEXT);
-- Crea implicitamente la secuencia "orders_id_seq"

SELECT nextval('orders_id_seq');   -- funciona
SELECT currval('orders_id_seq');   -- funciona despues de INSERT o nextval
```

Eliminar la tabla propaga la eliminacion a sus secuencias asociadas. `TRUNCATE` reinicia las secuencias de respaldo a sus valores iniciales.

### Funciones de secuencia

| Funcion | Descripcion |
|----------|-------------|
| `nextval('seq_name')` | Avanzar y retornar el siguiente valor |
| `currval('seq_name')` | Retornar el valor actual (debe llamar nextval primero en la sesion) |
| `setval('seq_name', value)` | Establecer valor actual; el siguiente nextval retorna value + incremento |
| `setval('seq_name', value, false)` | Establecer valor actual; el siguiente nextval retorna value |
| `lastval()` | Retornar el ultimo valor de cualquier secuencia en esta sesion |

## Funciones y procedimientos almacenados

Consulta [PL/pgSQL](/reference/plpgsql/) para documentacion completa.

```sql
CREATE FUNCTION double(x INT) RETURNS INT AS $$
BEGIN RETURN x * 2; END $$ LANGUAGE plpgsql;

CREATE PROCEDURE reset_counts() AS $$
BEGIN UPDATE counters SET val = 0; END $$ LANGUAGE plpgsql;

DROP FUNCTION double;
DROP PROCEDURE IF EXISTS reset_counts;
```

## Indices

```sql
CREATE INDEX idx_orders_status ON orders (status);
CREATE UNIQUE INDEX idx_orders_email ON orders (email);
DROP INDEX idx_orders_status;
```

### Indices parciales

Indexar solo filas que coincidan con una condicion, haciendo el indice mas pequeno y rapido:

```sql
CREATE INDEX idx_active_orders ON orders (customer_id) WHERE status = 'active';
CREATE UNIQUE INDEX idx_unique_active_email ON users (email) WHERE active = true;
```

El planificador de consultas usa un indice parcial solo cuando la clausula `WHERE` de la consulta incluye la condicion del indice.

### Indices de expresion

Indexar sobre valores calculados en lugar de columnas sin procesar:

```sql
CREATE INDEX idx_lower_email ON users ((lower(email)));
CREATE UNIQUE INDEX idx_lower_name ON users ((lower(name)));
```

La expresion debe estar encerrada entre parentesis. El planificador asocia automaticamente `WHERE lower(email) = 'alice@test.com'` con el indice.

Los indices parciales y de expresion se pueden combinar:

```sql
CREATE INDEX idx_active_lower ON users ((lower(name))) WHERE status = 'active';
```

## Comandos SHOW

Inspeccionar metadatos de la base de datos:

```sql
SHOW TABLES;
SHOW VIEWS;
SHOW SEQUENCES;
SHOW COLUMNS orders;
SHOW PRIMARY KEY orders;
SHOW FOREIGN KEYS orders;
SHOW INDEXES orders;
SHOW INDEXES;              -- todos los indices de todas las tablas
```

### Salida de SHOW VIEWS

| Columna | Tipo | Descripcion |
|--------|------|-------------|
| `view_name` | TEXT | Nombre de la vista |
| `definition` | TEXT | La consulta SQL que define la vista |

### Salida de SHOW COLUMNS

| Columna | Tipo | Descripcion |
|--------|------|-------------|
| `name` | TEXT | Nombre de columna |
| `type` | TEXT | Tipo de datos |
| `required` | BOOLEAN | Restriccion NOT NULL |
| `indexed` | BOOLEAN | Tiene un indice |
| `unique` | BOOLEAN | Tiene restriccion de unicidad |
| `fk_table` | TEXT | Tabla destino de clave foranea |
| `fk_column` | TEXT | Columna destino de clave foranea |
| `fk_on_delete` | TEXT | Accion ON DELETE |
| `fk_on_update` | TEXT | Accion ON UPDATE |
| `default_value` | TEXT | Expresion por defecto |

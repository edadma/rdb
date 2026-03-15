---
title: DML
description: Lenguaje de manipulacion de datos — sentencias INSERT, UPDATE, DELETE y COPY.
---

## INSERT

```sql
INSERT INTO orders (customer_name, amount)
VALUES ('Alice Smith', 149.99);
```

### RETURNING

Retornar valores de la fila insertada:

```sql
INSERT INTO orders (customer_name, amount)
VALUES ('Bob Johnson', 75.50)
RETURNING id;

INSERT INTO orders (customer_name, amount)
VALUES ('Carol', 200.00)
RETURNING *;
```

### Insertar desde una consulta

```sql
INSERT INTO archive (customer_name, amount)
SELECT customer_name, amount FROM orders WHERE status = 'delivered';
```

### INSERT ... ON CONFLICT (Upsert)

Omitir filas en conflicto:

```sql
INSERT INTO users (email, name)
VALUES ('alice@example.com', 'Alice')
ON CONFLICT DO NOTHING;
```

Actualizar en caso de conflicto con columnas especificas:

```sql
INSERT INTO users (email, name)
VALUES ('alice@example.com', 'Alice Updated')
ON CONFLICT (email) DO UPDATE SET name = 'Alice Updated';
```

Se puede combinar con RETURNING:

```sql
INSERT INTO users (email, name)
VALUES ('alice@example.com', 'Alice')
ON CONFLICT (email) DO UPDATE SET name = 'Alice'
RETURNING *;
```

## UPDATE

```sql
UPDATE orders SET status = 'shipped' WHERE amount > 100;
```

### Actualizacion masiva desde VALUES

`UPDATE ... FROM` al estilo PostgreSQL:

```sql
UPDATE orders
  SET status = d.status
  FROM (VALUES ('ord-1', 'shipped'), ('ord-2', 'delivered'))
       AS d (id, status)
  WHERE orders.id = d.id;
```

### UPDATE con RETURNING

```sql
UPDATE orders SET status = 'shipped'
WHERE id = 42
RETURNING id, status;
```

## DELETE

```sql
DELETE FROM orders WHERE status = 'delivered';
```

### DELETE ... USING

Unir otras tablas para determinar que filas eliminar:

```sql
DELETE FROM orders
USING customers
WHERE orders.customer_id = customers.id
  AND customers.status = 'inactive';
```

Multiples tablas USING:

```sql
DELETE FROM order_items
USING products, categories
WHERE order_items.product_id = products.id
  AND products.category_id = categories.id
  AND categories.name = 'discontinued';
```

### DELETE con RETURNING

```sql
DELETE FROM orders WHERE status = 'delivered'
RETURNING *;
```

## COPY

Importacion y exportacion masiva de datos en formato CSV.

### COPY FROM

Importar filas desde un archivo CSV:

```sql
COPY orders FROM 'data/orders.csv';
COPY orders FROM 'data/orders.csv' WITH (HEADER);
COPY orders FROM 'data/orders.csv' WITH (HEADER, DELIMITER '|');
COPY orders (customer_name, amount) FROM 'data/partial.csv' WITH (HEADER);
```

### COPY TO

Exportar una tabla o consulta a un archivo CSV:

```sql
COPY orders TO 'export/orders.csv';
COPY orders TO 'export/orders.csv' WITH (HEADER);
COPY (SELECT * FROM orders WHERE status = 'pending') TO 'export/pending.csv' WITH (HEADER);
```

### Opciones

| Opcion | Descripcion |
|--------|-------------|
| `HEADER` | La primera fila es un encabezado (se omite en importacion, se escribe en exportacion) |
| `DELIMITER 'c'` | Caracter separador de campos (por defecto: `,`) |

## csv_file() — Consultar archivos CSV directamente

Consultar un archivo CSV como tabla virtual sin importar:

```sql
SELECT * FROM csv_file('data/sales.csv');
SELECT name, age::int FROM csv_file('data/people.csv') WHERE age::int > 25;
```

Todos los valores se retornan como `TEXT` — usa `::tipo` para convertir. Soporta `WHERE`, `ORDER BY`, `LIMIT`, `JOIN` y agregados.

Opciones:

```sql
csv_file('path')                        -- con encabezado (por defecto)
csv_file('path', false)                 -- sin encabezado (columnas nombradas column1, column2, ...)
csv_file('path', true, '|')            -- delimitador personalizado
```

Unir archivos CSV entre si o con tablas de la base de datos:

```sql
SELECT e.name, d.department
FROM csv_file('employees.csv') e
JOIN csv_file('departments.csv') d ON e.dept_id = d.id;
```

## Tablas virtuales

Registrar fuentes de datos externas como tablas consultables usando `CREATE VIRTUAL TABLE`:

```sql
CREATE VIRTUAL TABLE sales USING csv('data/sales.csv');
CREATE VIRTUAL TABLE sales USING csv('data/sales.csv', 'no_header', '|');

SELECT * FROM sales WHERE amount::int > 100;
DROP TABLE sales;
```

Las tablas virtuales aparecen en `SHOW TABLES` y soportan `SELECT`, `WHERE`, `JOIN`, `ORDER BY` y agregados. Son de solo lectura — `INSERT`, `UPDATE` y `DELETE` no estan soportados.

El modulo `csv` integrado esta registrado por defecto. Los modulos personalizados se pueden registrar via la API de Scala:

```scala
db.registerVirtualTableModule("mymodule", new VirtualTableModule { ... })
```

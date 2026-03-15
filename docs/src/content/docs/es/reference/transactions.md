---
title: Transacciones
description: Transacciones y sentencias preparadas.
---

## Transacciones

Envuelve multiples sentencias en una transaccion para asegurar atomicidad:

```sql
BEGIN;
INSERT INTO accounts (name, balance) VALUES ('Alice', 1000);
UPDATE accounts SET balance = balance - 100 WHERE name = 'Alice';
COMMIT;
```

Revertir en caso de error:

```sql
BEGIN;
UPDATE accounts SET balance = balance - 9999 WHERE name = 'Alice';
ROLLBACK;
```

### DDL en transacciones

Las sentencias DDL (CREATE TABLE, CREATE INDEX, DROP TABLE, etc.) son completamente soportadas dentro de transacciones y se revierten atomicamente con cualquier DML en ROLLBACK. DDL y DML pueden intercalarse libremente dentro de una sola transaccion.

```sql
BEGIN;
CREATE TABLE orders (id SERIAL, product TEXT, qty INTEGER);
INSERT INTO orders (product, qty) VALUES ('Widget', 10);
CREATE INDEX idx_orders_product ON orders (product);
COMMIT;  -- tabla, datos e indice se confirman atomicamente
```

Si la transaccion se revierte, la tabla, sus datos y el indice se deshacen completamente.

## Sentencias preparadas

Crear sentencias parametrizadas con marcadores `$1`, `$2`, ...:

```sql
PREPARE get_user AS SELECT * FROM users WHERE id = $1;
EXECUTE get_user(42);
DEALLOCATE get_user;
```

Inserts parametrizados:

```sql
PREPARE add_user AS INSERT INTO users (name, email) VALUES ($1, $2);
EXECUTE add_user('Alice', 'alice@example.com');
```

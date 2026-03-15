---
title: Transactions
description: Transactions et prepared statements.
---

## Transactions

Encapsulez plusieurs instructions dans une transaction pour garantir l'atomicité :

```sql
BEGIN;
INSERT INTO accounts (name, balance) VALUES ('Alice', 1000);
UPDATE accounts SET balance = balance - 100 WHERE name = 'Alice';
COMMIT;
```

Annuler en cas d'erreur :

```sql
BEGIN;
UPDATE accounts SET balance = balance - 9999 WHERE name = 'Alice';
ROLLBACK;
```

### DDL dans les transactions

Les instructions DDL (CREATE TABLE, CREATE INDEX, DROP TABLE, etc.) sont entièrement supportées dans les transactions et sont annulées de manière atomique avec tout DML lors du ROLLBACK. Le DDL et le DML peuvent être librement entrelacés au sein d'une même transaction.

```sql
BEGIN;
CREATE TABLE orders (id SERIAL, product TEXT, qty INTEGER);
INSERT INTO orders (product, qty) VALUES ('Widget', 10);
CREATE INDEX idx_orders_product ON orders (product);
COMMIT;  -- la table, les données et l'index sont tous valides de manière atomique
```

Si la transaction est annulée, la table, ses données et l'index sont tous défaits.

## Prepared statements

Créez des instructions paramétrées avec les placeholders `$1`, `$2`, ... :

```sql
PREPARE get_user AS SELECT * FROM users WHERE id = $1;
EXECUTE get_user(42);
DEALLOCATE get_user;
```

Insertions paramétrées :

```sql
PREPARE add_user AS INSERT INTO users (name, email) VALUES ($1, $2);
EXECUTE add_user('Alice', 'alice@example.com');
```

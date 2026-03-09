---
title: Transactions
description: Transactions and prepared statements.
---

## Transactions

Wrap multiple statements in a transaction to ensure atomicity:

```sql
BEGIN;
INSERT INTO accounts (name, balance) VALUES ('Alice', 1000);
UPDATE accounts SET balance = balance - 100 WHERE name = 'Alice';
COMMIT;
```

Roll back on error:

```sql
BEGIN;
UPDATE accounts SET balance = balance - 9999 WHERE name = 'Alice';
ROLLBACK;
```

### DDL in transactions

DDL statements (CREATE TABLE, CREATE INDEX, DROP TABLE, etc.) are fully supported inside transactions and are rolled back atomically with any DML on ROLLBACK. DDL and DML can be freely interleaved within a single transaction.

```sql
BEGIN;
CREATE TABLE orders (id SERIAL, product TEXT, qty INTEGER);
INSERT INTO orders (product, qty) VALUES ('Widget', 10);
CREATE INDEX idx_orders_product ON orders (product);
COMMIT;  -- table, data, and index all committed atomically
```

If the transaction is rolled back, the table, its data, and the index are all undone.

## Prepared Statements

Create parameterized statements with `$1`, `$2`, ... placeholders:

```sql
PREPARE get_user AS SELECT * FROM users WHERE id = $1;
EXECUTE get_user(42);
DEALLOCATE get_user;
```

Parameterized inserts:

```sql
PREPARE add_user AS INSERT INTO users (name, email) VALUES ($1, $2);
EXECUTE add_user('Alice', 'alice@example.com');
```

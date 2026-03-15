---
title: Transacoes
description: Transacoes e prepared statements.
---

## Transacoes

Encapsule multiplos comandos em uma transacao para garantir atomicidade:

```sql
BEGIN;
INSERT INTO accounts (name, balance) VALUES ('Alice', 1000);
UPDATE accounts SET balance = balance - 100 WHERE name = 'Alice';
COMMIT;
```

Reverter em caso de erro:

```sql
BEGIN;
UPDATE accounts SET balance = balance - 9999 WHERE name = 'Alice';
ROLLBACK;
```

### DDL em transacoes

Comandos DDL (CREATE TABLE, CREATE INDEX, DROP TABLE, etc.) sao totalmente suportados dentro de transacoes e sao revertidos atomicamente com qualquer DML no ROLLBACK. DDL e DML podem ser intercalados livremente dentro de uma unica transacao.

```sql
BEGIN;
CREATE TABLE orders (id SERIAL, product TEXT, qty INTEGER);
INSERT INTO orders (product, qty) VALUES ('Widget', 10);
CREATE INDEX idx_orders_product ON orders (product);
COMMIT;  -- tabela, dados e indice sao todos commitados atomicamente
```

Se a transacao sofrer rollback, a tabela, seus dados e o indice sao todos desfeitos.

## Prepared Statements

Crie comandos parametrizados com placeholders `$1`, `$2`, ...:

```sql
PREPARE get_user AS SELECT * FROM users WHERE id = $1;
EXECUTE get_user(42);
DEALLOCATE get_user;
```

Insercoes parametrizadas:

```sql
PREPARE add_user AS INSERT INTO users (name, email) VALUES ($1, $2);
EXECUTE add_user('Alice', 'alice@example.com');
```

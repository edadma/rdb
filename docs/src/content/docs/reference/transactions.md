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

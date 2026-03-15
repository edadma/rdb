---
title: PL/pgSQL
description: Procedural language — DO blocks, stored functions, and stored procedures.
---

PetraDB supports PL/pgSQL, PostgreSQL's procedural language, for writing control flow logic, loops, and conditional SQL execution.

## DO Blocks

Anonymous blocks that execute immediately without being stored:

```sql
DO $$
DECLARE
  i INT;
  total INT := 0;
BEGIN
  FOR i IN 1..10 LOOP
    total := total + i;
  END LOOP;
  INSERT INTO results (sum) VALUES (total);
END $$;
```

## Stored Functions

Functions return a value and can be called in any SQL expression:

```sql
CREATE FUNCTION factorial(n INT) RETURNS INT AS $$
DECLARE
  result INT := 1;
  i INT := 1;
BEGIN
  WHILE i <= n LOOP
    result := result * i;
    i := i + 1;
  END LOOP;
  RETURN result;
END $$ LANGUAGE plpgsql;

SELECT factorial(5);  -- 120
```

Use `CREATE OR REPLACE FUNCTION` to overwrite an existing function.

### Using Functions in Queries

Functions work anywhere expressions are allowed:

```sql
SELECT name, classify(score) AS grade FROM students;
SELECT * FROM orders WHERE is_valid(status);
INSERT INTO logs VALUES (format_msg(code, detail));
```

## Stored Procedures

Procedures perform actions and are invoked with `CALL`:

```sql
CREATE PROCEDURE seed_users(n INT) AS $$
DECLARE
  i INT;
BEGIN
  FOR i IN 1..n LOOP
    INSERT INTO users (name) VALUES ('User ' || i::TEXT);
  END LOOP;
END $$ LANGUAGE plpgsql;

CALL seed_users(100);
```

Use `CREATE OR REPLACE PROCEDURE` to overwrite an existing procedure.

## DROP

```sql
DROP FUNCTION factorial;
DROP FUNCTION IF EXISTS factorial;
DROP PROCEDURE seed_users;
DROP PROCEDURE IF EXISTS seed_users;
```

## Block Structure

All PL/pgSQL blocks (DO blocks, function bodies, procedure bodies) share the same structure:

```
[DECLARE
  variable_name type [:= default_value];
  ...]
BEGIN
  statements...
[EXCEPTION
  WHEN condition THEN
    statements...]
END
```

## Variables

Declare variables with a type and optional default:

```sql
DECLARE
  x INT := 0;
  name TEXT;
  total NUMERIC := 100.50;
```

Variables without a default are initialized to `NULL`. Assignment uses `:=`:

```sql
x := x + 1;
name := 'Alice';
total := (SELECT SUM(amount) FROM orders);
```

Variables can be used in any SQL statement within the block — in `VALUES`, `WHERE`, `SET`, etc.

## Control Flow

### IF / ELSIF / ELSE

```sql
IF amount > 1000 THEN
  INSERT INTO vip_orders VALUES (order_id);
ELSIF amount > 100 THEN
  INSERT INTO normal_orders VALUES (order_id);
ELSE
  INSERT INTO small_orders VALUES (order_id);
END IF;
```

### WHILE Loop

```sql
WHILE balance > 0 LOOP
  balance := balance - payment;
  month := month + 1;
END LOOP;
```

Maximum 10,000 iterations as a safety limit.

### FOR Range Loop

```sql
FOR i IN 1..10 LOOP
  INSERT INTO numbers VALUES (i);
END LOOP;
```

The loop variable must be declared in `DECLARE`. Both bounds are inclusive.

### FOR Query Loop

Iterate over query results:

```sql
FOR name IN SELECT name FROM users ORDER BY id LOOP
  INSERT INTO greetings VALUES ('Hello, ' || name);
END LOOP;
```

For single-column queries, the variable receives the scalar value. For multi-column queries, the variable receives a record.

## RETURN

Exit a block or return a value from a function:

```sql
-- In a function:
RETURN x * 2;

-- In a DO block or procedure (exit early):
RETURN;
```

## RAISE

Print messages or throw errors:

```sql
RAISE NOTICE 'Processing row %', row_id;
RAISE EXCEPTION 'Invalid input: %', value;
```

`%` placeholders are replaced by argument values in order. `RAISE EXCEPTION` aborts execution.

## PERFORM

Execute a query and discard the result:

```sql
PERFORM SELECT notify_user(user_id);
```

## Exception Handling

Catch errors within a block:

```sql
DO $$
BEGIN
  CREATE TABLE t (id INT);
EXCEPTION
  WHEN duplicate_object THEN
    NULL;  -- table already exists, ignore
END $$;
```

Exception conditions:
- `duplicate_object` — table/type/constraint already exists
- `unique_violation` — unique constraint violated
- `others` — catches any exception

## NULL Statement

A no-op statement, commonly used in exception handlers:

```sql
EXCEPTION
  WHEN others THEN NULL;
```

## Triggers

Triggers execute a function automatically when rows are inserted, updated, or deleted:

```sql
CREATE FUNCTION audit_changes() RETURNS INT AS $$
BEGIN
  INSERT INTO audit_log VALUES (tg_op || ' on ' || tg_table_name);
  RETURN 0;
END $$ LANGUAGE plpgsql;

CREATE TRIGGER trg_audit AFTER INSERT ON orders
  FOR EACH ROW EXECUTE FUNCTION audit_changes();

CREATE TRIGGER trg_audit_del AFTER DELETE ON orders
  FOR EACH ROW EXECUTE FUNCTION audit_changes();
```

### Syntax

```sql
CREATE TRIGGER name BEFORE|AFTER INSERT|UPDATE|DELETE
  ON table FOR EACH ROW EXECUTE FUNCTION function_name();

DROP TRIGGER name ON table;
DROP TRIGGER IF EXISTS name ON table;
```

### Timing

- **BEFORE** triggers run before the operation. Return `NULL` to cancel the row operation. Return any non-NULL value to proceed.
- **AFTER** triggers run after the operation. Return value is ignored.

### Special Variables

Trigger functions have access to:

| Variable | Description |
|----------|-------------|
| `tg_op` | Operation name: `'INSERT'`, `'UPDATE'`, or `'DELETE'` |
| `tg_table_name` | Name of the table that fired the trigger |
| `OLD` | Row before the operation (UPDATE, DELETE) |
| `NEW` | Row after the operation (INSERT, UPDATE) |

### Events

Triggers fire for:
- `INSERT` — including rows inserted via `COPY FROM`
- `UPDATE` — fires per updated row
- `DELETE` — fires per deleted row

### Guard Trigger Example

```sql
CREATE FUNCTION prevent_delete() RETURNS INT AS $$
BEGIN
  RETURN NULL;  -- cancel the DELETE
END $$ LANGUAGE plpgsql;

CREATE TRIGGER trg_protect BEFORE DELETE ON important_data
  FOR EACH ROW EXECUTE FUNCTION prevent_delete();
```

### Native Function Callbacks

Registered native functions (Scala, JavaScript, or C) are callable from trigger functions, enabling integration with external systems:

```sql
-- Assuming notify_webhook() is registered as a native function
CREATE FUNCTION on_order() RETURNS INT AS $$
DECLARE dummy INT;
BEGIN
  dummy := notify_webhook(tg_op);
  RETURN 0;
END $$ LANGUAGE plpgsql;

CREATE TRIGGER trg AFTER INSERT ON orders
  FOR EACH ROW EXECUTE FUNCTION on_order();
```

See [Scala API](/reference/api-scala/), [JavaScript API](/reference/api-javascript/), and [C API](/reference/api-c/) for registering native functions.

## Persistence

Stored functions, procedures, and triggers persist across database restarts for both in-memory (session lifetime) and persistent storage (survives close/reopen). The source SQL is stored and re-executed on database open.

## Composition

Functions can call other functions. Procedures can call functions:

```sql
CREATE FUNCTION double(x INT) RETURNS INT AS $$
BEGIN RETURN x * 2; END $$ LANGUAGE plpgsql;

CREATE FUNCTION quadruple(x INT) RETURNS INT AS $$
BEGIN RETURN double(double(x)); END $$ LANGUAGE plpgsql;

SELECT quadruple(5);  -- 20
```

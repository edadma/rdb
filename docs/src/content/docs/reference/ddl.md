---
title: DDL
description: Data Definition Language — CREATE, ALTER, DROP, and TRUNCATE statements.
---

## Tables

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

### Foreign Keys

```sql
CREATE TABLE line_items (
  id SERIAL,
  order_id UUID REFERENCES orders (id) ON DELETE CASCADE ON UPDATE CASCADE,
  product TEXT NOT NULL
);
```

### ALTER TABLE

```sql
ALTER TABLE orders ADD COLUMN notes TEXT;
ALTER TABLE orders DROP COLUMN notes;
ALTER TABLE orders RENAME COLUMN amount TO total;
ALTER TABLE orders RENAME TO purchases;
```

### TRUNCATE TABLE

Removes all rows and resets serial sequences:

```sql
TRUNCATE TABLE orders;
```

### DROP TABLE

```sql
DROP TABLE orders;
DROP TABLE IF EXISTS orders;
```

## Views

### CREATE VIEW

Creates a named view backed by a query. Use `OR REPLACE` to overwrite an existing view:

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

## Custom Types

### CREATE TYPE

Defines an enumerated type:

```sql
CREATE TYPE order_status AS ENUM ('pending', 'shipped', 'delivered');
```

### DROP TYPE

```sql
DROP TYPE order_status CASCADE;
```

## Indexes

```sql
CREATE INDEX idx_orders_status ON orders (status);
CREATE UNIQUE INDEX idx_orders_email ON orders (email);
DROP INDEX idx_orders_status;
```

## SHOW Commands

Inspect database metadata:

```sql
SHOW TABLES;
SHOW VIEWS;
SHOW COLUMNS orders;
SHOW PRIMARY KEY orders;
SHOW FOREIGN KEYS orders;
SHOW INDEXES orders;
```

### SHOW VIEWS output

| Column | Type | Description |
|--------|------|-------------|
| `view_name` | TEXT | View name |
| `definition` | TEXT | The SQL query that defines the view |

### SHOW COLUMNS output

| Column | Type | Description |
|--------|------|-------------|
| `name` | TEXT | Column name |
| `type` | TEXT | Data type |
| `required` | BOOLEAN | NOT NULL constraint |
| `indexed` | BOOLEAN | Has an index |
| `unique` | BOOLEAN | Has a unique constraint |
| `fk_table` | TEXT | Foreign key target table |
| `fk_column` | TEXT | Foreign key target column |
| `fk_on_delete` | TEXT | ON DELETE action |
| `fk_on_update` | TEXT | ON UPDATE action |
| `default_value` | TEXT | Default expression |

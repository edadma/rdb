---
title: DML
description: Data Manipulation Language — INSERT, UPDATE, DELETE, and COPY statements.
---

## INSERT

```sql
INSERT INTO orders (customer_name, amount)
VALUES ('Alice Smith', 149.99);
```

### RETURNING

Return values from the inserted row:

```sql
INSERT INTO orders (customer_name, amount)
VALUES ('Bob Johnson', 75.50)
RETURNING id;

INSERT INTO orders (customer_name, amount)
VALUES ('Carol', 200.00)
RETURNING *;
```

### Insert from a Query

```sql
INSERT INTO archive (customer_name, amount)
SELECT customer_name, amount FROM orders WHERE status = 'delivered';
```

### INSERT ... ON CONFLICT (Upsert)

Skip conflicting rows:

```sql
INSERT INTO users (email, name)
VALUES ('alice@example.com', 'Alice')
ON CONFLICT DO NOTHING;
```

Update on conflict with specific columns:

```sql
INSERT INTO users (email, name)
VALUES ('alice@example.com', 'Alice Updated')
ON CONFLICT (email) DO UPDATE SET name = 'Alice Updated';
```

Can be combined with RETURNING:

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

### Bulk Update from VALUES

PostgreSQL-style `UPDATE ... FROM`:

```sql
UPDATE orders
  SET status = d.status
  FROM (VALUES ('ord-1', 'shipped'), ('ord-2', 'delivered'))
       AS d (id, status)
  WHERE orders.id = d.id;
```

### UPDATE with RETURNING

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

Join other tables to determine which rows to delete:

```sql
DELETE FROM orders
USING customers
WHERE orders.customer_id = customers.id
  AND customers.status = 'inactive';
```

Multiple USING tables:

```sql
DELETE FROM order_items
USING products, categories
WHERE order_items.product_id = products.id
  AND products.category_id = categories.id
  AND categories.name = 'discontinued';
```

### DELETE with RETURNING

```sql
DELETE FROM orders WHERE status = 'delivered'
RETURNING *;
```

## COPY

Bulk import and export data in CSV format.

### COPY FROM

Import rows from a CSV file:

```sql
COPY orders FROM 'data/orders.csv';
COPY orders FROM 'data/orders.csv' WITH (HEADER);
COPY orders FROM 'data/orders.csv' WITH (HEADER, DELIMITER '|');
COPY orders (customer_name, amount) FROM 'data/partial.csv' WITH (HEADER);
```

### COPY TO

Export a table or query to a CSV file:

```sql
COPY orders TO 'export/orders.csv';
COPY orders TO 'export/orders.csv' WITH (HEADER);
COPY (SELECT * FROM orders WHERE status = 'pending') TO 'export/pending.csv' WITH (HEADER);
```

### Options

| Option | Description |
|--------|-------------|
| `HEADER` | First row is a header (skipped on import, written on export) |
| `DELIMITER 'c'` | Field separator character (default: `,`) |

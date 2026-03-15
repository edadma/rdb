---
title: DML
description: Langage de manipulation de donnees -- instructions INSERT, UPDATE, DELETE et COPY.
---

## INSERT

```sql
INSERT INTO orders (customer_name, amount)
VALUES ('Alice Smith', 149.99);
```

### RETURNING

Retourne des valeurs de la ligne inseree :

```sql
INSERT INTO orders (customer_name, amount)
VALUES ('Bob Johnson', 75.50)
RETURNING id;

INSERT INTO orders (customer_name, amount)
VALUES ('Carol', 200.00)
RETURNING *;
```

### Insertion depuis une requete

```sql
INSERT INTO archive (customer_name, amount)
SELECT customer_name, amount FROM orders WHERE status = 'delivered';
```

### INSERT ... ON CONFLICT (Upsert)

Ignorer les lignes en conflit :

```sql
INSERT INTO users (email, name)
VALUES ('alice@example.com', 'Alice')
ON CONFLICT DO NOTHING;
```

Mettre a jour en cas de conflit avec des colonnes specifiques :

```sql
INSERT INTO users (email, name)
VALUES ('alice@example.com', 'Alice Updated')
ON CONFLICT (email) DO UPDATE SET name = 'Alice Updated';
```

Peut etre combine avec RETURNING :

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

### Mise a jour en masse depuis VALUES

`UPDATE ... FROM` style PostgreSQL :

```sql
UPDATE orders
  SET status = d.status
  FROM (VALUES ('ord-1', 'shipped'), ('ord-2', 'delivered'))
       AS d (id, status)
  WHERE orders.id = d.id;
```

### UPDATE avec RETURNING

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

Joindre d'autres tables pour determiner les lignes a supprimer :

```sql
DELETE FROM orders
USING customers
WHERE orders.customer_id = customers.id
  AND customers.status = 'inactive';
```

Plusieurs tables USING :

```sql
DELETE FROM order_items
USING products, categories
WHERE order_items.product_id = products.id
  AND products.category_id = categories.id
  AND categories.name = 'discontinued';
```

### DELETE avec RETURNING

```sql
DELETE FROM orders WHERE status = 'delivered'
RETURNING *;
```

## COPY

Import et export de donnees en masse au format CSV.

### COPY FROM

Importer des lignes depuis un fichier CSV :

```sql
COPY orders FROM 'data/orders.csv';
COPY orders FROM 'data/orders.csv' WITH (HEADER);
COPY orders FROM 'data/orders.csv' WITH (HEADER, DELIMITER '|');
COPY orders (customer_name, amount) FROM 'data/partial.csv' WITH (HEADER);
```

### COPY TO

Exporter une table ou une requete vers un fichier CSV :

```sql
COPY orders TO 'export/orders.csv';
COPY orders TO 'export/orders.csv' WITH (HEADER);
COPY (SELECT * FROM orders WHERE status = 'pending') TO 'export/pending.csv' WITH (HEADER);
```

### Options

| Option | Description |
|--------|-------------|
| `HEADER` | La premiere ligne est un en-tete (ignore a l'import, ecrit a l'export) |
| `DELIMITER 'c'` | Caractere separateur de champs (par defaut : `,`) |

## csv_file() -- Interroger des fichiers CSV directement

Interrogez un fichier CSV comme table virtuelle sans importation :

```sql
SELECT * FROM csv_file('data/sales.csv');
SELECT name, age::int FROM csv_file('data/people.csv') WHERE age::int > 25;
```

Toutes les valeurs sont retournees en `TEXT` -- utilisez `::type` pour la conversion. Supporte `WHERE`, `ORDER BY`, `LIMIT`, `JOIN` et les agregats.

Options :

```sql
csv_file('path')                        -- avec en-tete (par defaut)
csv_file('path', false)                 -- sans en-tete (colonnes nommees column1, column2, ...)
csv_file('path', true, '|')            -- delimiteur personnalise
```

Jointure de fichiers CSV entre eux ou avec des tables de la base de donnees :

```sql
SELECT e.name, d.department
FROM csv_file('employees.csv') e
JOIN csv_file('departments.csv') d ON e.dept_id = d.id;
```

## Tables virtuelles

Enregistrez des sources de donnees externes comme tables interrogeables avec `CREATE VIRTUAL TABLE` :

```sql
CREATE VIRTUAL TABLE sales USING csv('data/sales.csv');
CREATE VIRTUAL TABLE sales USING csv('data/sales.csv', 'no_header', '|');

SELECT * FROM sales WHERE amount::int > 100;
DROP TABLE sales;
```

Les tables virtuelles apparaissent dans `SHOW TABLES` et supportent `SELECT`, `WHERE`, `JOIN`, `ORDER BY` et les agregats. Elles sont en lecture seule -- `INSERT`, `UPDATE` et `DELETE` ne sont pas supportes.

Le module `csv` integre est enregistre par defaut. Des modules personnalises peuvent etre enregistres via l'API Scala :

```scala
db.registerVirtualTableModule("mymodule", new VirtualTableModule { ... })
```

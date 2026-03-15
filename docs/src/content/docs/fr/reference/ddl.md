---
title: DDL
description: Langage de definition de donnees -- instructions CREATE, ALTER, DROP et TRUNCATE.
---

## Schemas

PetraDB supporte les espaces de noms de schemas style PostgreSQL. Chaque base de donnees dispose d'un schema `public` par defaut. Les noms de tables non qualifies sont resolus dans `public`.

### CREATE SCHEMA

```sql
CREATE SCHEMA inventory;
CREATE SCHEMA IF NOT EXISTS inventory;
```

### Tables qualifiees par le schema

Utilisez la syntaxe `schema.table` dans toute instruction DDL ou DML :

```sql
CREATE TABLE inventory.products (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  price NUMERIC(10,2)
);

INSERT INTO inventory.products (name, price) VALUES ('Widget', 9.99);
SELECT * FROM inventory.products;
```

Des tables portant le meme nom peuvent exister dans differents schemas :

```sql
CREATE SCHEMA staging;
CREATE TABLE staging.products (id SERIAL, name TEXT);
CREATE TABLE public.products (id SERIAL, name TEXT);
-- Ce sont des tables separees
```

### information_schema

PetraDB fournit des tables virtuelles `information_schema` pour l'introspection de la structure de la base de donnees :

```sql
SELECT * FROM information_schema.schemata;
SELECT * FROM information_schema.tables;
SELECT * FROM information_schema.columns WHERE table_name = 'products';
```

Vues disponibles : `schemata`, `tables`, `columns`.

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

### Cles etrangeres

```sql
CREATE TABLE line_items (
  id SERIAL,
  order_id UUID REFERENCES orders (id) ON DELETE CASCADE ON UPDATE CASCADE,
  product TEXT NOT NULL
);
```

### Colonnes generees

Colonnes calculees automatiquement derivees d'autres colonnes :

```sql
CREATE TABLE products (
  price NUMERIC,
  tax_rate NUMERIC DEFAULT 0.08,
  total NUMERIC GENERATED ALWAYS AS (price * (1 + tax_rate)) STORED
);

INSERT INTO products (price) VALUES (100);
SELECT * FROM products;
-- price: 100, tax_rate: 0.08, total: 108
```

Les colonnes generees sont recalculees lors des INSERT et UPDATE. Elles ne peuvent pas etre ecrites directement.

### ALTER TABLE

Ajouter, supprimer ou renommer des colonnes :

```sql
ALTER TABLE orders ADD COLUMN notes TEXT;
ALTER TABLE orders DROP COLUMN notes;
ALTER TABLE orders RENAME COLUMN amount TO total;
ALTER TABLE orders RENAME TO purchases;
```

Modifier les proprietes de colonnes :

```sql
ALTER TABLE orders ALTER COLUMN notes SET NOT NULL;
ALTER TABLE orders ALTER COLUMN notes DROP NOT NULL;
ALTER TABLE orders ALTER COLUMN notes SET DEFAULT 'none';
ALTER TABLE orders ALTER COLUMN notes DROP DEFAULT;
ALTER TABLE orders ALTER COLUMN amount SET DATA TYPE NUMERIC(10,2);
```

Ajouter et supprimer des contraintes :

```sql
ALTER TABLE orders ADD CONSTRAINT chk_amount CHECK (amount > 0);
ALTER TABLE orders ADD CONSTRAINT uq_email UNIQUE (email);
ALTER TABLE orders ADD CONSTRAINT fk_customer
  FOREIGN KEY (customer_id) REFERENCES customers (id) ON DELETE CASCADE;
ALTER TABLE orders DROP CONSTRAINT chk_amount;
```

### Contraintes CHECK

Imposent des conditions sur les valeurs de colonnes :

```sql
CREATE TABLE products (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  price NUMERIC CHECK (price >= 0),
  quantity INT,
  CONSTRAINT positive_qty CHECK (quantity >= 0)
);
```

Les contraintes CHECK sont appliquees lors des INSERT et UPDATE.

## Declencheurs

Consultez [PL/pgSQL -- Declencheurs](/reference/plpgsql/#triggers) pour la documentation complete.

```sql
CREATE TRIGGER trg_audit AFTER INSERT ON orders
  FOR EACH ROW EXECUTE FUNCTION audit_log();

DROP TRIGGER trg_audit ON orders;
DROP TRIGGER IF EXISTS trg_audit ON orders;
```

### TRUNCATE TABLE

Supprime toutes les lignes et reinitialise les sequences serial :

```sql
TRUNCATE TABLE orders;
```

### DROP TABLE

```sql
DROP TABLE orders;
DROP TABLE IF EXISTS orders;
```

## Vues

### CREATE VIEW

Cree une vue nommee soutenue par une requete. Utilisez `OR REPLACE` pour remplacer une vue existante :

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

## Types personnalises

### CREATE TYPE

Definit un type enumere :

```sql
CREATE TYPE order_status AS ENUM ('pending', 'shipped', 'delivered');
```

### DROP TYPE

```sql
DROP TYPE order_status CASCADE;
```

## Sequences

Les sequences sont des compteurs nommes qui generent des valeurs numeriques sequentielles. Elles sont couramment utilisees pour la generation de cles primaires.

### CREATE SEQUENCE

```sql
CREATE SEQUENCE order_seq;
CREATE SEQUENCE order_seq START WITH 100 INCREMENT BY 10;
CREATE SEQUENCE IF NOT EXISTS order_seq;
```

Options :

| Option | Par defaut | Description |
|--------|-----------|-------------|
| `INCREMENT BY n` | 1 | Pas d'incrementation |
| `START WITH n` | 1 | Valeur initiale |
| `MINVALUE n` / `NO MINVALUE` | 1 | Valeur minimale |
| `MAXVALUE n` / `NO MAXVALUE` | 4611686018427387903 | Valeur maximale |
| `CYCLE` / `NO CYCLE` | `NO CYCLE` | Reprendre au debut aux limites |

### DROP SEQUENCE

```sql
DROP SEQUENCE order_seq;
DROP SEQUENCE IF EXISTS order_seq;
```

### SERIAL et sequences

Les colonnes `SERIAL`, `SMALLSERIAL` et `BIGSERIAL` creent automatiquement une sequence de support nommee `<table>_<column>_seq`. Cela correspond au comportement PostgreSQL :

```sql
CREATE TABLE orders (id SERIAL PRIMARY KEY, name TEXT);
-- Cree implicitement la sequence "orders_id_seq"

SELECT nextval('orders_id_seq');   -- fonctionne
SELECT currval('orders_id_seq');   -- fonctionne apres INSERT ou nextval
```

La suppression de la table supprime en cascade ses sequences possedees. `TRUNCATE` reinitialise les sequences de support a leurs valeurs de depart.

### Fonctions de sequence

| Fonction | Description |
|----------|-------------|
| `nextval('seq_name')` | Avancer et retourner la valeur suivante |
| `currval('seq_name')` | Retourner la valeur actuelle (nextval doit etre appele d'abord dans la session) |
| `setval('seq_name', value)` | Definir la valeur actuelle ; le prochain nextval retourne valeur + increment |
| `setval('seq_name', value, false)` | Definir la valeur actuelle ; le prochain nextval retourne valeur |
| `lastval()` | Retourner la derniere valeur de toute sequence dans cette session |

## Fonctions et procedures stockees

Consultez [PL/pgSQL](/reference/plpgsql/) pour la documentation complete.

```sql
CREATE FUNCTION double(x INT) RETURNS INT AS $$
BEGIN RETURN x * 2; END $$ LANGUAGE plpgsql;

CREATE PROCEDURE reset_counts() AS $$
BEGIN UPDATE counters SET val = 0; END $$ LANGUAGE plpgsql;

DROP FUNCTION double;
DROP PROCEDURE IF EXISTS reset_counts;
```

## Index

```sql
CREATE INDEX idx_orders_status ON orders (status);
CREATE UNIQUE INDEX idx_orders_email ON orders (email);
DROP INDEX idx_orders_status;
```

### Index partiels

Indexent uniquement les lignes correspondant a une condition, rendant l'index plus petit et plus rapide :

```sql
CREATE INDEX idx_active_orders ON orders (customer_id) WHERE status = 'active';
CREATE UNIQUE INDEX idx_unique_active_email ON users (email) WHERE active = true;
```

Le planificateur de requetes utilise un index partiel uniquement lorsque la clause `WHERE` de la requete inclut la condition de l'index.

### Index sur expression

Index sur des valeurs calculees au lieu de colonnes brutes :

```sql
CREATE INDEX idx_lower_email ON users ((lower(email)));
CREATE UNIQUE INDEX idx_lower_name ON users ((lower(name)));
```

L'expression doit etre entre parentheses. Le planificateur fait correspondre `WHERE lower(email) = 'alice@test.com'` a l'index automatiquement.

Les index partiels et sur expression peuvent etre combines :

```sql
CREATE INDEX idx_active_lower ON users ((lower(name))) WHERE status = 'active';
```

## Commandes SHOW

Inspecter les metadonnees de la base de donnees :

```sql
SHOW TABLES;
SHOW VIEWS;
SHOW SEQUENCES;
SHOW COLUMNS orders;
SHOW PRIMARY KEY orders;
SHOW FOREIGN KEYS orders;
SHOW INDEXES orders;
SHOW INDEXES;              -- tous les index de toutes les tables
```

### Sortie de SHOW VIEWS

| Colonne | Type | Description |
|---------|------|-------------|
| `view_name` | TEXT | Nom de la vue |
| `definition` | TEXT | La requete SQL qui definit la vue |

### Sortie de SHOW COLUMNS

| Colonne | Type | Description |
|---------|------|-------------|
| `name` | TEXT | Nom de la colonne |
| `type` | TEXT | Type de donnees |
| `required` | BOOLEAN | Contrainte NOT NULL |
| `indexed` | BOOLEAN | Possede un index |
| `unique` | BOOLEAN | Possede une contrainte d'unicite |
| `fk_table` | TEXT | Table cible de la cle etrangere |
| `fk_column` | TEXT | Colonne cible de la cle etrangere |
| `fk_on_delete` | TEXT | Action ON DELETE |
| `fk_on_update` | TEXT | Action ON UPDATE |
| `default_value` | TEXT | Expression par defaut |

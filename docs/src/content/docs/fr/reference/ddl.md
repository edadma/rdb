---
title: DDL
description: Langage de définition de données — instructions CREATE, ALTER, DROP et TRUNCATE.
---

## Schémas

PetraDB supporte les espaces de noms de schémas style PostgreSQL. Chaque base de données dispose d'un schéma `public` par défaut. Les noms de tables non qualifiés sont résolus dans `public`.

### CREATE SCHEMA

```sql
CREATE SCHEMA inventory;
CREATE SCHEMA IF NOT EXISTS inventory;
```

### Tables qualifiées par le schéma

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

Des tables portant le même nom peuvent exister dans différents schémas :

```sql
CREATE SCHEMA staging;
CREATE TABLE staging.products (id SERIAL, name TEXT);
CREATE TABLE public.products (id SERIAL, name TEXT);
-- Ce sont des tables séparées
```

### information_schema

PetraDB fournit des tables virtuelles `information_schema` pour l'introspection de la structure de la base de données :

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

### Clés étrangères

```sql
CREATE TABLE line_items (
  id SERIAL,
  order_id UUID REFERENCES orders (id) ON DELETE CASCADE ON UPDATE CASCADE,
  product TEXT NOT NULL
);
```

### Colonnes générées

Colonnes calculées automatiquement dérivées d'autres colonnes :

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

Les colonnes générées sont recalculées lors des INSERT et UPDATE. Elles ne peuvent pas être écrites directement.

### ALTER TABLE

Ajouter, supprimer ou renommer des colonnes :

```sql
ALTER TABLE orders ADD COLUMN notes TEXT;
ALTER TABLE orders DROP COLUMN notes;
ALTER TABLE orders RENAME COLUMN amount TO total;
ALTER TABLE orders RENAME TO purchases;
```

Modifier les propriétés de colonnes :

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

Les contraintes CHECK sont appliquées lors des INSERT et UPDATE.

## Déclencheurs

Consultez [PL/pgSQL — Déclencheurs](/reference/plpgsql/#triggers) pour la documentation complète.

```sql
CREATE TRIGGER trg_audit AFTER INSERT ON orders
  FOR EACH ROW EXECUTE FUNCTION audit_log();

DROP TRIGGER trg_audit ON orders;
DROP TRIGGER IF EXISTS trg_audit ON orders;
```

### TRUNCATE TABLE

Supprime toutes les lignes et réinitialise les séquences serial :

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

Crée une vue nommée soutenue par une requête. Utilisez `OR REPLACE` pour remplacer une vue existante :

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

## Types personnalisés

### CREATE TYPE

Définit un type énuméré :

```sql
CREATE TYPE order_status AS ENUM ('pending', 'shipped', 'delivered');
```

### DROP TYPE

```sql
DROP TYPE order_status CASCADE;
```

## Séquences

Les séquences sont des compteurs nommés qui génèrent des valeurs numériques séquentielles. Elles sont couramment utilisées pour la génération de clés primaires.

### CREATE SEQUENCE

```sql
CREATE SEQUENCE order_seq;
CREATE SEQUENCE order_seq START WITH 100 INCREMENT BY 10;
CREATE SEQUENCE IF NOT EXISTS order_seq;
```

Options :

| Option | Par défaut | Description |
|--------|-----------|-------------|
| `INCREMENT BY n` | 1 | Pas d'incrémentation |
| `START WITH n` | 1 | Valeur initiale |
| `MINVALUE n` / `NO MINVALUE` | 1 | Valeur minimale |
| `MAXVALUE n` / `NO MAXVALUE` | 4611686018427387903 | Valeur maximale |
| `CYCLE` / `NO CYCLE` | `NO CYCLE` | Reprendre au début aux limites |

### DROP SEQUENCE

```sql
DROP SEQUENCE order_seq;
DROP SEQUENCE IF EXISTS order_seq;
```

### SERIAL et séquences

Les colonnes `SERIAL`, `SMALLSERIAL` et `BIGSERIAL` créent automatiquement une séquence de support nommée `<table>_<column>_seq`. Cela correspond au comportement PostgreSQL :

```sql
CREATE TABLE orders (id SERIAL PRIMARY KEY, name TEXT);
-- Crée implicitement la séquence "orders_id_seq"

SELECT nextval('orders_id_seq');   -- fonctionne
SELECT currval('orders_id_seq');   -- fonctionne après INSERT ou nextval
```

La suppression de la table supprime en cascade ses séquences possédées. `TRUNCATE` réinitialise les séquences de support à leurs valeurs de départ.

### Fonctions de séquence

| Fonction | Description |
|----------|-------------|
| `nextval('seq_name')` | Avancer et retourner la valeur suivante |
| `currval('seq_name')` | Retourner la valeur actuelle (nextval doit être appelé d'abord dans la session) |
| `setval('seq_name', value)` | Définir la valeur actuelle ; le prochain nextval retourne valeur + incrément |
| `setval('seq_name', value, false)` | Définir la valeur actuelle ; le prochain nextval retourne valeur |
| `lastval()` | Retourner la dernière valeur de toute séquence dans cette session |

## Fonctions et procédures stockées

Consultez [PL/pgSQL](/reference/plpgsql/) pour la documentation complète.

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

Indexent uniquement les lignes correspondant à une condition, rendant l'index plus petit et plus rapide :

```sql
CREATE INDEX idx_active_orders ON orders (customer_id) WHERE status = 'active';
CREATE UNIQUE INDEX idx_unique_active_email ON users (email) WHERE active = true;
```

Le planificateur de requêtes utilise un index partiel uniquement lorsque la clause `WHERE` de la requête inclut la condition de l'index.

### Index sur expression

Index sur des valeurs calculées au lieu de colonnes brutes :

```sql
CREATE INDEX idx_lower_email ON users ((lower(email)));
CREATE UNIQUE INDEX idx_lower_name ON users ((lower(name)));
```

L'expression doit être entre parenthèses. Le planificateur fait correspondre `WHERE lower(email) = 'alice@test.com'` à l'index automatiquement.

Les index partiels et sur expression peuvent être combinés :

```sql
CREATE INDEX idx_active_lower ON users ((lower(name))) WHERE status = 'active';
```

## Commandes SHOW

Inspecter les métadonnées de la base de données :

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
| `definition` | TEXT | La requête SQL qui définit la vue |

### Sortie de SHOW COLUMNS

| Colonne | Type | Description |
|---------|------|-------------|
| `name` | TEXT | Nom de la colonne |
| `type` | TEXT | Type de données |
| `required` | BOOLEAN | Contrainte NOT NULL |
| `indexed` | BOOLEAN | Possède un index |
| `unique` | BOOLEAN | Possède une contrainte d'unicité |
| `fk_table` | TEXT | Table cible de la clé étrangère |
| `fk_column` | TEXT | Colonne cible de la clé étrangère |
| `fk_on_delete` | TEXT | Action ON DELETE |
| `fk_on_update` | TEXT | Action ON UPDATE |
| `default_value` | TEXT | Expression par défaut |

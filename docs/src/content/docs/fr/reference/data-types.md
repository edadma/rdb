---
title: Types de donnees
description: Types de donnees SQL supportes par PetraDB.
---

PetraDB suit les conventions PostgreSQL pour la syntaxe SQL, la gestion des identifiants et la conversion de types.

## Compatibilite SQL

- **Mots-cles insensibles a la casse** -- `SELECT`, `select` et `Select` sont equivalents
- **Pliage des identifiants non quotes** -- les identifiants non quotes sont convertis en minuscules (`CREATE TABLE Users` -> nom de table `users`)
- **Identifiants entre guillemets doubles** -- preservent la casse (`"MixedCase"` reste tel quel)
- **Echappement de chaines** -- guillemets simples doubles (`'it''s'`) et E-strings (`E'it\'s'`)
- **Operateurs** -- `!=` et `<>` pour l'inegalite

## Types supportes

| Type | Description |
|------|-------------|
| `SMALLINT` | Entier 16 bits (-32768 a 32767) |
| `INT` / `INTEGER` | Entier 32 bits |
| `BIGINT` | Entier 64 bits |
| `SMALLSERIAL` | Entier 16 bits auto-incremente (cree une sequence de support) |
| `SERIAL` | Entier 32 bits auto-incremente (cree une sequence de support) |
| `BIGSERIAL` | Entier 64 bits auto-incremente (cree une sequence de support) |
| `DOUBLE` / `FLOAT` / `REAL` | Virgule flottante double precision |
| `NUMERIC(p,s)` / `DECIMAL(p,s)` | Decimal a precision fixe |
| `TEXT` | Chaine de longueur variable |
| `CHAR(n)` | Chaine de longueur fixe (completee par des espaces a droite) |
| `VARCHAR(n)` | Chaine de longueur variable (max n caracteres, sans completion) |
| `BOOLEAN` | Vrai/faux |
| `DATE` | Date du calendrier (`yyyy-MM-dd`) |
| `TIME` | Heure du jour (`HH:mm:ss`) |
| `TIMESTAMP` | Date et heure |
| `TIMESTAMP WITH TIME ZONE` | Date et heure avec decalage de fuseau horaire |
| `INTERVAL` | Duree (ISO 8601 ou `N days N hours N minutes N seconds`) |
| `UUID` | Identifiant universel unique |
| `JSON` / `JSONB` | Objets et tableaux JSON structures |
| `BYTEA` | Donnees binaires |
| `ENUM` | Types enumeres personnalises (via `CREATE TYPE ... AS ENUM`) |
| `INT[]`, `TEXT[]`, etc. | Tableaux types (tout type de base avec le suffixe `[]`) |

## Conversion de types

Utilisez l'operateur `::` ou `CAST(expr AS type)` pour convertir entre types :

```sql
SELECT '2024-06-15'::DATE;
SELECT CAST('14:30:00' AS TIME);
SELECT '2 hours 30 minutes'::INTERVAL;
SELECT CAST(val AS TEXT);
SELECT '42'::INT;
SELECT 1::BOOLEAN;
SELECT EXTRACT(year FROM created_at);
```

## Arithmetique date/heure

```sql
SELECT '2024-01-01'::DATE + 10;                        -- ajouter des jours
SELECT '2024-01-15'::DATE - '2024-01-10'::DATE;        -- jours entre
SELECT now() + '2 hours'::INTERVAL;                     -- timestamp + intervalle
SELECT now() - '30 minutes'::INTERVAL;                  -- timestamp - intervalle
SELECT '1 hour'::INTERVAL * 3;                          -- multiplier l'intervalle
SELECT EXTRACT(year FROM now());                        -- extraire un champ
SELECT date_trunc('month', now());                      -- tronquer
```

## Contraintes

```sql
PRIMARY KEY (id)
UNIQUE (email)
NOT NULL
DEFAULT value
FOREIGN KEY (col) REFERENCES other_table (col) ON DELETE CASCADE ON UPDATE CASCADE
```

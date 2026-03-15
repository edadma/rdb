---
title: Types de données
description: Types de données SQL supportés par PetraDB.
---

PetraDB suit les conventions PostgreSQL pour la syntaxe SQL, la gestion des identifiants et la conversion de types.

## Compatibilité SQL

- **Mots-clés insensibles à la casse** — `SELECT`, `select` et `Select` sont équivalents
- **Pliage des identifiants non quotés** — les identifiants non quotés sont convertis en minuscules (`CREATE TABLE Users` -> nom de table `users`)
- **Identifiants entre guillemets doubles** — préservent la casse (`"MixedCase"` reste tel quel)
- **Échappement de chaînes** — guillemets simples doublés (`'it''s'`) et E-strings (`E'it\'s'`)
- **Opérateurs** — `!=` et `<>` pour l'inégalité

## Types supportés

| Type | Description |
|------|-------------|
| `SMALLINT` | Entier 16 bits (-32768 à 32767) |
| `INT` / `INTEGER` | Entier 32 bits |
| `BIGINT` | Entier 64 bits |
| `SMALLSERIAL` | Entier 16 bits auto-incrémenté (crée une séquence de support) |
| `SERIAL` | Entier 32 bits auto-incrémenté (crée une séquence de support) |
| `BIGSERIAL` | Entier 64 bits auto-incrémenté (crée une séquence de support) |
| `DOUBLE` / `FLOAT` / `REAL` | Virgule flottante double précision |
| `NUMERIC(p,s)` / `DECIMAL(p,s)` | Décimal à précision fixe |
| `TEXT` | Chaîne de longueur variable |
| `CHAR(n)` | Chaîne de longueur fixe (complétée par des espaces à droite) |
| `VARCHAR(n)` | Chaîne de longueur variable (max n caractères, sans complétion) |
| `BOOLEAN` | Vrai/faux |
| `DATE` | Date du calendrier (`yyyy-MM-dd`) |
| `TIME` | Heure du jour (`HH:mm:ss`) |
| `TIMESTAMP` | Date et heure |
| `TIMESTAMP WITH TIME ZONE` | Date et heure avec décalage de fuseau horaire |
| `INTERVAL` | Durée (ISO 8601 ou `N days N hours N minutes N seconds`) |
| `UUID` | Identifiant universel unique |
| `JSON` / `JSONB` | Objets et tableaux JSON structurés |
| `BYTEA` | Données binaires |
| `ENUM` | Types énumérés personnalisés (via `CREATE TYPE ... AS ENUM`) |
| `INT[]`, `TEXT[]`, etc. | Tableaux typés (tout type de base avec le suffixe `[]`) |

## Conversion de types

Utilisez l'opérateur `::` ou `CAST(expr AS type)` pour convertir entre types :

```sql
SELECT '2024-06-15'::DATE;
SELECT CAST('14:30:00' AS TIME);
SELECT '2 hours 30 minutes'::INTERVAL;
SELECT CAST(val AS TEXT);
SELECT '42'::INT;
SELECT 1::BOOLEAN;
SELECT EXTRACT(year FROM created_at);
```

## Arithmétique date/heure

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

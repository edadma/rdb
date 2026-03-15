---
title: Journal des modifications
---

## v1.5-20260315

### PL/pgSQL — Procédures stockées, fonctions et déclencheurs

Support complet du langage procédural dans les blocs DO, les fonctions stockées et les procédures stockées :

- **Blocs DO** — blocs PL/pgSQL anonymes avec DECLARE, BEGIN...END
- **Fonctions stockées** — `CREATE FUNCTION name(params) RETURNS type AS $$ ... $$ LANGUAGE plpgsql`, appelables dans toute expression SQL
- **Procédures stockées** — `CREATE PROCEDURE name(params) AS $$ ... $$ LANGUAGE plpgsql`, invoquées avec `CALL`
- **Déclencheurs** — `CREATE TRIGGER name BEFORE|AFTER INSERT|UPDATE|DELETE ON table FOR EACH ROW EXECUTE FUNCTION func()`
  - Les déclencheurs BEFORE peuvent annuler les opérations en retournant NULL
  - Les déclencheurs AFTER se déclenchent après l'opération
  - Les déclencheurs se déclenchent pour INSERT, UPDATE, DELETE et COPY FROM
  - Variables TG_OP, TG_TABLE_NAME, OLD, NEW disponibles
- **Flux de contrôle** — IF/ELSIF/ELSE, WHILE LOOP, FOR range/query LOOP, RETURN, RAISE NOTICE/EXCEPTION, PERFORM, EXCEPTION WHEN
- **Persistance** — les fonctions, procédures et déclencheurs survivent à la fermeture/réouverture sur PersistentDB et TextDB
- **OR REPLACE** — remplacement des fonctions et procédures existantes

### Fonctions natives définies par l'utilisateur

Enregistrement de callbacks du langage hôte en tant que fonctions SQL, appelables depuis les requêtes, les déclencheurs et les procédures :

- **Scala** — `db.registerScalarFunction("name", { args => result })`
- **JavaScript** — `session.registerFunction("name", (args) => result)`
- **C** — `petradb_create_function(db, "name", nargs, user_data, callback)` avec une API de valeurs/contexte typée style SQLite

### Bibliothèque C et API curseur

- **Bibliothèque partagée native** — `libpetradb-engine.so` via Scala Native avec des fonctions C appelables `@exported`
- **API C style SQLite** — `petradb_open`, `petradb_exec`, `petradb_prepare/step/finalize`, accesseurs de colonnes typés
- **Fonctions définies par l'utilisateur** — `petradb_value_int/double/text`, `petradb_result_int/double/text/null/error`, `petradb_user_data`
- **API curseur** — `session.openCursor(sql)` pour une itération paresseuse ligne par ligne avec `step()`, accesseurs de colonnes typés, `fetch(n)`, `move(n)`, requêtes paramétrées
- **En-tête C** — `petradb.h` avec documentation complète de l'API
- **Suite de tests C** — 67 tests
- **Test FFI Rust** — 38 tests prouvant l'interopérabilité entre langages

### Tables virtuelles

- **Framework extensible** — `CREATE VIRTUAL TABLE name USING module(args)`, lecture seule, apparaît dans SHOW TABLES
- **Module CSV intégré** — `CREATE VIRTUAL TABLE t USING csv('file.csv')` avec options d'en-tête/délimiteur
- **Modules personnalisés** — `db.registerVirtualTableModule("name", module)` dans l'API Scala

### Fonction de table csv_file()

Interrogez des fichiers CSV directement sans importation :

```sql
SELECT * FROM csv_file('data.csv');
SELECT e.name, d.dept FROM csv_file('employees.csv') e
  JOIN csv_file('departments.csv') d ON e.dept_id = d.id;
```

### Index avancés

- **Index partiels** — `CREATE INDEX ... WHERE condition` — indexe uniquement les lignes correspondant au prédicat
- **Index sur expression** — `CREATE INDEX ... ON table ((expr))` — indexe des valeurs calculées comme `lower(email)`
- **Combinés** — les index partiels et sur expression fonctionnent ensemble

### Fonctions de fenêtre

- **FIRST_VALUE(expr)** — valeur à la première ligne du cadre de fenêtre
- **LAST_VALUE(expr)** — valeur à la dernière ligne du cadre de fenêtre
- **NTH_VALUE(expr, n)** — valeur à la n-ième ligne du cadre

### DELETE ... USING

Suppressions multi-tables selon la syntaxe PostgreSQL :

```sql
DELETE FROM orders USING customers
WHERE orders.customer_id = customers.id AND customers.status = 'inactive';
```

### Quarry — Constructeur de requêtes type-safe par AST

Nouveau package `@petradb/quarry` : constructeur de requêtes type-safe qui génère des objets AST (pas des chaînes SQL) :

- Définition de schéma avec 21 types de colonnes
- CRUD complet : select, insert, update, delete avec références de colonnes type-safe
- Jointures : inner, left, right, full outer, cross avec résultats typés
- Expressions : 50+ opérateurs, agrégats, CASE/CAST/EXISTS, sous-requêtes
- Upsert : `onConflictDoNothing()`, `onConflictDoUpdate()`
- Alias de tables pour les auto-jointures
- Transactions, RETURNING, DISTINCT ON
- Tests de types à la compilation pour toutes les fonctionnalités
- Opérations ensemblistes : UNION, INTERSECT, EXCEPT
- Fonctions de fenêtre, CTE, helpers scalaires nommés

### Corrections de bogues

- **ByteaValue** — `ARRAY[...]` dans les colonnes BYTEA produit maintenant correctement `ByteaValue` au lieu de `ArrayValue`
- **Types de résultats JS/Client** — ajout des gestionnaires de types de résultats PL/pgSQL manquants (DoBlockResult, CreateFunctionResult, etc.) pour éviter les crashs de correspondance non exhaustive
- **Codecs** — ajout de la sérialisation pour tous les nouveaux types de résultats pour la communication client/serveur
- **llms.txt** — correction du champ `type` en champ `command`, mise à jour de tous les types de résultats et fonctionnalités

### Renommage de module

- **shared -> common** — le module de types partagés a été renommé de `petradb-shared` à `petradb-common`

### Documentation

- Nouvelle page de référence **PL/pgSQL** (déclencheurs, fonctions, procédures, flux de contrôle)
- Nouvelle page de référence **API C** (interface complète style SQLite)
- Nouveaux guides **Premiers pas** pour Java (JDBC) et C
- Documentation DDL mise à jour : index partiels/sur expression, contraintes CHECK, déclencheurs, routines stockées
- Documentation DML mise à jour : DELETE...USING, csv_file(), tables virtuelles
- Documentation API JS/Scala mise à jour : registerFunction, types de résultats
- Page d'accueil : quatre boutons de démarrage (JS, Java, Scala, C)
- llms.txt réécrit avec toutes les fonctionnalités actuelles

### Mises à jour de versions

| Composant | Maven Central | npm |
|-----------|---------------|-----|
| common | 1.5.0 | -- |
| engine | 1.5.0 | @petradb/engine 1.5.0 |
| client | 1.5.0 | @petradb/client 1.5.0 |
| server | 1.5.0 | @petradb/server 1.5.0 |
| cli | 1.5.0 | @petradb/cli 1.5.0 |
| jdbc | 1.5.0 | -- |
| drizzle | -- | @petradb/drizzle 1.5.0 |
| knex | -- | @petradb/knex 1.5.0 |
| lucid | -- | @petradb/lucid 1.5.0 |
| quarry | -- | @petradb/quarry 1.5.0 |

## v1.4-20260314

### Corrections de bogues et améliorations

- **Correction des sous-requêtes IN corrélées** — les sous-requêtes corrélées `IN (SELECT ...)` avec tables indexées fonctionnent maintenant correctement
- **Résolution de colonnes qualifiées** — corrections pour les références de colonnes ambiguës dans les jointures complexes
- **Gestion de IN/ANY vides** — les cas limites `IN ()` et `= ANY('{}')` sont résolus
- **Nettoyage CASCADE des clés étrangères** — `DROP TABLE ... CASCADE` supprime maintenant correctement les contraintes FK sur les tables filles
- **DROP TABLE IF EXISTS ... CASCADE** — la combinaison de `IF EXISTS` avec `CASCADE` ne provoque plus d'erreur de syntaxe
- **Conversion de littéraux tableau** — support de la syntaxe PostgreSQL `'{1,2,3}'::integer[]` pour les littéraux tableau
- **Corrections des requêtes paramétrées** — amélioration de la liaison de paramètres pour les sous-requêtes et les chemins corrélés

### Mises à jour de versions

| Composant | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.4.2 | -- |
| engine | 1.4.9 | @petradb/engine 1.4.3 |
| client | 1.4.2 | @petradb/client 1.4.2 |
| server | -- | @petradb/server 1.4.3 |
| cli | -- | @petradb/cli 1.4.3 |
| jdbc | 1.4.3 | -- |
| drizzle | -- | @petradb/drizzle 1.4.3 |

## v1.4-20260312

### Expressions de table communes (CTE)

Support complet des CTE avec `WITH` et `WITH RECURSIVE`.

**CTE non récursives** — sous-requêtes nommées pour la lisibilité et la réutilisation :

```sql
WITH active_orders AS (
  SELECT * FROM orders WHERE status = 'active'
)
SELECT customer_id, SUM(amount)
FROM active_orders
GROUP BY customer_id;
```

Plusieurs CTE peuvent être définies dans une seule requête, et les CTE suivantes peuvent référencer les précédentes. Les alias de colonnes sont supportés : `WITH t(x, y) AS (...)`. Les CTE masquent les noms de tables s'ils partagent le même nom.

**CTE récursives** — requêtes itératives pour les données hiérarchiques et les graphes :

```sql
WITH RECURSIVE descendants(id, name, depth) AS (
  SELECT id, name, 0 FROM employees WHERE manager_id IS NULL
  UNION ALL
  SELECT e.id, e.name, d.depth + 1
  FROM employees e INNER JOIN descendants d ON e.manager_id = d.id
)
SELECT name, depth FROM descendants ORDER BY depth, name;
```

`UNION ALL` (conserve les doublons) et `UNION` (déduplique) sont tous deux supportés. Limite de sécurité de 1000 itérations maximum.

### Fonctions de fenêtre

Support complet des fonctions de fenêtre en trois catégories :

**Fonctions de classement** — `ROW_NUMBER()`, `RANK()`, `DENSE_RANK()` avec `PARTITION BY` et `ORDER BY` :

```sql
SELECT name, department, salary,
  RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dept_rank
FROM employees;
```

**Fonctions de valeur** — `LAG()`, `LEAD()`, `NTILE()` avec décalage et valeurs par défaut configurables :

```sql
SELECT name, salary,
  LAG(salary, 1, 0) OVER (ORDER BY salary) AS prev_salary,
  NTILE(4) OVER (ORDER BY salary) AS quartile
FROM employees;
```

**Fonctions de fenêtre avec agrégats** — tout agrégat (`SUM`, `COUNT`, `AVG`, `MIN`, `MAX`, etc.) avec `OVER()`, y compris les spécifications de cadre :

```sql
SELECT name, salary,
  SUM(salary) OVER (ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_total,
  AVG(salary) OVER (PARTITION BY department) AS dept_avg
FROM employees;
```

Bornes de cadre : `UNBOUNDED PRECEDING`, `UNBOUNDED FOLLOWING`, `CURRENT ROW`, `N PRECEDING`, `N FOLLOWING`. Sans clause de cadre, les fonctions de fenêtre avec agrégats calculent sur l'ensemble de la partition.

### Clause FILTER sur les agrégats

`FILTER (WHERE ...)` sur les fonctions d'agrégation, à la fois dans les requêtes groupées et les fonctions de fenêtre :

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE status = 'active') AS active_count,
  SUM(amount) FILTER (WHERE amount > 100) OVER (ORDER BY created_at
    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_high_value
FROM orders;
```

### Optimisation par jointure de hachage

Les équijointures sans index utilisent maintenant une stratégie de jointure par hachage au lieu d'un produit cartésien, réduisant la complexité de la jointure de O(n*m) à O(n+m). S'applique aux jointures INNER, LEFT, RIGHT et FULL. Les jointures par boucle imbriquée sur index restent préférées quand un index est disponible. Les conditions de jointure non équi reviennent au produit cartésien. Visible dans la sortie `EXPLAIN` comme `Hash Join`, `Hash Left Join`, `Hash Right Join`, `Hash Full Join`.

### Colonnes générées

Colonnes calculées `GENERATED ALWAYS AS (expr) STORED` :

```sql
CREATE TABLE products (
  price NUMERIC,
  tax_rate NUMERIC DEFAULT 0.08,
  total NUMERIC GENERATED ALWAYS AS (price * (1 + tax_rate)) STORED
);
```

Les colonnes générées sont recalculées lors des INSERT et UPDATE. Elles ne peuvent pas être définies directement.

### Tri ORDER BY des valeurs nulles

`ORDER BY` suit maintenant par défaut le standard SQL pour le tri des valeurs nulles : `ASC` -> `NULLS LAST`, `DESC` -> `NULLS FIRST`. Les surcharges explicites `NULLS FIRST` / `NULLS LAST` sont supportées.

### Support des séquences

Support complet des séquences compatible PostgreSQL. `CREATE SEQUENCE` et `DROP SEQUENCE` avec options (`INCREMENT BY`, `START WITH`, `MINVALUE`, `MAXVALUE`, `CYCLE`, `IF NOT EXISTS` / `IF EXISTS`). Fonctions de séquence : `nextval()`, `currval()`, `setval()`, `lastval()`.

Les colonnes `SERIAL`, `SMALLSERIAL` et `BIGSERIAL` créent maintenant des séquences de support (nommées `<table>_<column>_seq`), conformément au comportement PostgreSQL. `DROP TABLE` supprime en cascade les séquences possédées. `TRUNCATE` réinitialise les séquences de support. L'état des séquences est entièrement transactionnel — `ROLLBACK` restaure les compteurs de séquence. Les bases de données persistantes sérialisent l'état des séquences dans le catalogue.

Nouvelles commandes SQL : `SHOW SEQUENCES`, `SHOW INDEXES` (tous les index de toutes les tables).

CLI : nouvelles méta-commandes `\ds` (lister les séquences) et `\di` (lister les index).

### Clause USING pour CREATE INDEX

La syntaxe `CREATE INDEX ... USING btree` est maintenant acceptée (btree est la seule méthode supportée). Cela améliore la compatibilité avec le DDL généré par PostgreSQL et les ORM.

### Corrections de bogues

- `ORDER BY` avec valeurs NULL : les comparateurs retournent maintenant 0 quand les deux valeurs sont NULL, corrigeant les résultats de tri non déterministes avec plusieurs clés de tri
- Résolution d'alias `ORDER BY` : les alias SELECT (ex. `SELECT x AS y ... ORDER BY y`) sont maintenant résolus correctement dans les requêtes non groupées, avec et sans fonctions de fenêtre
- Gestion des NULL dans `Type.convert()` : les valeurs NULL passées par la conversion de type (ex. via les paramètres de prepared statements dans UPDATE SET) sont maintenant préservées comme NULL au lieu d'être converties en représentation textuelle du type. Corrigé dans 13 types : TEXT, VARCHAR, CHAR, UUID, TIMESTAMP, DATE, TIME, TIMETZ, INTERVAL, TIMESTAMPTZ, BYTEA, JSON, ENUM
- Avertissement de correspondance exhaustive dans le parseur ORDER BY pour la clause nulls
- Erreurs silencieuses dans le terminal du playground pour les exceptions synchrones

### Mises à jour de versions

Tous les composants mis à jour en 1.4.1 :

| Composant | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.4.1 | -- |
| engine | 1.4.1 | @petradb/engine 1.4.1 |
| client | 1.4.1 | @petradb/client 1.4.1 |
| server | 1.4.1 | @petradb/server 1.4.1 |
| cli | 1.4.1 | @petradb/cli 1.4.1 |
| jdbc | 1.4.1 | -- |
| knex | -- | @petradb/knex 1.4.0 |
| lucid | -- | @petradb/lucid 1.4.0 |
| drizzle | -- | @petradb/drizzle 1.4.1 |

## v1.3-20260309

### DDL transactionnel

Les instructions DDL (CREATE TABLE, CREATE INDEX, DROP TABLE, etc.) sont maintenant entièrement supportées dans les transactions et sont annulées de manière atomique avec le DML. Le DDL et le DML peuvent être librement entrelacés dans un seul bloc BEGIN/COMMIT. MemoryDB et PersistentDB capturent un instantané complet du catalogue au moment du BEGIN et le restaurent lors du ROLLBACK.

### Requêtes relationnelles Drizzle

Support complet des requêtes relationnelles Drizzle ORM (`db.query.*.findMany()`, `db.query.*.findFirst()`). Ajout des fonctions scalaires `json_build_array` et `json_build_object`, et correction de la substitution de paramètres dans les sous-requêtes LATERAL.

### Mises à jour de versions

| Composant | Maven Central | npm |
|-----------|---------------|-----|
| engine | 1.3.1 | @petradb/engine 1.3.3 |
| server | 1.3.1 | @petradb/server 1.3.1 |
| cli | 1.3.1 | @petradb/cli 1.3.1 |
| jdbc | 1.3.1 | -- |
| drizzle | -- | @petradb/drizzle 1.3.1 |

## v1.3-20260308

### Support des schémas

Espaces de noms de schémas style PostgreSQL. Chaque base de données dispose d'un schéma `public` par défaut ; les noms de tables non qualifiés sont résolus dans `public`. Les noms qualifiés par le schéma (`schema.table`) fonctionnent dans toutes les instructions DDL et DML — CREATE TABLE, INSERT, UPDATE, DELETE, SELECT, ALTER TABLE, DROP TABLE, TRUNCATE, CREATE INDEX et COPY.

```sql
CREATE SCHEMA inventory;
CREATE TABLE inventory.products (id SERIAL PRIMARY KEY, name TEXT);
INSERT INTO inventory.products (name) VALUES ('Widget');
SELECT * FROM inventory.products;
```

### Tables virtuelles information_schema

`information_schema.schemata`, `information_schema.tables` et `information_schema.columns` sont maintenant interrogeables. Les tables qualifiées par le schéma rapportent leur `table_schema` correct. Ces vues sont générées dynamiquement à partir des métadonnées de la base de données.

### Migrations Drizzle ORM

Nouvelle fonction `migrate()` dans `@petradb/drizzle` pour appliquer les fichiers de migration Drizzle Kit. Lit le `meta/_journal.json` et exécute les fichiers de migration SQL dans l'ordre, en suivant les migrations appliquées dans `drizzle.__drizzle_migrations`.

```typescript
import { migrate } from "@petradb/drizzle";
await migrate(db, { migrationsFolder: "./drizzle" });
```

### Améliorations des métadonnées JDBC

`DatabaseMetaData.getColumns()` retourne maintenant des valeurs précises pour `COLUMN_SIZE`, `DECIMAL_DIGITS` et `CHAR_OCTET_LENGTH` basées sur le type de colonne et les déclarations de précision/échelle.

### Mises à jour de versions

Tous les composants mis à jour en 1.3.0 :

| Composant | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.3.0 | -- |
| engine | 1.3.0 | @petradb/engine 1.3.0 |
| client | 1.3.0 | @petradb/client 1.3.0 |
| server | 1.3.0 | @petradb/server 1.3.0 |
| cli | 1.3.0 | @petradb/cli 1.3.0 |
| jdbc | 1.3.0 | -- |
| knex | -- | @petradb/knex 1.3.0 |
| lucid | -- | @petradb/lucid 1.3.0 |
| drizzle | -- | @petradb/drizzle 1.3.0 |

## v1.2-20260308

### Réécriture du pilote Drizzle ORM

`@petradb/drizzle` réécrit d'un wrapper `drizzle-orm/pg-proxy` vers un pilote de dialecte PostgreSQL personnalisé étendant directement `PgSession`/`PgPreparedQuery`/`PgTransaction`. Cela donne une parité complète des fonctionnalités avec `drizzle-orm/node-postgres` :

- `db.transaction()` avec commit/rollback automatique
- `tx.rollback()` pour un rollback explicite
- `returning()` sur insert/update/delete (y compris la sélection partielle de colonnes)
- Support des requêtes relationnelles (en attente du support moteur pour `json_build_array`/`json_agg`)

### Coercition de types : paramètres texte vers colonnes NUMERIC

`NumericType.convert` accepte maintenant `TextValue` et l'analyse comme `BigDecimal`, conformément au comportement de coercition existant de `IntegerType`, `BigintType`, `SmallintType` et `DoubleType`. Cela corrige les INSERT/UPDATE paramétrés via les ORM qui envoient les valeurs numériques sous forme de texte (comportement standard du protocole filaire PostgreSQL).

### Logique NULL à trois valeurs

Logique complète à trois valeurs SQL pour la gestion des NULL :

- Les opérateurs de comparaison (`=`, `!=`, `<`, `>`, `<=`, `>=`) retournent NULL quand l'un des opérandes est NULL
- `AND`/`OR` implémentent les tables de vérité à trois valeurs correctes (ex. `FALSE AND NULL` -> `FALSE`, `TRUE OR NULL` -> `TRUE`)
- `IN`/`NOT IN` propagent NULL correctement (ex. `3 NOT IN (1, 2, NULL)` -> inconnu)
- L'arithmétique (`+`, `-`, `*`, `/`, `%`) et la concaténation de chaînes (`||`) propagent NULL
- `LIKE` gère les opérandes NULL

### Grammaire d'expressions unifiée

Les hiérarchies séparées `expression` et `booleanExpression` du parseur SQL ont été fusionnées en une seule syntaxe d'expression. Les opérateurs booléens (`AND`, `OR`, `NOT`) sont maintenant des opérateurs réguliers dans la chaîne de précédence. Cela permet les expressions booléennes partout où une expression est valide (ex. `SELECT a > 5 AND b < 10`).

### Validation anticipée des références de colonnes

Les références de colonnes dans `WHERE`, `GROUP BY`, `HAVING` et `ORDER BY` sont maintenant validées de manière anticipée lors de la construction du plan de requête, détectant les colonnes inexistantes même sur les tables vides ou les tris à une seule ligne. Auparavant, les mauvaises références n'étaient détectées qu'à l'évaluation par ligne, donc les requêtes sur des tables vides réussissaient silencieusement.

### Corrections de bogues

- La contrainte NOT NULL n'était pas appliquée lors des UPDATE
- La contrainte UNIQUE rejetait les NULL multiples (standard SQL : les NULL sont distincts)
- Les colonnes en double dans la liste de colonnes INSERT n'étaient pas détectées
- `SUM`/`AVG`/`MIN`/`MAX` sur une table vide retournait 0 au lieu de NULL
- `LIKE '_'` correspondait à la chaîne vide
- `LIMIT 0` générait une erreur

### Mises à jour de versions

| Composant | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | -- |
| engine | 1.2.9 | @petradb/engine 1.2.16 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | 1.2.6 | @petradb/server 1.2.9 |
| cli | 1.2.8 | @petradb/cli 1.2.9 |
| jdbc | 1.2.13 | -- |
| knex | -- | @petradb/knex 1.2.2 |
| lucid | -- | @petradb/lucid 1.2.1 |
| drizzle | -- | @petradb/drizzle 1.2.2 |

## v1.2-20260307

### SQL : mot-clé `DEFAULT` dans INSERT VALUES

`INSERT INTO t (id, name) VALUES (DEFAULT, 'Alice')` fonctionne maintenant. Le mot-clé standard SQL `DEFAULT` était auparavant rejeté par le parseur, cassant les instructions INSERT générées par les ORM qui passent explicitement `DEFAULT` pour les colonnes serial ou avec valeur par défaut.

### Intégration Drizzle ORM

Nouveau package `@petradb/drizzle` fournissant un pilote [Drizzle ORM](https://orm.drizzle.team) avec une implémentation personnalisée du dialecte PostgreSQL. Supporte les définitions de schémas avec `pgTable`, insert/select/update/delete, clauses returning, `db.transaction()` avec commit/rollback automatique, et les requêtes type-safe. Parité complète des fonctionnalités avec `drizzle-orm/node-postgres`.

### Mises à jour de versions des packages dépendants

Engine, server, cli et jdbc mis à jour pour intégrer la correction du mot-clé DEFAULT.

| Composant | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | -- |
| engine | 1.2.7 | @petradb/engine 1.2.14 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | -- | @petradb/server 1.2.7 |
| cli | -- | @petradb/cli 1.2.7 |
| jdbc | 1.2.11 | -- |
| knex | -- | @petradb/knex 1.2.2 |
| lucid | -- | @petradb/lucid 1.2.1 |
| drizzle | -- | @petradb/drizzle 1.2.0 |

## v1.2-20260306

### API JS : `close()` retourne `Promise<void>`
`Session.close()` retourne maintenant `Promise<void>` au lieu de `void`, correspondant à l'API du module client pour l'interchangeabilité.

### Analyse des timestamps
`parseTimestamp` gère maintenant le suffixe `Z`, les décalages `+/-HH:MM`, les millisecondes et les timestamps séparés par des espaces avec informations de fuseau horaire. Supprime le fuseau horaire vers `LocalDateTime` pour les colonnes `TIMESTAMP`.

### Complétude de la façade JS
`toJS` et `typeString` gèrent maintenant `DateValue`, `TimeValue`, `TimestampTZValue`, `TimeTZValue`, `IntervalValue` et `ByteaValue`.

### SQL : étoile qualifiée (`table.*`)
La syntaxe `SELECT t.*` fonctionne maintenant dans les requêtes, y compris les jointures et les expressions mixtes.

### Coercition de types dans les comparaisons
- `NumberValue` et `TextValue` peuvent maintenant être comparés entre types (paramètres texte vs colonnes numériques et vice versa)
- `TimestampValue` peut maintenant être comparé à `TextValue` en analysant le texte comme un timestamp

### Pilote Knex : liaison de Date
`_sanitizeBindings` convertit les objets JS `Date` en chaînes ISO avant de les passer au moteur, évitant les `DateTimeParseException` sur le format `Date.toString()`.

| Composant | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | -- |
| engine | 1.2.6 | @petradb/engine 1.2.13 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | -- | @petradb/server 1.2.5 |
| cli | -- | @petradb/cli 1.2.5 |
| jdbc | 1.2.10 | -- |
| knex | -- | @petradb/knex 1.2.2 |
| lucid | -- | @petradb/lucid 1.2.1 |

## v1.2-20260305

### Pilote JDBC
- Publication en fat jar — `io.github.edadma:petradb-jdbc` est maintenant un jar unique autonome sur Maven Central
- URLs de connexion claires — `jdbc:petradb:memory`, `jdbc:petradb:file:/path`, `jdbc:petradb://host:port`
- Auto-découverte ServiceLoader — `DriverManager.getConnection()` fonctionne sans `Class.forName`
- Correction des chaînes de version codées en dur dans les métadonnées

### Moteur JS/TS (`@petradb/engine`)
- Ajout de `CreateViewResult`, `DropViewResult`, `ExplainResult`, `CopyResult` à la façade JS
- Ajout de `ExplainResult` et `CopyResult` aux définitions de types TypeScript

### Documentation
- Nouveau guide Knex.js avec exemples complets
- Documentation JDBC : ajout de snippets d'installation Maven/Gradle/sbt, correction du numéro de port

### Infrastructure
- `petradb-shared` maintenant publiable sur Maven Central
- Script de test de fumée post-publication couvrant les artefacts npm, Scala et JDBC

| Composant | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.1 | -- |
| engine | 1.2.2 | @petradb/engine 1.2.5 |
| client | 1.2.3 | @petradb/client 1.2.3 |
| server | -- | @petradb/server 1.2.3 |
| cli | -- | @petradb/cli 1.2.3 |
| jdbc | 1.2.6 | -- |
| knex | -- | @petradb/knex 1.2.0 |

## v1.2.2

### Restructuration du sous-package engine
- Engine déplacé vers le sous-package `io.github.edadma.petradb.engine`
- Nouveau trait partagé `Session` étendu par engine et client

### Support client CLI
- Connexion à un serveur PetraDB distant : `petradb --host localhost --port 5480`
- Options `--user` et `--password` pour l'authentification
- Les méta-commandes fonctionnent via le réseau par SQL

### SQL
- Commande `SHOW VIEWS` retournant les noms et définitions des vues

### Dialecte Knex
- Adaptateur de dialecte `@petradb/knex` pour utiliser le constructeur de requêtes Knex.js avec PetraDB

### Corrections
- Correction de la publication npm du client
- Correction de la publication npm du CLI
- Correction des chaînes de version codées en dur dans les métadonnées JDBC
- Plus de 1013 tests réussis sur JVM, JS et Native

## v1.2

### Pilote JDBC
- Publié sur Maven Central comme `petradb-jdbc`
- `getGeneratedKeys()`, `addBatch()`/`executeBatch()`, métadonnées FK/index pour DBeaver
- Mode fichier (embarqué) et mode serveur (réseau) pour les connexions

### Moteur SQL
- `COPY FROM/TO` pour l'import/export CSV
- `CREATE TEMP TABLE`, `CREATE/DROP VIEW`
- Introspection `SHOW FOREIGN KEYS`/`SHOW INDEXES`
- Optimisation de jointure par boucle imbriquée sur index pour les équijointures
- Migration du parseur vers fastparse

### Serveur
- Support CORS avec configuration TOML
- `max_sessions` configurable, port par défaut 5480
- Plateforme serveur JS avec backend HTTP Node.js

### Client
- Nouveau package npm `@petradb/client` avec façade JS
- Classe `Session` avec `connect()`/`execute()`/`close()` retournant des Promises

### CLI
- Commandes `\timing`, `\copy`
- Historique persistant sur Native

### Build
- Scala 3.8.2, sbt 1.12.4
- 1000 tests réussis sur JVM, JS et Native

## v1.1.0

### TextDB — persistance par fichier texte éditable
Un nouveau backend de stockage qui persiste la base de données sous forme de fichier texte `.ptxt`. Charge en mémoire à l'ouverture et réécrit le fichier après chaque modification.

### Upsert — `ON CONFLICT DO UPDATE`
Sémantique d'insertion ou mise à jour avec la pseudo-table `EXCLUDED`.

### Hiérarchie d'exceptions améliorée
Des classes d'exceptions typées remplacent les appels génériques `problem()`.

### ALTER TABLE centralisé
`DB.alterTable()` centralise maintenant toute la distribution ALTER TABLE.

## v1.0.1

- Renommage de `ConnectSQL` en `Session` dans `@petradb/engine`
- API asynchrone `execute()` retournant `Promise<ExecuteResult[]>`
- Nouveau package `@petradb/client` pour l'utilisation réseau
- Formats de réponse alignés entre engine et serveur

## v1.0.0

Première version stable.

- Moteur SQL multi-plateforme (JVM, JavaScript, Native)
- Syntaxe compatible PostgreSQL
- Stockage en mémoire et persistant (résistant aux pannes)
- DDL, DML, jointures, sous-requêtes, agrégations, transactions
- Opérateurs JSONB, types tableau, contraintes CHECK
- 879 tests réussis

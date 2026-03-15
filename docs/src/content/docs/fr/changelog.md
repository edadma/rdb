---
title: Journal des modifications
---

## v1.5-20260315

### PL/pgSQL -- Procedures stockees, fonctions et declencheurs

Support complet du langage procedural dans les blocs DO, les fonctions stockees et les procedures stockees :

- **Blocs DO** -- blocs PL/pgSQL anonymes avec DECLARE, BEGIN...END
- **Fonctions stockees** -- `CREATE FUNCTION name(params) RETURNS type AS $$ ... $$ LANGUAGE plpgsql`, appelables dans toute expression SQL
- **Procedures stockees** -- `CREATE PROCEDURE name(params) AS $$ ... $$ LANGUAGE plpgsql`, invoquees avec `CALL`
- **Declencheurs** -- `CREATE TRIGGER name BEFORE|AFTER INSERT|UPDATE|DELETE ON table FOR EACH ROW EXECUTE FUNCTION func()`
  - Les declencheurs BEFORE peuvent annuler les operations en retournant NULL
  - Les declencheurs AFTER se declenchent apres l'operation
  - Les declencheurs se declenchent pour INSERT, UPDATE, DELETE et COPY FROM
  - Variables TG_OP, TG_TABLE_NAME, OLD, NEW disponibles
- **Flux de controle** -- IF/ELSIF/ELSE, WHILE LOOP, FOR range/query LOOP, RETURN, RAISE NOTICE/EXCEPTION, PERFORM, EXCEPTION WHEN
- **Persistance** -- les fonctions, procedures et declencheurs survivent a la fermeture/reouverture sur PersistentDB et TextDB
- **OR REPLACE** -- remplacement des fonctions et procedures existantes

### Fonctions natives definies par l'utilisateur

Enregistrement de callbacks du langage hote en tant que fonctions SQL, appelables depuis les requetes, les declencheurs et les procedures :

- **Scala** -- `db.registerScalarFunction("name", { args => result })`
- **JavaScript** -- `session.registerFunction("name", (args) => result)`
- **C** -- `petradb_create_function(db, "name", nargs, user_data, callback)` avec une API de valeurs/contexte typee style SQLite

### Bibliotheque C et API curseur

- **Bibliotheque partagee native** -- `libpetradb-engine.so` via Scala Native avec des fonctions C appelables `@exported`
- **API C style SQLite** -- `petradb_open`, `petradb_exec`, `petradb_prepare/step/finalize`, accesseurs de colonnes types
- **Fonctions definies par l'utilisateur** -- `petradb_value_int/double/text`, `petradb_result_int/double/text/null/error`, `petradb_user_data`
- **API curseur** -- `session.openCursor(sql)` pour une iteration paresseuse ligne par ligne avec `step()`, accesseurs de colonnes types, `fetch(n)`, `move(n)`, requetes parametrees
- **En-tete C** -- `petradb.h` avec documentation complete de l'API
- **Suite de tests C** -- 67 tests
- **Test FFI Rust** -- 38 tests prouvant l'interoperabilite entre langages

### Tables virtuelles

- **Framework extensible** -- `CREATE VIRTUAL TABLE name USING module(args)`, lecture seule, apparait dans SHOW TABLES
- **Module CSV integre** -- `CREATE VIRTUAL TABLE t USING csv('file.csv')` avec options d'en-tete/delimiteur
- **Modules personnalises** -- `db.registerVirtualTableModule("name", module)` dans l'API Scala

### Fonction de table csv_file()

Interrogez des fichiers CSV directement sans importation :

```sql
SELECT * FROM csv_file('data.csv');
SELECT e.name, d.dept FROM csv_file('employees.csv') e
  JOIN csv_file('departments.csv') d ON e.dept_id = d.id;
```

### Index avances

- **Index partiels** -- `CREATE INDEX ... WHERE condition` -- indexe uniquement les lignes correspondant au predicat
- **Index sur expression** -- `CREATE INDEX ... ON table ((expr))` -- indexe des valeurs calculees comme `lower(email)`
- **Combines** -- les index partiels et sur expression fonctionnent ensemble

### Fonctions de fenetre

- **FIRST_VALUE(expr)** -- valeur a la premiere ligne du cadre de fenetre
- **LAST_VALUE(expr)** -- valeur a la derniere ligne du cadre de fenetre
- **NTH_VALUE(expr, n)** -- valeur a la n-ieme ligne du cadre

### DELETE ... USING

Suppressions multi-tables selon la syntaxe PostgreSQL :

```sql
DELETE FROM orders USING customers
WHERE orders.customer_id = customers.id AND customers.status = 'inactive';
```

### Quarry -- Constructeur de requetes type-safe par AST

Nouveau package `@petradb/quarry` : constructeur de requetes type-safe qui genere des objets AST (pas des chaines SQL) :

- Definition de schema avec 21 types de colonnes
- CRUD complet : select, insert, update, delete avec references de colonnes type-safe
- Jointures : inner, left, right, full outer, cross avec resultats types
- Expressions : 50+ operateurs, aggregats, CASE/CAST/EXISTS, sous-requetes
- Upsert : `onConflictDoNothing()`, `onConflictDoUpdate()`
- Alias de tables pour les auto-jointures
- Transactions, RETURNING, DISTINCT ON
- Tests de types a la compilation pour toutes les fonctionnalites
- Operations ensemblistes : UNION, INTERSECT, EXCEPT
- Fonctions de fenetre, CTE, helpers scalaires nommes

### Corrections de bogues

- **ByteaValue** -- `ARRAY[...]` dans les colonnes BYTEA produit maintenant correctement `ByteaValue` au lieu de `ArrayValue`
- **Types de resultats JS/Client** -- ajout des gestionnaires de types de resultats PL/pgSQL manquants (DoBlockResult, CreateFunctionResult, etc.) pour eviter les crashs de correspondance non exhaustive
- **Codecs** -- ajout de la serialisation pour tous les nouveaux types de resultats pour la communication client/serveur
- **llms.txt** -- correction du champ `type` en champ `command`, mise a jour de tous les types de resultats et fonctionnalites

### Renommage de module

- **shared -> common** -- le module de types partages a ete renomme de `petradb-shared` a `petradb-common`

### Documentation

- Nouvelle page de reference **PL/pgSQL** (declencheurs, fonctions, procedures, flux de controle)
- Nouvelle page de reference **API C** (interface complete style SQLite)
- Nouveaux guides **Premiers pas** pour Java (JDBC) et C
- Documentation DDL mise a jour : index partiels/sur expression, contraintes CHECK, declencheurs, routines stockees
- Documentation DML mise a jour : DELETE...USING, csv_file(), tables virtuelles
- Documentation API JS/Scala mise a jour : registerFunction, types de resultats
- Page d'accueil : quatre boutons de demarrage (JS, Java, Scala, C)
- llms.txt reecrit avec toutes les fonctionnalites actuelles

### Mises a jour de versions

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

### Corrections de bogues et ameliorations

- **Correction des sous-requetes IN correlees** -- les sous-requetes correlees `IN (SELECT ...)` avec tables indexees fonctionnent maintenant correctement
- **Resolution de colonnes qualifiees** -- corrections pour les references de colonnes ambigues dans les jointures complexes
- **Gestion de IN/ANY vides** -- les cas limites `IN ()` et `= ANY('{}')` sont resolus
- **Nettoyage CASCADE des cles etrangeres** -- `DROP TABLE ... CASCADE` supprime maintenant correctement les contraintes FK sur les tables filles
- **DROP TABLE IF EXISTS ... CASCADE** -- la combinaison de `IF EXISTS` avec `CASCADE` ne provoque plus d'erreur de syntaxe
- **Conversion de litteraux tableau** -- support de la syntaxe PostgreSQL `'{1,2,3}'::integer[]` pour les litteraux tableau
- **Corrections des requetes parametrees** -- amelioration de la liaison de parametres pour les sous-requetes et les chemins correles

### Mises a jour de versions

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

**CTE non recursives** -- sous-requetes nommees pour la lisibilite et la reutilisation :

```sql
WITH active_orders AS (
  SELECT * FROM orders WHERE status = 'active'
)
SELECT customer_id, SUM(amount)
FROM active_orders
GROUP BY customer_id;
```

Plusieurs CTE peuvent etre definies dans une seule requete, et les CTE suivantes peuvent referencer les precedentes. Les alias de colonnes sont supportes : `WITH t(x, y) AS (...)`. Les CTE masquent les noms de tables s'ils partagent le meme nom.

**CTE recursives** -- requetes iteratives pour les donnees hierarchiques et les graphes :

```sql
WITH RECURSIVE descendants(id, name, depth) AS (
  SELECT id, name, 0 FROM employees WHERE manager_id IS NULL
  UNION ALL
  SELECT e.id, e.name, d.depth + 1
  FROM employees e INNER JOIN descendants d ON e.manager_id = d.id
)
SELECT name, depth FROM descendants ORDER BY depth, name;
```

`UNION ALL` (conserve les doublons) et `UNION` (deduplique) sont tous deux supportes. Limite de securite de 1000 iterations maximum.

### Fonctions de fenetre

Support complet des fonctions de fenetre en trois categories :

**Fonctions de classement** -- `ROW_NUMBER()`, `RANK()`, `DENSE_RANK()` avec `PARTITION BY` et `ORDER BY` :

```sql
SELECT name, department, salary,
  RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dept_rank
FROM employees;
```

**Fonctions de valeur** -- `LAG()`, `LEAD()`, `NTILE()` avec decalage et valeurs par defaut configurables :

```sql
SELECT name, salary,
  LAG(salary, 1, 0) OVER (ORDER BY salary) AS prev_salary,
  NTILE(4) OVER (ORDER BY salary) AS quartile
FROM employees;
```

**Fonctions de fenetre avec agregats** -- tout agregat (`SUM`, `COUNT`, `AVG`, `MIN`, `MAX`, etc.) avec `OVER()`, y compris les specifications de cadre :

```sql
SELECT name, salary,
  SUM(salary) OVER (ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_total,
  AVG(salary) OVER (PARTITION BY department) AS dept_avg
FROM employees;
```

Bornes de cadre : `UNBOUNDED PRECEDING`, `UNBOUNDED FOLLOWING`, `CURRENT ROW`, `N PRECEDING`, `N FOLLOWING`. Sans clause de cadre, les fonctions de fenetre avec agregats calculent sur l'ensemble de la partition.

### Clause FILTER sur les agregats

`FILTER (WHERE ...)` sur les fonctions d'agregation, a la fois dans les requetes groupees et les fonctions de fenetre :

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE status = 'active') AS active_count,
  SUM(amount) FILTER (WHERE amount > 100) OVER (ORDER BY created_at
    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_high_value
FROM orders;
```

### Optimisation par jointure de hachage

Les equijointures sans index utilisent maintenant une strategie de jointure par hachage au lieu d'un produit cartesien, reduisant la complexite de la jointure de O(n*m) a O(n+m). S'applique aux jointures INNER, LEFT, RIGHT et FULL. Les jointures par boucle imbriquee sur index restent preferees quand un index est disponible. Les conditions de jointure non equi reviennent au produit cartesien. Visible dans la sortie `EXPLAIN` comme `Hash Join`, `Hash Left Join`, `Hash Right Join`, `Hash Full Join`.

### Colonnes generees

Colonnes calculees `GENERATED ALWAYS AS (expr) STORED` :

```sql
CREATE TABLE products (
  price NUMERIC,
  tax_rate NUMERIC DEFAULT 0.08,
  total NUMERIC GENERATED ALWAYS AS (price * (1 + tax_rate)) STORED
);
```

Les colonnes generees sont recalculees lors des INSERT et UPDATE. Elles ne peuvent pas etre definies directement.

### Tri ORDER BY des valeurs nulles

`ORDER BY` suit maintenant par defaut le standard SQL pour le tri des valeurs nulles : `ASC` -> `NULLS LAST`, `DESC` -> `NULLS FIRST`. Les surcharges explicites `NULLS FIRST` / `NULLS LAST` sont supportees.

### Support des sequences

Support complet des sequences compatible PostgreSQL. `CREATE SEQUENCE` et `DROP SEQUENCE` avec options (`INCREMENT BY`, `START WITH`, `MINVALUE`, `MAXVALUE`, `CYCLE`, `IF NOT EXISTS` / `IF EXISTS`). Fonctions de sequence : `nextval()`, `currval()`, `setval()`, `lastval()`.

Les colonnes `SERIAL`, `SMALLSERIAL` et `BIGSERIAL` creent maintenant des sequences de support (nommees `<table>_<column>_seq`), conformement au comportement PostgreSQL. `DROP TABLE` supprime en cascade les sequences possedees. `TRUNCATE` reinitialise les sequences de support. L'etat des sequences est entierement transactionnel -- `ROLLBACK` restaure les compteurs de sequence. Les bases de donnees persistantes serialisent l'etat des sequences dans le catalogue.

Nouvelles commandes SQL : `SHOW SEQUENCES`, `SHOW INDEXES` (tous les index de toutes les tables).

CLI : nouvelles meta-commandes `\ds` (lister les sequences) et `\di` (lister les index).

### Clause USING pour CREATE INDEX

La syntaxe `CREATE INDEX ... USING btree` est maintenant acceptee (btree est la seule methode supportee). Cela ameliore la compatibilite avec le DDL genere par PostgreSQL et les ORM.

### Corrections de bogues

- `ORDER BY` avec valeurs NULL : les comparateurs retournent maintenant 0 quand les deux valeurs sont NULL, corrigeant les resultats de tri non deterministes avec plusieurs cles de tri
- Resolution d'alias `ORDER BY` : les alias SELECT (ex. `SELECT x AS y ... ORDER BY y`) sont maintenant resolus correctement dans les requetes non groupees, avec et sans fonctions de fenetre
- Gestion des NULL dans `Type.convert()` : les valeurs NULL passees par la conversion de type (ex. via les parametres de prepared statements dans UPDATE SET) sont maintenant preservees comme NULL au lieu d'etre converties en representation textuelle du type. Corrige dans 13 types : TEXT, VARCHAR, CHAR, UUID, TIMESTAMP, DATE, TIME, TIMETZ, INTERVAL, TIMESTAMPTZ, BYTEA, JSON, ENUM
- Avertissement de correspondance exhaustive dans le parseur ORDER BY pour la clause nulls
- Erreurs silencieuses dans le terminal du playground pour les exceptions synchrones

### Mises a jour de versions

Tous les composants mis a jour en 1.4.1 :

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

Les instructions DDL (CREATE TABLE, CREATE INDEX, DROP TABLE, etc.) sont maintenant entierement supportees dans les transactions et sont annulees de maniere atomique avec le DML. Le DDL et le DML peuvent etre librement entrelaces dans un seul bloc BEGIN/COMMIT. MemoryDB et PersistentDB capturent un instantane complet du catalogue au moment du BEGIN et le restaurent lors du ROLLBACK.

### Requetes relationnelles Drizzle

Support complet des requetes relationnelles Drizzle ORM (`db.query.*.findMany()`, `db.query.*.findFirst()`). Ajout des fonctions scalaires `json_build_array` et `json_build_object`, et correction de la substitution de parametres dans les sous-requetes LATERAL.

### Mises a jour de versions

| Composant | Maven Central | npm |
|-----------|---------------|-----|
| engine | 1.3.1 | @petradb/engine 1.3.3 |
| server | 1.3.1 | @petradb/server 1.3.1 |
| cli | 1.3.1 | @petradb/cli 1.3.1 |
| jdbc | 1.3.1 | -- |
| drizzle | -- | @petradb/drizzle 1.3.1 |

## v1.3-20260308

### Support des schemas

Espaces de noms de schemas style PostgreSQL. Chaque base de donnees dispose d'un schema `public` par defaut ; les noms de tables non qualifies sont resolus dans `public`. Les noms qualifies par le schema (`schema.table`) fonctionnent dans toutes les instructions DDL et DML -- CREATE TABLE, INSERT, UPDATE, DELETE, SELECT, ALTER TABLE, DROP TABLE, TRUNCATE, CREATE INDEX et COPY.

```sql
CREATE SCHEMA inventory;
CREATE TABLE inventory.products (id SERIAL PRIMARY KEY, name TEXT);
INSERT INTO inventory.products (name) VALUES ('Widget');
SELECT * FROM inventory.products;
```

### Tables virtuelles information_schema

`information_schema.schemata`, `information_schema.tables` et `information_schema.columns` sont maintenant interrogeables. Les tables qualifiees par le schema rapportent leur `table_schema` correct. Ces vues sont generees dynamiquement a partir des metadonnees de la base de donnees.

### Migrations Drizzle ORM

Nouvelle fonction `migrate()` dans `@petradb/drizzle` pour appliquer les fichiers de migration Drizzle Kit. Lit le `meta/_journal.json` et execute les fichiers de migration SQL dans l'ordre, en suivant les migrations appliquees dans `drizzle.__drizzle_migrations`.

```typescript
import { migrate } from "@petradb/drizzle";
await migrate(db, { migrationsFolder: "./drizzle" });
```

### Ameliorations des metadonnees JDBC

`DatabaseMetaData.getColumns()` retourne maintenant des valeurs precises pour `COLUMN_SIZE`, `DECIMAL_DIGITS` et `CHAR_OCTET_LENGTH` basees sur le type de colonne et les declarations de precision/echelle.

### Mises a jour de versions

Tous les composants mis a jour en 1.3.0 :

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

### Reecriture du pilote Drizzle ORM

`@petradb/drizzle` reecrit d'un wrapper `drizzle-orm/pg-proxy` vers un pilote de dialecte PostgreSQL personnalise etendant directement `PgSession`/`PgPreparedQuery`/`PgTransaction`. Cela donne une parite complete des fonctionnalites avec `drizzle-orm/node-postgres` :

- `db.transaction()` avec commit/rollback automatique
- `tx.rollback()` pour un rollback explicite
- `returning()` sur insert/update/delete (y compris la selection partielle de colonnes)
- Support des requetes relationnelles (en attente du support moteur pour `json_build_array`/`json_agg`)

### Coercition de types : parametres texte vers colonnes NUMERIC

`NumericType.convert` accepte maintenant `TextValue` et l'analyse comme `BigDecimal`, conformement au comportement de coercition existant de `IntegerType`, `BigintType`, `SmallintType` et `DoubleType`. Cela corrige les INSERT/UPDATE parametres via les ORM qui envoient les valeurs numeriques sous forme de texte (comportement standard du protocole filaire PostgreSQL).

### Logique NULL a trois valeurs

Logique complete a trois valeurs SQL pour la gestion des NULL :

- Les operateurs de comparaison (`=`, `!=`, `<`, `>`, `<=`, `>=`) retournent NULL quand l'un des operandes est NULL
- `AND`/`OR` implementent les tables de verite a trois valeurs correctes (ex. `FALSE AND NULL` -> `FALSE`, `TRUE OR NULL` -> `TRUE`)
- `IN`/`NOT IN` propagent NULL correctement (ex. `3 NOT IN (1, 2, NULL)` -> inconnu)
- L'arithmetique (`+`, `-`, `*`, `/`, `%`) et la concatenation de chaines (`||`) propagent NULL
- `LIKE` gere les operandes NULL

### Grammaire d'expressions unifiee

Les hierarchies separees `expression` et `booleanExpression` du parseur SQL ont ete fusionnees en une seule syntaxe d'expression. Les operateurs booleens (`AND`, `OR`, `NOT`) sont maintenant des operateurs reguliers dans la chaine de precedence. Cela permet les expressions booleennes partout ou une expression est valide (ex. `SELECT a > 5 AND b < 10`).

### Validation anticipee des references de colonnes

Les references de colonnes dans `WHERE`, `GROUP BY`, `HAVING` et `ORDER BY` sont maintenant validees de maniere anticipee lors de la construction du plan de requete, detectant les colonnes inexistantes meme sur les tables vides ou les tris a une seule ligne. Auparavant, les mauvaises references n'etaient detectees qu'a l'evaluation par ligne, donc les requetes sur des tables vides reussissaient silencieusement.

### Corrections de bogues

- La contrainte NOT NULL n'etait pas appliquee lors des UPDATE
- La contrainte UNIQUE rejetait les NULL multiples (standard SQL : les NULL sont distincts)
- Les colonnes en double dans la liste de colonnes INSERT n'etaient pas detectees
- `SUM`/`AVG`/`MIN`/`MAX` sur une table vide retournait 0 au lieu de NULL
- `LIKE '_'` correspondait a la chaine vide
- `LIMIT 0` generait une erreur

### Mises a jour de versions

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

### SQL : mot-cle `DEFAULT` dans INSERT VALUES

`INSERT INTO t (id, name) VALUES (DEFAULT, 'Alice')` fonctionne maintenant. Le mot-cle standard SQL `DEFAULT` etait auparavant rejete par le parseur, cassant les instructions INSERT generees par les ORM qui passent explicitement `DEFAULT` pour les colonnes serial ou avec valeur par defaut.

### Integration Drizzle ORM

Nouveau package `@petradb/drizzle` fournissant un pilote [Drizzle ORM](https://orm.drizzle.team) avec une implementation personnalisee du dialecte PostgreSQL. Supporte les definitions de schemas avec `pgTable`, insert/select/update/delete, clauses returning, `db.transaction()` avec commit/rollback automatique, et les requetes type-safe. Parite complete des fonctionnalites avec `drizzle-orm/node-postgres`.

### Mises a jour de versions des packages dependants

Engine, server, cli et jdbc mis a jour pour integrer la correction du mot-cle DEFAULT.

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
`Session.close()` retourne maintenant `Promise<void>` au lieu de `void`, correspondant a l'API du module client pour l'interchangeabilite.

### Analyse des timestamps
`parseTimestamp` gere maintenant le suffixe `Z`, les decalages `+/-HH:MM`, les millisecondes et les timestamps separes par des espaces avec informations de fuseau horaire. Supprime le fuseau horaire vers `LocalDateTime` pour les colonnes `TIMESTAMP`.

### Completude de la facade JS
`toJS` et `typeString` gerent maintenant `DateValue`, `TimeValue`, `TimestampTZValue`, `TimeTZValue`, `IntervalValue` et `ByteaValue`.

### SQL : etoile qualifiee (`table.*`)
La syntaxe `SELECT t.*` fonctionne maintenant dans les requetes, y compris les jointures et les expressions mixtes.

### Coercition de types dans les comparaisons
- `NumberValue` et `TextValue` peuvent maintenant etre compares entre types (parametres texte vs colonnes numeriques et vice versa)
- `TimestampValue` peut maintenant etre compare a `TextValue` en analysant le texte comme un timestamp

### Pilote Knex : liaison de Date
`_sanitizeBindings` convertit les objets JS `Date` en chaines ISO avant de les passer au moteur, evitant les `DateTimeParseException` sur le format `Date.toString()`.

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
- Publication en fat jar -- `io.github.edadma:petradb-jdbc` est maintenant un jar unique autonome sur Maven Central
- URLs de connexion claires -- `jdbc:petradb:memory`, `jdbc:petradb:file:/path`, `jdbc:petradb://host:port`
- Auto-decouverte ServiceLoader -- `DriverManager.getConnection()` fonctionne sans `Class.forName`
- Correction des chaines de version codees en dur dans les metadonnees

### Moteur JS/TS (`@petradb/engine`)
- Ajout de `CreateViewResult`, `DropViewResult`, `ExplainResult`, `CopyResult` a la facade JS
- Ajout de `ExplainResult` et `CopyResult` aux definitions de types TypeScript

### Documentation
- Nouveau guide Knex.js avec exemples complets
- Documentation JDBC : ajout de snippets d'installation Maven/Gradle/sbt, correction du numero de port

### Infrastructure
- `petradb-shared` maintenant publiable sur Maven Central
- Script de test de fumee post-publication couvrant les artefacts npm, Scala et JDBC

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
- Engine deplace vers le sous-package `io.github.edadma.petradb.engine`
- Nouveau trait partage `Session` etendu par engine et client

### Support client CLI
- Connexion a un serveur PetraDB distant : `petradb --host localhost --port 5480`
- Options `--user` et `--password` pour l'authentification
- Les meta-commandes fonctionnent via le reseau par SQL

### SQL
- Commande `SHOW VIEWS` retournant les noms et definitions des vues

### Dialecte Knex
- Adaptateur de dialecte `@petradb/knex` pour utiliser le constructeur de requetes Knex.js avec PetraDB

### Corrections
- Correction de la publication npm du client
- Correction de la publication npm du CLI
- Correction des chaines de version codees en dur dans les metadonnees JDBC
- Plus de 1013 tests reussis sur JVM, JS et Native

## v1.2

### Pilote JDBC
- Publie sur Maven Central comme `petradb-jdbc`
- `getGeneratedKeys()`, `addBatch()`/`executeBatch()`, metadonnees FK/index pour DBeaver
- Mode fichier (embarque) et mode serveur (reseau) pour les connexions

### Moteur SQL
- `COPY FROM/TO` pour l'import/export CSV
- `CREATE TEMP TABLE`, `CREATE/DROP VIEW`
- Introspection `SHOW FOREIGN KEYS`/`SHOW INDEXES`
- Optimisation de jointure par boucle imbriquee sur index pour les equijointures
- Migration du parseur vers fastparse

### Serveur
- Support CORS avec configuration TOML
- `max_sessions` configurable, port par defaut 5480
- Plateforme serveur JS avec backend HTTP Node.js

### Client
- Nouveau package npm `@petradb/client` avec facade JS
- Classe `Session` avec `connect()`/`execute()`/`close()` retournant des Promises

### CLI
- Commandes `\timing`, `\copy`
- Historique persistant sur Native

### Build
- Scala 3.8.2, sbt 1.12.4
- 1000 tests reussis sur JVM, JS et Native

## v1.1.0

### TextDB -- persistance par fichier texte editable
Un nouveau backend de stockage qui persiste la base de donnees sous forme de fichier texte `.ptxt`. Charge en memoire a l'ouverture et reecrit le fichier apres chaque modification.

### Upsert -- `ON CONFLICT DO UPDATE`
Semantique d'insertion ou mise a jour avec la pseudo-table `EXCLUDED`.

### Hierarchie d'exceptions amelioree
Des classes d'exceptions typees remplacent les appels generiques `problem()`.

### ALTER TABLE centralise
`DB.alterTable()` centralise maintenant toute la distribution ALTER TABLE.

## v1.0.1

- Renommage de `ConnectSQL` en `Session` dans `@petradb/engine`
- API asynchrone `execute()` retournant `Promise<ExecuteResult[]>`
- Nouveau package `@petradb/client` pour l'utilisation reseau
- Formats de reponse alignes entre engine et serveur

## v1.0.0

Premiere version stable.

- Moteur SQL multi-plateforme (JVM, JavaScript, Native)
- Syntaxe compatible PostgreSQL
- Stockage en memoire et persistant (resistant aux pannes)
- DDL, DML, jointures, sous-requetes, agregations, transactions
- Operateurs JSONB, types tableau, contraintes CHECK
- 879 tests reussis

---
title: PL/pgSQL
description: Langage procédural — blocs DO, fonctions stockées et procédures stockées.
---

PetraDB supporte PL/pgSQL, le langage procédural de PostgreSQL, pour écrire de la logique de flux de contrôle, des boucles et l'exécution SQL conditionnelle.

## Blocs DO

Blocs anonymes qui s'exécutent immédiatement sans être stockés :

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

## Fonctions stockées

Les fonctions retournent une valeur et peuvent être appelées dans toute expression SQL :

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

Utilisez `CREATE OR REPLACE FUNCTION` pour remplacer une fonction existante.

### Utilisation des fonctions dans les requêtes

Les fonctions fonctionnent partout où les expressions sont autorisées :

```sql
SELECT name, classify(score) AS grade FROM students;
SELECT * FROM orders WHERE is_valid(status);
INSERT INTO logs VALUES (format_msg(code, detail));
```

## Procédures stockées

Les procédures effectuent des actions et sont invoquées avec `CALL` :

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

Utilisez `CREATE OR REPLACE PROCEDURE` pour remplacer une procédure existante.

## DROP

```sql
DROP FUNCTION factorial;
DROP FUNCTION IF EXISTS factorial;
DROP PROCEDURE seed_users;
DROP PROCEDURE IF EXISTS seed_users;
```

## Structure de bloc

Tous les blocs PL/pgSQL (blocs DO, corps de fonctions, corps de procédures) partagent la même structure :

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

Déclarez les variables avec un type et une valeur par défaut optionnelle :

```sql
DECLARE
  x INT := 0;
  name TEXT;
  total NUMERIC := 100.50;
```

Les variables sans valeur par défaut sont initialisées à `NULL`. L'affectation utilise `:=` :

```sql
x := x + 1;
name := 'Alice';
total := (SELECT SUM(amount) FROM orders);
```

Les variables peuvent être utilisées dans toute instruction SQL au sein du bloc — dans `VALUES`, `WHERE`, `SET`, etc.

## Flux de contrôle

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

### Boucle WHILE

```sql
WHILE balance > 0 LOOP
  balance := balance - payment;
  month := month + 1;
END LOOP;
```

Limite de sécurité de 10 000 itérations maximum.

### Boucle FOR avec plage

```sql
FOR i IN 1..10 LOOP
  INSERT INTO numbers VALUES (i);
END LOOP;
```

La variable de boucle doit être déclarée dans `DECLARE`. Les deux bornes sont inclusives.

### Boucle FOR avec requête

Itérez sur les résultats d'une requête :

```sql
FOR name IN SELECT name FROM users ORDER BY id LOOP
  INSERT INTO greetings VALUES ('Hello, ' || name);
END LOOP;
```

Pour les requêtes à colonne unique, la variable reçoit la valeur scalaire. Pour les requêtes à colonnes multiples, la variable reçoit un enregistrement.

## RETURN

Sortir d'un bloc ou retourner une valeur depuis une fonction :

```sql
-- Dans une fonction :
RETURN x * 2;

-- Dans un bloc DO ou une procédure (sortie anticipée) :
RETURN;
```

## RAISE

Afficher des messages ou lancer des erreurs :

```sql
RAISE NOTICE 'Processing row %', row_id;
RAISE EXCEPTION 'Invalid input: %', value;
```

Les placeholders `%` sont remplis par les valeurs des arguments dans l'ordre. `RAISE EXCEPTION` interrompt l'exécution.

## PERFORM

Exécuter une requête et ignorer le résultat :

```sql
PERFORM SELECT notify_user(user_id);
```

## Gestion des exceptions

Capturer les erreurs au sein d'un bloc :

```sql
DO $$
BEGIN
  CREATE TABLE t (id INT);
EXCEPTION
  WHEN duplicate_object THEN
    NULL;  -- la table existe déjà, ignorer
END $$;
```

Conditions d'exception :
- `duplicate_object` — table/type/contrainte existe déjà
- `unique_violation` — contrainte d'unicité violée
- `others` — capture toute exception

## Instruction NULL

Une instruction sans effet, couramment utilisée dans les gestionnaires d'exceptions :

```sql
EXCEPTION
  WHEN others THEN NULL;
```

## Déclencheurs

Les déclencheurs exécutent une fonction automatiquement lorsque des lignes sont insérées, mises à jour ou supprimées :

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

### Syntaxe

```sql
CREATE TRIGGER name BEFORE|AFTER INSERT|UPDATE|DELETE
  ON table FOR EACH ROW EXECUTE FUNCTION function_name();

DROP TRIGGER name ON table;
DROP TRIGGER IF EXISTS name ON table;
```

### Chronologie

- Les déclencheurs **BEFORE** s'exécutent avant l'opération. Retournez `NULL` pour annuler l'opération sur la ligne. Retournez toute valeur non-NULL pour continuer.
- Les déclencheurs **AFTER** s'exécutent après l'opération. La valeur de retour est ignorée.

### Variables spéciales

Les fonctions déclencheur ont accès à :

| Variable | Description |
|----------|-------------|
| `tg_op` | Nom de l'opération : `'INSERT'`, `'UPDATE'` ou `'DELETE'` |
| `tg_table_name` | Nom de la table qui a déclenché le trigger |
| `OLD` | Ligne avant l'opération (UPDATE, DELETE) |
| `NEW` | Ligne après l'opération (INSERT, UPDATE) |

### Événements

Les déclencheurs se déclenchent pour :
- `INSERT` — y compris les lignes insérées via `COPY FROM`
- `UPDATE` — se déclenche par ligne mise à jour
- `DELETE` — se déclenche par ligne supprimée

### Exemple de déclencheur de garde

```sql
CREATE FUNCTION prevent_delete() RETURNS INT AS $$
BEGIN
  RETURN NULL;  -- annuler le DELETE
END $$ LANGUAGE plpgsql;

CREATE TRIGGER trg_protect BEFORE DELETE ON important_data
  FOR EACH ROW EXECUTE FUNCTION prevent_delete();
```

### Callbacks de fonctions natives

Les fonctions natives enregistrées (Scala, JavaScript ou C) sont appelables depuis les fonctions déclencheur, permettant l'intégration avec des systèmes externes :

```sql
-- En supposant que notify_webhook() est enregistrée comme fonction native
CREATE FUNCTION on_order() RETURNS INT AS $$
DECLARE dummy INT;
BEGIN
  dummy := notify_webhook(tg_op);
  RETURN 0;
END $$ LANGUAGE plpgsql;

CREATE TRIGGER trg AFTER INSERT ON orders
  FOR EACH ROW EXECUTE FUNCTION on_order();
```

Consultez l'[API Scala](/reference/api-scala/), l'[API JavaScript](/reference/api-javascript/) et l'[API C](/reference/api-c/) pour l'enregistrement de fonctions natives.

## Persistance

Les fonctions stockées, les procédures et les déclencheurs persistent entre les redémarrages de la base de données, tant pour le stockage en mémoire (durée de la session) que pour le stockage persistant (survit à la fermeture/réouverture). Le SQL source est stocké et ré-exécuté à l'ouverture de la base de données.

## Composition

Les fonctions peuvent appeler d'autres fonctions. Les procédures peuvent appeler des fonctions :

```sql
CREATE FUNCTION double(x INT) RETURNS INT AS $$
BEGIN RETURN x * 2; END $$ LANGUAGE plpgsql;

CREATE FUNCTION quadruple(x INT) RETURNS INT AS $$
BEGIN RETURN double(double(x)); END $$ LANGUAGE plpgsql;

SELECT quadruple(5);  -- 20
```

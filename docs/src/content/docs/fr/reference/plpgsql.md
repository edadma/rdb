---
title: PL/pgSQL
description: Langage procedural -- blocs DO, fonctions stockees et procedures stockees.
---

PetraDB supporte PL/pgSQL, le langage procedural de PostgreSQL, pour ecrire de la logique de flux de controle, des boucles et l'execution SQL conditionnelle.

## Blocs DO

Blocs anonymes qui s'executent immediatement sans etre stockes :

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

## Fonctions stockees

Les fonctions retournent une valeur et peuvent etre appelees dans toute expression SQL :

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

### Utilisation des fonctions dans les requetes

Les fonctions fonctionnent partout ou les expressions sont autorisees :

```sql
SELECT name, classify(score) AS grade FROM students;
SELECT * FROM orders WHERE is_valid(status);
INSERT INTO logs VALUES (format_msg(code, detail));
```

## Procedures stockees

Les procedures effectuent des actions et sont invoquees avec `CALL` :

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

Utilisez `CREATE OR REPLACE PROCEDURE` pour remplacer une procedure existante.

## DROP

```sql
DROP FUNCTION factorial;
DROP FUNCTION IF EXISTS factorial;
DROP PROCEDURE seed_users;
DROP PROCEDURE IF EXISTS seed_users;
```

## Structure de bloc

Tous les blocs PL/pgSQL (blocs DO, corps de fonctions, corps de procedures) partagent la meme structure :

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

Declarez les variables avec un type et une valeur par defaut optionnelle :

```sql
DECLARE
  x INT := 0;
  name TEXT;
  total NUMERIC := 100.50;
```

Les variables sans valeur par defaut sont initialisees a `NULL`. L'affectation utilise `:=` :

```sql
x := x + 1;
name := 'Alice';
total := (SELECT SUM(amount) FROM orders);
```

Les variables peuvent etre utilisees dans toute instruction SQL au sein du bloc -- dans `VALUES`, `WHERE`, `SET`, etc.

## Flux de controle

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

Limite de securite de 10 000 iterations maximum.

### Boucle FOR avec plage

```sql
FOR i IN 1..10 LOOP
  INSERT INTO numbers VALUES (i);
END LOOP;
```

La variable de boucle doit etre declaree dans `DECLARE`. Les deux bornes sont inclusives.

### Boucle FOR avec requete

Iterez sur les resultats d'une requete :

```sql
FOR name IN SELECT name FROM users ORDER BY id LOOP
  INSERT INTO greetings VALUES ('Hello, ' || name);
END LOOP;
```

Pour les requetes a colonne unique, la variable recoit la valeur scalaire. Pour les requetes a colonnes multiples, la variable recoit un enregistrement.

## RETURN

Sortir d'un bloc ou retourner une valeur depuis une fonction :

```sql
-- Dans une fonction :
RETURN x * 2;

-- Dans un bloc DO ou une procedure (sortie anticipee) :
RETURN;
```

## RAISE

Afficher des messages ou lancer des erreurs :

```sql
RAISE NOTICE 'Processing row %', row_id;
RAISE EXCEPTION 'Invalid input: %', value;
```

Les placeholders `%` sont remplaces par les valeurs des arguments dans l'ordre. `RAISE EXCEPTION` interrompt l'execution.

## PERFORM

Executer une requete et ignorer le resultat :

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
    NULL;  -- la table existe deja, ignorer
END $$;
```

Conditions d'exception :
- `duplicate_object` -- table/type/contrainte existe deja
- `unique_violation` -- contrainte d'unicite violee
- `others` -- capture toute exception

## Instruction NULL

Une instruction sans effet, couramment utilisee dans les gestionnaires d'exceptions :

```sql
EXCEPTION
  WHEN others THEN NULL;
```

## Declencheurs

Les declencheurs executent une fonction automatiquement lorsque des lignes sont inserees, mises a jour ou supprimees :

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

- Les declencheurs **BEFORE** s'executent avant l'operation. Retournez `NULL` pour annuler l'operation sur la ligne. Retournez toute valeur non-NULL pour continuer.
- Les declencheurs **AFTER** s'executent apres l'operation. La valeur de retour est ignoree.

### Variables speciales

Les fonctions declencheur ont acces a :

| Variable | Description |
|----------|-------------|
| `tg_op` | Nom de l'operation : `'INSERT'`, `'UPDATE'` ou `'DELETE'` |
| `tg_table_name` | Nom de la table qui a declenche le trigger |
| `OLD` | Ligne avant l'operation (UPDATE, DELETE) |
| `NEW` | Ligne apres l'operation (INSERT, UPDATE) |

### Evenements

Les declencheurs se declenchent pour :
- `INSERT` -- y compris les lignes inserees via `COPY FROM`
- `UPDATE` -- se declenche par ligne mise a jour
- `DELETE` -- se declenche par ligne supprimee

### Exemple de declencheur de garde

```sql
CREATE FUNCTION prevent_delete() RETURNS INT AS $$
BEGIN
  RETURN NULL;  -- annuler le DELETE
END $$ LANGUAGE plpgsql;

CREATE TRIGGER trg_protect BEFORE DELETE ON important_data
  FOR EACH ROW EXECUTE FUNCTION prevent_delete();
```

### Callbacks de fonctions natives

Les fonctions natives enregistrees (Scala, JavaScript ou C) sont appelables depuis les fonctions declencheur, permettant l'integration avec des systemes externes :

```sql
-- En supposant que notify_webhook() est enregistree comme fonction native
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

Les fonctions stockees, les procedures et les declencheurs persistent entre les redemarrages de la base de donnees, tant pour le stockage en memoire (duree de la session) que pour le stockage persistant (survit a la fermeture/reouverture). Le SQL source est stocke et re-execute a l'ouverture de la base de donnees.

## Composition

Les fonctions peuvent appeler d'autres fonctions. Les procedures peuvent appeler des fonctions :

```sql
CREATE FUNCTION double(x INT) RETURNS INT AS $$
BEGIN RETURN x * 2; END $$ LANGUAGE plpgsql;

CREATE FUNCTION quadruple(x INT) RETURNS INT AS $$
BEGIN RETURN double(double(x)); END $$ LANGUAGE plpgsql;

SELECT quadruple(5);  -- 20
```

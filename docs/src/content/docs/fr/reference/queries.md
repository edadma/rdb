---
title: Requetes
description: Instructions SELECT, jointures, sous-requetes, operations ensemblistes et plus.
---

## Requetes de base

```sql
SELECT * FROM orders
WHERE amount > 50
ORDER BY amount DESC
LIMIT 10 OFFSET 5;
```

## Agregations

```sql
SELECT status, COUNT(*), AVG(amount), SUM(amount)
FROM orders
GROUP BY status
HAVING COUNT(*) > 5;
```

### FILTER sur les agregats

Appliquer un filtre par agregat sans affecter les autres agregats de la meme requete :

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE status = 'active') AS active,
  SUM(amount) FILTER (WHERE amount > 100) AS high_value_total
FROM orders;
```

## Fonctions de fenetre

Les fonctions de fenetre calculent des valeurs sur un ensemble de lignes liees a la ligne courante, sans regrouper les lignes comme `GROUP BY`.

### Fonctions de classement

```sql
SELECT name, department, salary,
  ROW_NUMBER() OVER (ORDER BY salary DESC) AS row_num,
  RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dept_rank,
  DENSE_RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dense_rank
FROM employees;
```

### Fonctions de decalage

```sql
SELECT name, salary,
  LAG(salary, 1) OVER (ORDER BY salary) AS prev_salary,
  LEAD(salary, 1, 0) OVER (ORDER BY salary) AS next_salary,
  NTILE(4) OVER (ORDER BY salary) AS quartile
FROM employees;
```

`LAG(expr [, offset [, default]])` et `LEAD(expr [, offset [, default]])` acceptent un decalage optionnel (par defaut 1) et une valeur par defaut (par defaut NULL).

### Fonctions de valeur

```sql
SELECT name, department, salary,
  FIRST_VALUE(name) OVER (PARTITION BY department ORDER BY salary) AS lowest_paid,
  LAST_VALUE(name) OVER (PARTITION BY department ORDER BY salary
    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS highest_paid,
  NTH_VALUE(name, 2) OVER (PARTITION BY department ORDER BY salary
    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS second_lowest
FROM employees;
```

| Fonction | Description |
|----------|-------------|
| `FIRST_VALUE(expr)` | Valeur de `expr` a la premiere ligne du cadre de fenetre |
| `LAST_VALUE(expr)` | Valeur de `expr` a la derniere ligne du cadre de fenetre |
| `NTH_VALUE(expr, n)` | Valeur de `expr` a la n-ieme ligne du cadre (base 1), ou NULL si pas de telle ligne |

Le cadre par defaut est `ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW`. Pour `LAST_VALUE` et `NTH_VALUE`, vous voudrez generalement `ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING` pour voir la partition entiere.

### Fonctions de fenetre avec agregats

Toute fonction d'agregation peut etre utilisee avec `OVER()` :

```sql
SELECT name, department, salary,
  SUM(salary) OVER (PARTITION BY department) AS dept_total,
  AVG(salary) OVER () AS overall_avg,
  COUNT(*) OVER (PARTITION BY department) AS dept_size
FROM employees;
```

### Specifications de cadre

Controlez quelles lignes au sein de la partition contribuent a une fonction de fenetre avec agregat :

```sql
SELECT name, salary,
  SUM(salary) OVER (ORDER BY salary
    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_total,
  AVG(salary) OVER (ORDER BY salary
    ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) AS moving_avg
FROM employees;
```

Bornes de cadre supportees :
- `UNBOUNDED PRECEDING` / `UNBOUNDED FOLLOWING` -- debut/fin de la partition
- `CURRENT ROW` -- la ligne courante
- `N PRECEDING` / `N FOLLOWING` -- N lignes avant/apres la ligne courante

Sans clause de cadre, les fonctions de fenetre avec agregats calculent sur l'ensemble de la partition.

### FILTER avec fonctions de fenetre

La clause `FILTER` fonctionne avec les fonctions de fenetre avec agregats :

```sql
SELECT name, salary,
  SUM(salary) FILTER (WHERE salary > 70000)
    OVER (ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
    AS running_high_earner_total
FROM employees;
```

## Jointures

Jointures INNER, LEFT, RIGHT, FULL et CROSS :

```sql
SELECT o.id, o.amount, c.name
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id;
```

## Sous-requetes et EXISTS

```sql
SELECT * FROM customers c
WHERE EXISTS (
  SELECT 1 FROM orders o
  WHERE o.customer_id = c.id AND o.amount > 100
);
```

## Jointures LATERAL

Sous-requetes correlees dans FROM :

```sql
SELECT c.name, recent.amount
FROM customers c,
LATERAL (
  SELECT amount FROM orders
  WHERE customer_id = c.id
  ORDER BY created_at DESC LIMIT 1
) AS recent;
```

## VALUES comme source

```sql
SELECT * FROM (VALUES (1, 'a'), (2, 'b')) AS t (id, name);
```

## Operations ensemblistes

```sql
SELECT name FROM customers
UNION
SELECT name FROM suppliers;
```

`UNION`, `UNION ALL`, `INTERSECT` et `EXCEPT` sont supportes.

## Expressions de table communes (WITH)

Les CTE definissent des sous-requetes nommees qui peuvent etre referencees dans la requete principale, ameliorant la lisibilite et permettant la reutilisation :

```sql
WITH active_users AS (
  SELECT id, name FROM users WHERE active = TRUE
),
user_orders AS (
  SELECT u.name, COUNT(*) AS order_count
  FROM active_users u
  INNER JOIN orders o ON u.id = o.user_id
  GROUP BY u.name
)
SELECT name, order_count FROM user_orders ORDER BY order_count DESC;
```

Les alias de colonnes peuvent etre specifies : `WITH t(x, y) AS (SELECT 1, 2)`.

Les CTE suivantes peuvent referencer les CTE precedentes. Un nom de CTE masque toute table portant le meme nom.

### CTE recursives

`WITH RECURSIVE` permet des requetes iteratives pour les donnees hierarchiques, la traversee de graphes et la generation de series :

```sql
-- Generer une serie de nombres
WITH RECURSIVE nums(n) AS (
  SELECT 1
  UNION ALL
  SELECT n + 1 FROM nums WHERE n < 10
)
SELECT n FROM nums;

-- Traversee d'arbre
WITH RECURSIVE tree(id, name, depth) AS (
  SELECT id, name, 0 FROM categories WHERE parent_id IS NULL
  UNION ALL
  SELECT c.id, c.name, t.depth + 1
  FROM categories c INNER JOIN tree t ON c.parent_id = t.id
)
SELECT name, depth FROM tree ORDER BY depth, name;
```

Le corps de la CTE recursive doit etre un `UNION ALL` ou `UNION` d'une requete ancre (cas de base non recursif) et d'une requete recursive (qui reference le nom de la CTE). L'execution s'arrete quand la requete recursive ne produit plus de nouvelles lignes, ou apres 1000 iterations.

## Expressions CASE

```sql
SELECT name,
  CASE WHEN amount > 100 THEN 'high' ELSE 'low' END AS tier
FROM orders;
```

## Correspondance de motifs

```sql
SELECT * FROM products WHERE name LIKE '%phone%';
SELECT * FROM products WHERE name ILIKE '%Phone%';   -- insensible a la casse
```

## BETWEEN, IN, ANY

```sql
SELECT * FROM orders WHERE amount BETWEEN 10 AND 100;
SELECT * FROM orders WHERE status IN ('pending', 'shipped');
SELECT * FROM orders WHERE status = ANY(ARRAY['pending', 'shipped']);
```

## OVERLAPS

Teste si deux intervalles de dates/heures se chevauchent :

```sql
SELECT (DATE '2024-01-01', DATE '2024-01-31')
  OVERLAPS (DATE '2024-01-15', DATE '2024-02-15');
```

## DISTINCT

```sql
SELECT DISTINCT category FROM products;
```

### DISTINCT ON

Retourne une ligne par valeur distincte des expressions donnees. La premiere ligne de chaque groupe (selon `ORDER BY`) est conservee :

```sql
SELECT DISTINCT ON (department) department, name, salary
FROM employees
ORDER BY department, salary DESC;
```

Cela retourne l'employe le mieux paye par departement.

## EXPLAIN

Afficher le plan d'execution de la requete :

```sql
EXPLAIN SELECT * FROM orders WHERE status = 'pending';
```

## Constructeur ARRAY

```sql
SELECT ARRAY[1, 2, 3];
```

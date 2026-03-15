---
title: Consultas
description: Sentencias SELECT, joins, subconsultas, operaciones de conjuntos y mas.
---

## Consultas basicas

```sql
SELECT * FROM orders
WHERE amount > 50
ORDER BY amount DESC
LIMIT 10 OFFSET 5;
```

## Agregaciones

```sql
SELECT status, COUNT(*), AVG(amount), SUM(amount)
FROM orders
GROUP BY status
HAVING COUNT(*) > 5;
```

### FILTER en agregados

Aplicar un filtro por agregado sin afectar otros agregados en la misma consulta:

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE status = 'active') AS active,
  SUM(amount) FILTER (WHERE amount > 100) AS high_value_total
FROM orders;
```

## Funciones de ventana

Las funciones de ventana calculan valores sobre un conjunto de filas relacionadas con la fila actual, sin colapsar filas como `GROUP BY`.

### Funciones de clasificacion

```sql
SELECT name, department, salary,
  ROW_NUMBER() OVER (ORDER BY salary DESC) AS row_num,
  RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dept_rank,
  DENSE_RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dense_rank
FROM employees;
```

### Funciones de desplazamiento

```sql
SELECT name, salary,
  LAG(salary, 1) OVER (ORDER BY salary) AS prev_salary,
  LEAD(salary, 1, 0) OVER (ORDER BY salary) AS next_salary,
  NTILE(4) OVER (ORDER BY salary) AS quartile
FROM employees;
```

`LAG(expr [, offset [, default]])` y `LEAD(expr [, offset [, default]])` aceptan un desplazamiento opcional (por defecto 1) y un valor por defecto (por defecto NULL).

### Funciones de valor

```sql
SELECT name, department, salary,
  FIRST_VALUE(name) OVER (PARTITION BY department ORDER BY salary) AS lowest_paid,
  LAST_VALUE(name) OVER (PARTITION BY department ORDER BY salary
    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS highest_paid,
  NTH_VALUE(name, 2) OVER (PARTITION BY department ORDER BY salary
    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS second_lowest
FROM employees;
```

| Funcion | Descripcion |
|----------|-------------|
| `FIRST_VALUE(expr)` | Valor de `expr` en la primera fila del marco de ventana |
| `LAST_VALUE(expr)` | Valor de `expr` en la ultima fila del marco de ventana |
| `NTH_VALUE(expr, n)` | Valor de `expr` en la fila n del marco (base 1), o NULL si no existe tal fila |

El marco por defecto es `ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW`. Para `LAST_VALUE` y `NTH_VALUE`, tipicamente quieres `ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING` para ver toda la particion.

### Funciones de ventana con agregados

Cualquier funcion de agregado puede usarse con `OVER()`:

```sql
SELECT name, department, salary,
  SUM(salary) OVER (PARTITION BY department) AS dept_total,
  AVG(salary) OVER () AS overall_avg,
  COUNT(*) OVER (PARTITION BY department) AS dept_size
FROM employees;
```

### Especificaciones de marco

Controlar que filas dentro de la particion contribuyen a una funcion de ventana con agregado:

```sql
SELECT name, salary,
  SUM(salary) OVER (ORDER BY salary
    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_total,
  AVG(salary) OVER (ORDER BY salary
    ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) AS moving_avg
FROM employees;
```

Limites de marco soportados:
- `UNBOUNDED PRECEDING` / `UNBOUNDED FOLLOWING` — inicio/fin de la particion
- `CURRENT ROW` — la fila actual
- `N PRECEDING` / `N FOLLOWING` — N filas antes/despues de la actual

Sin clausula de marco, las funciones de ventana con agregados calculan sobre toda la particion.

### FILTER con funciones de ventana

La clausula `FILTER` funciona con funciones de ventana con agregados:

```sql
SELECT name, salary,
  SUM(salary) FILTER (WHERE salary > 70000)
    OVER (ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
    AS running_high_earner_total
FROM employees;
```

## Joins

Joins INNER, LEFT, RIGHT, FULL y CROSS:

```sql
SELECT o.id, o.amount, c.name
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id;
```

## Subconsultas y EXISTS

```sql
SELECT * FROM customers c
WHERE EXISTS (
  SELECT 1 FROM orders o
  WHERE o.customer_id = c.id AND o.amount > 100
);
```

## Joins LATERAL

Subconsultas correlacionadas en FROM:

```sql
SELECT c.name, recent.amount
FROM customers c,
LATERAL (
  SELECT amount FROM orders
  WHERE customer_id = c.id
  ORDER BY created_at DESC LIMIT 1
) AS recent;
```

## VALUES como fuente

```sql
SELECT * FROM (VALUES (1, 'a'), (2, 'b')) AS t (id, name);
```

## Operaciones de conjuntos

```sql
SELECT name FROM customers
UNION
SELECT name FROM suppliers;
```

Se soportan `UNION`, `UNION ALL`, `INTERSECT` y `EXCEPT`.

## Expresiones de tabla comunes (WITH)

Las CTEs definen subconsultas nombradas que pueden referenciarse en la consulta principal, mejorando la legibilidad y permitiendo la reutilizacion:

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

Se pueden especificar alias de columna: `WITH t(x, y) AS (SELECT 1, 2)`.

Las CTEs posteriores pueden referenciar a las anteriores. Un nombre de CTE oculta cualquier tabla con el mismo nombre.

### CTEs recursivas

`WITH RECURSIVE` permite consultas iterativas para datos jerarquicos, recorrido de grafos y generacion de series:

```sql
-- Generar una serie de numeros
WITH RECURSIVE nums(n) AS (
  SELECT 1
  UNION ALL
  SELECT n + 1 FROM nums WHERE n < 10
)
SELECT n FROM nums;

-- Recorrido de arbol
WITH RECURSIVE tree(id, name, depth) AS (
  SELECT id, name, 0 FROM categories WHERE parent_id IS NULL
  UNION ALL
  SELECT c.id, c.name, t.depth + 1
  FROM categories c INNER JOIN tree t ON c.parent_id = t.id
)
SELECT name, depth FROM tree ORDER BY depth, name;
```

El cuerpo de la CTE recursiva debe ser un `UNION ALL` o `UNION` de una consulta ancla (caso base no recursivo) y una consulta recursiva (que referencia el nombre de la CTE). La ejecucion se detiene cuando la consulta recursiva no produce nuevas filas, o despues de 1000 iteraciones.

## Expresiones CASE

```sql
SELECT name,
  CASE WHEN amount > 100 THEN 'high' ELSE 'low' END AS tier
FROM orders;
```

## Coincidencia de patrones

```sql
SELECT * FROM products WHERE name LIKE '%phone%';
SELECT * FROM products WHERE name ILIKE '%Phone%';   -- insensible a mayusculas
```

## BETWEEN, IN, ANY

```sql
SELECT * FROM orders WHERE amount BETWEEN 10 AND 100;
SELECT * FROM orders WHERE status IN ('pending', 'shipped');
SELECT * FROM orders WHERE status = ANY(ARRAY['pending', 'shipped']);
```

## OVERLAPS

Probar si dos rangos de fecha/hora se superponen:

```sql
SELECT (DATE '2024-01-01', DATE '2024-01-31')
  OVERLAPS (DATE '2024-01-15', DATE '2024-02-15');
```

## DISTINCT

```sql
SELECT DISTINCT category FROM products;
```

### DISTINCT ON

Retornar una fila por valor distinto de las expresiones dadas. Se mantiene la primera fila de cada grupo (segun `ORDER BY`):

```sql
SELECT DISTINCT ON (department) department, name, salary
FROM employees
ORDER BY department, salary DESC;
```

Esto retorna el empleado mejor pagado por departamento.

## EXPLAIN

Mostrar el plan de ejecucion de la consulta:

```sql
EXPLAIN SELECT * FROM orders WHERE status = 'pending';
```

## Constructor ARRAY

```sql
SELECT ARRAY[1, 2, 3];
```

---
title: Consultas
description: Comandos SELECT, joins, subconsultas, operacoes de conjunto e mais.
---

## Consultas Basicas

```sql
SELECT * FROM orders
WHERE amount > 50
ORDER BY amount DESC
LIMIT 10 OFFSET 5;
```

## Agregacoes

```sql
SELECT status, COUNT(*), AVG(amount), SUM(amount)
FROM orders
GROUP BY status
HAVING COUNT(*) > 5;
```

### FILTER em Agregacoes

Aplique um filtro por agregacao sem afetar outras agregacoes na mesma consulta:

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE status = 'active') AS active,
  SUM(amount) FILTER (WHERE amount > 100) AS high_value_total
FROM orders;
```

## Funcoes de Janela

Funcoes de janela calculam valores sobre um conjunto de linhas relacionadas a linha atual, sem colapsar linhas como `GROUP BY`.

### Funcoes de classificacao

```sql
SELECT name, department, salary,
  ROW_NUMBER() OVER (ORDER BY salary DESC) AS row_num,
  RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dept_rank,
  DENSE_RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dense_rank
FROM employees;
```

### Funcoes de offset

```sql
SELECT name, salary,
  LAG(salary, 1) OVER (ORDER BY salary) AS prev_salary,
  LEAD(salary, 1, 0) OVER (ORDER BY salary) AS next_salary,
  NTILE(4) OVER (ORDER BY salary) AS quartile
FROM employees;
```

`LAG(expr [, offset [, default]])` e `LEAD(expr [, offset [, default]])` aceitam um offset opcional (padrao 1) e valor padrao (padrao NULL).

### Funcoes de valor

```sql
SELECT name, department, salary,
  FIRST_VALUE(name) OVER (PARTITION BY department ORDER BY salary) AS lowest_paid,
  LAST_VALUE(name) OVER (PARTITION BY department ORDER BY salary
    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS highest_paid,
  NTH_VALUE(name, 2) OVER (PARTITION BY department ORDER BY salary
    ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS second_lowest
FROM employees;
```

| Funcao | Descricao |
|----------|-------------|
| `FIRST_VALUE(expr)` | Valor de `expr` na primeira linha do quadro da janela |
| `LAST_VALUE(expr)` | Valor de `expr` na ultima linha do quadro da janela |
| `NTH_VALUE(expr, n)` | Valor de `expr` na enesima linha do quadro (base 1), ou NULL se nao existir |

O quadro padrao e `ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW`. Para `LAST_VALUE` e `NTH_VALUE`, voce normalmente quer `ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING` para ver a particao inteira.

### Funcoes de janela agregadas

Qualquer funcao de agregacao pode ser usada com `OVER()`:

```sql
SELECT name, department, salary,
  SUM(salary) OVER (PARTITION BY department) AS dept_total,
  AVG(salary) OVER () AS overall_avg,
  COUNT(*) OVER (PARTITION BY department) AS dept_size
FROM employees;
```

### Especificacoes de quadro

Controle quais linhas dentro da particao contribuem para uma funcao de janela agregada:

```sql
SELECT name, salary,
  SUM(salary) OVER (ORDER BY salary
    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_total,
  AVG(salary) OVER (ORDER BY salary
    ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) AS moving_avg
FROM employees;
```

Limites de quadro suportados:
- `UNBOUNDED PRECEDING` / `UNBOUNDED FOLLOWING` — inicio/fim da particao
- `CURRENT ROW` — a linha atual
- `N PRECEDING` / `N FOLLOWING` — N linhas antes/depois da atual

Sem clausula de quadro, funcoes de janela agregadas calculam sobre a particao inteira.

### FILTER com funcoes de janela

A clausula `FILTER` funciona com funcoes de janela agregadas:

```sql
SELECT name, salary,
  SUM(salary) FILTER (WHERE salary > 70000)
    OVER (ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)
    AS running_high_earner_total
FROM employees;
```

## Joins

Joins INNER, LEFT, RIGHT, FULL e CROSS:

```sql
SELECT o.id, o.amount, c.name
FROM orders o
INNER JOIN customers c ON o.customer_id = c.id;
```

## Subconsultas e EXISTS

```sql
SELECT * FROM customers c
WHERE EXISTS (
  SELECT 1 FROM orders o
  WHERE o.customer_id = c.id AND o.amount > 100
);
```

## Joins LATERAL

Subconsultas correlacionadas em FROM:

```sql
SELECT c.name, recent.amount
FROM customers c,
LATERAL (
  SELECT amount FROM orders
  WHERE customer_id = c.id
  ORDER BY created_at DESC LIMIT 1
) AS recent;
```

## VALUES como Fonte

```sql
SELECT * FROM (VALUES (1, 'a'), (2, 'b')) AS t (id, name);
```

## Operacoes de Conjunto

```sql
SELECT name FROM customers
UNION
SELECT name FROM suppliers;
```

`UNION`, `UNION ALL`, `INTERSECT` e `EXCEPT` sao suportados.

## Common Table Expressions (WITH)

CTEs definem subconsultas nomeadas que podem ser referenciadas na consulta principal, melhorando a legibilidade e habilitando reutilizacao:

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

Aliases de coluna podem ser especificados: `WITH t(x, y) AS (SELECT 1, 2)`.

CTEs posteriores podem referenciar CTEs anteriores. Um nome de CTE sombrea qualquer tabela com o mesmo nome.

### CTEs Recursivas

`WITH RECURSIVE` habilita consultas iterativas para dados hierarquicos, travessia de grafos e geracao de series:

```sql
-- Gerar uma serie de numeros
WITH RECURSIVE nums(n) AS (
  SELECT 1
  UNION ALL
  SELECT n + 1 FROM nums WHERE n < 10
)
SELECT n FROM nums;

-- Travessia de arvore
WITH RECURSIVE tree(id, name, depth) AS (
  SELECT id, name, 0 FROM categories WHERE parent_id IS NULL
  UNION ALL
  SELECT c.id, c.name, t.depth + 1
  FROM categories c INNER JOIN tree t ON c.parent_id = t.id
)
SELECT name, depth FROM tree ORDER BY depth, name;
```

O corpo da CTE recursiva deve ser um `UNION ALL` ou `UNION` de uma consulta ancora (caso base nao recursivo) e uma consulta recursiva (referencia o nome da CTE). A execucao para quando a consulta recursiva nao produz novas linhas, ou apos 1000 iteracoes.

## Expressoes CASE

```sql
SELECT name,
  CASE WHEN amount > 100 THEN 'high' ELSE 'low' END AS tier
FROM orders;
```

## Correspondencia de Padroes

```sql
SELECT * FROM products WHERE name LIKE '%phone%';
SELECT * FROM products WHERE name ILIKE '%Phone%';   -- insensível a maiusculas
```

## BETWEEN, IN, ANY

```sql
SELECT * FROM orders WHERE amount BETWEEN 10 AND 100;
SELECT * FROM orders WHERE status IN ('pending', 'shipped');
SELECT * FROM orders WHERE status = ANY(ARRAY['pending', 'shipped']);
```

## OVERLAPS

Testa se dois intervalos de data/hora se sobrepoem:

```sql
SELECT (DATE '2024-01-01', DATE '2024-01-31')
  OVERLAPS (DATE '2024-01-15', DATE '2024-02-15');
```

## DISTINCT

```sql
SELECT DISTINCT category FROM products;
```

### DISTINCT ON

Retorna uma linha por valor distinto das expressoes especificadas. A primeira linha de cada grupo (de acordo com `ORDER BY`) e mantida:

```sql
SELECT DISTINCT ON (department) department, name, salary
FROM employees
ORDER BY department, salary DESC;
```

Isso retorna o funcionario com maior salario por departamento.

## EXPLAIN

Mostra o plano de execucao da consulta:

```sql
EXPLAIN SELECT * FROM orders WHERE status = 'pending';
```

## Construtor ARRAY

```sql
SELECT ARRAY[1, 2, 3];
```

---
title: DML
description: Linguagem de Manipulacao de Dados — comandos INSERT, UPDATE, DELETE e COPY.
---

## INSERT

```sql
INSERT INTO orders (customer_name, amount)
VALUES ('Alice Smith', 149.99);
```

### RETURNING

Retorna valores da linha inserida:

```sql
INSERT INTO orders (customer_name, amount)
VALUES ('Bob Johnson', 75.50)
RETURNING id;

INSERT INTO orders (customer_name, amount)
VALUES ('Carol', 200.00)
RETURNING *;
```

### Insert a partir de uma Consulta

```sql
INSERT INTO archive (customer_name, amount)
SELECT customer_name, amount FROM orders WHERE status = 'delivered';
```

### INSERT ... ON CONFLICT (Upsert)

Ignorar linhas conflitantes:

```sql
INSERT INTO users (email, name)
VALUES ('alice@example.com', 'Alice')
ON CONFLICT DO NOTHING;
```

Atualizar em caso de conflito com colunas especificas:

```sql
INSERT INTO users (email, name)
VALUES ('alice@example.com', 'Alice Updated')
ON CONFLICT (email) DO UPDATE SET name = 'Alice Updated';
```

Pode ser combinado com RETURNING:

```sql
INSERT INTO users (email, name)
VALUES ('alice@example.com', 'Alice')
ON CONFLICT (email) DO UPDATE SET name = 'Alice'
RETURNING *;
```

## UPDATE

```sql
UPDATE orders SET status = 'shipped' WHERE amount > 100;
```

### Atualizacao em massa a partir de VALUES

`UPDATE ... FROM` estilo PostgreSQL:

```sql
UPDATE orders
  SET status = d.status
  FROM (VALUES ('ord-1', 'shipped'), ('ord-2', 'delivered'))
       AS d (id, status)
  WHERE orders.id = d.id;
```

### UPDATE com RETURNING

```sql
UPDATE orders SET status = 'shipped'
WHERE id = 42
RETURNING id, status;
```

## DELETE

```sql
DELETE FROM orders WHERE status = 'delivered';
```

### DELETE ... USING

Junte outras tabelas para determinar quais linhas excluir:

```sql
DELETE FROM orders
USING customers
WHERE orders.customer_id = customers.id
  AND customers.status = 'inactive';
```

Multiplas tabelas USING:

```sql
DELETE FROM order_items
USING products, categories
WHERE order_items.product_id = products.id
  AND products.category_id = categories.id
  AND categories.name = 'discontinued';
```

### DELETE com RETURNING

```sql
DELETE FROM orders WHERE status = 'delivered'
RETURNING *;
```

## COPY

Importacao e exportacao em massa de dados em formato CSV.

### COPY FROM

Importar linhas de um arquivo CSV:

```sql
COPY orders FROM 'data/orders.csv';
COPY orders FROM 'data/orders.csv' WITH (HEADER);
COPY orders FROM 'data/orders.csv' WITH (HEADER, DELIMITER '|');
COPY orders (customer_name, amount) FROM 'data/partial.csv' WITH (HEADER);
```

### COPY TO

Exportar uma tabela ou consulta para um arquivo CSV:

```sql
COPY orders TO 'export/orders.csv';
COPY orders TO 'export/orders.csv' WITH (HEADER);
COPY (SELECT * FROM orders WHERE status = 'pending') TO 'export/pending.csv' WITH (HEADER);
```

### Opcoes

| Opcao | Descricao |
|--------|-------------|
| `HEADER` | Primeira linha e cabecalho (ignorada na importacao, escrita na exportacao) |
| `DELIMITER 'c'` | Caractere separador de campo (padrao: `,`) |

## csv_file() — Consultar Arquivos CSV Diretamente

Consulte um arquivo CSV como tabela virtual sem importar:

```sql
SELECT * FROM csv_file('data/sales.csv');
SELECT name, age::int FROM csv_file('data/people.csv') WHERE age::int > 25;
```

Todos os valores sao retornados como `TEXT` — use `::type` para converter. Suporta `WHERE`, `ORDER BY`, `LIMIT`, `JOIN` e agregacoes.

Opcoes:

```sql
csv_file('path')                        -- com cabecalho (padrao)
csv_file('path', false)                 -- sem cabecalho (colunas nomeadas column1, column2, ...)
csv_file('path', true, '|')            -- delimitador personalizado
```

Junte arquivos CSV entre si ou com tabelas do banco de dados:

```sql
SELECT e.name, d.department
FROM csv_file('employees.csv') e
JOIN csv_file('departments.csv') d ON e.dept_id = d.id;
```

## Tabelas Virtuais

Registre fontes de dados externas como tabelas consultaveis usando `CREATE VIRTUAL TABLE`:

```sql
CREATE VIRTUAL TABLE sales USING csv('data/sales.csv');
CREATE VIRTUAL TABLE sales USING csv('data/sales.csv', 'no_header', '|');

SELECT * FROM sales WHERE amount::int > 100;
DROP TABLE sales;
```

Tabelas virtuais aparecem em `SHOW TABLES` e suportam `SELECT`, `WHERE`, `JOIN`, `ORDER BY` e agregacoes. Sao somente leitura — `INSERT`, `UPDATE` e `DELETE` nao sao suportados.

O modulo `csv` embutido e registrado por padrao. Modulos personalizados podem ser registrados via API Scala:

```scala
db.registerVirtualTableModule("mymodule", new VirtualTableModule { ... })
```

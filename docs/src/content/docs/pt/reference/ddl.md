---
title: DDL
description: Linguagem de Definicao de Dados — comandos CREATE, ALTER, DROP e TRUNCATE.
---

## Schemas

O PetraDB suporta namespaces de schema estilo PostgreSQL. Todo banco de dados possui um schema `public` por padrao. Nomes de tabela nao qualificados resolvem para `public`.

### CREATE SCHEMA

```sql
CREATE SCHEMA inventory;
CREATE SCHEMA IF NOT EXISTS inventory;
```

### Tabelas qualificadas por schema

Use a sintaxe `schema.table` em qualquer comando DDL ou DML:

```sql
CREATE TABLE inventory.products (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  price NUMERIC(10,2)
);

INSERT INTO inventory.products (name, price) VALUES ('Widget', 9.99);
SELECT * FROM inventory.products;
```

Tabelas com o mesmo nome podem existir em schemas diferentes:

```sql
CREATE SCHEMA staging;
CREATE TABLE staging.products (id SERIAL, name TEXT);
CREATE TABLE public.products (id SERIAL, name TEXT);
-- Estas sao tabelas separadas
```

### information_schema

O PetraDB fornece tabelas virtuais `information_schema` para introspeccao da estrutura do banco de dados:

```sql
SELECT * FROM information_schema.schemata;
SELECT * FROM information_schema.tables;
SELECT * FROM information_schema.columns WHERE table_name = 'products';
```

Views disponiveis: `schemata`, `tables`, `columns`.

## Tabelas

### CREATE TABLE

```sql
CREATE TABLE orders (
  id UUID DEFAULT gen_random_uuid(),
  customer_name TEXT NOT NULL,
  amount NUMERIC(10,2),
  status order_status,
  tags INT[],
  metadata JSON,
  PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS orders (...);
```

### Chaves Estrangeiras

```sql
CREATE TABLE line_items (
  id SERIAL,
  order_id UUID REFERENCES orders (id) ON DELETE CASCADE ON UPDATE CASCADE,
  product TEXT NOT NULL
);
```

### Colunas Geradas

Colunas computadas que sao derivadas automaticamente de outras colunas:

```sql
CREATE TABLE products (
  price NUMERIC,
  tax_rate NUMERIC DEFAULT 0.08,
  total NUMERIC GENERATED ALWAYS AS (price * (1 + tax_rate)) STORED
);

INSERT INTO products (price) VALUES (100);
SELECT * FROM products;
-- price: 100, tax_rate: 0.08, total: 108
```

Colunas geradas sao recalculadas em INSERT e UPDATE. Elas nao podem ser escritas diretamente.

### ALTER TABLE

Adicionar, remover ou renomear colunas:

```sql
ALTER TABLE orders ADD COLUMN notes TEXT;
ALTER TABLE orders DROP COLUMN notes;
ALTER TABLE orders RENAME COLUMN amount TO total;
ALTER TABLE orders RENAME TO purchases;
```

Modificar propriedades de coluna:

```sql
ALTER TABLE orders ALTER COLUMN notes SET NOT NULL;
ALTER TABLE orders ALTER COLUMN notes DROP NOT NULL;
ALTER TABLE orders ALTER COLUMN notes SET DEFAULT 'none';
ALTER TABLE orders ALTER COLUMN notes DROP DEFAULT;
ALTER TABLE orders ALTER COLUMN amount SET DATA TYPE NUMERIC(10,2);
```

Adicionar e remover restricoes:

```sql
ALTER TABLE orders ADD CONSTRAINT chk_amount CHECK (amount > 0);
ALTER TABLE orders ADD CONSTRAINT uq_email UNIQUE (email);
ALTER TABLE orders ADD CONSTRAINT fk_customer
  FOREIGN KEY (customer_id) REFERENCES customers (id) ON DELETE CASCADE;
ALTER TABLE orders DROP CONSTRAINT chk_amount;
```

### Restricoes CHECK

Impoe condicoes nos valores das colunas:

```sql
CREATE TABLE products (
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  price NUMERIC CHECK (price >= 0),
  quantity INT,
  CONSTRAINT positive_qty CHECK (quantity >= 0)
);
```

Restricoes CHECK sao aplicadas em INSERT e UPDATE.

## Triggers

Veja [PL/pgSQL — Triggers](/reference/plpgsql/#triggers) para documentacao completa.

```sql
CREATE TRIGGER trg_audit AFTER INSERT ON orders
  FOR EACH ROW EXECUTE FUNCTION audit_log();

DROP TRIGGER trg_audit ON orders;
DROP TRIGGER IF EXISTS trg_audit ON orders;
```

### TRUNCATE TABLE

Remove todas as linhas e reseta sequencias serial:

```sql
TRUNCATE TABLE orders;
```

### DROP TABLE

```sql
DROP TABLE orders;
DROP TABLE IF EXISTS orders;
```

## Views

### CREATE VIEW

Cria uma view nomeada apoiada por uma consulta. Use `OR REPLACE` para sobrescrever uma view existente:

```sql
CREATE VIEW active_orders AS
  SELECT * FROM orders WHERE status = 'pending';

CREATE OR REPLACE VIEW active_orders AS
  SELECT * FROM orders WHERE status != 'delivered';
```

### DROP VIEW

```sql
DROP VIEW active_orders;
DROP VIEW IF EXISTS active_orders;
```

## Tipos Personalizados

### CREATE TYPE

Define um tipo enumerado:

```sql
CREATE TYPE order_status AS ENUM ('pending', 'shipped', 'delivered');
```

### DROP TYPE

```sql
DROP TYPE order_status CASCADE;
```

## Sequencias

Sequencias sao contadores nomeados que geram valores numericos sequenciais. Sao comumente usadas para geracao de chaves primarias.

### CREATE SEQUENCE

```sql
CREATE SEQUENCE order_seq;
CREATE SEQUENCE order_seq START WITH 100 INCREMENT BY 10;
CREATE SEQUENCE IF NOT EXISTS order_seq;
```

Opcoes:

| Opcao | Padrao | Descricao |
|--------|---------|-------------|
| `INCREMENT BY n` | 1 | Tamanho do passo |
| `START WITH n` | 1 | Valor inicial |
| `MINVALUE n` / `NO MINVALUE` | 1 | Valor minimo |
| `MAXVALUE n` / `NO MAXVALUE` | 4611686018427387903 | Valor maximo |
| `CYCLE` / `NO CYCLE` | `NO CYCLE` | Se deve reciclar nos limites |

### DROP SEQUENCE

```sql
DROP SEQUENCE order_seq;
DROP SEQUENCE IF EXISTS order_seq;
```

### SERIAL e Sequencias

Colunas `SERIAL`, `SMALLSERIAL` e `BIGSERIAL` criam automaticamente uma sequencia auxiliar chamada `<table>_<column>_seq`. Isso corresponde ao comportamento do PostgreSQL:

```sql
CREATE TABLE orders (id SERIAL PRIMARY KEY, name TEXT);
-- Implicitamente cria a sequencia "orders_id_seq"

SELECT nextval('orders_id_seq');   -- funciona
SELECT currval('orders_id_seq');   -- funciona apos INSERT ou nextval
```

Excluir a tabela cascateia para remover suas sequencias pertencentes. `TRUNCATE` reseta sequencias auxiliares para seus valores iniciais.

### Funcoes de Sequencia

| Funcao | Descricao |
|----------|-------------|
| `nextval('seq_name')` | Avanca e retorna proximo valor |
| `currval('seq_name')` | Retorna valor atual (deve chamar nextval primeiro na sessao) |
| `setval('seq_name', value)` | Define valor atual; proximo nextval retorna value + incremento |
| `setval('seq_name', value, false)` | Define valor atual; proximo nextval retorna value |
| `lastval()` | Retorna ultimo valor de qualquer sequencia nesta sessao |

## Funcoes e Procedures Armazenados

Veja [PL/pgSQL](/reference/plpgsql/) para documentacao completa.

```sql
CREATE FUNCTION double(x INT) RETURNS INT AS $$
BEGIN RETURN x * 2; END $$ LANGUAGE plpgsql;

CREATE PROCEDURE reset_counts() AS $$
BEGIN UPDATE counters SET val = 0; END $$ LANGUAGE plpgsql;

DROP FUNCTION double;
DROP PROCEDURE IF EXISTS reset_counts;
```

## Indices

```sql
CREATE INDEX idx_orders_status ON orders (status);
CREATE UNIQUE INDEX idx_orders_email ON orders (email);
DROP INDEX idx_orders_status;
```

### Indices Parciais

Indexa apenas linhas que correspondem a uma condicao, tornando o indice menor e mais rapido:

```sql
CREATE INDEX idx_active_orders ON orders (customer_id) WHERE status = 'active';
CREATE UNIQUE INDEX idx_unique_active_email ON users (email) WHERE active = true;
```

O planejador de consultas usa um indice parcial apenas quando a clausula `WHERE` da consulta inclui a condicao do indice.

### Indices de Expressao

Indexa em valores computados em vez de colunas brutas:

```sql
CREATE INDEX idx_lower_email ON users ((lower(email)));
CREATE UNIQUE INDEX idx_lower_name ON users ((lower(name)));
```

A expressao deve ser envolvida em parenteses. O planejador corresponde `WHERE lower(email) = 'alice@test.com'` ao indice automaticamente.

Indices parciais e de expressao podem ser combinados:

```sql
CREATE INDEX idx_active_lower ON users ((lower(name))) WHERE status = 'active';
```

## Comandos SHOW

Inspecione metadados do banco de dados:

```sql
SHOW TABLES;
SHOW VIEWS;
SHOW SEQUENCES;
SHOW COLUMNS orders;
SHOW PRIMARY KEY orders;
SHOW FOREIGN KEYS orders;
SHOW INDEXES orders;
SHOW INDEXES;              -- todos os indices de todas as tabelas
```

### Saida de SHOW VIEWS

| Coluna | Tipo | Descricao |
|--------|------|-------------|
| `view_name` | TEXT | Nome da view |
| `definition` | TEXT | A consulta SQL que define a view |

### Saida de SHOW COLUMNS

| Coluna | Tipo | Descricao |
|--------|------|-------------|
| `name` | TEXT | Nome da coluna |
| `type` | TEXT | Tipo de dados |
| `required` | BOOLEAN | Restricao NOT NULL |
| `indexed` | BOOLEAN | Possui um indice |
| `unique` | BOOLEAN | Possui restricao unique |
| `fk_table` | TEXT | Tabela alvo da chave estrangeira |
| `fk_column` | TEXT | Coluna alvo da chave estrangeira |
| `fk_on_delete` | TEXT | Acao ON DELETE |
| `fk_on_update` | TEXT | Acao ON UPDATE |
| `default_value` | TEXT | Expressao padrao |

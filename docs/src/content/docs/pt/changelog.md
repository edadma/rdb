---
title: Registro de Alteracoes
---

## v1.5-20260315

### PL/pgSQL — Stored Procedures, Functions e Triggers

Suporte completo a linguagem procedural em blocos DO, funcoes armazenadas e procedures armazenados:

- **Blocos DO** — blocos PL/pgSQL anonimos com DECLARE, BEGIN...END
- **Funcoes armazenadas** — `CREATE FUNCTION name(params) RETURNS type AS $$ ... $$ LANGUAGE plpgsql`, chamaveis em qualquer expressao SQL
- **Procedures armazenados** — `CREATE PROCEDURE name(params) AS $$ ... $$ LANGUAGE plpgsql`, invocados com `CALL`
- **Triggers** — `CREATE TRIGGER name BEFORE|AFTER INSERT|UPDATE|DELETE ON table FOR EACH ROW EXECUTE FUNCTION func()`
  - Triggers BEFORE podem cancelar operacoes retornando NULL
  - Triggers AFTER disparam apos a operacao
  - Triggers disparam para INSERT, UPDATE, DELETE e COPY FROM
  - Variaveis TG_OP, TG_TABLE_NAME, OLD, NEW disponiveis
- **Fluxo de controle** — IF/ELSIF/ELSE, WHILE LOOP, FOR range/query LOOP, RETURN, RAISE NOTICE/EXCEPTION, PERFORM, EXCEPTION WHEN
- **Persistencia** — funcoes, procedures e triggers sobrevivem ao fechar/reabrir em PersistentDB e TextDB
- **OR REPLACE** — sobrescrever funcoes e procedures existentes

### Funcoes Nativas Definidas pelo Usuario

Registre callbacks da linguagem hospedeira como funcoes SQL, chamaveis em consultas, triggers e procedures:

- **Scala** — `db.registerScalarFunction("name", { args => result })`
- **JavaScript** — `session.registerFunction("name", (args) => result)`
- **C** — `petradb_create_function(db, "name", nargs, user_data, callback)` com API de valor/contexto tipado estilo SQLite

### Biblioteca C e API de Cursor

- **Biblioteca nativa compartilhada** — `libpetradb-engine.so` via Scala Native com funcoes C exportadas via `@exported`
- **API C estilo SQLite** — `petradb_open`, `petradb_exec`, `petradb_prepare/step/finalize`, acessores de coluna tipados
- **Funcoes definidas pelo usuario** — `petradb_value_int/double/text`, `petradb_result_int/double/text/null/error`, `petradb_user_data`
- **API de Cursor** — `session.openCursor(sql)` para iteracao preguicosa linha a linha com `step()`, acessores de coluna tipados, `fetch(n)`, `move(n)`, consultas parametrizadas
- **Header C** — `petradb.h` com documentacao completa da API
- **Suite de testes C** — 67 testes
- **Teste FFI Rust** — 38 testes comprovando interoperabilidade entre linguagens

### Tabelas Virtuais

- **Framework extensivel** — `CREATE VIRTUAL TABLE name USING module(args)`, somente leitura, aparece em SHOW TABLES
- **Modulo CSV embutido** — `CREATE VIRTUAL TABLE t USING csv('file.csv')` com opcoes de cabecalho/delimitador
- **Modulos personalizados** — `db.registerVirtualTableModule("name", module)` na API Scala

### Funcao de Tabela csv_file()

Consulte arquivos CSV diretamente sem importar:

```sql
SELECT * FROM csv_file('data.csv');
SELECT e.name, d.dept FROM csv_file('employees.csv') e
  JOIN csv_file('departments.csv') d ON e.dept_id = d.id;
```

### Indices Avancados

- **Indices parciais** — `CREATE INDEX ... WHERE condition` — indexa apenas linhas que correspondem ao predicado
- **Indices de expressao** — `CREATE INDEX ... ON table ((expr))` — indexa valores computados como `lower(email)`
- **Combinados** — indices parciais + de expressao funcionam juntos

### Funcoes de Janela

- **FIRST_VALUE(expr)** — valor na primeira linha do quadro da janela
- **LAST_VALUE(expr)** — valor na ultima linha do quadro da janela
- **NTH_VALUE(expr, n)** — valor na enesima linha do quadro

### DELETE ... USING

Exclusoes multi-tabela com sintaxe PostgreSQL:

```sql
DELETE FROM orders USING customers
WHERE orders.customer_id = customers.id AND customers.status = 'inactive';
```

### Quarry — Construtor de Consultas Type-Safe com AST

Novo pacote `@petradb/quarry`: construtor de consultas type-safe que gera objetos AST (nao strings SQL):

- Definicao de schema com 21 tipos de coluna
- CRUD completo: select, insert, update, delete com referencias de coluna type-safe
- Joins: inner, left, right, full outer, cross com resultados tipados
- Expressoes: mais de 50 operadores, agregacoes, CASE/CAST/EXISTS, subconsultas
- Upsert: `onConflictDoNothing()`, `onConflictDoUpdate()`
- Aliases de tabela para self-joins
- Transacoes, RETURNING, DISTINCT ON
- Testes de tipo em tempo de compilacao para todos os recursos
- Operacoes de conjunto: UNION, INTERSECT, EXCEPT
- Funcoes de janela, CTEs, helpers escalares nomeados

### Correcoes de Bugs

- **ByteaValue** — `ARRAY[...]` em colunas BYTEA agora produz corretamente `ByteaValue` em vez de `ArrayValue`
- **Tipos de resultado JS/Client** — adicionados handlers de tipo de resultado PL/pgSQL ausentes (DoBlockResult, CreateFunctionResult, etc.) para evitar crashes de match nao exaustivo
- **Codecs** — adicionada serializacao para todos os novos tipos de resultado para comunicacao cliente/servidor
- **llms.txt** — corrigido campo `type` para campo `command`, atualizados todos os tipos de resultado e recursos

### Renomeacao de Modulo

- **shared -> common** — renomeado o modulo de tipos compartilhados de `petradb-shared` para `petradb-common`

### Documentacao

- Nova pagina de referencia **PL/pgSQL** (triggers, funcoes, procedures, fluxo de controle)
- Nova pagina de referencia **API C** (interface completa estilo SQLite)
- Novos guias de **Primeiros Passos** para Java (JDBC) e C
- Documentacao DDL atualizada: indices parciais/de expressao, restricoes CHECK, triggers, rotinas armazenadas
- Documentacao DML atualizada: DELETE...USING, csv_file(), tabelas virtuais
- Documentacao API JS/Scala atualizada: registerFunction, tipos de resultado
- Pagina inicial: quatro botoes de primeiros passos (JS, Java, Scala, C)
- llms.txt reescrito com todos os recursos atuais

### Atualizacoes de Versao

| Componente | Maven Central | npm |
|-----------|---------------|-----|
| common | 1.5.0 | — |
| engine | 1.5.0 | @petradb/engine 1.5.0 |
| client | 1.5.0 | @petradb/client 1.5.0 |
| server | 1.5.0 | @petradb/server 1.5.0 |
| cli | 1.5.0 | @petradb/cli 1.5.0 |
| jdbc | 1.5.0 | — |
| drizzle | — | @petradb/drizzle 1.5.0 |
| knex | — | @petradb/knex 1.5.0 |
| lucid | — | @petradb/lucid 1.5.0 |
| quarry | — | @petradb/quarry 1.5.0 |

## v1.4-20260314

### Correcoes de bugs e melhorias

- **Correcao de subconsulta IN correlacionada** — `IN (SELECT ...)` correlacionado com tabelas indexadas agora funciona corretamente
- **Resolucao de coluna qualificada** — correcoes para referencias de coluna ambiguas em joins complexos
- **Tratamento de IN/ANY vazio** — casos extremos de `IN ()` e `= ANY('{}')` resolvidos
- **Limpeza de CASCADE em chave estrangeira** — `DROP TABLE ... CASCADE` agora remove corretamente restricoes FK em tabelas filhas
- **DROP TABLE IF EXISTS ... CASCADE** — combinar `IF EXISTS` com `CASCADE` nao causa mais erro de parse
- **Conversao de literal de array** — suporte para sintaxe de literal de array PostgreSQL `'{1,2,3}'::integer[]`
- **Correcoes de consulta parametrizada** — vinculacao de parametros melhorada para subconsultas e caminhos correlacionados

### Atualizacoes de versao

| Componente | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.4.2 | — |
| engine | 1.4.9 | @petradb/engine 1.4.3 |
| client | 1.4.2 | @petradb/client 1.4.2 |
| server | — | @petradb/server 1.4.3 |
| cli | — | @petradb/cli 1.4.3 |
| jdbc | 1.4.3 | — |
| drizzle | — | @petradb/drizzle 1.4.3 |

## v1.4-20260312

### Common Table Expressions (CTEs)

Suporte completo a CTEs com `WITH` e `WITH RECURSIVE`.

**CTEs nao recursivas** — subconsultas nomeadas para legibilidade e reutilizacao:

```sql
WITH active_orders AS (
  SELECT * FROM orders WHERE status = 'active'
)
SELECT customer_id, SUM(amount)
FROM active_orders
GROUP BY customer_id;
```

Multiplas CTEs podem ser definidas em uma unica consulta, e CTEs posteriores podem referenciar anteriores. Aliases de coluna sao suportados: `WITH t(x, y) AS (...)`. CTEs sombreiam nomes de tabela se compartilharem o mesmo nome.

**CTEs recursivas** — consultas iterativas para dados hierarquicos e de grafo:

```sql
WITH RECURSIVE descendants(id, name, depth) AS (
  SELECT id, name, 0 FROM employees WHERE manager_id IS NULL
  UNION ALL
  SELECT e.id, e.name, d.depth + 1
  FROM employees e INNER JOIN descendants d ON e.manager_id = d.id
)
SELECT name, depth FROM descendants ORDER BY depth, name;
```

Tanto `UNION ALL` (manter duplicatas) quanto `UNION` (deduplicado) sao suportados. Maximo de 1000 iteracoes como limite de seguranca.

### Funcoes de janela

Suporte completo a funcoes de janela com tres categorias:

**Funcoes de classificacao** — `ROW_NUMBER()`, `RANK()`, `DENSE_RANK()` com `PARTITION BY` e `ORDER BY`:

```sql
SELECT name, department, salary,
  RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dept_rank
FROM employees;
```

**Funcoes de valor** — `LAG()`, `LEAD()`, `NTILE()` com offset e valores padrao configuraveis:

```sql
SELECT name, salary,
  LAG(salary, 1, 0) OVER (ORDER BY salary) AS prev_salary,
  NTILE(4) OVER (ORDER BY salary) AS quartile
FROM employees;
```

**Funcoes de janela agregadas** — qualquer funcao de agregacao (`SUM`, `COUNT`, `AVG`, `MIN`, `MAX`, etc.) com `OVER()`, incluindo especificacoes de quadro:

```sql
SELECT name, salary,
  SUM(salary) OVER (ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_total,
  AVG(salary) OVER (PARTITION BY department) AS dept_avg
FROM employees;
```

Limites de quadro: `UNBOUNDED PRECEDING`, `UNBOUNDED FOLLOWING`, `CURRENT ROW`, `N PRECEDING`, `N FOLLOWING`. Sem clausula de quadro, funcoes de janela agregadas calculam sobre a particao inteira.

### Clausula FILTER em agregacoes

`FILTER (WHERE ...)` em funcoes de agregacao, tanto em consultas agrupadas quanto em funcoes de janela:

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE status = 'active') AS active_count,
  SUM(amount) FILTER (WHERE amount > 100) OVER (ORDER BY created_at
    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_high_value
FROM orders;
```

### Otimizacao de hash join

Equijoins sem indice agora usam uma estrategia de hash join em vez de produto cartesiano, reduzindo a complexidade do join de O(n*m) para O(n+m). Aplica-se a INNER, LEFT, RIGHT e FULL joins. Index nested loop joins continuam sendo preferidos quando um indice esta disponivel. Condicoes de non-equijoin ainda recorrem ao produto cartesiano. Visivel na saida do `EXPLAIN` como `Hash Join`, `Hash Left Join`, `Hash Right Join`, `Hash Full Join`.

### Colunas geradas

Colunas computadas com `GENERATED ALWAYS AS (expr) STORED`:

```sql
CREATE TABLE products (
  price NUMERIC,
  tax_rate NUMERIC DEFAULT 0.08,
  total NUMERIC GENERATED ALWAYS AS (price * (1 + tax_rate)) STORED
);
```

Colunas geradas sao recalculadas em INSERT e UPDATE. Elas nao podem ser definidas diretamente.

### Ordenacao de nulos em ORDER BY

`ORDER BY` agora usa a ordenacao de nulos padrao SQL: `ASC` -> `NULLS LAST`, `DESC` -> `NULLS FIRST`. Sobrescrita explicita com `NULLS FIRST` / `NULLS LAST` e suportada.

### Suporte a sequencias

Suporte completo a sequencias compativel com PostgreSQL. `CREATE SEQUENCE` e `DROP SEQUENCE` com opcoes (`INCREMENT BY`, `START WITH`, `MINVALUE`, `MAXVALUE`, `CYCLE`, `IF NOT EXISTS` / `IF EXISTS`). Funcoes de sequencia: `nextval()`, `currval()`, `setval()`, `lastval()`.

Colunas `SERIAL`, `SMALLSERIAL` e `BIGSERIAL` agora criam sequencias auxiliares (nomeadas `<table>_<column>_seq`), correspondendo ao comportamento do PostgreSQL. `DROP TABLE` cascateia para remover sequencias pertencentes. `TRUNCATE` reseta sequencias auxiliares. O estado da sequencia e totalmente transacional — `ROLLBACK` restaura contadores de sequencia. Bancos de dados persistentes serializam o estado da sequencia no catalogo.

Novos comandos SQL: `SHOW SEQUENCES`, `SHOW INDEXES` (todos os indices de todas as tabelas).

CLI: novos meta-comandos `\ds` (listar sequencias) e `\di` (listar indices).

### Clausula USING em CREATE INDEX

A sintaxe `CREATE INDEX ... USING btree` agora e aceita (btree e o unico metodo suportado). Isso melhora a compatibilidade com DDL gerado pelo PostgreSQL e ORMs.

### Correcoes de bugs

- `ORDER BY` com valores NULL: comparadores agora retornam 0 quando ambos os valores sao NULL, corrigindo resultados de ordenacao nao deterministicos com multiplas chaves de ordenacao
- Resolucao de alias em `ORDER BY`: aliases de SELECT (ex: `SELECT x AS y ... ORDER BY y`) agora resolvem corretamente em consultas nao agrupadas, com e sem funcoes de janela
- Tratamento de null em `Type.convert()`: valores NULL passados por conversao de tipo (ex: via parametros de prepared statement em UPDATE SET) agora sao preservados como NULL em vez de serem convertidos para a representacao textual do tipo. Corrigido em 13 tipos: TEXT, VARCHAR, CHAR, UUID, TIMESTAMP, DATE, TIME, TIMETZ, INTERVAL, TIMESTAMPTZ, BYTEA, JSON, ENUM
- Aviso de match exaustivo no parser ORDER BY para clausula de nulos
- Erros silenciosos no terminal do playground para throws sincronos

### Atualizacoes de versao

Todos os componentes atualizados para 1.4.1:

| Componente | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.4.1 | — |
| engine | 1.4.1 | @petradb/engine 1.4.1 |
| client | 1.4.1 | @petradb/client 1.4.1 |
| server | 1.4.1 | @petradb/server 1.4.1 |
| cli | 1.4.1 | @petradb/cli 1.4.1 |
| jdbc | 1.4.1 | — |
| knex | — | @petradb/knex 1.4.0 |
| lucid | — | @petradb/lucid 1.4.0 |
| drizzle | — | @petradb/drizzle 1.4.1 |

## v1.3-20260309

### DDL Transacional

Comandos DDL (CREATE TABLE, CREATE INDEX, DROP TABLE, etc.) agora sao totalmente suportados dentro de transacoes e sao revertidos atomicamente com DML. DDL e DML podem ser intercalados livremente dentro de um unico bloco BEGIN/COMMIT. Tanto MemoryDB quanto PersistentDB capturam um snapshot completo do catalogo no momento do BEGIN e o restauram no ROLLBACK.

### Consultas relacionais Drizzle

Suporte completo para consultas relacionais do Drizzle ORM (`db.query.*.findMany()`, `db.query.*.findFirst()`). Adicionadas funcoes escalares `json_build_array` e `json_build_object`, e corrigida a substituicao de parametros em subconsultas LATERAL.

### Atualizacoes de versao

| Componente | Maven Central | npm |
|-----------|---------------|-----|
| engine | 1.3.1 | @petradb/engine 1.3.3 |
| server | 1.3.1 | @petradb/server 1.3.1 |
| cli | 1.3.1 | @petradb/cli 1.3.1 |
| jdbc | 1.3.1 | — |
| drizzle | — | @petradb/drizzle 1.3.1 |

## v1.3-20260308

### Suporte a schemas

Namespaces de schema estilo PostgreSQL. Todo banco de dados possui um schema `public` por padrao; nomes de tabela nao qualificados resolvem para `public`. Nomes qualificados por schema (`schema.table`) funcionam em todos os comandos DDL e DML — CREATE TABLE, INSERT, UPDATE, DELETE, SELECT, ALTER TABLE, DROP TABLE, TRUNCATE, CREATE INDEX e COPY.

```sql
CREATE SCHEMA inventory;
CREATE TABLE inventory.products (id SERIAL PRIMARY KEY, name TEXT);
INSERT INTO inventory.products (name) VALUES ('Widget');
SELECT * FROM inventory.products;
```

### Tabelas virtuais information_schema

`information_schema.schemata`, `information_schema.tables` e `information_schema.columns` agora sao consultaveis. Tabelas qualificadas por schema reportam seu `table_schema` correto. Essas views sao geradas dinamicamente a partir dos metadados do banco de dados.

### Migracoes do Drizzle ORM

Nova funcao `migrate()` em `@petradb/drizzle` aplica arquivos de migracao do Drizzle Kit. Le o `meta/_journal.json` e executa arquivos de migracao SQL em ordem, rastreando migracoes aplicadas em `drizzle.__drizzle_migrations`.

```typescript
import { migrate } from "@petradb/drizzle";
await migrate(db, { migrationsFolder: "./drizzle" });
```

### Melhorias em metadados JDBC

`DatabaseMetaData.getColumns()` agora retorna valores precisos de `COLUMN_SIZE`, `DECIMAL_DIGITS` e `CHAR_OCTET_LENGTH` baseados no tipo de coluna e declaracoes de precisao/escala.

### Atualizacoes de versao

Todos os componentes atualizados para 1.3.0:

| Componente | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.3.0 | — |
| engine | 1.3.0 | @petradb/engine 1.3.0 |
| client | 1.3.0 | @petradb/client 1.3.0 |
| server | 1.3.0 | @petradb/server 1.3.0 |
| cli | 1.3.0 | @petradb/cli 1.3.0 |
| jdbc | 1.3.0 | — |
| knex | — | @petradb/knex 1.3.0 |
| lucid | — | @petradb/lucid 1.3.0 |
| drizzle | — | @petradb/drizzle 1.3.0 |

## v1.2-20260308

### Reescrita do driver Drizzle ORM

`@petradb/drizzle` reescrito de um wrapper `drizzle-orm/pg-proxy` para um driver de dialeto PostgreSQL personalizado estendendo `PgSession`/`PgPreparedQuery`/`PgTransaction` diretamente. Isso proporciona paridade completa de recursos com `drizzle-orm/node-postgres`:

- `db.transaction()` com commit/rollback automatico
- `tx.rollback()` para rollback explicito
- `returning()` em insert/update/delete (incluindo selecao parcial de colunas)
- Suporte a consultas relacionais (pendente suporte do engine para `json_build_array`/`json_agg`)

### Coercao de tipo: parametros de texto para colunas NUMERIC

`NumericType.convert` agora aceita `TextValue` e o analisa como `BigDecimal`, correspondendo ao comportamento de coercao existente de `IntegerType`, `BigintType`, `SmallintType` e `DoubleType`. Isso corrige INSERT/UPDATE parametrizado via ORMs que enviam valores numericos como texto (comportamento padrao do protocolo wire do PostgreSQL).

### Logica NULL de tres valores

Logica de tres valores SQL completa para tratamento de NULL:

- Operadores de comparacao (`=`, `!=`, `<`, `>`, `<=`, `>=`) retornam NULL quando qualquer operando e NULL
- `AND`/`OR` implementam tabelas verdade de tres valores (ex: `FALSE AND NULL` -> `FALSE`, `TRUE OR NULL` -> `TRUE`)
- `IN`/`NOT IN` propagam NULL corretamente (ex: `3 NOT IN (1, 2, NULL)` -> desconhecido)
- Aritmetica (`+`, `-`, `*`, `/`, `%`) e concatenacao de string (`||`) propagam NULL
- `LIKE` trata operandos NULL

### Gramatica de expressao unificada

As hierarquias separadas `expression` e `booleanExpression` do parser SQL foram mescladas em uma unica sintaxe de expressao. Operadores booleanos (`AND`, `OR`, `NOT`) agora sao operadores regulares na cadeia de precedencia. Isso permite expressoes booleanas em qualquer lugar onde uma expressao e valida (ex: `SELECT a > 5 AND b < 10`).

### Validacao antecipada de referencia de coluna

Referencias de coluna em `WHERE`, `GROUP BY`, `HAVING` e `ORDER BY` agora sao validadas antecipadamente no momento da construcao do plano de consulta, capturando colunas inexistentes mesmo em tabelas vazias ou ordenacoes de linha unica. Anteriormente, referencias invalidas so eram detectadas no momento da avaliacao por linha, entao consultas em tabelas vazias tinham sucesso silenciosamente.

### Correcoes de bugs

- Restricao NOT NULL nao aplicada em UPDATE
- Restricao UNIQUE rejeitava multiplos NULLs (padrao SQL: NULLs sao distintos)
- Colunas duplicadas na lista de colunas do INSERT nao eram detectadas
- `SUM`/`AVG`/`MIN`/`MAX` em tabela vazia retornava 0 em vez de NULL
- `LIKE '_'` correspondia a string vazia
- `LIMIT 0` gerava um erro

### Atualizacoes de versao

| Componente | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | — |
| engine | 1.2.9 | @petradb/engine 1.2.16 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | 1.2.6 | @petradb/server 1.2.9 |
| cli | 1.2.8 | @petradb/cli 1.2.9 |
| jdbc | 1.2.13 | — |
| knex | — | @petradb/knex 1.2.2 |
| lucid | — | @petradb/lucid 1.2.1 |
| drizzle | — | @petradb/drizzle 1.2.2 |

## v1.2-20260307

### SQL: Palavra-chave `DEFAULT` em INSERT VALUES

`INSERT INTO t (id, name) VALUES (DEFAULT, 'Alice')` agora funciona. A palavra-chave `DEFAULT` do padrao SQL era anteriormente rejeitada pelo parser, quebrando comandos INSERT gerados por ORMs que passam explicitamente `DEFAULT` para colunas serial ou com valores padrao.

### Integracao com Drizzle ORM

Novo pacote `@petradb/drizzle` fornece um driver [Drizzle ORM](https://orm.drizzle.team) com uma implementacao de dialeto PostgreSQL personalizada. Suporta definicoes de schema com `pgTable`, insert/select/update/delete, clausulas returning, `db.transaction()` com commit/rollback automatico e consultas type-safe. Paridade completa de recursos com `drizzle-orm/node-postgres`.

### Atualizacoes de versao para pacotes dependentes

Engine, server, cli e jdbc atualizados para incorporar a correcao da palavra-chave DEFAULT.

| Componente | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | — |
| engine | 1.2.7 | @petradb/engine 1.2.14 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | — | @petradb/server 1.2.7 |
| cli | — | @petradb/cli 1.2.7 |
| jdbc | 1.2.11 | — |
| knex | — | @petradb/knex 1.2.2 |
| lucid | — | @petradb/lucid 1.2.1 |
| drizzle | — | @petradb/drizzle 1.2.0 |

## v1.2-20260306

### API JS: `close()` retorna `Promise<void>`
`Session.close()` agora retorna `Promise<void>` em vez de `void`, correspondendo a API do modulo client para intercambiabilidade.

### Parsing de timestamp
`parseTimestamp` agora trata sufixo `Z`, offsets `+/-HH:MM`, milissegundos e timestamps separados por espaco com informacao de fuso horario. Remove o fuso horario para `LocalDateTime` em colunas `TIMESTAMP`.

### Completude da fachada JS
`toJS` e `typeString` agora tratam `DateValue`, `TimeValue`, `TimestampTZValue`, `TimeTZValue`, `IntervalValue` e `ByteaValue`.

### SQL: estrela qualificada (`table.*`)
A sintaxe `SELECT t.*` agora funciona em consultas, incluindo joins e expressoes mistas.

### Coercao de tipo em comparacoes
- `NumberValue` e `TextValue` agora podem comparar entre tipos (parametros de texto vs colunas numericas e vice-versa)
- `TimestampValue` agora pode comparar com `TextValue` analisando o texto como timestamp

### Driver Knex: Vinculacao de Date
`_sanitizeBindings` converte objetos JS `Date` para strings ISO antes de passar para o engine, prevenindo `DateTimeParseException` no formato `Date.toString()`.

| Componente | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | — |
| engine | 1.2.6 | @petradb/engine 1.2.13 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | — | @petradb/server 1.2.5 |
| cli | — | @petradb/cli 1.2.5 |
| jdbc | 1.2.10 | — |
| knex | — | @petradb/knex 1.2.2 |
| lucid | — | @petradb/lucid 1.2.1 |

## v1.2-20260305

### Driver JDBC
- Publicacao de fat jar — `io.github.edadma:petradb-jdbc` agora e um unico jar autocontido no Maven Central
- URLs de conexao limpos — `jdbc:petradb:memory`, `jdbc:petradb:file:/path`, `jdbc:petradb://host:port`
- Auto-descoberta via ServiceLoader — `DriverManager.getConnection()` funciona sem `Class.forName`
- Corrigidas strings de versao de metadados hardcoded

### Engine JS/TS (`@petradb/engine`)
- Adicionados `CreateViewResult`, `DropViewResult`, `ExplainResult`, `CopyResult` a fachada JS
- Adicionados `ExplainResult` e `CopyResult` as definicoes de tipo TypeScript

### Documentacao
- Novo guia Knex.js com exemplos completos
- Documentacao JDBC: adicionados trechos de instalacao Maven/Gradle/sbt, corrigido numero da porta

### Infraestrutura
- `petradb-shared` agora publicavel no Maven Central
- Script de smoke test pos-publicacao cobrindo artefatos npm, Scala e JDBC

| Componente | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.1 | — |
| engine | 1.2.2 | @petradb/engine 1.2.5 |
| client | 1.2.3 | @petradb/client 1.2.3 |
| server | — | @petradb/server 1.2.3 |
| cli | — | @petradb/cli 1.2.3 |
| jdbc | 1.2.6 | — |
| knex | — | @petradb/knex 1.2.0 |

## v1.2.2

### Reestruturacao de subpacote do engine
- Engine movido para subpacote `io.github.edadma.petradb.engine`
- Novo trait compartilhado `Session` estendido por engine e client

### Suporte a cliente CLI
- Conecte a um servidor PetraDB remoto: `petradb --host localhost --port 5480`
- Flags `--user` e `--password` para autenticacao
- Meta-comandos funcionam pela rede via SQL

### SQL
- Comando `SHOW VIEWS` retorna nomes e definicoes de views

### Dialeto Knex
- Adaptador de dialeto `@petradb/knex` para usar o construtor de consultas Knex.js com PetraDB

### Correcoes
- Correcao de publicacao npm do client
- Correcao de publicacao npm do CLI
- Corrigidas strings de versao de metadados JDBC hardcoded
- Mais de 1013 testes passando em JVM, JS e Native

## v1.2

### Driver JDBC
- Publicado no Maven Central como `petradb-jdbc`
- `getGeneratedKeys()`, `addBatch()`/`executeBatch()`, metadados de FK/indice para DBeaver
- Conexoes em modo arquivo (embarcado) e modo servidor (rede)

### Engine SQL
- `COPY FROM/TO` para importacao/exportacao CSV
- `CREATE TEMP TABLE`, `CREATE/DROP VIEW`
- Introspeccao `SHOW FOREIGN KEYS`/`SHOW INDEXES`
- Otimizacao de index nested loop join para equijoins
- Parser migrado para fastparse

### Servidor
- Suporte CORS com configuracao TOML
- `max_sessions` configuravel, porta padrao 5480
- Plataforma de servidor JS com backend HTTP Node.js

### Cliente
- Novo pacote npm `@petradb/client` com fachada JS
- Classe `Session` com `connect()`/`execute()`/`close()` retornando Promises

### CLI
- Comandos `\timing`, `\copy`
- Historico persistente no Native

### Build
- Scala 3.8.2, sbt 1.12.4
- 1000 testes passando em JVM, JS e Native

## v1.1.0

### TextDB — persistencia em arquivo de texto editavel
Um novo backend de armazenamento que persiste o banco de dados como um arquivo de texto `.ptxt`. Carrega na memoria ao abrir e reescreve o arquivo apos cada alteracao.

### Upsert — `ON CONFLICT DO UPDATE`
Semantica de inserir-ou-atualizar com a pseudo-tabela `EXCLUDED`.

### Hierarquia de excecoes melhorada
Classes de excecao tipadas substituem chamadas genericas `problem()`.

### ALTER TABLE centralizado
`DB.alterTable()` agora centraliza todo o despacho de ALTER TABLE.

## v1.0.1

- Renomear `ConnectSQL` para `Session` em `@petradb/engine`
- API assincrona `execute()` retornando `Promise<ExecuteResult[]>`
- Novo pacote `@petradb/client` para uso em rede
- Formatos de resposta alinhados entre engine e servidor

## v1.0.0

Primeiro lancamento estavel.

- Engine SQL multiplataforma (JVM, JavaScript, Native)
- Sintaxe compativel com PostgreSQL
- Armazenamento em memoria e persistente (seguro contra falhas)
- DDL, DML, joins, subconsultas, agregacoes, transacoes
- Operadores JSONB, tipos de array, restricoes CHECK
- 879 testes passando

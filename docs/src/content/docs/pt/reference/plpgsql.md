---
title: PL/pgSQL
description: Linguagem procedural — blocos DO, funcoes armazenadas e procedures armazenados.
---

O PetraDB suporta PL/pgSQL, a linguagem procedural do PostgreSQL, para escrever logica de fluxo de controle, loops e execucao SQL condicional.

## Blocos DO

Blocos anonimos que executam imediatamente sem serem armazenados:

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

## Funcoes Armazenadas

Funcoes retornam um valor e podem ser chamadas em qualquer expressao SQL:

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

Use `CREATE OR REPLACE FUNCTION` para sobrescrever uma funcao existente.

### Usando Funcoes em Consultas

Funcoes funcionam em qualquer lugar onde expressoes sao permitidas:

```sql
SELECT name, classify(score) AS grade FROM students;
SELECT * FROM orders WHERE is_valid(status);
INSERT INTO logs VALUES (format_msg(code, detail));
```

## Procedures Armazenados

Procedures executam acoes e sao invocados com `CALL`:

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

Use `CREATE OR REPLACE PROCEDURE` para sobrescrever um procedure existente.

## DROP

```sql
DROP FUNCTION factorial;
DROP FUNCTION IF EXISTS factorial;
DROP PROCEDURE seed_users;
DROP PROCEDURE IF EXISTS seed_users;
```

## Estrutura de Bloco

Todos os blocos PL/pgSQL (blocos DO, corpos de funcao, corpos de procedure) compartilham a mesma estrutura:

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

## Variaveis

Declare variaveis com um tipo e valor padrao opcional:

```sql
DECLARE
  x INT := 0;
  name TEXT;
  total NUMERIC := 100.50;
```

Variaveis sem valor padrao sao inicializadas como `NULL`. Atribuicao usa `:=`:

```sql
x := x + 1;
name := 'Alice';
total := (SELECT SUM(amount) FROM orders);
```

Variaveis podem ser usadas em qualquer comando SQL dentro do bloco — em `VALUES`, `WHERE`, `SET`, etc.

## Fluxo de Controle

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

### Loop WHILE

```sql
WHILE balance > 0 LOOP
  balance := balance - payment;
  month := month + 1;
END LOOP;
```

Maximo de 10.000 iteracoes como limite de seguranca.

### Loop FOR com Intervalo

```sql
FOR i IN 1..10 LOOP
  INSERT INTO numbers VALUES (i);
END LOOP;
```

A variavel do loop deve ser declarada em `DECLARE`. Ambos os limites sao inclusivos.

### Loop FOR com Consulta

Itere sobre resultados de consulta:

```sql
FOR name IN SELECT name FROM users ORDER BY id LOOP
  INSERT INTO greetings VALUES ('Hello, ' || name);
END LOOP;
```

Para consultas de coluna unica, a variavel recebe o valor escalar. Para consultas de multiplas colunas, a variavel recebe um registro.

## RETURN

Sair de um bloco ou retornar um valor de uma funcao:

```sql
-- Em uma funcao:
RETURN x * 2;

-- Em um bloco DO ou procedure (saida antecipada):
RETURN;
```

## RAISE

Imprimir mensagens ou lancar erros:

```sql
RAISE NOTICE 'Processing row %', row_id;
RAISE EXCEPTION 'Invalid input: %', value;
```

Placeholders `%` sao substituidos pelos valores dos argumentos em ordem. `RAISE EXCEPTION` aborta a execucao.

## PERFORM

Executar uma consulta e descartar o resultado:

```sql
PERFORM SELECT notify_user(user_id);
```

## Tratamento de Excecoes

Capturar erros dentro de um bloco:

```sql
DO $$
BEGIN
  CREATE TABLE t (id INT);
EXCEPTION
  WHEN duplicate_object THEN
    NULL;  -- tabela ja existe, ignorar
END $$;
```

Condicoes de excecao:
- `duplicate_object` — tabela/tipo/restricao ja existe
- `unique_violation` — restricao unique violada
- `others` — captura qualquer excecao

## Comando NULL

Um comando sem operacao, comumente usado em handlers de excecao:

```sql
EXCEPTION
  WHEN others THEN NULL;
```

## Triggers

Triggers executam uma funcao automaticamente quando linhas sao inseridas, atualizadas ou excluidas:

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

### Sintaxe

```sql
CREATE TRIGGER name BEFORE|AFTER INSERT|UPDATE|DELETE
  ON table FOR EACH ROW EXECUTE FUNCTION function_name();

DROP TRIGGER name ON table;
DROP TRIGGER IF EXISTS name ON table;
```

### Momento

- Triggers **BEFORE** executam antes da operacao. Retorne `NULL` para cancelar a operacao na linha. Retorne qualquer valor nao NULL para prosseguir.
- Triggers **AFTER** executam apos a operacao. O valor de retorno e ignorado.

### Variaveis Especiais

Funcoes de trigger tem acesso a:

| Variavel | Descricao |
|----------|-------------|
| `tg_op` | Nome da operacao: `'INSERT'`, `'UPDATE'` ou `'DELETE'` |
| `tg_table_name` | Nome da tabela que disparou o trigger |
| `OLD` | Linha antes da operacao (UPDATE, DELETE) |
| `NEW` | Linha apos a operacao (INSERT, UPDATE) |

### Eventos

Triggers disparam para:
- `INSERT` — incluindo linhas inseridas via `COPY FROM`
- `UPDATE` — dispara por linha atualizada
- `DELETE` — dispara por linha excluida

### Exemplo de Trigger de Guarda

```sql
CREATE FUNCTION prevent_delete() RETURNS INT AS $$
BEGIN
  RETURN NULL;  -- cancelar o DELETE
END $$ LANGUAGE plpgsql;

CREATE TRIGGER trg_protect BEFORE DELETE ON important_data
  FOR EACH ROW EXECUTE FUNCTION prevent_delete();
```

### Callbacks de Funcao Nativa

Funcoes nativas registradas (Scala, JavaScript ou C) sao chamaveis a partir de funcoes de trigger, habilitando integracao com sistemas externos:

```sql
-- Assumindo que notify_webhook() e registrada como funcao nativa
CREATE FUNCTION on_order() RETURNS INT AS $$
DECLARE dummy INT;
BEGIN
  dummy := notify_webhook(tg_op);
  RETURN 0;
END $$ LANGUAGE plpgsql;

CREATE TRIGGER trg AFTER INSERT ON orders
  FOR EACH ROW EXECUTE FUNCTION on_order();
```

Veja [API Scala](/reference/api-scala/), [API JavaScript](/reference/api-javascript/) e [API C](/reference/api-c/) para registrar funcoes nativas.

## Persistencia

Funcoes armazenadas, procedures e triggers persistem entre reinicializacoes do banco de dados tanto para armazenamento em memoria (tempo de vida da sessao) quanto persistente (sobrevive ao fechar/reabrir). O SQL fonte e armazenado e re-executado na abertura do banco de dados.

## Composicao

Funcoes podem chamar outras funcoes. Procedures podem chamar funcoes:

```sql
CREATE FUNCTION double(x INT) RETURNS INT AS $$
BEGIN RETURN x * 2; END $$ LANGUAGE plpgsql;

CREATE FUNCTION quadruple(x INT) RETURNS INT AS $$
BEGIN RETURN double(double(x)); END $$ LANGUAGE plpgsql;

SELECT quadruple(5);  -- 20
```

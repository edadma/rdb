---
title: CLI
description: Shell SQL interativo e ferramentas de linha de comando para o PetraDB.
---

## Instalacao

```bash
npm install -g @petradb/cli
```

## Uso

```bash
petradb [OPTIONS] [path]
```

Se um caminho for fornecido, o CLI abre (ou cria) um banco de dados persistente naquele local. Sem um caminho, um banco de dados em memoria e utilizado.

### Opcoes

| Opcao | Descricao |
|--------|-------------|
| `-m`, `--memory` | Usar um banco de dados em memoria (mesmo se um caminho for fornecido) |
| `-e`, `--execute` | Executar SQL e sair (repetivel) |
| `-f`, `--file` | Executar um arquivo SQL e sair (repetivel) |
| `--stdin` | Ler SQL do stdin e sair |
| `--host` | Conectar a um servidor PetraDB remoto |
| `--port` | Porta do servidor (padrao: 5480) |
| `--user` | Nome de usuario para autenticacao |
| `--password` | Senha para autenticacao |

### Exemplos

```bash
# Iniciar uma sessao interativa em memoria
petradb

# Abrir ou criar um banco de dados persistente
petradb mydata.db

# Executar SQL nao interativamente
petradb mydata.db -e "SELECT * FROM users"

# Executar um arquivo e depois uma consulta
petradb mydata.db -f schema.sql -e "SELECT count(*) FROM users"

# Enviar SQL via stdin
echo "SELECT 1 + 1" | petradb --stdin

# Conectar a um servidor remoto
petradb --host myserver.example.com --port 5480

# Conectar com autenticacao
petradb --host localhost --user admin --password secret
```

## Modos de Banco de Dados

A extensao do arquivo determina o modo de armazenamento:

| Extensao | Modo | Descricao |
|-----------|------|-------------|
| `.petra` (ou qualquer outra) | Persistente | Armazenamento duravel seguro contra falhas via paginas copy-on-write |
| `.ptxt` | Texto | Arquivo legivel por humanos, reescrito apos cada alteracao |
| *(sem caminho)* | Em memoria | Dados existem apenas durante a sessao |

## Dump

O subcomando `dump` imprime o schema completo e os dados de um banco de dados persistente como comandos SQL.

```bash
petradb dump mydata.db
```

A saida inclui comandos `CREATE TYPE`, `CREATE TABLE`, `INSERT INTO` e `CREATE VIEW` que podem recriar o banco de dados do zero.

## REPL Interativo

Quando iniciado sem `-e`, `-f` ou `--stdin`, o CLI entra em um shell SQL interativo com historico readline.

Digite SQL terminado por `;` para executar. Entrada multi-linha e suportada — um prompt de continuacao (`  -> `) aparece ate que voce termine com `;`.

### Meta-Comandos

| Comando | Descricao |
|---------|-------------|
| `\dt` | Listar todas as tabelas |
| `\dv` | Listar todas as views |
| `\ds` | Listar todas as sequencias |
| `\di` | Listar todos os indices |
| `\d <name>` | Descrever uma tabela ou view (colunas, tipos, nulabilidade) |
| `\i <file>` | Executar SQL de um arquivo |
| `\dump` | Imprimir o schema completo e dados como SQL |
| `\copy <args>` | Executar um comando `COPY` |
| `\timing` | Alternar cronometragem de consulta ligado/desligado |
| `\q` | Sair |

### Exemplo de Sessao

```
$ petradb
PetraDB — interactive SQL shell
Type \q to quit, \dt to list tables, \d <table> to describe a table.

petra> CREATE TABLE cities (name TEXT, population INT);
CREATE TABLE
petra> INSERT INTO cities (name, population) VALUES ('Oslo', 709037);
INSERT 0 1
petra> SELECT * FROM cities;
 name | population
------+------------
 Oslo |     709037
(1 row)
petra> \dt
 Table
--------
 cities
(1 row)
petra> \d cities
Table "cities"
 Column     | Type | Nullable
------------+------+----------
 name       | TEXT | null
 population | INT  | null
petra> \timing
Timing is on.
petra> SELECT count(*) FROM cities;
 count
-------
     1
(1 row)
Time: 0.002 s
petra> \q
```

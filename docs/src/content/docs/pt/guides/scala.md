---
title: Uso com Scala
description: Usando o PetraDB a partir de Scala em JVM, JS e Native.
---

## Banco de Dados em Memoria

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

given Session = new MemoryDB().connect()

val results = executeSQL("""
  CREATE TABLE products (
    id SERIAL,
    name TEXT NOT NULL,
    price NUMERIC(10,2),
    category TEXT
  );

  INSERT INTO products (name, price, category) VALUES
    ('Laptop', 999.99, 'Electronics'),
    ('Coffee', 4.50, 'Food'),
    ('Book', 19.99, 'Education');

  SELECT category, COUNT(*), AVG(price)
  FROM products
  GROUP BY category
  ORDER BY category;
""")

results.foreach(println)
```

Cada instancia `MemoryDB` e isolada e independente. Todos os dados ficam na memoria.

## Banco de Dados Persistente

O PetraDB suporta armazenamento duravel seguro contra falhas em JVM e Native via [stow](https://github.com/edadma/stow).

### Criando um Novo Banco de Dados

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

val db = PersistentDB.create("mydata.db", 4096)
given Session = db.connect()

executeSQL("""
  CREATE TABLE users (
    id SERIAL,
    name TEXT NOT NULL,
    email TEXT,
    PRIMARY KEY (id)
  );

  INSERT INTO users (name, email) VALUES ('Alice', 'alice@example.com');
""")

db.close()
```

### Reabrindo um Banco de Dados Existente

```scala
val db = PersistentDB.open("mydata.db")
given Session = db.connect()

val results = executeSQL("SELECT * FROM users")
results.foreach(println)

db.close()
```

Todas as tabelas, dados, tipos enum e estado de auto-incremento sao restaurados ao reabrir.

Bancos de dados persistentes usam paginas copy-on-write e cabecalhos com buffer duplo para seguranca contra falhas. Todas as operacoes DDL e DML sao duraveis.

## Banco de Dados de Texto

`TextDB` armazena o banco de dados como um arquivo `.ptxt` editavel por humanos. Carrega na memoria ao abrir e reescreve o arquivo apos cada alteracao. Ideal para desenvolvimento inicial, configuracao e controle de versao.

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

val db = TextDB.open("mydata.ptxt")
given Session = db.connect()

executeSQL("""
  CREATE TABLE settings (key TEXT, value TEXT);
  INSERT INTO settings (key, value) VALUES ('theme', 'dark');
""")

db.close()
```

Reabra o mesmo arquivo para restaurar todos os dados:

```scala
val db = TextDB.open("mydata.ptxt")
given Session = db.connect()

val results = executeSQL("SELECT * FROM settings")
results.foreach(println)
```

Funciona em JVM e Native. O formato `.ptxt` e legivel por humanos e amigavel para diff. Use `PersistentDB` quando precisar de durabilidade segura contra falhas para dados de producao.

## Executando SQL

`executeSQL(sql)` executa um ou mais comandos separados por ponto e virgula e retorna uma `Seq[Result]`.

```scala
val results: Seq[Result] = executeSQL("SELECT * FROM users; SELECT * FROM products;")
```

Veja a [referencia da API Scala](/reference/api-scala/) para tipos de resultado, extracao de valores e a API completa.

## Testes

```bash
# Executar testes para todas as plataformas
sbt test

# Executar apenas testes JavaScript
sbt engineJS/test

# Executar apenas testes JVM
sbt engineJVM/test

# Executar apenas testes Native
sbt engineNative/test
```

## Notas sobre Plataformas

- **JVM** — thread-safe, integra com Spring Boot, Play Framework, Akka, etc.
- **Scala.js** — executa em Node.js e navegadores
- **Scala Native** — compila para executaveis nativos; ideal para ferramentas CLI e sistemas embarcados

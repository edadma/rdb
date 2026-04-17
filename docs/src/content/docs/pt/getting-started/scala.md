---
title: Primeiros Passos com Scala
description: Adicione o PetraDB ao seu projeto Scala e execute suas primeiras consultas SQL.
---

## Instalacao

Adicione ao seu `build.sbt`:

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.2"
```

O operador `%%%` seleciona o artefato correto para sua plataforma — JVM, Scala.js ou Scala Native.

## Execute sua primeira consulta

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

given Session = new MemoryDB().connect()

val results = executeSQL("""
  CREATE TABLE users (
    id SERIAL,
    name TEXT NOT NULL,
    email TEXT
  );

  INSERT INTO users (name, email) VALUES
    ('Alice', 'alice@example.com'),
    ('Bob', 'bob@example.com');

  SELECT * FROM users;
""")

results.foreach(println)
```

Cada instancia `MemoryDB` e um banco de dados em memoria totalmente isolado. Todos os dados ficam na memoria — nada toca o sistema de arquivos.

## Armazenamento persistente

Quando voce precisa que os dados sobrevivam a reinicializacoes, o PetraDB possui duas opcoes que nao requerem infraestrutura externa:

**`PersistentDB`** — armazenamento duravel seguro contra falhas em um unico arquivo, usando paginas copy-on-write e cabecalhos com buffer duplo via [stow](https://github.com/edadma/stow). Disponivel em JVM e Native.

**`TextDB`** — armazena o banco de dados como um arquivo `.ptxt` legivel por humanos. Ideal para desenvolvimento, dados de configuracao e controle de versao.

Ambos sao cobertos em detalhes no [guia Scala](/guides/scala/).

## Experimente no navegador

Voce pode experimentar o suporte SQL do PetraDB agora mesmo — sem necessidade de configurar um projeto. O [playground](/playground/) executa o engine completo no seu navegador.

## Proximos passos

O [guia Scala](/guides/scala/) cobre bancos de dados persistentes e de texto, execucao de SQL, tratamento de resultados e a API completa. Para executar o PetraDB como um servico de rede, veja os guias [Servidor](/guides/server/) e [Cliente](/guides/client/).

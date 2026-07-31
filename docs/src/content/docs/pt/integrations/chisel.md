---
title: Chisel
description: Uma camada de acesso Scala tipada e multiplataforma para PetraDB — mais leve que um ORM.
---

Chisel e uma camada de acesso a dados tipada para PetraDB escrita em Scala 3. Ela mapeia `Value`s para tipos Scala, constroi SQL seguro contra injecao com um interpolador de strings e gera CRUD para uma tabela — sem o peso de um ORM completo (sem identity map, sem unit of work, sem proxies lazy). Funciona em todos os lugares onde PetraDB funciona: JVM, Node.js (Scala.js) e Native, usando derivacao em tempo de compilacao em vez de reflexao em tempo de execucao.

Chisel conversa diretamente com a `Session` do engine via parametros vinculados, entao nao precisa de JDBC e nao renderiza nenhum valor no texto SQL.

## Instalacao

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-chisel" % "1.5.1"
```

Chisel depende do engine para uma sessao:

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.6"
```

## Configuracao

Tudo roda sobre uma `Session` e precisa de um `ExecutionContext`. As operacoes do Chisel retornam `Future`s.

```scala
import io.github.edadma.petradb.Session
import io.github.edadma.petradb.engine.MemoryDB
import io.github.edadma.petradb.chisel.*

import scala.concurrent.ExecutionContext.Implicits.global

given Session = new MemoryDB().connect()
```

A sessao e passada implicitamente para cada operacao do Chisel, entao um `given Session` em escopo e tudo o que e necessario.

## Mapeamento de tipos

Chisel mapeia entre valores Scala e `Value`s do PetraDB atraves de quatro type classes:

| Type class | Direcao | Papel |
|---|---|---|
| `Get[A]` | `Value => A` | decodifica uma coluna |
| `Put[A]` | `A => Value` | codifica um valor (vincula parametros) |
| `Read[A]` | `Row => A` | decodifica uma linha inteira |
| `Write[A]` | `A => Seq[(String, Value)]` | codifica uma linha inteira |

### Codecs de coluna embutidos

Instancias de `Get` e `Put` sao fornecidas para os tipos escalares comuns:

| Tipo Scala | Tipo PetraDB |
|---|---|
| `Int`, `Long`, `Short`, `Byte` | familia integer |
| `Double`, `Float` | ponto flutuante |
| `BigDecimal` | `NUMERIC` |
| `String` | `TEXT` |
| `Boolean` | `BOOLEAN` |
| `java.time.LocalDate` | `DATE` |
| `java.time.LocalTime` | `TIME` |
| `java.time.LocalDateTime` | `TIMESTAMP` |
| `java.time.OffsetDateTime` | `TIMESTAMPTZ` |
| `java.time.OffsetTime` | `TIMETZ` |
| `java.time.Duration` | `INTERVAL` |
| `Array[Byte]` | `BYTEA` |
| `java.util.UUID` | `UUID` |
| `Option[A]` | `A` nulavel (`None` ⇄ SQL `NULL`) |

Uma coluna cujo tipo nao corresponde levanta `DecodeException`.

### Mapeamento de linha para case classes

Derive `Read` e `Write` para uma case class para mapear linhas inteiras. A derivacao e por nome de campo, entao ela e robusta a reordenacao de colunas e funciona diretamente com `SELECT *`:

```scala
case class User(id: Long, name: String, age: Int) derives Read, Write
```

Campos `Option` mapeiam para colunas nulaveis:

```scala
case class Account(id: Long, nickname: Option[String]) derives Read, Write
```

### Named tuples

Qualquer product deriva, incluindo uma named tuple — util para projecoes ad-hoc sem declarar uma classe:

```scala
val r = Read.derived[(id: Long, name: String)]
```

### Adaptando codecs

`Get`/`Read` tem `.map` e `Put`/`Write` tem `.contramap` para adaptar a tipos que o Chisel nao conhece:

```scala
enum Color:
  case Red, Green, Blue

given Get[Color] = Get[String].map(Color.valueOf)
given Put[Color] = Put[String].contramap[Color](_.toString)
```

## O interpolador `sql`

`sql"…"` constroi um `Fragment` — texto SQL mais parametros vinculados. Cada argumento interpolado e codificado atraves de seu `Put` e vinculado como um parametro, entao os valores nunca se tornam texto SQL e a injecao e impossivel:

```scala
val minAge = 18
val frag   = sql"select id, name, age from users where age >= $minAge"
```

Um `Fragment` renderiza para a forma de placeholder `$1`, `$2`, … do engine:

```scala
frag.sql    // "select id, name, age from users where age >= $1"
frag.params // Seq(NumberValue(18))
```

`Option` e `Value`s pre-construidos tambem podem ser interpolados — `None` vincula SQL `NULL`.

### Compondo fragments

Fragments sao armazenados sem renderizar, entao eles compoem com `++` e renumeram placeholders automaticamente. `Fragment.const` contribui com texto bruto (para identificadores confiaveis como nomes de tabela), `Fragment.param` um unico valor vinculado:

```scala
val table = "users"
val q = sql"select * from " ++ Fragment.const(table) ++ sql" where age >= $minAge"
```

## Executando consultas

Anexe um decodificador de linha com `.query[A]`, depois escolha como coletar o resultado:

```scala
val users: Future[List[User]] =
  sql"select * from users order by id".query[User].toList

val one: Future[Option[User]] =
  sql"select * from users where id = ${1L}".query[User].option

val exactlyOne: Future[User] =
  sql"select * from users where id = ${1L}".query[User].unique
```

| Metodo | Resultado | Notas |
|---|---|---|
| `.toList` | `Future[List[A]]` | todas as linhas |
| `.toVector` | `Future[Vector[A]]` | todas as linhas |
| `.option` | `Future[Option[A]]` | primeira linha ou `None` |
| `.unique` | `Future[A]` | exatamente uma linha; falha caso contrario |

`INSERT … RETURNING` tambem decodifica atraves de `.query`:

```scala
val inserted: Future[User] =
  sql"insert into users (name, age) values (${"alice"}, ${30}) returning *"
    .query[User].unique
```

### Resultados escalares

Leia a primeira coluna da primeira linha diretamente com `queryValue` — para `count(*)`, `exists`, `max` e similares:

```scala
val total: Future[Long]        = sql"select count(*) from users".queryValue[Long]
val maybeMax: Future[Option[Int]] = sql"select max(age) from users".queryValueOption[Int]
```

## Executando comandos

Para SQL que nao e de consulta, `.update` retorna a contagem de linhas afetadas e `.run` retorna os resultados brutos do engine:

```scala
val changed: Future[Int] =
  sql"update users set age = ${31} where id = ${1L}".update

val raw: Future[Seq[Result]] =
  sql"create table users (id serial primary key, name text, age integer)".run
```

## Repositorios

`Repo[A, Id]` gera CRUD para uma unica tabela a partir das instancias `Read`/`Write` da entidade. Use `sql"…"` para qualquer coisa que ele nao cubra.

```scala
case class User(id: Long, name: String, age: Int) derives Read, Write

val users = Repo[User, Long]("users") // idColumn = "id", generatedId = true
```

Por padrao, `Repo` assume uma chave gerada pelo banco de dados (`SERIAL`), entao `insert`/`insertReturning` omitem a coluna id e o banco de dados a atribui. Passe um id placeholder ao construir a entidade — ele e ignorado no insert:

```scala
val saved: Future[User] = users.insertReturning(User(0, "alice", 30))
// saved.id e a chave gerada
```

### Operacoes

```scala
users.findById(1L)              // Future[Option[User]]
users.findAll                   // Future[List[User]]
users.count                     // Future[Long]
users.existsById(1L)            // Future[Boolean]

users.insert(User(0, "bob", 25))        // Future[Int] — linhas inseridas
users.insertReturning(User(0, "eve", 22)) // Future[User] — recupera o id gerado

users.update(saved.copy(age = 31))      // Future[Int] — define todas as colunas nao-id onde id corresponde
users.deleteById(1L)                    // Future[Int]
users.deleteAll                         // Future[Int]
```

### Chaves fornecidas pelo chamador

Para tabelas cuja chave primaria e fornecida pela aplicacao (nao gerada), defina `generatedId = false` e nomeie a coluna id se ela nao for `"id"`:

```scala
case class Widget(sku: String, label: String) derives Read, Write

val widgets = Repo[Widget, String]("widgets", idColumn = "sku", generatedId = false)

widgets.insert(Widget("w-1", "Sprocket"))
widgets.findById("w-1")
```

## Notas multiplataforma

Chisel e publicado para JVM, Scala.js e Scala Native a partir de uma unica fonte. A decodificacao de linha usa derivacao inline de `Mirror` do Scala 3, entao nao ha reflexao em tempo de execucao em nenhuma plataforma.

## Limpeza

```scala
summon[Session].close()
```

---
title: Chisel
description: A typed, cross-platform Scala access layer for PetraDB — lighter than an ORM.
---

Chisel is a typed data-access layer for PetraDB written in Scala 3. It maps `Value`s to Scala types, builds injection-safe SQL with a string interpolator, and generates CRUD for a table — without the weight of a full ORM (no identity map, no unit of work, no lazy proxies). It runs everywhere PetraDB does: JVM, Node.js (Scala.js), and Native, using compile-time derivation rather than runtime reflection.

Chisel talks to the engine's `Session` directly via bound parameters, so it needs no JDBC and renders no values into SQL text.

## Install

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-chisel" % "1.5.1"
```

Chisel depends on the engine for a session:

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.6"
```

## Setup

Everything runs against a `Session` and needs an `ExecutionContext`. Chisel's operations return `Future`s.

```scala
import io.github.edadma.petradb.Session
import io.github.edadma.petradb.engine.MemoryDB
import io.github.edadma.petradb.chisel.*

import scala.concurrent.ExecutionContext.Implicits.global

given Session = new MemoryDB().connect()
```

The session is passed implicitly to every Chisel operation, so a `given Session` in scope is all that's required.

## Mapping types

Chisel maps between Scala values and PetraDB `Value`s through four type classes:

| Type class | Direction | Role |
|---|---|---|
| `Get[A]` | `Value => A` | decode one column |
| `Put[A]` | `A => Value` | encode one value (bind parameters) |
| `Read[A]` | `Row => A` | decode a whole row |
| `Write[A]` | `A => Seq[(String, Value)]` | encode a whole row |

### Built-in column codecs

`Get` and `Put` instances are provided for the common scalar types:

| Scala type | PetraDB type |
|---|---|
| `Int`, `Long`, `Short`, `Byte` | integer family |
| `Double`, `Float` | floating point |
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
| `Option[A]` | nullable `A` (`None` ⇄ SQL `NULL`) |

A column whose type doesn't match raises `DecodeException`.

### Row mapping for case classes

Derive `Read` and `Write` for a case class to map whole rows. Derivation is by field name, so it is robust to column reordering and works directly with `SELECT *`:

```scala
case class User(id: Long, name: String, age: Int) derives Read, Write
```

`Option` fields map to nullable columns:

```scala
case class Account(id: Long, nickname: Option[String]) derives Read, Write
```

### Named tuples

Any product derives, including a named tuple — handy for ad-hoc projections without declaring a class:

```scala
val r = Read.derived[(id: Long, name: String)]
```

### Adapting codecs

`Get`/`Read` have `.map` and `Put`/`Write` have `.contramap` for adapting to types Chisel doesn't know:

```scala
enum Color:
  case Red, Green, Blue

given Get[Color] = Get[String].map(Color.valueOf)
given Put[Color] = Put[String].contramap[Color](_.toString)
```

## The `sql` interpolator

`sql"…"` builds a `Fragment` — SQL text plus bound parameters. Each interpolated argument is encoded through its `Put` and bound as a parameter, so values never become SQL text and injection is impossible:

```scala
val minAge = 18
val frag   = sql"select id, name, age from users where age >= $minAge"
```

A `Fragment` renders to the engine's `$1`, `$2`, … placeholder form:

```scala
frag.sql    // "select id, name, age from users where age >= $1"
frag.params // Seq(NumberValue(18))
```

`Option` and pre-built `Value`s can be interpolated too — `None` binds SQL `NULL`.

### Composing fragments

Fragments are stored unrendered, so they compose with `++` and re-number placeholders automatically. `Fragment.const` contributes raw text (for trusted identifiers like table names), `Fragment.param` a single bound value:

```scala
val table = "users"
val q = sql"select * from " ++ Fragment.const(table) ++ sql" where age >= $minAge"
```

## Running queries

Attach a row decoder with `.query[A]`, then choose how to collect the result:

```scala
val users: Future[List[User]] =
  sql"select * from users order by id".query[User].toList

val one: Future[Option[User]] =
  sql"select * from users where id = ${1L}".query[User].option

val exactlyOne: Future[User] =
  sql"select * from users where id = ${1L}".query[User].unique
```

| Method | Result | Notes |
|---|---|---|
| `.toList` | `Future[List[A]]` | all rows |
| `.toVector` | `Future[Vector[A]]` | all rows |
| `.option` | `Future[Option[A]]` | first row or `None` |
| `.unique` | `Future[A]` | exactly one row; fails otherwise |

`INSERT … RETURNING` decodes through `.query` as well:

```scala
val inserted: Future[User] =
  sql"insert into users (name, age) values (${"alice"}, ${30}) returning *"
    .query[User].unique
```

### Scalar results

Read the first column of the first row directly with `queryValue` — for `count(*)`, `exists`, `max`, and the like:

```scala
val total: Future[Long]        = sql"select count(*) from users".queryValue[Long]
val maybeMax: Future[Option[Int]] = sql"select max(age) from users".queryValueOption[Int]
```

## Running statements

For non-query SQL, `.update` returns the affected-row count and `.run` returns the raw engine results:

```scala
val changed: Future[Int] =
  sql"update users set age = ${31} where id = ${1L}".update

val raw: Future[Seq[Result]] =
  sql"create table users (id serial primary key, name text, age integer)".run
```

## Repositories

`Repo[A, Id]` generates CRUD for a single table from the entity's `Read`/`Write` instances. Use `sql"…"` for anything it doesn't cover.

```scala
case class User(id: Long, name: String, age: Int) derives Read, Write

val users = Repo[User, Long]("users") // idColumn = "id", generatedId = true
```

By default `Repo` assumes a database-generated key (`SERIAL`), so `insert`/`insertReturning` omit the id column and the database assigns it. Pass a placeholder id when constructing the entity — it is ignored on insert:

```scala
val saved: Future[User] = users.insertReturning(User(0, "alice", 30))
// saved.id is the generated key
```

### Operations

```scala
users.findById(1L)              // Future[Option[User]]
users.findAll                   // Future[List[User]]
users.count                     // Future[Long]
users.existsById(1L)            // Future[Boolean]

users.insert(User(0, "bob", 25))        // Future[Int] — rows inserted
users.insertReturning(User(0, "eve", 22)) // Future[User] — recovers generated id

users.update(saved.copy(age = 31))      // Future[Int] — sets all non-id columns where id matches
users.deleteById(1L)                    // Future[Int]
users.deleteAll                         // Future[Int]
```

### Caller-supplied keys

For tables whose primary key is provided by the application (not generated), set `generatedId = false` and name the id column if it isn't `"id"`:

```scala
case class Widget(sku: String, label: String) derives Read, Write

val widgets = Repo[Widget, String]("widgets", idColumn = "sku", generatedId = false)

widgets.insert(Widget("w-1", "Sprocket"))
widgets.findById("w-1")
```

## Cross-platform notes

Chisel is published for JVM, Scala.js, and Scala Native from a single source. Row decoding uses Scala 3 inline `Mirror` derivation, so there is no runtime reflection on any platform.

## Clean up

```scala
summon[Session].close()
```

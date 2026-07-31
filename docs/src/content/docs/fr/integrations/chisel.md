---
title: Chisel
description: Une couche d'accès Scala typée et multiplateforme pour PetraDB — plus légère qu'un ORM.
---

Chisel est une couche d'accès aux données typée pour PetraDB écrite en Scala 3. Elle mappe les `Value`s vers des types Scala, construit du SQL à l'abri des injections grâce à un interpolateur de chaînes, et génère les opérations CRUD pour une table — sans le poids d'un ORM complet (pas d'identity map, pas d'unit of work, pas de proxies paresseux). Elle fonctionne partout où PetraDB fonctionne : JVM, Node.js (Scala.js) et Native, en utilisant la dérivation à la compilation plutôt que la réflexion à l'exécution.

Chisel dialogue directement avec la `Session` du moteur via des paramètres liés, elle n'a donc besoin d'aucun JDBC et ne rend aucune valeur dans le texte SQL.

## Installation

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-chisel" % "1.5.1"
```

Chisel dépend du moteur pour obtenir une session :

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.6"
```

## Configuration

Tout s'exécute sur une `Session` et nécessite un `ExecutionContext`. Les opérations de Chisel retournent des `Future`s.

```scala
import io.github.edadma.petradb.Session
import io.github.edadma.petradb.engine.MemoryDB
import io.github.edadma.petradb.chisel.*

import scala.concurrent.ExecutionContext.Implicits.global

given Session = new MemoryDB().connect()
```

La session est passée implicitement à chaque opération de Chisel ; un `given Session` dans la portée est donc tout ce qui est requis.

## Mapping des types

Chisel fait correspondre les valeurs Scala et les `Value`s de PetraDB à travers quatre type classes :

| Type class | Direction | Rôle |
|---|---|---|
| `Get[A]` | `Value => A` | décode une colonne |
| `Put[A]` | `A => Value` | encode une valeur (lie les paramètres) |
| `Read[A]` | `Row => A` | décode une ligne entière |
| `Write[A]` | `A => Seq[(String, Value)]` | encode une ligne entière |

### Codecs de colonnes intégrés

Des instances de `Get` et `Put` sont fournies pour les types scalaires courants :

| Type Scala | Type PetraDB |
|---|---|
| `Int`, `Long`, `Short`, `Byte` | famille entière |
| `Double`, `Float` | virgule flottante |
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
| `Option[A]` | `A` nullable (`None` ⇄ SQL `NULL`) |

Une colonne dont le type ne correspond pas lève une `DecodeException`.

### Mapping de lignes pour les case classes

Dérivez `Read` et `Write` pour une case class afin de mapper des lignes entières. La dérivation se fait par nom de champ, elle est donc robuste au réordonnancement des colonnes et fonctionne directement avec `SELECT *` :

```scala
case class User(id: Long, name: String, age: Int) derives Read, Write
```

Les champs `Option` correspondent à des colonnes nullables :

```scala
case class Account(id: Long, nickname: Option[String]) derives Read, Write
```

### Tuples nommés

Tout produit dérive, y compris un tuple nommé — pratique pour des projections ad hoc sans déclarer de classe :

```scala
val r = Read.derived[(id: Long, name: String)]
```

### Adaptation des codecs

`Get`/`Read` possèdent `.map` et `Put`/`Write` possèdent `.contramap` pour s'adapter à des types que Chisel ne connaît pas :

```scala
enum Color:
  case Red, Green, Blue

given Get[Color] = Get[String].map(Color.valueOf)
given Put[Color] = Put[String].contramap[Color](_.toString)
```

## L'interpolateur `sql`

`sql"…"` construit un `Fragment` — du texte SQL accompagné de paramètres liés. Chaque argument interpolé est encodé via son `Put` et lié comme paramètre, de sorte que les valeurs ne deviennent jamais du texte SQL et que l'injection est impossible :

```scala
val minAge = 18
val frag   = sql"select id, name, age from users where age >= $minAge"
```

Un `Fragment` se rend sous la forme à espaces réservés `$1`, `$2`, … du moteur :

```scala
frag.sql    // "select id, name, age from users where age >= $1"
frag.params // Seq(NumberValue(18))
```

Les `Option` et les `Value`s pré-construites peuvent aussi être interpolées — `None` lie un `NULL` SQL.

### Composition de fragments

Les fragments sont stockés non rendus, ils se composent donc avec `++` et renumérotent automatiquement les espaces réservés. `Fragment.const` apporte du texte brut (pour des identifiants de confiance comme les noms de tables), `Fragment.param` une seule valeur liée :

```scala
val table = "users"
val q = sql"select * from " ++ Fragment.const(table) ++ sql" where age >= $minAge"
```

## Exécution de requêtes

Attachez un décodeur de lignes avec `.query[A]`, puis choisissez comment collecter le résultat :

```scala
val users: Future[List[User]] =
  sql"select * from users order by id".query[User].toList

val one: Future[Option[User]] =
  sql"select * from users where id = ${1L}".query[User].option

val exactlyOne: Future[User] =
  sql"select * from users where id = ${1L}".query[User].unique
```

| Méthode | Résultat | Notes |
|---|---|---|
| `.toList` | `Future[List[A]]` | toutes les lignes |
| `.toVector` | `Future[Vector[A]]` | toutes les lignes |
| `.option` | `Future[Option[A]]` | première ligne ou `None` |
| `.unique` | `Future[A]` | exactement une ligne ; échoue sinon |

`INSERT … RETURNING` se décode également via `.query` :

```scala
val inserted: Future[User] =
  sql"insert into users (name, age) values (${"alice"}, ${30}) returning *"
    .query[User].unique
```

### Résultats scalaires

Lisez directement la première colonne de la première ligne avec `queryValue` — pour `count(*)`, `exists`, `max` et compagnie :

```scala
val total: Future[Long]        = sql"select count(*) from users".queryValue[Long]
val maybeMax: Future[Option[Int]] = sql"select max(age) from users".queryValueOption[Int]
```

## Exécution d'instructions

Pour le SQL hors requête, `.update` retourne le nombre de lignes affectées et `.run` retourne les résultats bruts du moteur :

```scala
val changed: Future[Int] =
  sql"update users set age = ${31} where id = ${1L}".update

val raw: Future[Seq[Result]] =
  sql"create table users (id serial primary key, name text, age integer)".run
```

## Repositories

`Repo[A, Id]` génère les opérations CRUD pour une seule table à partir des instances `Read`/`Write` de l'entité. Utilisez `sql"…"` pour tout ce qu'il ne couvre pas.

```scala
case class User(id: Long, name: String, age: Int) derives Read, Write

val users = Repo[User, Long]("users") // idColumn = "id", generatedId = true
```

Par défaut, `Repo` suppose une clé générée par la base de données (`SERIAL`), de sorte que `insert`/`insertReturning` omettent la colonne id et que la base l'attribue. Passez un id factice lors de la construction de l'entité — il est ignoré à l'insertion :

```scala
val saved: Future[User] = users.insertReturning(User(0, "alice", 30))
// saved.id est la clé générée
```

### Opérations

```scala
users.findById(1L)              // Future[Option[User]]
users.findAll                   // Future[List[User]]
users.count                     // Future[Long]
users.existsById(1L)            // Future[Boolean]

users.insert(User(0, "bob", 25))        // Future[Int] — lignes insérées
users.insertReturning(User(0, "eve", 22)) // Future[User] — récupère l'id généré

users.update(saved.copy(age = 31))      // Future[Int] — définit toutes les colonnes hors id où l'id correspond
users.deleteById(1L)                    // Future[Int]
users.deleteAll                         // Future[Int]
```

### Clés fournies par l'appelant

Pour les tables dont la clé primaire est fournie par l'application (et non générée), définissez `generatedId = false` et nommez la colonne id si ce n'est pas `"id"` :

```scala
case class Widget(sku: String, label: String) derives Read, Write

val widgets = Repo[Widget, String]("widgets", idColumn = "sku", generatedId = false)

widgets.insert(Widget("w-1", "Sprocket"))
widgets.findById("w-1")
```

## Notes multiplateformes

Chisel est publié pour JVM, Scala.js et Scala Native à partir d'une source unique. Le décodage des lignes utilise la dérivation `Mirror` inline de Scala 3, il n'y a donc aucune réflexion à l'exécution sur aucune plateforme.

## Nettoyage

```scala
summon[Session].close()
```

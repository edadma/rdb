---
title: Chisel
description: Una capa de acceso tipada y multiplataforma en Scala para PetraDB, mas ligera que un ORM.
---

Chisel es una capa de acceso a datos tipada para PetraDB escrita en Scala 3. Mapea `Value`s a tipos de Scala, construye SQL seguro ante inyecciones con un interpolador de cadenas y genera operaciones CRUD para una tabla, sin el peso de un ORM completo (sin mapa de identidad, sin unidad de trabajo, sin proxies perezosos). Funciona en todas partes donde PetraDB funciona: JVM, Node.js (Scala.js) y Native, usando derivacion en tiempo de compilacion en lugar de reflexion en tiempo de ejecucion.

Chisel se comunica directamente con la `Session` del motor mediante parametros vinculados, por lo que no necesita JDBC y no renderiza ningun valor dentro del texto SQL.

## Instalacion

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-chisel" % "1.5.1"
```

Chisel depende del motor para obtener una sesion:

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.6"
```

## Configuracion

Todo se ejecuta contra una `Session` y necesita un `ExecutionContext`. Las operaciones de Chisel retornan `Future`s.

```scala
import io.github.edadma.petradb.Session
import io.github.edadma.petradb.engine.MemoryDB
import io.github.edadma.petradb.chisel.*

import scala.concurrent.ExecutionContext.Implicits.global

given Session = new MemoryDB().connect()
```

La sesion se pasa implicitamente a cada operacion de Chisel, asi que basta con tener un `given Session` en alcance.

## Mapeo de tipos

Chisel mapea entre valores de Scala y `Value`s de PetraDB a traves de cuatro clases de tipos:

| Clase de tipo | Direccion | Rol |
|---|---|---|
| `Get[A]` | `Value => A` | decodifica una columna |
| `Put[A]` | `A => Value` | codifica un valor (vincula parametros) |
| `Read[A]` | `Row => A` | decodifica una fila completa |
| `Write[A]` | `A => Seq[(String, Value)]` | codifica una fila completa |

### Codecs de columna integrados

Se proporcionan instancias de `Get` y `Put` para los tipos escalares comunes:

| Tipo de Scala | Tipo de PetraDB |
|---|---|
| `Int`, `Long`, `Short`, `Byte` | familia de enteros |
| `Double`, `Float` | punto flotante |
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
| `Option[A]` | `A` anulable (`None` ⇄ SQL `NULL`) |

Una columna cuyo tipo no coincide lanza `DecodeException`.

### Mapeo de filas para case classes

Deriva `Read` y `Write` para una case class para mapear filas completas. La derivacion es por nombre de campo, asi que es robusta ante reordenamientos de columnas y funciona directamente con `SELECT *`:

```scala
case class User(id: Long, name: String, age: Int) derives Read, Write
```

Los campos `Option` se mapean a columnas anulables:

```scala
case class Account(id: Long, nickname: Option[String]) derives Read, Write
```

### Tuplas con nombre

Cualquier producto deriva, incluyendo una tupla con nombre, util para proyecciones ad-hoc sin declarar una clase:

```scala
val r = Read.derived[(id: Long, name: String)]
```

### Adaptacion de codecs

`Get`/`Read` tienen `.map` y `Put`/`Write` tienen `.contramap` para adaptarse a tipos que Chisel no conoce:

```scala
enum Color:
  case Red, Green, Blue

given Get[Color] = Get[String].map(Color.valueOf)
given Put[Color] = Put[String].contramap[Color](_.toString)
```

## El interpolador `sql`

`sql"…"` construye un `Fragment`: texto SQL mas parametros vinculados. Cada argumento interpolado se codifica a traves de su `Put` y se vincula como un parametro, por lo que los valores nunca se convierten en texto SQL y la inyeccion es imposible:

```scala
val minAge = 18
val frag   = sql"select id, name, age from users where age >= $minAge"
```

Un `Fragment` se renderiza al formato de marcadores de posicion `$1`, `$2`, … del motor:

```scala
frag.sql    // "select id, name, age from users where age >= $1"
frag.params // Seq(NumberValue(18))
```

Tambien se pueden interpolar `Option` y `Value`s preconstruidos; `None` vincula SQL `NULL`.

### Composicion de fragmentos

Los fragmentos se almacenan sin renderizar, asi que se componen con `++` y renumeran los marcadores de posicion automaticamente. `Fragment.const` aporta texto sin procesar (para identificadores confiables como nombres de tabla), `Fragment.param` un unico valor vinculado:

```scala
val table = "users"
val q = sql"select * from " ++ Fragment.const(table) ++ sql" where age >= $minAge"
```

## Ejecucion de consultas

Adjunta un decodificador de filas con `.query[A]`, luego elige como recolectar el resultado:

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
| `.toList` | `Future[List[A]]` | todas las filas |
| `.toVector` | `Future[Vector[A]]` | todas las filas |
| `.option` | `Future[Option[A]]` | primera fila o `None` |
| `.unique` | `Future[A]` | exactamente una fila; falla en caso contrario |

`INSERT … RETURNING` tambien se decodifica a traves de `.query`:

```scala
val inserted: Future[User] =
  sql"insert into users (name, age) values (${"alice"}, ${30}) returning *"
    .query[User].unique
```

### Resultados escalares

Lee la primera columna de la primera fila directamente con `queryValue`, para `count(*)`, `exists`, `max` y similares:

```scala
val total: Future[Long]        = sql"select count(*) from users".queryValue[Long]
val maybeMax: Future[Option[Int]] = sql"select max(age) from users".queryValueOption[Int]
```

## Ejecucion de sentencias

Para SQL que no es de consulta, `.update` retorna el conteo de filas afectadas y `.run` retorna los resultados sin procesar del motor:

```scala
val changed: Future[Int] =
  sql"update users set age = ${31} where id = ${1L}".update

val raw: Future[Seq[Result]] =
  sql"create table users (id serial primary key, name text, age integer)".run
```

## Repositorios

`Repo[A, Id]` genera operaciones CRUD para una sola tabla a partir de las instancias `Read`/`Write` de la entidad. Usa `sql"…"` para cualquier cosa que no cubra.

```scala
case class User(id: Long, name: String, age: Int) derives Read, Write

val users = Repo[User, Long]("users") // idColumn = "id", generatedId = true
```

Por defecto, `Repo` asume una clave generada por la base de datos (`SERIAL`), asi que `insert`/`insertReturning` omiten la columna id y la base de datos la asigna. Pasa un id de marcador de posicion al construir la entidad; se ignora en el insert:

```scala
val saved: Future[User] = users.insertReturning(User(0, "alice", 30))
// saved.id es la clave generada
```

### Operaciones

```scala
users.findById(1L)              // Future[Option[User]]
users.findAll                   // Future[List[User]]
users.count                     // Future[Long]
users.existsById(1L)            // Future[Boolean]

users.insert(User(0, "bob", 25))        // Future[Int] — filas insertadas
users.insertReturning(User(0, "eve", 22)) // Future[User] — recupera el id generado

users.update(saved.copy(age = 31))      // Future[Int] — establece todas las columnas no-id donde el id coincide
users.deleteById(1L)                    // Future[Int]
users.deleteAll                         // Future[Int]
```

### Claves proporcionadas por el llamador

Para tablas cuya clave primaria es proporcionada por la aplicacion (no generada), establece `generatedId = false` y nombra la columna id si no es `"id"`:

```scala
case class Widget(sku: String, label: String) derives Read, Write

val widgets = Repo[Widget, String]("widgets", idColumn = "sku", generatedId = false)

widgets.insert(Widget("w-1", "Sprocket"))
widgets.findById("w-1")
```

## Notas multiplataforma

Chisel se publica para JVM, Scala.js y Scala Native desde un unico codigo fuente. La decodificacion de filas usa derivacion inline de `Mirror` de Scala 3, asi que no hay reflexion en tiempo de ejecucion en ninguna plataforma.

## Limpieza

```scala
summon[Session].close()
```

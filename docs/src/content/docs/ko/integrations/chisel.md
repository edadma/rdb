---
title: Chisel
description: PetraDB용 타입 안전 크로스 플랫폼 Scala 접근 계층 — ORM보다 가볍습니다.
---

Chisel은 Scala 3로 작성된 PetraDB용 타입 안전 데이터 접근 계층입니다. `Value`를 Scala 타입에 매핑하고, 문자열 인터폴레이터로 인젝션에 안전한 SQL을 빌드하며, 테이블에 대한 CRUD를 생성합니다 — 완전한 ORM의 무게 없이 말입니다(아이덴티티 맵 없음, 작업 단위(unit of work) 없음, 지연 프록시 없음). PetraDB가 실행되는 모든 곳에서 실행됩니다: JVM, Node.js(Scala.js), Native에서, 런타임 리플렉션이 아닌 컴파일 타임 파생을 사용합니다.

Chisel은 바인딩된 파라미터를 통해 엔진의 `Session`과 직접 통신하므로, JDBC가 필요 없고 SQL 텍스트에 값을 렌더링하지 않습니다.

## 설치

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-chisel" % "1.5.0"
```

Chisel은 세션을 위해 엔진에 의존합니다:

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.4"
```

## 설정

모든 것은 `Session`을 대상으로 실행되며 `ExecutionContext`가 필요합니다. Chisel의 작업은 `Future`를 반환합니다.

```scala
import io.github.edadma.petradb.Session
import io.github.edadma.petradb.engine.MemoryDB
import io.github.edadma.petradb.chisel.*

import scala.concurrent.ExecutionContext.Implicits.global

given Session = new MemoryDB().connect()
```

세션은 모든 Chisel 작업에 암시적으로 전달되므로, 스코프 내에 `given Session`만 있으면 됩니다.

## 타입 매핑

Chisel은 네 가지 타입 클래스를 통해 Scala 값과 PetraDB `Value` 사이를 매핑합니다:

| 타입 클래스 | 방향 | 역할 |
|---|---|---|
| `Get[A]` | `Value => A` | 컬럼 하나를 디코딩 |
| `Put[A]` | `A => Value` | 값 하나를 인코딩(파라미터 바인딩) |
| `Read[A]` | `Row => A` | 행 전체를 디코딩 |
| `Write[A]` | `A => Seq[(String, Value)]` | 행 전체를 인코딩 |

### 내장 컬럼 코덱

`Get`과 `Put` 인스턴스는 일반적인 스칼라 타입에 대해 제공됩니다:

| Scala 타입 | PetraDB 타입 |
|---|---|
| `Int`, `Long`, `Short`, `Byte` | 정수 계열 |
| `Double`, `Float` | 부동 소수점 |
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
| `Option[A]` | null 허용 `A` (`None` ⇄ SQL `NULL`) |

타입이 일치하지 않는 컬럼은 `DecodeException`을 발생시킵니다.

### 케이스 클래스의 행 매핑

행 전체를 매핑하려면 케이스 클래스에 대해 `Read`와 `Write`를 파생합니다. 파생은 필드 이름 기준이므로 컬럼 재정렬에 견고하며 `SELECT *`와 직접 동작합니다:

```scala
case class User(id: Long, name: String, age: Int) derives Read, Write
```

`Option` 필드는 null 허용 컬럼에 매핑됩니다:

```scala
case class Account(id: Long, nickname: Option[String]) derives Read, Write
```

### 네임드 튜플

네임드 튜플을 포함한 모든 프로덕트가 파생됩니다 — 클래스를 선언하지 않고 임시 프로젝션을 만들 때 편리합니다:

```scala
val r = Read.derived[(id: Long, name: String)]
```

### 코덱 적응

`Get`/`Read`에는 `.map`이, `Put`/`Write`에는 `.contramap`이 있어 Chisel이 모르는 타입에 적응할 수 있습니다:

```scala
enum Color:
  case Red, Green, Blue

given Get[Color] = Get[String].map(Color.valueOf)
given Put[Color] = Put[String].contramap[Color](_.toString)
```

## `sql` 인터폴레이터

`sql"…"`은 `Fragment`를 빌드합니다 — SQL 텍스트와 바인딩된 파라미터입니다. 인터폴레이션된 각 인수는 자신의 `Put`을 통해 인코딩되어 파라미터로 바인딩되므로, 값이 결코 SQL 텍스트가 되지 않으며 인젝션이 불가능합니다:

```scala
val minAge = 18
val frag   = sql"select id, name, age from users where age >= $minAge"
```

`Fragment`는 엔진의 `$1`, `$2`, … 플레이스홀더 형식으로 렌더링됩니다:

```scala
frag.sql    // "select id, name, age from users where age >= $1"
frag.params // Seq(NumberValue(18))
```

`Option`과 미리 빌드된 `Value`도 인터폴레이션할 수 있습니다 — `None`은 SQL `NULL`을 바인딩합니다.

### 프래그먼트 합성

프래그먼트는 렌더링되지 않은 채 저장되므로, `++`로 합성되며 플레이스홀더 번호를 자동으로 다시 매깁니다. `Fragment.const`는 원시 텍스트를(테이블 이름 같은 신뢰할 수 있는 식별자용), `Fragment.param`은 단일 바인딩 값을 기여합니다:

```scala
val table = "users"
val q = sql"select * from " ++ Fragment.const(table) ++ sql" where age >= $minAge"
```

## 쿼리 실행

`.query[A]`로 행 디코더를 부착한 다음, 결과를 수집할 방법을 선택합니다:

```scala
val users: Future[List[User]] =
  sql"select * from users order by id".query[User].toList

val one: Future[Option[User]] =
  sql"select * from users where id = ${1L}".query[User].option

val exactlyOne: Future[User] =
  sql"select * from users where id = ${1L}".query[User].unique
```

| 메서드 | 결과 | 비고 |
|---|---|---|
| `.toList` | `Future[List[A]]` | 모든 행 |
| `.toVector` | `Future[Vector[A]]` | 모든 행 |
| `.option` | `Future[Option[A]]` | 첫 번째 행 또는 `None` |
| `.unique` | `Future[A]` | 정확히 한 행; 그렇지 않으면 실패 |

`INSERT … RETURNING`도 `.query`를 통해 디코딩됩니다:

```scala
val inserted: Future[User] =
  sql"insert into users (name, age) values (${"alice"}, ${30}) returning *"
    .query[User].unique
```

### 스칼라 결과

`queryValue`로 첫 번째 행의 첫 번째 컬럼을 직접 읽습니다 — `count(*)`, `exists`, `max` 등에 사용합니다:

```scala
val total: Future[Long]        = sql"select count(*) from users".queryValue[Long]
val maybeMax: Future[Option[Int]] = sql"select max(age) from users".queryValueOption[Int]
```

## 문 실행

쿼리가 아닌 SQL의 경우, `.update`는 영향받은 행 수를 반환하고 `.run`은 원시 엔진 결과를 반환합니다:

```scala
val changed: Future[Int] =
  sql"update users set age = ${31} where id = ${1L}".update

val raw: Future[Seq[Result]] =
  sql"create table users (id serial primary key, name text, age integer)".run
```

## 리포지토리

`Repo[A, Id]`는 엔티티의 `Read`/`Write` 인스턴스에서 단일 테이블에 대한 CRUD를 생성합니다. 다루지 않는 것은 무엇이든 `sql"…"`을 사용합니다.

```scala
case class User(id: Long, name: String, age: Int) derives Read, Write

val users = Repo[User, Long]("users") // idColumn = "id", generatedId = true
```

기본적으로 `Repo`는 데이터베이스가 생성하는 키(`SERIAL`)를 가정하므로, `insert`/`insertReturning`은 id 컬럼을 생략하고 데이터베이스가 이를 할당합니다. 엔티티를 생성할 때 플레이스홀더 id를 전달하세요 — 삽입 시 무시됩니다:

```scala
val saved: Future[User] = users.insertReturning(User(0, "alice", 30))
// saved.id는 생성된 키입니다
```

### 작업

```scala
users.findById(1L)              // Future[Option[User]]
users.findAll                   // Future[List[User]]
users.count                     // Future[Long]
users.existsById(1L)            // Future[Boolean]

users.insert(User(0, "bob", 25))        // Future[Int] — 삽입된 행 수
users.insertReturning(User(0, "eve", 22)) // Future[User] — 생성된 id 복구

users.update(saved.copy(age = 31))      // Future[Int] — id가 일치하는 곳에서 id가 아닌 모든 컬럼을 설정
users.deleteById(1L)                    // Future[Int]
users.deleteAll                         // Future[Int]
```

### 호출자가 제공하는 키

기본 키를 애플리케이션이 제공하는(생성되지 않는) 테이블의 경우, `generatedId = false`로 설정하고 id 컬럼이 `"id"`가 아니라면 이름을 지정합니다:

```scala
case class Widget(sku: String, label: String) derives Read, Write

val widgets = Repo[Widget, String]("widgets", idColumn = "sku", generatedId = false)

widgets.insert(Widget("w-1", "Sprocket"))
widgets.findById("w-1")
```

## 크로스 플랫폼 참고 사항

Chisel은 단일 소스에서 JVM, Scala.js, Scala Native용으로 게시됩니다. 행 디코딩은 Scala 3 인라인 `Mirror` 파생을 사용하므로, 어떤 플랫폼에서도 런타임 리플렉션이 없습니다.

## 정리

```scala
summon[Session].close()
```

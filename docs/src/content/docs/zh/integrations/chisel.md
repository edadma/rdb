---
title: Chisel
description: 为 PetraDB 提供的类型化、跨平台 Scala 访问层 —— 比 ORM 更轻量。
---

Chisel 是用 Scala 3 编写的 PetraDB 类型化数据访问层。它将 `Value` 映射到 Scala 类型，使用字符串插值器构建注入安全的 SQL，并为表生成 CRUD —— 无需完整 ORM 的负担（没有标识映射，没有工作单元，没有惰性代理）。它能在 PetraDB 运行的所有地方运行：JVM、Node.js（Scala.js）和 Native，使用编译时派生而非运行时反射。

Chisel 通过绑定参数直接与引擎的 `Session` 通信，因此不需要 JDBC，也不会将任何值渲染进 SQL 文本。

## 安装

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-chisel" % "1.5.1"
```

Chisel 依赖引擎来获取会话：

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.5"
```

## 设置

一切都基于 `Session` 运行，并需要一个 `ExecutionContext`。Chisel 的操作返回 `Future`。

```scala
import io.github.edadma.petradb.Session
import io.github.edadma.petradb.engine.MemoryDB
import io.github.edadma.petradb.chisel.*

import scala.concurrent.ExecutionContext.Implicits.global

given Session = new MemoryDB().connect()
```

会话会隐式传递给每个 Chisel 操作，因此只需在作用域中有一个 `given Session` 即可。

## 类型映射

Chisel 通过四个类型类在 Scala 值和 PetraDB `Value` 之间进行映射：

| 类型类 | 方向 | 作用 |
|---|---|---|
| `Get[A]` | `Value => A` | 解码一列 |
| `Put[A]` | `A => Value` | 编码一个值（绑定参数） |
| `Read[A]` | `Row => A` | 解码整行 |
| `Write[A]` | `A => Seq[(String, Value)]` | 编码整行 |

### 内置列编解码器

为常见的标量类型提供了 `Get` 和 `Put` 实例：

| Scala 类型 | PetraDB 类型 |
|---|---|
| `Int`、`Long`、`Short`、`Byte` | 整数家族 |
| `Double`、`Float` | 浮点数 |
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
| `Option[A]` | 可空的 `A`（`None` ⇄ SQL `NULL`） |

类型不匹配的列会引发 `DecodeException`。

### case class 的行映射

为 case class 派生 `Read` 和 `Write` 以映射整行。派生是按字段名进行的，因此它对列重排序是健壮的，并且可以直接配合 `SELECT *` 使用：

```scala
case class User(id: Long, name: String, age: Int) derives Read, Write
```

`Option` 字段映射到可空列：

```scala
case class Account(id: Long, nickname: Option[String]) derives Read, Write
```

### 具名元组

任何 product 都可以派生，包括具名元组 —— 这对于无需声明类的临时投影很方便：

```scala
val r = Read.derived[(id: Long, name: String)]
```

### 适配编解码器

`Get`/`Read` 有 `.map`，`Put`/`Write` 有 `.contramap`，用于适配 Chisel 不认识的类型：

```scala
enum Color:
  case Red, Green, Blue

given Get[Color] = Get[String].map(Color.valueOf)
given Put[Color] = Put[String].contramap[Color](_.toString)
```

## `sql` 插值器

`sql"…"` 构建一个 `Fragment` —— SQL 文本加上绑定参数。每个被插入的参数都通过其 `Put` 进行编码并作为参数绑定，因此值永远不会变成 SQL 文本，注入是不可能的：

```scala
val minAge = 18
val frag   = sql"select id, name, age from users where age >= $minAge"
```

`Fragment` 渲染为引擎的 `$1`、`$2`、… 占位符形式：

```scala
frag.sql    // "select id, name, age from users where age >= $1"
frag.params // Seq(NumberValue(18))
```

`Option` 和预先构建的 `Value` 也可以被插入 —— `None` 绑定 SQL `NULL`。

### 组合片段

片段以未渲染的形式存储，因此它们可以用 `++` 组合并自动重新编号占位符。`Fragment.const` 贡献原始文本（用于受信任的标识符，如表名），`Fragment.param` 贡献单个绑定值：

```scala
val table = "users"
val q = sql"select * from " ++ Fragment.const(table) ++ sql" where age >= $minAge"
```

## 运行查询

用 `.query[A]` 附加行解码器，然后选择如何收集结果：

```scala
val users: Future[List[User]] =
  sql"select * from users order by id".query[User].toList

val one: Future[Option[User]] =
  sql"select * from users where id = ${1L}".query[User].option

val exactlyOne: Future[User] =
  sql"select * from users where id = ${1L}".query[User].unique
```

| 方法 | 结果 | 说明 |
|---|---|---|
| `.toList` | `Future[List[A]]` | 所有行 |
| `.toVector` | `Future[Vector[A]]` | 所有行 |
| `.option` | `Future[Option[A]]` | 第一行或 `None` |
| `.unique` | `Future[A]` | 恰好一行；否则失败 |

`INSERT … RETURNING` 同样通过 `.query` 解码：

```scala
val inserted: Future[User] =
  sql"insert into users (name, age) values (${"alice"}, ${30}) returning *"
    .query[User].unique
```

### 标量结果

用 `queryValue` 直接读取第一行的第一列 —— 用于 `count(*)`、`exists`、`max` 等：

```scala
val total: Future[Long]        = sql"select count(*) from users".queryValue[Long]
val maybeMax: Future[Option[Int]] = sql"select max(age) from users".queryValueOption[Int]
```

## 运行语句

对于非查询的 SQL，`.update` 返回受影响的行数，`.run` 返回原始引擎结果：

```scala
val changed: Future[Int] =
  sql"update users set age = ${31} where id = ${1L}".update

val raw: Future[Seq[Result]] =
  sql"create table users (id serial primary key, name text, age integer)".run
```

## 仓库

`Repo[A, Id]` 根据实体的 `Read`/`Write` 实例为单个表生成 CRUD。对于它未覆盖的任何内容，使用 `sql"…"`。

```scala
case class User(id: Long, name: String, age: Int) derives Read, Write

val users = Repo[User, Long]("users") // idColumn = "id", generatedId = true
```

默认情况下，`Repo` 假定使用数据库生成的键（`SERIAL`），因此 `insert`/`insertReturning` 会省略 id 列，由数据库进行分配。构造实体时传入一个占位 id —— 它在插入时会被忽略：

```scala
val saved: Future[User] = users.insertReturning(User(0, "alice", 30))
// saved.id 是生成的键
```

### 操作

```scala
users.findById(1L)              // Future[Option[User]]
users.findAll                   // Future[List[User]]
users.count                     // Future[Long]
users.existsById(1L)            // Future[Boolean]

users.insert(User(0, "bob", 25))        // Future[Int] — 插入的行数
users.insertReturning(User(0, "eve", 22)) // Future[User] — 取回生成的 id

users.update(saved.copy(age = 31))      // Future[Int] — 在 id 匹配处设置所有非 id 列
users.deleteById(1L)                    // Future[Int]
users.deleteAll                         // Future[Int]
```

### 调用方提供的键

对于主键由应用程序提供（而非生成）的表，设置 `generatedId = false`，如果 id 列不是 `"id"`，则为其命名：

```scala
case class Widget(sku: String, label: String) derives Read, Write

val widgets = Repo[Widget, String]("widgets", idColumn = "sku", generatedId = false)

widgets.insert(Widget("w-1", "Sprocket"))
widgets.findById("w-1")
```

## 跨平台说明

Chisel 从单一源代码为 JVM、Scala.js 和 Scala Native 发布。行解码使用 Scala 3 内联 `Mirror` 派生，因此在任何平台上都没有运行时反射。

## 清理

```scala
summon[Session].close()
```

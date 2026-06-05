---
title: Chisel
description: PetraDB用の型付きクロスプラットフォームScalaアクセスレイヤー — ORMより軽量です。
---

Chiselは、Scala 3で書かれたPetraDB用の型付きデータアクセスレイヤーです。`Value`をScala型にマッピングし、文字列インターポレーターでインジェクション安全なSQLを構築し、テーブルのCRUDを生成します — フルORMの重さ（アイデンティティマップ、ユニットオブワーク、遅延プロキシなし）を伴いません。実行時リフレクションではなくコンパイル時導出を使用し、PetraDBが動作するあらゆる場所で動作します: JVM、Node.js（Scala.js）、Native。

Chiselはバインドパラメーターを介してエンジンの`Session`と直接やり取りするため、JDBCを必要とせず、SQLテキストに値をレンダリングしません。

## インストール

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-chisel" % "1.5.1"
```

Chiselはセッションのためにエンジンに依存します。

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.5"
```

## セットアップ

すべては`Session`に対して実行され、`ExecutionContext`を必要とします。Chiselの操作は`Future`を返します。

```scala
import io.github.edadma.petradb.Session
import io.github.edadma.petradb.engine.MemoryDB
import io.github.edadma.petradb.chisel.*

import scala.concurrent.ExecutionContext.Implicits.global

given Session = new MemoryDB().connect()
```

セッションはすべてのChisel操作に暗黙的に渡されるため、スコープ内の`given Session`だけが必要です。

## 型のマッピング

Chiselは、4つの型クラスを介してScalaの値とPetraDBの`Value`の間をマッピングします。

| 型クラス | 方向 | 役割 |
|---|---|---|
| `Get[A]` | `Value => A` | 1カラムをデコード |
| `Put[A]` | `A => Value` | 1値をエンコード（パラメーターをバインド） |
| `Read[A]` | `Row => A` | 行全体をデコード |
| `Write[A]` | `A => Seq[(String, Value)]` | 行全体をエンコード |

### 組み込みカラムコーデック

一般的なスカラー型には`Get`と`Put`のインスタンスが提供されています。

| Scala型 | PetraDB型 |
|---|---|
| `Int`, `Long`, `Short`, `Byte` | 整数ファミリー |
| `Double`, `Float` | 浮動小数点 |
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
| `Option[A]` | NULLable `A`（`None` ⇄ SQLの`NULL`） |

型が一致しないカラムは`DecodeException`を発生させます。

### ケースクラスの行マッピング

行全体をマッピングするには、ケースクラスに対して`Read`と`Write`を導出します。導出はフィールド名によって行われるため、カラムの並び替えに対して堅牢で、`SELECT *`と直接連携します。

```scala
case class User(id: Long, name: String, age: Int) derives Read, Write
```

`Option`フィールドはNULLableカラムにマッピングされます。

```scala
case class Account(id: Long, nickname: Option[String]) derives Read, Write
```

### 名前付きタプル

名前付きタプルを含む任意のプロダクトが導出されます — クラスを宣言せずにアドホックなプロジェクションを行うのに便利です。

```scala
val r = Read.derived[(id: Long, name: String)]
```

### コーデックの適応

`Get`/`Read`には`.map`があり、`Put`/`Write`には`.contramap`があり、Chiselが知らない型に適応させます。

```scala
enum Color:
  case Red, Green, Blue

given Get[Color] = Get[String].map(Color.valueOf)
given Put[Color] = Put[String].contramap[Color](_.toString)
```

## `sql`インターポレーター

`sql"…"`は`Fragment`を構築します — SQLテキストとバインドされたパラメーターです。各インターポレートされた引数はその`Put`を介してエンコードされ、パラメーターとしてバインドされるため、値がSQLテキストになることはなく、インジェクションは不可能です。

```scala
val minAge = 18
val frag   = sql"select id, name, age from users where age >= $minAge"
```

`Fragment`はエンジンの`$1`、`$2`、…というプレースホルダー形式にレンダリングされます。

```scala
frag.sql    // "select id, name, age from users where age >= $1"
frag.params // Seq(NumberValue(18))
```

`Option`と事前構築された`Value`もインターポレートできます — `None`はSQLの`NULL`をバインドします。

### フラグメントの合成

フラグメントはレンダリングされない状態で保存されるため、`++`で合成され、プレースホルダーが自動的に再番号付けされます。`Fragment.const`は生のテキスト（テーブル名のような信頼できる識別子用）を提供し、`Fragment.param`は単一のバインド値を提供します。

```scala
val table = "users"
val q = sql"select * from " ++ Fragment.const(table) ++ sql" where age >= $minAge"
```

## クエリの実行

`.query[A]`で行デコーダーをアタッチし、結果の収集方法を選択します。

```scala
val users: Future[List[User]] =
  sql"select * from users order by id".query[User].toList

val one: Future[Option[User]] =
  sql"select * from users where id = ${1L}".query[User].option

val exactlyOne: Future[User] =
  sql"select * from users where id = ${1L}".query[User].unique
```

| メソッド | 結果 | 備考 |
|---|---|---|
| `.toList` | `Future[List[A]]` | すべての行 |
| `.toVector` | `Future[Vector[A]]` | すべての行 |
| `.option` | `Future[Option[A]]` | 最初の行または`None` |
| `.unique` | `Future[A]` | ちょうど1行; それ以外は失敗 |

`INSERT … RETURNING`も`.query`を介してデコードします。

```scala
val inserted: Future[User] =
  sql"insert into users (name, age) values (${"alice"}, ${30}) returning *"
    .query[User].unique
```

### スカラー結果

`queryValue`で最初の行の最初のカラムを直接読み取ります — `count(*)`、`exists`、`max`などに使用します。

```scala
val total: Future[Long]        = sql"select count(*) from users".queryValue[Long]
val maybeMax: Future[Option[Int]] = sql"select max(age) from users".queryValueOption[Int]
```

## ステートメントの実行

クエリ以外のSQLには、`.update`が影響を受けた行数を返し、`.run`が生のエンジン結果を返します。

```scala
val changed: Future[Int] =
  sql"update users set age = ${31} where id = ${1L}".update

val raw: Future[Seq[Result]] =
  sql"create table users (id serial primary key, name text, age integer)".run
```

## リポジトリ

`Repo[A, Id]`は、エンティティの`Read`/`Write`インスタンスから単一テーブルのCRUDを生成します。カバーされていないものには`sql"…"`を使用します。

```scala
case class User(id: Long, name: String, age: Int) derives Read, Write

val users = Repo[User, Long]("users") // idColumn = "id", generatedId = true
```

デフォルトでは`Repo`はデータベース生成のキー（`SERIAL`）を想定するため、`insert`/`insertReturning`はidカラムを省略し、データベースがそれを割り当てます。エンティティを構築する際にプレースホルダーidを渡します — それはinsert時に無視されます。

```scala
val saved: Future[User] = users.insertReturning(User(0, "alice", 30))
// saved.id は生成されたキーです
```

### 操作

```scala
users.findById(1L)              // Future[Option[User]]
users.findAll                   // Future[List[User]]
users.count                     // Future[Long]
users.existsById(1L)            // Future[Boolean]

users.insert(User(0, "bob", 25))        // Future[Int] — 挿入された行数
users.insertReturning(User(0, "eve", 22)) // Future[User] — 生成されたidを復元

users.update(saved.copy(age = 31))      // Future[Int] — idが一致する箇所のid以外の全カラムを設定
users.deleteById(1L)                    // Future[Int]
users.deleteAll                         // Future[Int]
```

### 呼び出し側が指定するキー

主キーがアプリケーションによって提供される（生成されない）テーブルの場合、`generatedId = false`を設定し、idカラムが`"id"`でない場合は名前を指定します。

```scala
case class Widget(sku: String, label: String) derives Read, Write

val widgets = Repo[Widget, String]("widgets", idColumn = "sku", generatedId = false)

widgets.insert(Widget("w-1", "Sprocket"))
widgets.findById("w-1")
```

## クロスプラットフォームに関する注意

Chiselは、単一のソースからJVM、Scala.js、Scala Native向けに公開されています。行のデコードはScala 3のインライン`Mirror`導出を使用するため、どのプラットフォームでも実行時リフレクションはありません。

## クリーンアップ

```scala
summon[Session].close()
```

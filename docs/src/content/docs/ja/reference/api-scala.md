---
title: Scala
description: PetraDBのScala APIリファレンスです。
---

## インストール

`build.sbt`に追加します。

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.0"
```

## パッケージ構成

PetraDBは2つのパッケージに分かれています。

- **`io.github.edadma.petradb`** — 共有型（`Result`、`Value`、`Row`、`TableValue`、`Session`トレイト）
- **`io.github.edadma.petradb.engine`** — データベースエンジン（`MemoryDB`、`PersistentDB`、`TextDB`、`Session`、`executeSQL`）

エンジンを直接使用するには両方をインポートします。

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*
```

## インメモリデータベース

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

given Session = new MemoryDB().connect()
```

## 永続データベース

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

// 新規作成
val db = PersistentDB.create("path/to/db", pageSize = 4096)
given Session = db.connect()

// 既存を開く
val db = PersistentDB.open("path/to/db")
given Session = db.connect()

// 完了時にクローズ
db.close()
```

## テキストデータベース

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

val db = TextDB.open("path/to/data.ptxt")
given Session = db.connect()

db.close()
```

人間が読める`.ptxt`ファイルです。開くとメモリにロードされ、変更のたびに書き換えます。JVMとNativeで動作します。

## SQLの実行

### `executeSQL(sql: String)(using Session): Seq[Result]`

1つ以上のセミコロン区切りのSQL文を実行し、結果のシーケンスを返します。

```scala
val results: Seq[Result] = executeSQL("SELECT * FROM users")
```

## 結果型

```scala
sealed trait Result
case class QueryResult(table: TableValue)                           extends Result
case class InsertResult(obj: Map[String, Value], table: TableValue) extends Result
case class CreateTableResult(table: String)                         extends Result
case class DropTableResult(table: String)                           extends Result
case class CreateIndexResult(name: String)                          extends Result
case class DropIndexResult(name: String)                            extends Result
case class CreateTypeResult(typ: String)                            extends Result
case class DropTypeResult(name: String)                             extends Result
case class CreateViewResult(name: String)                           extends Result
case class DropViewResult(name: String)                             extends Result
case class CreateSequenceResult(name: String)                       extends Result
case class DropSequenceResult(name: String)                         extends Result
case class UpdateResult(rows: Int)                                  extends Result
case class DeleteResult(rows: Int)                                  extends Result
case class TruncateResult(table: String)                            extends Result
case class AlterTableResult()                                       extends Result
case class ExplainResult(plan: String)                              extends Result
case class PrepareResult(name: String)                              extends Result
case class DeallocateResult(name: String)                           extends Result
case class CopyResult(rows: Int)                                    extends Result
case class CreateSchemaResult(name: String)                         extends Result
case object BeginResult                                             extends Result
case object CommitResult                                            extends Result
case object RollbackResult                                          extends Result
case object DoBlockResult                                           extends Result
case class CreateFunctionResult(name: String)                       extends Result
case class DropFunctionResult(name: String)                         extends Result
case class CreateProcedureResult(name: String)                      extends Result
case class DropProcedureResult(name: String)                        extends Result
case class CreateTriggerResult(name: String)                        extends Result
case class DropTriggerResult(name: String)                          extends Result
case object CallResult                                              extends Result
```

## クエリデータへのアクセス

```scala
val QueryResult(table) = executeQuery("SELECT * FROM users")

// 行へのアクセス
val rows: IndexedSeq[Row] = table.data

for (row <- table.data) {
  val id: Int = row.getInt("id")
  val name: String = row.getString("name")
  val email: Option[String] = row.getStringOption("email")
}
```

## ユーザー定義関数

SQLから呼び出し可能なネイティブScala関数を登録します。トリガーやストアドプロシージャからも使用できます。

```scala
db.registerScalarFunction("my_double", {
  case Seq(v) => NumberValue(v.intValue * 2)
}, NumberType)

// SQLで使用可能に:
// SELECT my_double(age) FROM users;
```

この方法で登録された関数はどこでも動作します：`SELECT`、`WHERE`、`DO`ブロック、ストアド関数、トリガー。

## 値の抽出

```scala
val row: Row = table.data.head

// 型安全な抽出
val id: Int = row.getInt("id")
val name: String = row.getString("name")
val email: Option[String] = row.getStringOption("email")
val isActive: Boolean = row.getBoolean("is_active")

// 直接アクセス
val value: Value = row("column_name")
```

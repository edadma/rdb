---
title: Scala
description: PetraDB 的 Scala API 参考。
---

## 安装

添加到你的 `build.sbt`：

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.0"
```

## 包结构

PetraDB 分为两个包：

- **`io.github.edadma.petradb`** — 共享类型（`Result`、`Value`、`Row`、`TableValue`、`Session` trait）
- **`io.github.edadma.petradb.engine`** — 数据库引擎（`MemoryDB`、`PersistentDB`、`TextDB`、`Session`、`executeSQL`）

导入两者以直接使用引擎：

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*
```

## 内存数据库

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

given Session = new MemoryDB().connect()
```

## 持久化数据库

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

// 创建新数据库
val db = PersistentDB.create("path/to/db", pageSize = 4096)
given Session = db.connect()

// 重新打开已有数据库
val db = PersistentDB.open("path/to/db")
given Session = db.connect()

// 完成后关闭
db.close()
```

## 文本数据库

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

val db = TextDB.open("path/to/data.ptxt")
given Session = db.connect()

db.close()
```

人类可读的 `.ptxt` 文件。打开时加载到内存，每次更改后重写。适用于 JVM 和 Native。

## 执行 SQL

### `executeSQL(sql: String)(using Session): Seq[Result]`

执行一个或多个以分号分隔的 SQL 语句，返回结果序列。

```scala
val results: Seq[Result] = executeSQL("SELECT * FROM users")
```

## 结果类型

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

## 访问查询数据

```scala
val QueryResult(table) = executeQuery("SELECT * FROM users")

// 访问行
val rows: IndexedSeq[Row] = table.data

for (row <- table.data) {
  val id: Int = row.getInt("id")
  val name: String = row.getString("name")
  val email: Option[String] = row.getStringOption("email")
}
```

## 用户自定义函数

注册可从 SQL、触发器和存储过程中调用的原生 Scala 函数：

```scala
db.registerScalarFunction("my_double", {
  case Seq(v) => NumberValue(v.intValue * 2)
}, NumberType)

// 现在可以在 SQL 中使用：
// SELECT my_double(age) FROM users;
```

以这种方式注册的函数可在所有位置使用：`SELECT`、`WHERE`、`DO` 块、存储函数和触发器。

## 值提取

```scala
val row: Row = table.data.head

// 类型安全提取
val id: Int = row.getInt("id")
val name: String = row.getString("name")
val email: Option[String] = row.getStringOption("email")
val isActive: Boolean = row.getBoolean("is_active")

// 直接访问
val value: Value = row("column_name")
```

---
title: Scala
description: PetraDB의 Scala API 레퍼런스.
---

## 설치

`build.sbt`에 추가합니다:

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.6"
```

## 패키지 구조

PetraDB는 두 패키지로 나뉩니다:

- **`io.github.edadma.petradb`** — 공유 타입 (`Result`, `Value`, `Row`, `TableValue`, `Session` 트레이트)
- **`io.github.edadma.petradb.engine`** — 데이터베이스 엔진 (`MemoryDB`, `PersistentDB`, `TextDB`, `Session`, `executeSQL`)

엔진을 직접 사용하려면 둘 다 가져옵니다:

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*
```

## 인메모리 데이터베이스

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

given Session = new MemoryDB().connect()
```

## 영구 데이터베이스

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

// 새로 생성
val db = PersistentDB.create("path/to/db", pageSize = 4096)
given Session = db.connect()

// 기존 열기
val db = PersistentDB.open("path/to/db")
given Session = db.connect()

// 완료 시 닫기
db.close()
```

## 텍스트 데이터베이스

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

val db = TextDB.open("path/to/data.ptxt")
given Session = db.connect()

db.close()
```

사람이 읽을 수 있는 `.ptxt` 파일. 열 때 메모리에 로드하고 변경 후마다 다시 작성합니다. JVM과 Native에서 작동합니다.

## SQL 실행

### `executeSQL(sql: String)(using Session): Seq[Result]`

세미콜론으로 구분된 하나 이상의 SQL 문을 실행하고 결과 시퀀스를 반환합니다.

```scala
val results: Seq[Result] = executeSQL("SELECT * FROM users")
```

## 결과 타입

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

## 쿼리 데이터 접근

```scala
val QueryResult(table) = executeQuery("SELECT * FROM users")

// 행 접근
val rows: IndexedSeq[Row] = table.data

for (row <- table.data) {
  val id: Int = row.getInt("id")
  val name: String = row.getString("name")
  val email: Option[String] = row.getStringOption("email")
}
```

## 사용자 정의 함수

SQL, 트리거, 저장 프로시저에서 호출 가능한 네이티브 Scala 함수를 등록합니다:

```scala
db.registerScalarFunction("my_double", {
  case Seq(v) => NumberValue(v.intValue * 2)
}, NumberType)

// SQL에서 사용 가능:
// SELECT my_double(age) FROM users;
```

이 방식으로 등록된 함수는 모든 곳에서 작동합니다: `SELECT`, `WHERE`, `DO` 블록, 저장 함수, 트리거.

## 값 추출

```scala
val row: Row = table.data.head

// 타입 안전 추출
val id: Int = row.getInt("id")
val name: String = row.getString("name")
val email: Option[String] = row.getStringOption("email")
val isActive: Boolean = row.getBoolean("is_active")

// 직접 접근
val value: Value = row("column_name")
```

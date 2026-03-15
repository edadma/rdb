---
title: Scala
description: Referencia da API Scala para PetraDB.
---

## Instalacao

Adicione ao seu `build.sbt`:

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.0"
```

## Estrutura de Pacotes

O PetraDB e dividido em dois pacotes:

- **`io.github.edadma.petradb`** — tipos compartilhados (`Result`, `Value`, `Row`, `TableValue`, trait `Session`)
- **`io.github.edadma.petradb.engine`** — o engine do banco de dados (`MemoryDB`, `PersistentDB`, `TextDB`, `Session`, `executeSQL`)

Importe ambos para usar o engine diretamente:

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*
```

## Banco de Dados em Memoria

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

given Session = new MemoryDB().connect()
```

## Banco de Dados Persistente

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

// Criar novo
val db = PersistentDB.create("path/to/db", pageSize = 4096)
given Session = db.connect()

// Reabrir existente
val db = PersistentDB.open("path/to/db")
given Session = db.connect()

// Fechar quando terminar
db.close()
```

## Banco de Dados de Texto

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

val db = TextDB.open("path/to/data.ptxt")
given Session = db.connect()

db.close()
```

Arquivo `.ptxt` legivel por humanos. Carrega na memoria ao abrir, reescreve apos cada alteracao. Funciona em JVM e Native.

## Executando SQL

### `executeSQL(sql: String)(using Session): Seq[Result]`

Executa um ou mais comandos SQL separados por ponto e virgula e retorna uma sequencia de resultados.

```scala
val results: Seq[Result] = executeSQL("SELECT * FROM users")
```

## Tipos de Resultado

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

## Acessando Dados de Consulta

```scala
val QueryResult(table) = executeQuery("SELECT * FROM users")

// Acessar linhas
val rows: IndexedSeq[Row] = table.data

for (row <- table.data) {
  val id: Int = row.getInt("id")
  val name: String = row.getString("name")
  val email: Option[String] = row.getStringOption("email")
}
```

## Funcoes Definidas pelo Usuario

Registre funcoes Scala nativas chamaveis a partir de SQL, triggers e stored procedures:

```scala
db.registerScalarFunction("my_double", {
  case Seq(v) => NumberValue(v.intValue * 2)
}, NumberType)

// Agora utilizavel em SQL:
// SELECT my_double(age) FROM users;
```

Funcoes registradas dessa forma funcionam em todos os lugares: `SELECT`, `WHERE`, blocos `DO`, funcoes armazenadas e triggers.

## Extracao de Valores

```scala
val row: Row = table.data.head

// Extracao tipada
val id: Int = row.getInt("id")
val name: String = row.getString("name")
val email: Option[String] = row.getStringOption("email")
val isActive: Boolean = row.getBoolean("is_active")

// Acesso direto
val value: Value = row("column_name")
```
